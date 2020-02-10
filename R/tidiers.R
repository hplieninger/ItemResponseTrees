#' Glance at an irtree_fit object.
#'
#' Glance accepts an `irtree_fit` object and returns a [tibble][tibble::tibble-package]
#'   with exactly one row of model summaries.
#'
#' @section Converged: The column `converged` indicates whether the model
#'   converged or not. For Mplus, this is `TRUE` if the output contained the
#'   phrase "The model estimation terminated normally". For mirt, this is equal
#'   to the output of [`mirt::extract.mirt(x,
#'   "converged")`][mirt::extract.mirt]. You are encouraged to check any warnings
#'   or errors in any case.
#'
#' @section Iterations: `iterations` is `NA` for Mplus models since respective
#'   information is not easily obtained from the output.
#'
#' @param x object of class `irtree_fit` as returned from [`fit()`][fit.irtree_model].
#' @param ... Additional arguments. Not used.
#' @return A one-row [tibble][tibble::tibble-package] with columns such as `AIC` and `BIC`.
#' @example inst/examples/example-fit.R
#' @seealso [broom::glance()], [`mirt::extract.mirt(x,
#'   "secondordertest")`][mirt::extract.mirt]
#' @export
glance.irtree_fit <- function(x = NULL, ...) {

    # ellipsis::check_dots_used()
    checkmate::qassert(x$spec$engine, "S1")
    engine <- match.arg(x$spec$engine, c("mplus", "mirt", "tam"))

    if (is.null(x[[engine]])) {
        return(tibble::tibble())
    } else if (engine == "mplus") {
        out <- tibble::as_tibble(x$mplus$summaries)
        out$converged <- x$mplus$converged
        names(out) <-
            stringr::str_replace_all(
                names(out),
                c("AICC" = "AICc",
                  "LL" = "logLik",
                  "NContinuousLatentVars" = "n.factors",
                  "Observations" = "nobs",
                  "Parameters" = "npar",
                  "NGroups" = "ngroups",
                  "Estimator" = "estimator"))

        out$iterations <- NA_integer_

    } else if (engine == "mirt") {
        out <- mirt::anova(x$mirt)
        out$converged <- mirt::extract.mirt(x$mirt, "converged")
        out$iterations <- mirt::extract.mirt(x$mirt, "iterations")
        out$estimator <- mirt::extract.mirt(x$mirt, "method")
        out$npar <- mirt::extract.mirt(x$mirt, "nest")
        out$nobs <- x$mirt@Data$N
        out$n.factors <- mirt::extract.mirt(x$mirt, "nfact")
        out$ngroups <- mirt::extract.mirt(x$mirt, "ngroups")
        out <- tibble::as_tibble(out)
    } else if (engine == "tam") {
        out <- data.frame(AIC = x$tam$ic$AIC)
        out$BIC <- x$tam$ic$BIC
        out$AICc <- x$tam$ic$AICc

        out$converged <- NA
        tmp1 <- diff(x$tam$deviance.history)
        tmp2 <- utils::tail(tmp1[, "deviance", drop = TRUE], 5)
        if (x$tam$iter == x$tam$control$maxiter) {
            out$converged <- FALSE
        } else if (any(tmp2 >= 0)) {
            out$converged <- FALSE

        }

        out$logLik <- x$tam$ic$loglike
        out$iterations <- x$tam$iter
        out$estimator <- NA_character_
        out$npar <- x$tam$ic$np
        out$nobs <- x$tam$ic$n
        out$n.factors <- x$tam$ndim
        out$ngroups <- x$tam$groups
        out <- tibble::as_tibble(out)
    }

    out <-
        dplyr::select(
            out,
            tidyselect::vars_select(
                names(out),
                "AIC", "BIC", "AICc", "logLik", "converged",
                "iterations",
                "estimator",
                "npar",
                "nobs",
                "n.factors",
                "ngroups",
                .strict = FALSE)) %>%
        dplyr::mutate_if(rlang::is_integerish, as.integer)

    checkmate::assert_tibble(out, nrows = 1)

    return(out)
}

#' Tidy an irtree_fit object.
#'
#' Tidy summarizes information about the parameter estimates of the IR-tree model.
#'
#' @param x object of class irtree_fit as returned from  [`fit()`][fit.irtree_model].
#' @param ... Additional arguments.
#' @return A [tibble][tibble::tibble-package] with one row for each model
#'   parameter and the following columns:
#' \describe{
#'   \item{`term`}{The name of the model parameter.}
#'   \item{`estimate`}{The estimated value of the term.}
#'   \item{`std.error`}{The standard error of the term.}
#'   \item{`statistic`}{The value of the test statistic of the term (Mplus only).}
#'   \item{`p.value`}{The p-value associated with the statistic.}
#' }
#' @example inst/examples/example-fit.R
#' @seealso [broom::tidy()]
#' @export
tidy.irtree_fit <- function(x = NULL, ...) {
    # ellipsis::check_dots_used()
    engine <- match.arg(x$spec$engine, c("mplus", "mirt", "tam"))

    if (is.null(x[[engine]])) {
        return(tibble::tibble())
    } else if (engine == "mplus") {
        out <- tidy_mplus(x)
    } else if (engine == "mirt") {
        out <- tidy_mirt(x, ...)
    } else if (engine == "tam") {
        out <- .tidy_tam(x)
    }
    return(out)
}

#' Tidy an irtree_fit object estimated using mirt.
#'
#' Tidy summarizes information about the parameter estimates of the IR-tree model.
#'
#' @param difficulty Logical. The [mirt][mirt::mirt-package] package uses easiness
#'   parameters. These are returned with a message if `difficulty = NA` (the
#'   default), without a message if `difficulty = FALSE`, and they are
#'   transformed to difficulty parameters if `difficulty = TRUE`.
#' @inheritParams tidy.irtree_fit
#' @inherit tidy.irtree_fit return description
#' @seealso [`tidy()`][tidy.irtree_fit()], [broom::tidy()]
tidy_mirt <- function(x = NULL, difficulty = NA) {

    checkmate::qassert(difficulty, "b1")

    f1 <- function(x) {
        tidyr::separate(x, .data$term, into = c("i", "p"), sep = "[.]") %>%
            dplyr::arrange(.data$p) %>%
            tidyr::unite("term", .data$i, .data$p, sep = ".")
    }

    est1 <- mirt::coef(x$mirt, printSE = TRUE, as.data.frame = TRUE) %>%
        as.data.frame %>%
        tibble::rownames_to_column(var = "term") %>%
        tibble::as_tibble() %>%
        {if ("SE" %in% names(.)) . else dplyr::mutate(., SE = NA_real_)} %>%
        dplyr::select("term", estimate = "par", std.error = .data$SE) %>%
        dplyr::filter(!grepl("logit", .data$term)) %>%
        dplyr::filter(!grepl("[.]u|g$", .data$term)) %>%
        dplyr::filter(!(grepl("[.]a\\d+$", .data$term) &
                            .data$estimate == 0 &
                            is.na(.data$std.error))) %>%
        dplyr::mutate(group = grepl("GroupPars", .data$term),
                      term = sub("GroupPars[.]", "", .data$term))

    if (is.na(difficulty)) {
        message("The mirt package returns easiness (instead of difficulty) ",
                "parameters and so does tidy(). Use\n",
                "tidy(x, difficulty = TRUE)    to change this, or\n",
                "tidy(x, difficulty = FALSE)   to silence this message.")
    } else if (difficulty) {
        est1 <- dplyr::mutate(
            est1, estimate = unlist(
                purrr::map_if(.data$estimate,
                              grepl("[.]d$", .data$term), ~-.x)))
    }

    est2 <- tidyr::nest(est1, data = c("term", "estimate", "std.error"))

    est3 <- est2 %>%
        dplyr::mutate(data = purrr::map_if(.data$data, !.data$group, f1)) %>%
        tidyr::unnest(cols = .data$data) %>%
        dplyr::select(-"group")

    est4 <- mirt::coef(x$mirt, simplify = TRUE)$cov %>%
        cov2cor %>%
        {.[lower.tri(., diag = TRUE)] <- NA; return(.)} %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        tidyr::pivot_longer(cols = -.data$rowname, values_to = "estimate") %>%
        na.omit %>%
        tidyr::unite(col = "term", .data$rowname, .data$name, sep = ".") %>%
        dplyr::mutate(term = paste0("COR_", .data$term))

    out <- dplyr::bind_rows(est3, est4) %>%
        dplyr::mutate(effect = ifelse(grepl("^(MEAN_|COV_|COR_)", .data$term), "ran_pars", "fixed")) %>%
        dplyr::select(.data$effect, dplyr::everything()) %>%
        dplyr::arrange(.data$effect)

    return(out)
}

tidy_mplus <- function(x = NULL) {

    x1 <- MplusAutomation:::coef.mplus.model(x$mplus, type = "un")
    tmp1 <- sub(".standardized", "",
                grep("[.]standardized", names(x$mplus$parameters), value = T))

    f1 <-
        purrr::possibly(
            ~ MplusAutomation:::coef.mplus.model(.x, type = tmp1, params = "undirected") %>%
                dplyr::mutate(Label = paste0("COR_", trimws(.data$Label))),
            otherwise = dplyr::slice(x1, 0))

    x2 <- f1(x$mplus)

    out <- dplyr::bind_rows(x1, x2) %>%
        dplyr::mutate(effect = ifelse(grepl("<->", .data$Label), "ran_pars", "fixed")) %>%
        dplyr::select("effect", term = "Label", estimate = "est", std.error = "se",
                      p.value = "pval") %>%
        dplyr::mutate(tmp1 = .data$std.error == 0 & .data$p.value == 999,
                      std.error = ifelse(.data$tmp1, NA, .data$std.error),
                      p.value = ifelse(.data$tmp1, NA, .data$p.value),
                      tmp1 = NULL,
                      term = trimws(.data$term)) %>%
        dplyr::arrange(.data$effect) %>%
        tibble::as_tibble()
    return(out)
}

.tidy_tam <- function(x = NULL) {
    xsi <- x$tam$xsi
    names(xsi) <- c("estimate", "std.error")
    out1 <- data.frame(
        effect = "fixed",
        tibble::as_tibble(xsi, rownames = "term"))

    varx <- x$tam$variance
    corx <- cov2cor(varx)
    tmp1 <- unlist(as.data.frame(varx, row.names = FALSE))
    tmp2 <- tmp1[as.vector(lower.tri(varx, TRUE))]
    names(tmp2) <- sub("V", "COV_", names(tmp2))

    tmp3 <- unlist(as.data.frame(corx, row.names = FALSE))
    tmp4 <- tmp3[as.vector(lower.tri(corx))]
    names(tmp4) <- sub("V", "COR_", names(tmp4))

    out2 <- data.frame(
        effect = "ran_pars",
        tibble::enframe(c(tmp2, tmp4), "term", "estimate"),
        std.error = NA_real_)

    out <- tibble::as_tibble(rbind(out1, out2))
    return(out)
}

#' Augment data with information from an irtree_fit object.
#'
#' @description Augment accepts a model object and a dataset and adds
#'   information about each observation in the dataset, namely, predicted values
#'   in the .fitted column. New columns always begin with a . prefix to avoid
#'   overwriting columns in the original dataset.
#'
#' @details Note that `method` is used only for objects returned from
#'   `irtree_fit(engine = "mirt")`.
#'
#' @param x object of class irtree_fit as returned from  [`fit()`][fit.irtree_model].
#' @param data Optional data frame that is returned together with the predicted
#'   values. Argument is not needed since the data are contained in the fitted
#'   object.
#' @param se.fit Logical indicating whether standard errors for the fitted
#'   values should be returned as well.
#' @param method This is passed to [mirt::fscores()] or
#'   [`TAM:::IRT.factor.scores()`][TAM::IRT.factor.scores.tam.mml] (as argument
#'   `type`) if applicable.
#' @param ... Additional arguments passed to [mirt::fscores()] or
#'   [`TAM:::IRT.factor.scores()`][TAM::IRT.factor.scores.tam.mml] if
#'   applicable.
#' @return Returns a [tibble][tibble::tibble-package] with one row for each
#'   observation and one (two) additional columns for each latent variable if
#'   `se.fit = FALSE` (if `se.fit = TRUE`). The names of the new columns start
#'   with `.fit` (and `.se.fit`).
#' @example inst/examples/example-fit.R
#' @seealso [broom::augment()]
#' @export
augment.irtree_fit <- function(x = NULL,
                               data = NULL,
                               se.fit = TRUE,
                               method = "EAP",
                               ...) {
    # ellipsis::check_dots_used()
    engine <- match.arg(x$spec$engine, c("mplus", "mirt", "tam"))

    if (is.null(data)) {
        data <- x$spec$data
    }

    if (is.null(x[[engine]])) {
        return(tibble::as_tibble(data))
    } else if (engine == "mplus") {
        if (!x$spec$save_fscores) {
            return(tibble::as_tibble(data))
        }
        if (!checkmate::test_string(method, na.ok = TRUE,
                                    pattern = "^EAP$", null.ok = TRUE)) {
            warning("Argument 'method' is only implemented for 'engine = \"mirt\".'", call. = FALSE)
        }

        out <- tryCatch({

            tmp1 <- 2*x$mplus$summaries$NContinuousLatentVars

            fscores <- as.data.frame(x$mplus[["savedata"]])
            est <- dplyr::select(fscores,
                                 ncol(fscores) - seq(to = 1, by = -2,
                                                     length.out = tmp1/2))
            se <- dplyr::select(fscores,
                                ncol(fscores) - seq(to = 0, by = -2,
                                                    length.out = tmp1/2))
            names(est) <- paste0(".fitted", names(est))
            names(se) <- paste0(".se.fit",
                                sub("_SE$", "", names(se)))
            if (se.fit) {
                out <- cbind(data, est, se)
            } else {
                out <- cbind(data, est)
            }
            return(tibble::as_tibble(out))
        },
        error = function(cond) {
            warning(cond)
            return(tibble::as_tibble(data))
        })

        return(out)

    } else if (engine == "mirt") {

        out <- tryCatch(
            {
                tmp1 <- augment_mirt(x = x, se.fit = se.fit, method = method, ...)
                tibble::as_tibble(cbind(data, tmp1))
            },
            error = function(cond) {
                warning(cond)
                return(tibble::as_tibble(data))
            }
        )
        return(out)
    } else if (engine == "tam") {

        out <- tryCatch(
            {
                tmp1 <- TAM:::IRT.factor.scores.tam.mml(x$tam, type = method, ...)
                names(tmp1) <- sub("^EAP(.*)", ".fitted\\1", names(tmp1))
                names(tmp1) <- sub("^SD.EAP(.*)", ".se.fit\\1", names(tmp1))

                tibble::as_tibble(cbind(data, tmp1[order(names(tmp1))]))
            },
            error = function(cond) {
                warning(cond)
                return(tibble::as_tibble(data))
            }
        )
        return(out)
    }
}

augment_mirt <- function(x, se.fit = TRUE, method = "EAP", ...) {

    out <- as.data.frame(
        mirt::fscores(x$mirt,
                      method = method,
                      full.scores = TRUE,
                      append_response.pattern = FALSE,
                      returnER = FALSE,
                      return.acov = FALSE,
                      full.scores.SE = se.fit,
                      ...))

    names(out) <- paste0(".fitted", names(out))
    if (se.fit) {
        names(out) <- sub("^.fittedSE_", ".se.fit", names(out))
    }

    return(out)
}

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics augment
#' @export
generics::augment
