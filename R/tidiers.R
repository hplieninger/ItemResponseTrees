#' Glance at an irtree_fit object
#'
#' Glance accepts an `irtree_fit` object and returns a [tibble][tibble::tibble-package]
#'   with exactly one row of model summaries.
#'
#' @return A one-row [tibble][tibble::tibble-package] with columns such as `AIC` and `BIC`.
#' ## Converged
#' The column `converged` indicates whether the model converged or not. For
#' Mplus, this is `TRUE` if the output contained the phrase "The model
#' estimation terminated normally". For mirt, this is equal to the output of
#' [`mirt::extract.mirt(x, "converged")`][mirt::extract.mirt]. For TAM, this is
#' `NA` if no clear signs of non-convergence were observed. You are encouraged
#' to check any warnings or errors in any case.
#'
#' ## Iterations
#' `iterations` is `NA` for Mplus models since respective
#'   information is not easily obtained from the output.
#'
#' @param x object of class `irtree_fit` as returned from [`fit()`][fit.irtree_model].
#' @param ... Additional arguments. Not used.
#' @example inst/examples/example-tidiers.R
#' @seealso [generics::glance()], [`mirt::extract.mirt(x,
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

    glance_cols <- c("AIC", "BIC", "AICc", "logLik", "converged", "iterations",
                     "estimator", "npar", "nobs", "n.factors", "ngroups")

    out <- out[names(out)[na.omit(
        match(glance_cols, names(out)))]]

    out <- dplyr::mutate_if(out, rlang::is_integerish, as.integer)

    checkmate::assert_tibble(out, nrows = 1)

    return(out)
}

#' Tidy an irtree_fit object
#'
#' Tidy summarizes information about the parameter estimates of an ItemResponseTrees model.
#'
#' @param x object of class `irtree_fit` as returned from  [`fit()`][fit.irtree_model].
#' @param par_type Only used if the fit engine was mirt. Item parameters (or
#'   thresholds) can be either of type `easiness` (the mirt default) or
#'   `difficulty` (as in Mplus and TAM).
#' @param ... Not currently used.
#' @return A [tibble][tibble::tibble-package] with one row for each model
#'   parameter and the following columns:
#' \describe{
#'   \item{`term`}{The name of the model parameter.}
#'   \item{`estimate`}{The estimated value of the term.}
#'   \item{`std.error`}{The standard error of the term.}
#'   \item{`statistic`}{The value of the test statistic of the term (Mplus only).}
#'   \item{`p.value`}{The p-value associated with the statistic (Mplus only).}
#' }
#' @example inst/examples/example-tidiers.R
#' @seealso [generics::tidy()]
#' @export
tidy.irtree_fit <- function(x = NULL, par_type = NULL, ...) {
    engine <- match.arg(x$spec$engine, c("mplus", "mirt", "tam"))

    if (is.null(x[[engine]])) {
        return(tibble::tibble())
    } else if (engine == "mplus") {
        match.arg(par_type, choices = "difficulty")
        out <- tidy_mplus(x)
    } else if (engine == "mirt") {
        if (is.null(par_type)) {
            checkmate::assert_choice(par_type,
                                     c("difficulty", "easiness"))
        }
        match.arg(par_type, choices = c("difficulty", "easiness"))
        out <- tidy_mirt(x, par_type = par_type, ...)
    } else if (engine == "tam") {
        match.arg(par_type, choices = "difficulty")
        out <- tidy_tam(x)
    }
    return(out)
}

tidy_mirt <- function(x = NULL, par_type = NULL) {

    nfact <- x$mirt@Model$nfact
    facnm <- x$mirt@Model$factorNames

    pattern_component <- c(
        setNames(facnm, paste0("^a", 1:nfact, "$")),
        setNames(facnm, paste0("^MEAN_", 1:nfact, "$")),
        setNames(facnm, paste0("^COV_", 1:nfact, 1:nfact, "$")),
        "^d\\d?$" = NA_character_,
        "^COV_\\d+" = NA_character_
        )

    pattern_parameter <- c(
        setNames(rep("Discrim.", nfact), paste0("^a", 1:nfact)),
        setNames(rep("Mean", nfact),     paste0("^MEAN_", 1:nfact)),
        setNames(rep("Var", nfact),      paste0("^COV_", 1:nfact, 1:nfact)),
        "^d\\d?$" = "Threshold",
        "^COV_\\d+$" = "Cov"
    )

    est1 <-
        mirt::coef(x$mirt, printSE = TRUE, as.data.frame = TRUE) %>%
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
        dplyr::mutate(
            term = sub("GroupPars[.]", "", .data$term),
            parameter = stringr::str_extract(
                string = .data$term,
                pattern = "(a\\d+$)|(d\\d?$)|(^MEAN_\\d+$)|(^COV_\\d+$)"),
            component = stringr::str_replace_all(.data$parameter, pattern_component),
            parameter = stringr::str_replace_all(.data$parameter, pattern_parameter),
            myorder = match(.data$parameter, c("Threshold", "Discrim.", "Mean", "Var", "Cov"))) %>%

        dplyr::arrange(.data$myorder) %>%
        dplyr::select(.data$parameter, .data$component, .data$term,
                      .data$estimate, .data$std.error)

    if (par_type == "difficulty") {
        est1 <- dplyr::mutate(
            est1, estimate = unlist(
                purrr::map_if(.data$estimate,
                              grepl("[.]d$", .data$term), ~-.x)))
    }

    cor1 <- cov2cor(mirt::coef(x$mirt, simplify = TRUE)$cov)
    cor1[upper.tri(cor1, diag = TRUE)] <- NA
    cor2 <- cbind(expand.grid(1:nfact, 1:nfact), estimate = as.numeric(cor1))

    cor3 <- dplyr::transmute(na.omit(cor2),
                      parameter = "Corr",
                      component = NA_character_,
                      term      = paste0("CORR_", .data$Var1, .data$Var2),
                      estimate  = .data$estimate,
                      std.error = NA_real_)

    out <- dplyr::bind_rows(est1, cor3)

    return(out)
}

tidy_mplus <- function(x = NULL) {

    nfact <- x$spec$object$S
    facnm <- x$spec$object$latent_names$theta
    pars <- c("Threshold", "Discrim.", "Var", "Cov", "Corr")

    pattern_component <- c(
        setNames(facnm, toupper(paste0(facnm, "<->", facnm))),
        setNames(facnm, toupper(paste0(".*<-", facnm, "$")))
    )

    pattern_parameter <- c(
        ".*<-Thresholds$" = "Threshold",
        setNames(rep("Var", nfact), toupper(paste0(facnm, "<->", facnm))),
        setNames(rep("Discrim.", nfact), toupper(paste0(".*<-", facnm, "$"))),
        setNames("Cov", toupper(glue::glue("({clps('|' , facnm)})<->({clps('|', facnm)})")))
    )

    est1 <-
        coef(x$mplus, type = "un") %>%
        as.data.frame() %>%
        dplyr::rename(term = "Label", estimate = "est", std.error = "se",
                      p.value = "pval") %>%
        dplyr::mutate(term = trimws(.data$term),
                      component = stringr::str_replace_all(.data$term, pattern_component),
                      component = stringr::str_extract(.data$component, clps("|", facnm))) %>%
        dplyr::mutate(parameter = stringr::str_replace_all(.data$term, pattern_parameter),
                      parameter = stringr::str_extract(.data$parameter, clps("|", pars)))

    stdyx <- na.omit(stringr::str_extract(names(x$mplus$parameters),
                                         ".*(?=[.]standardized)"))

    coef_corr <- purrr::possibly(
        ~ coef(.x, type = stdyx, params = "undirected") %>%
            dplyr::mutate(Label = paste0("CORR_", trimws(.data$Label))) %>%
            as.data.frame(),
        otherwise = data.frame(Label = character(0), est = numeric(0),
                               se = numeric(0), pval = numeric(0), stringsAsFactors = FALSE))

    est2 <- coef_corr(x$mplus) %>%
        dplyr::rename(term = "Label", estimate = "est", std.error = "se",
                      p.value = "pval") %>%
        dplyr::mutate(parameter = "Corr", component = NA_character_)

    out <-
        dplyr::bind_rows(est1, est2) %>%
        dplyr::mutate(tmp1 = .data$p.value == 999,
                      std.error = dplyr::if_else(.data$tmp1, NA_real_, .data$std.error),
                      p.value = dplyr::if_else(.data$tmp1, NA_real_, .data$p.value),
                      tmp1 = NULL,
                      myorder = match(.data$parameter, pars)) %>%
        dplyr::arrange(.data$myorder) %>%
        dplyr::select(.data$parameter, .data$component, .data$term,
                      .data$estimate, .data$std.error, .data$p.value) %>%
        tibble::as_tibble()

    return(out)
}

tidy_tam <- function(x = NULL) {

    nfact <- x$spec$object$S
    facnm <- x$spec$object$latent_names$theta
    pars <- c("Threshold", "Discrim.", "Var", "Cov", "Corr")

    est1 <- tibble::as_tibble(x$tam$xsi, rownames = "term") %>%
        dplyr::rename(estimate = "xsi", std.error = "se.xsi") %>%
        dplyr::mutate(parameter = "Threshold",
                      component = NA_character_)

    pattern_var <- c(
        setNames(paste0("VAR_", 1:nfact), paste0("V", 1:nfact, 1:nfact)),
        setNames(paste0("COV_\\1"), "V(\\d+)")
    )

    pattern_parameter <- c(
        "^VAR_\\d+" = "Var",
        "^COV_\\d+" = "Cov",
        "^CORR_\\d+" = "Corr"
    )

    varx <- x$tam$variance
    tmp1 <- unlist(as.data.frame(varx))
    tmp2 <- tmp1[as.vector(upper.tri(varx, TRUE))]
    names(tmp2) <- stringr::str_replace_all(names(tmp2), pattern_var)

    corx <- cov2cor(varx)
    tmp3 <- unlist(as.data.frame(corx, row.names = FALSE))
    tmp4 <- tmp3[as.vector(upper.tri(corx))]
    names(tmp4) <- sub("V", "CORR_", names(tmp4))

    est2 <-
        tibble::enframe(c(tmp2, tmp4), "term", "estimate") %>%
        dplyr::mutate(
            parameter = stringr::str_replace_all(.data$term,
                                                 pattern_parameter),
            component = stringr::str_extract(.data$term, "(?<=VAR_)\\d+"),
            component = stringr::str_replace_all(.data$component,
                                                 setNames(facnm, 1:nfact)))

    out <- dplyr::bind_rows(est1, est2) %>%
        dplyr::mutate(myorder = match(.data$parameter, pars)) %>%
        dplyr::arrange(.data$myorder) %>%
        dplyr::select(.data$parameter, .data$component, .data$term,
                      .data$estimate, .data$std.error) %>%
        tibble::as_tibble()
    return(out)
}

#' Augment data with information from an irtree_fit object
#'
#' @description Augment accepts a model object and a dataset and adds
#'   information about each observation in the dataset, namely, predicted values
#'   in the .fitted column. New columns always begin with a . prefix to avoid
#'   overwriting columns in the original dataset.
#'
#' @details Note that argument `method` is used only for engines mirt and TAM.
#'
#' @param x object of class `irtree_fit` as returned from  [`fit()`][fit.irtree_model].
#' @param data Optional data frame that is returned together with the predicted
#'   values. Argument is not needed since the data are contained in the fitted
#'   object.
#' @param se_fit Logical indicating whether standard errors for the fitted
#'   values should be returned as well.
#' @param method This is passed to [mirt::fscores()] or
#'   [`TAM:::IRT.factor.scores()`][TAM::IRT.factor.scores.tam.mml] (as argument
#'   `type`) if applicable.
#' @param ... Additional arguments passed to [mirt::fscores()] or
#'   [`TAM:::IRT.factor.scores()`][TAM::IRT.factor.scores.tam.mml] if
#'   applicable.
#' @return Returns a [tibble][tibble::tibble-package] with one row for each
#'   observation and one (two) additional columns for each latent variable if
#'   `se_fit = FALSE` (if `se_fit = TRUE`). The names of the new columns start
#'   with `.fit` (and `.se.fit`).
#' @example inst/examples/example-tidiers.R
#' @seealso [generics::augment()]
#' @export
augment.irtree_fit <- function(x = NULL,
                               data = NULL,
                               se_fit = TRUE,
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
        if (!x$spec$control$save_fscores) {
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
            names(est) <- paste0(".fitted.", names(est))
            names(se) <- paste0(".se.fit.",
                                sub("_SE$", "", names(se)))
            if (se_fit) {
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

        out <- tryCatch({
            tmp1 <- augment_mirt(x = x, se_fit = se_fit, method = method, ...)
            tibble::as_tibble(cbind(data, tmp1))
        },
        error = function(cond) {
            warning(cond)
            return(tibble::as_tibble(data))
        })
        return(out)

    } else if (engine == "tam") {

        out <- tryCatch({
            tmp1 <- augment_tam(x$tam, method = method, ...)
            tibble::as_tibble(cbind(data, tmp1))
        },
        error = function(cond) {
            warning(cond)
            return(tibble::as_tibble(data))
        })
        return(out)
    }
}

augment_mirt <- function(x, se_fit = TRUE, method = "EAP", ...) {

    out <- as.data.frame(
        mirt::fscores(x$mirt,
                      method = method,
                      full.scores = TRUE,
                      append_response.pattern = FALSE,
                      returnER = FALSE,
                      return.acov = FALSE,
                      full.scores.SE = se_fit,
                      ...))

    names(out) <- paste0(".fitted.", names(out))
    if (se_fit) {
        names(out) <- sub("^.fitted.SE_", ".se.fit.", names(out))
    }

    return(out)
}

#' @seealso [TAM::IRT.factor.scores.tam]
augment_tam <- function(x = NULL, method = c("EAP", "WLE", "MLE"), progress = FALSE, ...) {
    method <- match.arg(method)

    if (method == "EAP") {
        thetas <- x$person[, grep("EAP", colnames(x$person))]
    } else {
        thetas <- TAM::tam.wle(x, WLE = (method == "WLE"), progress = progress, ...)
        class(thetas) <- "data.frame"
        thetas <- thetas[, grep("theta|error", colnames(thetas))]
    }
    names(thetas) <- sub("^(EAP|theta)(.*)", ".fitted\\2", names(thetas))
    names(thetas) <- sub("^(SD.EAP|error)(.*)", ".se.fit\\2", names(thetas))

    thetas <- thetas[order(names(thetas))]

    return(thetas)
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
