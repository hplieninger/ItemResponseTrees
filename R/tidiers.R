#' Glance at an irtree_fit object.
#'
#' Glance accepts an `irtree_fit` object and returns a [tibble][tibble::tibble-package]
#'   with exactly one row of model summaries.
#'
#' @section Converged: The column `converged` indicates whether the model
#'   converged or not. For Mplus, this is `TRUE` if the output contained the
#'   phrase "The model estimation terminated normally". For mirt, this is equal
#'   to the output of [`mirt::extract.mirt(x,
#'   "converged")`][mirt::extract.mirt]. You are encoured to check any warnings
#'   or errors in any case.
#'
#' @section Iterations: `iterations` is `NA` for Mplus models since respective
#'   information is not easily obtained from the output.
#'
#' @param x object of class `irtree_fit` as returned from [`fit()`][fit.irtree_model].
#' @param ... Additional arguments. Not used.
#' @return A one-row [tibble][tibble::tibble-package] with columns such as \code{AIC} and \code{BIC}.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso [`broom::glance()`], [`mirt::extract.mirt(x,
#'   "secondordertest")`][mirt::extract.mirt]
#' @export
glance.irtree_fit <- function(x = NULL, ...) {

    ellipsis::check_dots_used()
    engine <- x$args$engine

    if (engine == "mplus") {
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

        # out <- tibble::as_tibble(out) %>%
        #     dplyr::select(c("AIC":"LL"), "converged", dplyr::everything())

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
                .strict = F)) %>%
        dplyr::mutate_if(rlang::is_integerish, as.integer)

    checkmate::assert_tibble(out, nrows = 1)

    return(out)
}

#' Tidy an irtree_fit object.
#'
#' Tidy summarizes information about the parameter estimates of the IR-tree model.
#'
#' @param x object of class irtree_fit as returned from  [`fit()`][fit.irtree_model].
#' @param ... Additional arguments. Not used.
#' @return A [tibble][tibble::tibble-package] with one row for each model
#'   parameter and the following columns:
#' \describe{
#'   \item{\code{term}}{The name of the model parameter.}
#'   \item{\code{estimate}}{The estimated value of the term.}
#'   \item{\code{std.error}}{The standard error of the term.}
#'   \item{\code{statistic}}{The value of the test statistic of the term (Mplus only).}
#'   \item{\code{p.value}}{The p-value associated with the statistic.}
#' }
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso [broom::tidy()]
#' @export
tidy.irtree_fit <- function(x = NULL, ...) {
    ellipsis::check_dots_used()
    engine <- x$args$engine

    if (engine == "mplus") {
        out <- tidy_mplus(x)
    } else if (engine == "mirt") {
        out <- tidy_mirt(x)
    }
    return(out)
}

tidy_mirt <- function(x = NULL) {

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
                      term = sub("GroupPars[.]", "", .data$term)) %>%
        tidyr::nest(data = c("term", "estimate", "std.error")) %>%
        dplyr::mutate(data = purrr::map_if(.data$data, !.data$group, f1)) %>%
        tidyr::unnest(cols = .data$data) %>%
        dplyr::select(-"group")

    est2 <- mirt::coef(x$mirt, simplify = TRUE)$cov %>%
        cov2cor %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        tidyr::gather(key = "key", value = "estimate", -"rowname") %>%
        dplyr::filter(.data$rowname != .data$key) %>%
        na.omit %>%
        tidyr::unite(col = "term", .data$rowname, .data$key, sep = ".") %>%
        dplyr::mutate(term = paste0("COR_", .data$term))

    out <- dplyr::bind_rows(est1, est2) %>%
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
#' @param ... Additional arguments passed to [mirt::fscores()] if applicable.
#' @inheritParams mirt::fscores
#' @return Returns a [tibble][tibble::tibble-package] with one row for each
#'   observation and one (two) additional columns for each latent variable if
#'   `se.fit = FALSE` (if `se.fit = TRUE`). The names of the new columns start
#'   with `.fit` (and `.se.fit`).
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso [broom::augment()]
#' @export
augment.irtree_fit <- function(x = NULL,
                               data = NULL,
                               se.fit = TRUE,
                               method = "EAP",
                               ...) {
    ellipsis::check_dots_used()
    engine <- x$args$engine

    if (is.null(data)) {
        data <- x$args$data
    }

    if (engine == "mplus") {
        if (!checkmate::test_string(method, na.ok = TRUE,
                                   pattern = "^EAP$", null.ok = TRUE)) {
            warning("Argument 'method' is only implemented for 'engine = \"mirt\".'")
        }

        out <- tryCatch({

            tmp1 <- 2*x$mplus$summaries$NContinuousLatentVars

            fscores <- as.data.frame(x$mplus[["savedata"]])
            est <- dplyr::select(fscores,
                                 dplyr::last_col(seq(to = 1, by = -2,
                                                     length.out = tmp1/2)))
            se <- dplyr::select(fscores,
                                dplyr::last_col(seq(to = 0, by = -2,
                                                    length.out = tmp1/2)))
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
