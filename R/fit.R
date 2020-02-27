#' Fit an IR-Tree model.
#'
#' This function takes a `data` frame and an `object` of class [irtree_model]
#' and runs the model in either mirt, Mplus, or TAM.
#'
#' @section Methods: The methods `coef()`, `summary()`, and `print()` are
#'   implemented for objects of class `irtree_fit`, and those wrap the
#'   respective functions of [mirt][mirt::mirt-package],
#'   [MplusAutomation][MplusAutomation::MplusAutomation-package], or
#'   [TAM][TAM::TAM-package]. However, [`glance()`][glance.irtree_fit],
#'   [`tidy()`][tidy.irtree_fit], and [`augment()`][augment.irtree_fit] may be
#'   more helpful.
#'
#' @param object Object of class `irtree_model`. See [irtree_model] for more
#'   information.
#' @param data Data frame containing containing one row per respondent and one
#'   column per variable. The variable names must correspond to those used in
#'   `object`.
#' @param engine String specifying whether to use mirt, Mplus, or TAM for
#'   estimation.
#' @param link String specifying the link function. May be either logit, or (in
#'   case of Mplus), probit.
#' @param verbose Logical indicating whether output should be printed to the
#'   console.
#' @param control List. The allowed elements of this list depend on the
#'   `engine`. Use [control_mirt], [control_mplus], or [control_tam] for
#'   convenience. Note that the `fit()` function does not use `...`, but that you
#'   can use the `control_*()` functions to pass additional arguments.
#' @param improper_okay Logical indicating whether the model should also be fit
#'   if it is not a proper IR-tree model. Set this only to `TRUE` if you really
#'   know what you are doing.
#' @return Returns a list of class `irtree_fit`. The first list element is the
#'   return value of either [mirt::mirt()], [MplusAutomation::readModels()] or
#'   [TAM::tam.mml()]. Further information is provided in the element
#'   `spec`.
#' @param ... Not currently used. Use `control` instead.
#' @example inst/examples/example-fit.R
#' @export
fit.irtree_model <- function(object = NULL,
                             data = NULL,
                             engine = c("mplus", "mirt", "tam"),
                             ...,
                             link = c("logit", "probit"),
                             verbose = interactive(),
                             control = NULL,
                             improper_okay = FALSE) {

    engine <- match.arg(engine)
    if (length(list(...)) > 0) {
        stop("The ... are currently not used. Use ",
             paste0("control_", engine, "() "), "instead.", call. = FALSE)
    }
    link   <- match.arg(link)
    if (is.null(control)) {
        control <- switch (engine,
            mplus = control_mplus(),
            mirt  = control_mirt(),
            tam   = control_tam()
        )
    }

    if (engine == "mplus") {
        .must_have(object, "weights", FALSE, .engine = engine)
        out <- irtree_fit_mplus(object = object, data = data, link = link,
                                verbose = verbose, control = control,
                                improper_okay = improper_okay)
    } else if (engine == "mirt") {
        .must_have(object, "addendum", FALSE, .engine = engine)
        .must_have(object, "weights", FALSE, .engine = engine)
        out <- irtree_fit_mirt(object = object, data = data, link = link,
                               verbose = verbose, control = control,
                               improper_okay = improper_okay)
    } else if (engine == "tam") {
        if (object$class == "pcm") {
            .must_have(object, "constraints", FALSE, .engine = engine)
        }
        .must_have(object, "addendum", FALSE, .engine = engine)
        out <- irtree_fit_tam(object = object, data = data, link = link,
                              verbose = verbose, control = control,
                              improper_okay = improper_okay)
    } else {
        .stop_not_implemented()
    }

    return(out)
}

#' @importFrom generics fit
#' @export
generics::fit

#' @export
summary.irtree_fit <- function(object, ...) {
    # ellipsis::check_dots_used()
    if (object$spec$engine == "mplus") {
        print(object$mplus$parameters$unstandardized)
    } else if (object$spec$engine == "mirt") {
        mirt::summary(object$mirt, ...)
    } else if (object$spec$engine == "tam") {
        TAM:::summary.tam.mml(object$tam, ...)
    }
}

#' @export
coef.irtree_fit <- function(object, ...) {
    # ellipsis::check_dots_used()
    if (object$spec$engine == "mplus") {
        MplusAutomation:::coef.mplus.model(object$mplus, ...)
    } else if (object$spec$engine == "mirt") {
        mirt::coef(object$mirt, ...)
    } else {
        return(NULL)
    }
}

#' @export
print.irtree_fit <- function(x, ...) {
    if (x$spec$engine == "mplus") {
        print(x$mplus, ...)
    } else if (x$spec$engine == "mirt") {
        mirt:::print(x$mirt)
    } else if (x$spec$engine == "tam") {
        TAM:::print.tam.mml(x$tam)
    }
}
