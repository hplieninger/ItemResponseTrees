#' Fit an IR-Tree model.
#'
#' This function takes a `data` frame and an `object` of class [irtree_model],
#' and runs the model in either Mplus or mirt.
#'
#' @section Methods: The methods `coef()`, `summary()`, and `print()` are
#'   implemented for objects of class `irtree_fit`, and those wrap the
#'   respective functions of [mirt][mirt-package] or
#'   [MplusAutomation][MplusAutomation-package]. However,
#'   [`glance()`][glance.irtree_fit], [`tidy()`][tidy.irtree_fit], and
#'   [`augment()`][augment.irtree_fit] may be more helpful.
#'
#' @param object Object of class `irtree_model`. See [irtree_model] for more
#'   information.
#' @param data Data frame containing containing one row per respondent and one
#'   column per variable. The variable names must correspond to those used in
#'   `object`.
#' @param engine String specifying whether to use Mplus or mirt for estimation.
#' @param verbose Logical indicating whether Mplus/mirt output should be printed
#'   to the console.
#' @param ... further arguments passed either to [irtree_fit_mplus()] or
#'   [irtree_fit_mirt()]
#' @param .improper_okay Logical indicating whether the model should also be fit
#'   if it is not a proper IR-tree model. Set this only to `TRUE` if you really
#'   know what you are doing.
#' @return Returns a list of class `irtree_fit`. The first list element is the
#'   return value of [MplusAutomation][MplusAutomation::readModels()] or
#'   [mirt][mirt::mirt()], and further information is provided in the element
#'   `spec`.
#' @example inst/examples/example-fit.R
#' @export
#' @seealso The wrapped functions [irtree_fit_mplus()] and [irtree_fit_mirt()].
fit.irtree_model <- function(object = NULL,
                             data = NULL,
                             engine = c("mplus", "mirt", "tam"),
                             verbose = interactive(),
                             ...,
                             .improper_okay = FALSE) {

    engine <- match.arg(engine)

    if (!is.null(object$equations)) {
        irtree_model_check_equations(object$equations, object$p_names)
    }

    if (.improper_okay == FALSE & object$proper_model == FALSE) {
        stop("The model seems to be an improper model. You might set ",
             "'.improper_okay' to TRUE, but do this only if you really ",
             "know what you are doing.")
    }

    if (engine == "mplus") {
        .must_have(object, "weights", FALSE, .engine = engine)
        out <- irtree_fit_mplus(object = object, data = data,
                                verbose = verbose, ...)
    } else if (engine == "mirt") {
        .must_have(object, "subtree", FALSE, .engine = engine)
        .must_have(object, "constraints", FALSE, .engine = engine)
        .must_have(object, "addendum", FALSE, .engine = engine)
        .must_have(object, "weights", FALSE, .engine = engine)
        out <- irtree_fit_mirt(object = object, data = data,
                               verbose = verbose, ...)
    } else if (engine == "tam") {
        .must_have(object, "subtree", FALSE, .engine = engine)
        .must_have(object, "constraints", FALSE, .engine = engine)
        .must_have(object, "addendum", FALSE, .engine = engine)
        out <- irtree_fit_tam(object = object, data = data,
                              verbose = verbose, ...)
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
