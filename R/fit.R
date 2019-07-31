#' Fit an IR-Tree model.
#'
#' This function takes a `data` frame and a model `object` and runs the model in
#' either Mplus or mirt.
#'
#' @param data Data frame containing containing one row per respondent and one
#'   column per variable.
#' @param object A description of the user-specified model. See
#'   [irtree_model] for more information.
#' @param engine String specifying whether to use Mplus or mirt for estimation.
#' @param verbose Logical indicating whether Mplus/mirt output should be printed
#'   to the console.
#' @param ... further arguments passed either to \code{\link{irtree_fit_mplus}} or
#'   \code{\link{irtree_fit_mirt}}
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @seealso irtree_fit_mplus irtree_fit_mirt
fit.irtree_model <- function(object = NULL,
                             data = NULL,
                             engine = c("mplus", "mirt"),
                             verbose = interactive(),
                             ...) {

    ellipsis::check_dots_used()
    engine <- match.arg(engine)

    if (engine == "mplus") {
        out <- irtree_fit_mplus(object = object, data = data,
                                verbose = verbose, ...)
    } else if (engine == "mirt") {
        out <- irtree_fit_mirt(object = object, data = data,
                               verbose = verbose, ...)
    }
    out$args$engine <- engine
    class(out) <- c("irtree_fit", class(out))
    return(out)
}

#' @importFrom generics fit
#' @export
generics::fit

#' @export
summary.irtree_fit <- function(object, ...) {
    ellipsis::check_dots_used()
    if (object$args$engine == "mplus") {
        print(object$mplus$parameters$unstandardized)
    } else if (object$args$engine == "mirt") {
        mirt::summary(object$mirt, ...)
    }
}

#' @export
coef.irtree_fit <- function(object, ...) {
    ellipsis::check_dots_used()
    if (object$args$engine == "mplus") {
        MplusAutomation:::coef.mplus.model(object$mplus, ...)
    } else if (object$args$engine == "mirt") {
        mirt::coef(object$mirt, ...)
    }
}

#' @export
print.irtree_fit <- function(x, ...) {
    if (x$args$engine == "mplus") {
        print(x$mplus, ...)
    } else if (x$args$engine == "mirt") {
        mirt:::print(x$mirt)
    }
}
