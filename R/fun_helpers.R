#' Wrapper Around Paste to Collapse a Character Vector
#'
#' @inheritParams base::paste
#' @inheritDotParams base::paste
#' @keywords internal
clps <- function(collapse = " ", ..., sep = " ") {
    paste(..., sep = sep, collapse = collapse)
}

#' Custom tryCatch() that also returns the value of the expression
#'
#' @param expr expression to be evaluated
#' @return list with three elements, namely, \code{value} giving the value of
#'   \code{expr}, \code{warning} giving any warning messages, and \code{error}
#'   giving any error messages.
#' @references https://stackoverflow.com/a/4952908; https://stackoverflow.com/a/24569739
#' @author https://stackoverflow.com/users/2161065
#' @seealso \code{\link{tryCatch}}
#' @keywords internal
#' @examples
#' ItemResponseTrees:::myTryCatch(log(1))
#' ItemResponseTrees:::myTryCatch(log(-1))
#' ItemResponseTrees:::myTryCatch(log("a"))
myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(
            expr,
            error = function(e) {
                err <<- e
                NULL
            }
        ),
        warning = function(w) {
            warn <<- w
            invokeRestart("muffleWarning")
        }
    )
    list(value = value, warning = warn, error = err)
}

#' Symmetric difference

# @param a PARAM_DESCRIPTION
# @param b PARAM_DESCRIPTION
#' @inheritParams base::setdiff
#' @return vector
#' @references https://stackoverflow.com/a/35949294
#' @author https://stackoverflow.com/users/3798973
#' @seealso \code{\link[base]{setdiff}}
#' @keywords internal
sym_diff <- function(x, y) {
    unique(c(setdiff(x, y), setdiff(y, x)))
}

#' @title Sort a Vector Based on a Second Vector
#' @description This function takes a vector \code{x} and returns \code{x} in
#'   the order provided in \code{y}.
#' @param x vector that should be sorted
#' @param y vector providing the sort order
#' @return sorted \code{x}
#' @references https://stackoverflow.com/a/2117080
#' @author George Dontas (https://stackoverflow.com/users/170792)
#' @keywords internal
sort2 <- function(x = NULL, y = NULL) {
    x[order(match(x, y))]
}
