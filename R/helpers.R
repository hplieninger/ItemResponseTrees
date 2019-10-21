#' Wrapper Around Paste to Collapse a Character Vector
#'
#' @inheritParams base::paste
#' @inheritDotParams base::paste
#' @keywords internal
clps <- function(collapse = " ", ..., sep = " ") {
    # ellipsis::check_dots_used()
    paste(..., sep = sep, collapse = collapse)
}

#' Custom tryCatch() that also returns the value of the expression
#'
#' @param expr expression to be evaluated
#' @return list with three elements, namely, `value` giving the value of
#'   `expr`, `warning` giving any warning messages, and `error`
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
#' @description This function takes a vector `x` and returns `x` in
#'   the order provided in `y`.
#' @param x vector that should be sorted
#' @param y vector providing the sort order
#' @param x_names Logical. If `TRUE`, `x` is sorted but not on
#'   `x` but on \code{names(x)}.
#' @param subset Logical. If `TRUE`, `x` must be a subset of `y`.
#' @return sorted `x`
#' @references https://stackoverflow.com/a/2117080
#' @author George Dontas (https://stackoverflow.com/users/170792)
#' @keywords internal
sort2 <- function(x = NULL, y = NULL, x_names = FALSE, subset = TRUE) {
    if (!checkmate::test_character(y, min.len = 1, null.ok = FALSE)) {
        return(x)
    }
    checkmate::assert_character(y,
                                min.len = ifelse(subset, length(unique(x)), 1),
                                unique = TRUE)
    if (x_names) {
        checkmate::assert_character(x, min.chars = 1, any.missing = FALSE,
                                    names = "unique")
        if (subset) checkmate::assert_subset(names(x), y)
        x[order(match(names(x), y))]
    } else {
        checkmate::assert_character(x, min.chars = 1, any.missing = FALSE)
        if (subset) checkmate::assert_subset(x, y)
        x[order(match(x, y))]
    }
}

.stop_not_implemented <- function() {
    stop("The requested behavior is not implemented. ",
         "Please modify your function call. For example, ",
         "try to use a different 'engine' or modify your model ",
         "(especially the model class). Or contact the ",
         "package maintainer.", call. = FALSE)
}
