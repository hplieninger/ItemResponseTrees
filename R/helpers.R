#' Wrapper Around Paste to Collapse a Character Vector
#'
#' @inheritParams base::paste
#' @inheritDotParams base::paste
#' @seealso [base::paste()]
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
#' @seealso [tryCatch()]
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
#' @seealso [base::setdiff()]
#' @keywords internal
sym_diff <- function(x, y) {
    unique(c(setdiff(x, y), setdiff(y, x)))
}

#' @title Sort a Vector Based on a Second Vector
#' @description This function takes a vector `x` and returns `x` in
#'   the order provided in `y`.
#' @param x vector that should be sorted
#' @param y vector providing the sort order
#' @param subset Logical. If `TRUE`, `x` must be a subset of `y`.
#' @return sorted `x`
#' @details This function was inspired by an
#'   [answer](https://stackoverflow.com/a/2117080) by [George
#'   Dontas](https://stackoverflow.com/users/170792) on Stack Overflow. User
#'   contributions on Stack Overflow are licensed under [CC BY-SA
#'   4.0](https://creativecommons.org/licenses/by-sa/4.0/).
#' @keywords internal
sort2 <- function(x = NULL, y = NULL, subset = TRUE) {
    if (!checkmate::test_character(y, min.len = 1, null.ok = FALSE)) {
        return(x)
    }
    checkmate::assert_character(y,
                                min.len = ifelse(subset, length(unique(x)), 1),
                                unique = TRUE)
    checkmate::assert_character(x, min.chars = 1, any.missing = FALSE,
                                names = "unnamed")
    if (subset) checkmate::assert_subset(x, y)
    x[order(match(x, y))]
}

.stop_not_implemented <- function() {
    stop("The requested behavior is not implemented. ",
         "Please modify your function call. For example, ",
         "try to use a different 'engine' or modify your model ",
         "(especially the model class). Or contact the ",
         "package maintainer.", call. = FALSE)
}

.must_have <- function(model_list = NULL,
                       element = NA_character_,
                       must_have = TRUE,
                       .name = NULL,
                       .class = NULL,
                       .engine = NULL,
                       skip = FALSE) {
    if (skip) {
        return()
    }
    if (is.null(model_list[[element]]) == must_have &&
        (length(model_list[[element]]) == 0) == must_have) {
        if (is.null(.name)) {
            .name <- stringr::str_to_title(element)
        }

        if (!is.null(.class)) {
            why <- glue::glue(" if class is '{.class}'")
        } else if (!is.null(.engine)) {
            why <- glue::glue(" if engine is '{.engine}'")
        } else {
            why <- ""
        }
        tmp1 <- glue::glue(
            "Problem in 'model': must {ifelse(must_have, '', 'NOT ')}",
             "contain a part with heading ",
             "'{.name}'{why}."
        )
        stop(tmp1, call. = FALSE)
    }
}

#' Sample from a truncated normal distribution
#'
#' @param n Number of observations
#' @param mean Mean
#' @param sd Standard deviation
#' @param ll Lower bound
#' @param ul Upper bound
#' @seealso There is a discussion and code on
#'   \url{https://stackoverflow.com/a/14034577}, and there is also the truncnorm
#'   package.
#' @keywords internal
rtruncatednorm <- function(n = NULL, mean = 0, sd = 1, ll = -Inf, ul = Inf) {
    checkmate::qassert(n, "X1[1,)")
    checkmate::qassert(mean, "N1")
    checkmate::qassert(sd, "N1[0,)")
    checkmate::qassert(ll, "N1")
    checkmate::assert_number(ul, lower = ll)
    more_n <- ceiling(
        (1 - pnorm(ul, mean = mean, sd = sd) + pnorm(ll, mean = mean, sd = sd)) * n * 2)
    x <- .rtruncatednorm1(n = n + more_n, mean = mean, sd = sd, ll = ll, ul = ul)
    x <- utils::head(x, n)

    while (length(x) < n) {
        x <- c(.rtruncatednorm1(n = more_n, mean = mean, sd = sd, ll = ll, ul = ul))
        x <- utils::head(x, n)
    }
    return(x)
}

.rtruncatednorm1 <- function(n = NULL, mean = 0, sd = 1, ll = -Inf, ul = Inf) {
    x <- rnorm(n = n, mean = mean, sd = sd)
    x <- x[x > ll & x < ul]
    return(x)
}
