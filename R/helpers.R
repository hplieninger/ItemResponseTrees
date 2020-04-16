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
