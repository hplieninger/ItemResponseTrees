#' Create a template of a model string
#'
#' This is a helper function that prints a template of a model string to the
#' console based on the supplied data frame. This template can be
#' copy-pasted and modified to define an [irtree_model].
#'
#' @param data Data frame.
#' @param rasch Logical. The string `@1` will be appended to each variable name
#'   if `TRUE` with no effect otherwise.
#' @return NULL
#' @examples
#' irtree_create_template(jackson[, c(1, 6, 11)])
#' #> m1 <- "
#' #> IRT:
#' #> ... BY E1@1, E2@1, E3@1;
#' #> ... BY E1@1, E2@1, E3@1;
#' #>
#' #> Equations:
#' #> 1 = ...
#' #> 2 = ...
#' #> 3 = ...
#' #> 4 = ...
#' #> 5 = ...
#' #>
#' #> Class:
#' #> Tree
#' #> "
#' @export
irtree_create_template <- function(data = NULL,
                                   rasch = TRUE) {
    checkmate::qassert(data, "d")
    checkmate::qassert(rasch, "B1")
    k_names <- 0:1
    try(silent = TRUE, expr = {
        k_names <-
            utils::head(
                sort(
                    unique(
                        dplyr::pull(
                            dplyr::select_if(data, checkmate::test_integerish), 1))
                ), 9)
    })
    variables <- names(data)
    irt <- paste0("... BY ",
                  paste0(variables, ifelse(rasch, "@1", ""), collapse = ", "),
                  ";")
    irt <- strwrap(irt, exdent = 9)
    irt <- paste0("\n", irt, collapse = "")
    message("m1 <- \"\nIRT:",
            replicate(2, irt),
            "\n\nEquations:\n",
            clps("\n", k_names, "= ..."),
            # 0 = ...\n1 = ...,
            "\n\n",
            "Class:\nTree\n\"")
    return(invisible(NULL))
}
