#' Create a template of a model string
#'
#' This is a helper function that prints a template of a model string to the
#' console based on the supplied variable names. This template can be
#' copy-pasted and modified to define an [`irtree_model`].
#'
#' @param variables Character vector of variable names.
#' @param rasch Logical. The string `@1` will be appended to each variable name
#'   if `TRUE` with no effect otherwise.
#' @return NULL
#' @seealso irtree_model()
#' @examples
#' irtree_create_template(c("X1", "X2", "X3"))
#' #> model1 <- "
#' #> IRT:
#' #> ... BY X1@1, X2@1, X3@1;
#' #> ... BY X1@1, X2@1, X3@1;
#' #>
#' #> Equations:
#' #> 0 = ...
#' #> 1 = ...
#' #>
#' #> Class:
#' #> Tree
#' #> "
#' @export
irtree_create_template <- function(variables = NULL,
                            rasch = TRUE) {
    if (is.data.frame(variables)) {
        variables <- names(variables)
    }
    checkmate::qassert(variables, "S+")
    checkmate::qassert(rasch, "B1")
    message("model1 <- \"\nIRT:",
            replicate(2, paste0("\n... BY ",
                                paste0(variables, ifelse(rasch, "@1", ""), collapse = ", "),
                                ";")),
            "\n\nEquations:\n0 = ...\n1 = ...\n\n",
            "Class:\nTree\n\"")
    return(invisible(NULL))
}
