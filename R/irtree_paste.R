#' Create a template of a model string
#'
#' This function prints a template of a model string to the console based on the
#' supplied data frame. This template can be copy-pasted and modified to define
#' an [irtree_model].
#'
#' @param data Data frame.
#' @param mapping_matrix Matrix of so-called pseudo-items, optional. The
#'   observed response categories must appear in the first column. The other
#'   columns contain the pseudo-items and each entry may be either `1`, `0`, or
#'   `NA`.
#' @param rasch Logical. The string `@1` will be appended to each variable name
#'   if `TRUE` with no effect otherwise.
#' @return NULL
#' @examples
#' irtree_create_template(jackson[, c(1, 6, 11)])
#' #> m1 <- "
#' #> Equations:
#' #> 1 = ...
#' #> 2 = ...
#' #> 3 = ...
#' #> 4 = ...
#' #> 5 = ...
#' #>
#' #> IRT:
#' #> ... BY E1@1, E2@1, E3@1;
#' #> ... BY E1@1, E2@1, E3@1;
#' #>
#' #> Class:
#' #> Tree
#' #> "
#'
#' irtree_create_template(jackson[, c(1, 6, 11)],
#'                        cbind(1:5,
#'                              m = c(0, 0, 1, 0, 0),
#'                              t = c(1, 1, NA, 0, 0),
#'                              e = c(1, 0, NA, 0, 1)))
#' #> m1 <- "
#' #> Equations:
#' #> 1 = (1-m)*t*e
#' #> 2 = (1-m)*t*(1-e)
#' #> 3 = m
#' #> 4 = (1-m)*(1-t)*(1-e)
#' #> 5 = (1-m)*(1-t)*e
#' #>
#' #> IRT:
#' #> m BY E1@1, E2@1, E3@1;
#' #> t BY E1@1, E2@1, E3@1;
#' #> e BY E1@1, E2@1, E3@1;
#' #>
#' #> Class:
#' #> Tree
#' #> "
#' @export
irtree_create_template <- function(data = NULL,
                                   mapping_matrix = NULL,
                                   rasch = TRUE) {
    checkmate::qassert(data, "d")
    checkmate::qassert(rasch, "B1")
    checkmate::assert_matrix(mapping_matrix, null.ok = TRUE)

    variables <- names(data)

    if (is.null(mapping_matrix)) {
        k_names <- 0:1
        try(silent = TRUE, expr = {
            k_names <-
                utils::head(
                    sort(
                        unique(
                            dplyr::pull(
                                dplyr::select(data, rlang::is_integerish), 1))
                    ), 9)
        })
        equations <- clps("\n", k_names, "= ...")

        irt <- paste0("... BY ",
                      paste0(variables, ifelse(rasch, "@1", ""), collapse = ", "),
                      ";")
        irt <- strwrap(irt, exdent = 9)
        irt <- paste0("\n", irt, collapse = "")
        irt <- replicate(2, irt)

    } else {

        f1 <- function(x) {
            x2 <- dplyr::cur_column()
            dplyr::case_when(
                x == 1 ~ x2,
                x == 0 ~ paste0("(1-", x2, ")"))
        }
        mm2 <- dplyr::mutate(as.data.frame(mapping_matrix),
                             dplyr::across(-1, f1))
        equations <- paste(mm2[, 1], "=", purrr::pmap_chr(mm2[, -1], ~paste(..., sep = "*")))
        equations <- clps("\n", gsub("[*]NA|NA[*]", "", equations))

        irt <- paste0(names(mm2)[-1], " BY ",
                      paste0(variables, ifelse(rasch, "@1", ""), collapse = ", "),
                      ";")
        irt <- strwrap(irt, exdent = 5)
        irt <- paste0("\n", irt, collapse = "")
    }

    message("\nm1 <- \"\nEquations:\n",
            equations,
            "\n\nIRT:",
            irt,
            # 0 = ...\n1 = ...,
            "\n\n",
            "Class:\nTree\n\"\n")
    return(invisible(NULL))
}
