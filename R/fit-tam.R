#' Fit an IR-Tree Model using TAM.
#'
#' This function takes a `data` frame and a model `object` and runs the model in TAM.
#'
#' @param link String specifying the link function. Only `logit` is
#'   implemented in TAM.
#' @param ... Other arguments passed to [TAM::tam.mml()].
#' @param .set_min_to_0 Logical. [TAM::tam.mml()] expects the data to be scored 0,
#'   ..., K. If `.set_min_to_0 = TRUE`, the minimum of the data is subtracted from
#'   each response, which will likely both satisfy TAM and do no harm to the
#'   data.
#' @inheritParams TAM::tam.mml
#' @inheritParams fit.irtree_model
#' @return List with two elements. `tam` contains the TAM output, namely
#'   an object of class [`tam.mml`][TAM::tam.mml] . `spec`
#'   contains the input specifications.
#' @example inst/examples/example-fit.R
#' @export
irtree_fit_tam <- function(object = NULL,
                           data = NULL,
                           link = "logit",
                           verbose = interactive(),
                           ...,
                           .set_min_to_0 = FALSE,
                           .improper_okay = FALSE) {

    checkmate::assert_class(object, "irtree_model")

    assert_irtree_equations(object)
    assert_irtree_proper(object, .improper_okay = .improper_okay)

    if (!isTRUE(all(unlist(object$irt_loadings) == "@1"))) {
        stop("2Pl is not implemented in TAM.")
    }
    checkmate::assert_data_frame(data,
                                 # types = "numeric",
                                 all.missing = FALSE, min.rows = 1, min.cols = object$J)
    checkmate::assert_data_frame(data[names(object$j_names)], types = "integerish",
                                 ncols = object$J)
    data <- tibble::as_tibble(data)
    checkmate::assert_subset(names(object$j_names), choices = names(data))

    object$j_names <- sort2(object$j_names, names(data))
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    object$lambda <- object$lambda[order(object$lambda$item, object$lambda$irt), ]

    link <- match.arg(link)

    spec <- c(as.list(environment()))
    spec$engine <- "tam"

    if (object$class == "tree") {

        categ_dat <- unique(unlist(data, use.names = FALSE))
        categ_mod <- as.integer(names(object$expr))
        if (length(sym_diff(categ_dat, categ_mod)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'object' has equations for categories ", clps(", ", categ_mod), "."
                 , call. = FALSE)
        }
        pseudoitems <- irtree_recode(object = object, data = data[object$j_names])

        Q <- .make_tam_Q(object = object, pseudoitems = pseudoitems)

    } else if (object$class == "pcm") {

        if (.set_min_to_0) {
            pseudoitems <- data - min(data[names(object$j_names)])
        } else {
            pseudoitems <- data
            if (min(data[names(object$j_names)]) != 0) {
                warning("Minimum of data is not equal to zero. ",
                        "You should probably recode your data or set '.set_min_to_0 = TRUE'.", call. = FALSE)
            }
        }

        B <- .make_tam_B(object, array = TRUE)

    } else {
        stop("Class ", object$class, " not implemented in TAM.", call. = FALSE)
    }

    if (TRUE) {
        res <- myTryCatch(
            TAM::tam.mml(resp     = pseudoitems,
                         irtmodel = "1PL",
                         Q        = get0("Q", environment(), inherits = FALSE),
                         B        = get0("B", environment(), inherits = FALSE),
                         verbose  = verbose,
                         ...))
        if (!is.null(res$warning)) {
            warning(conditionMessage(res$warning))
        }
        if (!is.null(res$error)) {
            warning(conditionMessage(res$error))
        }
    } else {
        res <- list(value = NULL)
    }

    out <- list(tam = res$value, error = res$error, warning = res$warning, spec = spec)
    class(out) <- c("irtree_fit", class(out))
    return(out)
}

.make_tam_Q <- function(object = NULL, pseudoitems = NULL) {

    Q <- data.frame(object$lambda, dim = 1) %>%
        dplyr::select(.data$theta, .data$new_name, .data$dim) %>%
        reshape(direction = "wide", v.names = "dim",
                idvar = "new_name", timevar = "theta") %>%
        dplyr::mutate(new_name = factor(.data$new_name, levels = names(pseudoitems))) %>%
        dplyr::mutate_at(-1, tidyr::replace_na, 0)

    tmp1 <- paste0("dim.", levels(object$lambda$theta))
    tmp2 <- tmp1[tmp1 %in% names(Q)[-1]]

    Q <- Q[order(Q$new_name), tmp2]

    return(as.matrix(Q, rownames.force = FALSE))
}

.make_tam_B <- function(object = NULL, array = TRUE) {

    weights_df <- tibble::enframe(object$weights, "theta", "weights")
    weights_df$theta <- factor(weights_df$theta, levels(object$lambda$theta))

    B1 <- dplyr::full_join(object$lambda, weights_df, by = "theta")
    B2 <- tidyr::pivot_wider(B1, id_cols = "item", names_from = "theta",
                             values_from = "weights",
                             values_fill = list(weights = list(rep(0, object$K))))
    B3 <- tidyr::unnest(B2, cols = -.data$item)
    B4 <- B3[levels(object$lambda$theta)]
    B   <- Matrix::Matrix(as.matrix(B4))

    if (array) {
        B5 <- array(B, dim = c(object$K, object$J, object$S))
        B <- array(apply(B5, 3, t),
                   dim = c(object$J, object$K, object$S))
    }
    return(B)

}
