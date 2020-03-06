#' Fit an `irtree_model` using TAM
#'
#' This function takes a `data` frame and a model `object` and runs the model in TAM.
#'
#' @param link String specifying the link function. Only `logit` is
#'   implemented in TAM.
#' @inheritParams fit.irtree_model
#' @keywords internal
irtree_fit_tam <- function(object = NULL,
                           data = NULL,
                           link = "logit",
                           verbose = interactive(),
                           control = control_tam(),
                           improper_okay = FALSE) {

    checkmate::assert_class(object, "irtree_model")

    if (control$set_min_to_0 && min(data[object$j_names], na.rm = TRUE) != 0) {
        data[object$j_names] <- data[object$j_names] -
            min(data[object$j_names], na.rm = TRUE)
    }

    assert_irtree_data(data = data, object = object, engine = "tam",
                       set_min_to_0 = control$set_min_to_0)
    data <- tibble::as_tibble(data)

    assert_irtree_proper(object, improper_okay = improper_okay)

    if (!isTRUE(all(unlist(object$irt_loadings) == "@1"))) {
        stop("2PL is not implemented in TAM.")
    }

    object$j_names <- sort2(object$j_names, names(data))
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    object$lambda <- object$lambda[order(object$lambda$item, object$lambda$irt), ]

    link <- match.arg(link)
    checkmate::qassert(verbose, "B1")
    checkmate::qassert(control, "l")
    tmp1 <- formalArgs(control_tam)
    checkmate::assert_names(names(control), must.include = tmp1[tmp1 != "..."])

    spec <- c(as.list(environment()))
    spec$engine <- "tam"

    if (object$class == "tree") {

        assert_irtree_not_mixture(object)

        pseudoitems <- irtree_recode(object = object, data = data[object$j_names])

        Q <- .make_tam_Q(object = object, pseudoitems = pseudoitems)

    } else if (object$class == "pcm") {

        pseudoitems <- data

        B <- .make_tam_B(object, array = TRUE)

    } else {
        stop("Class ", object$class, " is not implemented in TAM.", call. = FALSE)
    }

    if (TRUE) {
        tmp1 <- c(list(resp     = pseudoitems,
                       irtmodel = "1PL",
                       Q        = get0("Q", environment(), inherits = FALSE),
                       B        = get0("B", environment(), inherits = FALSE),
                       verbose  = verbose),
                  control[names(control) != "set_min_to_0"])
        res <- myTryCatch(
            do.call(TAM::tam.mml, tmp1))
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

#' Control aspects of fitting a model in TAM
#'
#' This function should be used to generate the `control` argument of the
#' [`fit()`][fit.irtree_model] function.
#'
#' @param set_min_to_0 Logical. [TAM::tam.mml()] expects the data to be scored 0,
#'   ..., K. If `set_min_to_0 = TRUE`, the minimum of the data is subtracted from
#'   each response, which will likely both satisfy TAM and do no harm to the
#'   data.
#' @param control List of arguments passed to argument `control` of
#'   [TAM::tam.mml()]. See examples below.
#' @param ... Other arguments passed to [TAM::tam.mml()].
#' @return A list with one element for every argument of `control_tam()`.
#' @examples
#' control_tam(set_min_to_0 = TRUE,
#'             control = list(snodes = 0,
#'                            maxiter = 1000,
#'                            increment.factor = 1,
#'                            fac.oldxsi = 0),
#'             constraint = "items")
#' @export
control_tam <- function(
    set_min_to_0 = FALSE,
    control = list(snodes = 0,
                   maxiter = 1000,
                   increment.factor = 1,
                   fac.oldxsi = 0),
    ...) {

    ctrl <- c(as.list(environment()), list(...))

    checkmate::qassert(set_min_to_0, "B1")
    checkmate::qassert(control, "L")

    return(ctrl)
}
