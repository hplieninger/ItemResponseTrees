#' Fit an `irtree_model` using mirt
#'
#' This function takes a `data` frame and a model `object` and runs the model in mirt.
#'
#' @param link String specifying the link function. Only `logit` is
#'   implemented in mirt.
#' @inheritParams fit.irtree_model
#' @inheritParams mirt::mirt
#' @keywords internal
irtree_fit_mirt <- function(object = NULL,
                            data = NULL,
                            link = "logit",
                            verbose = interactive(),
                            control = control_mirt(),
                            improper_okay = FALSE
                            ) {

    checkmate::assert_class(object, "irtree_model")
    assert_irtree_data(data = data, object = object, engine = "mirt")
    data <- tibble::as_tibble(data)

    assert_irtree_proper(object, improper_okay = improper_okay)

    object$j_names <- sort2(object$j_names, names(data))
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    object$lambda <- object$lambda[order(object$lambda$item, object$lambda$irt), ]

    link <- match.arg(link)
    checkmate::qassert(verbose, "B1")
    checkmate::qassert(control, "l")
    tmp1 <- formalArgs(control_mirt)
    checkmate::assert_names(names(control), must.include = tmp1[tmp1 != "..."])
    rm(tmp1)

    spec <- c(as.list(environment()))
    spec$engine <- "mirt"

    if (object$class == "tree") {
        assert_irtree_not_mixture(object)
        pseudoitems <- irtree_recode(object = object, data = data[object$j_names])
    } else if (object$class == "grm") {
        pseudoitems <- data
    } else {
        .stop_not_implemented()
    }

    mirt_input <- write_mirt_input(object = object, data = pseudoitems)
    mirt_string <- mirt_input$mirt_string

    if (TRUE) {
        mirt_call <- rlang::call2("mirt",
                                  .ns = "mirt",
                                  data     = rlang::expr(pseudoitems),
                                  model    = rlang::expr(mirt_string),
                                  itemtype = mirt_input$itemtype,
                                  verbose  = verbose,
                                  !!!control)
        res <- rlang::eval_tidy(mirt_call)
    }

    out <- list(mirt = res, spec = spec)
    class(out) <- c("irtree_fit", class(out))
    return(out)
}

#' Prepare a mirt model
#'
#' This is an internal function used by [irtree_fit_mirt()]. It receives its
#' inputs from the model object and the data set and returns a
#' [mirt::mirt.model] object.
#'
#' @inheritParams irtree_fit_mirt
#' @return A list with three elements. `mirt_string` is the [mirt::mirt.model]
#'   object; `itemtype` can be passed to [mirt::mirt()];
#'   `lambda` is the modified lambda matrix from the `object`-argument.
#' @keywords internal
write_mirt_input <- function(object = NULL,
                             data = NULL) {

    checkmate::assert_class(object, "irtree_model")

    checkmate::assert_data_frame(data, types = "integerish", any.missing = TRUE,
                                 all.missing = FALSE, ncols = nrow(object$lambda))

    lambda <- object$lambda

    lambda <- lambda[order(lambda$irt, lambda$item), ]
    lambda$new_name <- names(data)

    my_values <- lambda[, c("item", "theta", "loading", "new_name")]

    my_values$est <- !grepl("@", my_values$loading)
    my_values$value <- as.numeric(sub("@|[*]", "", my_values$loading))
    my_values$name <- factor(my_values$theta, labels = paste0("a", seq_len(object$S)))

    # mirt1 <- split(lambda, lambda$theta)
    mirt1 <- split(my_values, my_values$theta)

    # mirt2 <- vapply(seq_along(mirt1), function(x) {
    #     glue::glue_collapse(c(glue::glue("{names(mirt1[x])} = "),
    #                           # glue::glue(" {mirt1[[x]]$new_name}", ",")
    #                           glue::glue_collapse(glue::glue("{mirt1[[x]]$new_name}"), sep = ", ")
    #     ))
    # }, FUN.VALUE = "")

    mod <- character()

    # REMEMBER: The order in which the latent variables are defined in
    # mirt_string (via 'LV = var1, var2, var3') is crucial, because the labels
    # of the loadings (e.g., a1, a2, a3) depend on that order

    for (ii in seq_along(mirt1)) {
        # mirt_string: Factor = var1, var2, var3
        mod <- c(mod,
                 paste0(names(mirt1[ii]),
                        " = ",
                        clps(", ", mirt1[[ii]]$new_name)))

        if (!all(mirt1[[ii]]$est)) {
            # mirt_string: COV = Factor1*Factor2
            mod <- c(mod,
                     paste0("COV = ",
                            clps("*", rep(names(mirt1[ii]), 2))))

            # mirt_string: Fix loadings
            start_vals <- na.omit(mirt1[[ii]])
            start_val_groups <- split(start_vals, start_vals$value)
            rm(start_vals)

            for (jj in seq_along(start_val_groups)) {
                # MAKE: FIXED = (var1, var2, var3, a1)
                mod <- c(mod,
                         paste0("FIXED = (",
                                  clps(", ", start_val_groups[[jj]]$new_name),
                                  ", ", start_val_groups[[jj]]$name[1], ")"))

                # MAKE: FIXED = (var1,       a1, 1.0)
                # MAKE: FIXED = (var2, var3, a1, 1.234)
                mod <- c(
                    mod,
                    paste0(
                        "START = (",
                        clps(", ",
                             c(clps(", ", start_val_groups[[jj]]$new_name),
                               as.character(start_val_groups[[jj]]$name[1]),
                               start_val_groups[[jj]]$value[1])),
                        ")"))
            }
        } else {
            # do nothing
        }
    }

    # MAKE: bring all COV statements on one line, add covariances
    tmp1 <- split(mod, grepl("^COV", mod))

    string_vars <- sub("COV =\\s+", "", tmp1$`TRUE`)
    string_covs <- clps("*", names(mirt1))
    tmp1$`TRUE` <- paste0("COV = ", clps(", ", c(string_vars, string_covs)))
    mod <- unname(unlist(tmp1))

    mirt_string <- clps("\n", mod)

    if (object$class == "tree") {
        itemtype <- unname(ifelse(lambda$loading == "@1", "Rasch", "2PL"))
        if (length(unique(itemtype)) == 1) {
            itemtype <- unique(itemtype)
        }
    } else if (object$class == "grm") {
        itemtype <- "graded"
    } else {
        stop("Only model classes Tree and GRM are implemented for mirt.")
    }
    return(out1 = list(mirt_string = mirt_string,
                       itemtype    = itemtype,
                       lambda      = lambda))

}

#' Control aspects of fitting a model in mirt
#'
#' This function should be used to generate the `control` argument of the
#' [`fit()`][fit.irtree_model] function.
#'
#' @param SE,method,quadpts,... These arguments are passed to and documented
#'   in [mirt::mirt()]. They can be used to tweak the estimation algorithm.
#' @param control List of arguments passed to argument `control` of
#'   [mirt::mirt()].
#' @param technical List of arguments passed to argument `technical` of
#'   [mirt::mirt()].
#' @return A list with one element for every argument of `control_mirt()`.
#' @examples
#' control_mirt(SE = FALSE,
#'              method = "QMCEM",
#'              quadpts = 4455,
#'              technical = list(NCYCLES = 567),
#'              TOL = .001)
#' control_mirt(method = "MHRM",
#'              draws = 5544)
#' @export
control_mirt <- function(SE = TRUE,
                         method = "EM",
                         quadpts = NULL,
                         control = list(),
                         technical = list(),
                         ...) {

    ctrl <- c(as.list(environment()), list(...))

    checkmate::qassert(SE, "B1")
    checkmate::assert_int(quadpts, null.ok = TRUE, lower = 3)
    checkmate::qassert(control, "l")
    checkmate::qassert(technical, "l")

    return(ctrl)
}
