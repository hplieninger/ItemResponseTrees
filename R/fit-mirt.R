#' Fit an `irtree_model` using mirt
#'
#' This function takes a `data` frame and a model `object` and runs the model in mirt.
#'
#' @param link String specifying the link function. Only `logit` is
#'   implemented in mirt.
#' @inheritParams fit.irtree_model
#' @inheritParams mirt::mirt
#' @example inst/examples/example-fit.R
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

    assert_irtree_equations(object)
    assert_irtree_proper(object, improper_okay = improper_okay)

    object$j_names <- sort2(object$j_names, names(data))
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    object$lambda <- object$lambda[order(object$lambda$item, object$lambda$irt), ]

    link <- match.arg(link)
    checkmate::qassert(verbose, "B1")
    checkmate::qassert(control, "l")
    tmp1 <- formalArgs(control_mirt)
    checkmate::assert_names(names(control), must.include = tmp1[tmp1 != "..."])

    spec <- c(as.list(environment()))
    spec$engine <- "mirt"

    if (object$class == "tree") {
        pseudoitems <- irtree_recode(object = object, data = data[object$j_names])
    } else if (object$class == "grm") {
        pseudoitems <- data
    } else {
        .stop_not_implemented()
    }

    mirt_input <- write_mirt_input(object = object, data = pseudoitems)

    if (TRUE) {
        tmp1 <- c(list(data     = pseudoitems,
                       model    = mirt_input$mirt_string,
                       itemtype = mirt_input$itemtype,
                       verbose  = verbose),
                  control[names(control) != "rm_mirt_internal"])
        res <- myTryCatch(
            do.call(mirt::mirt, tmp1))
        if (!is.null(res$warning)) {
            warning(conditionMessage(res$warning))
        }
        if (!is.null(res$error)) {
            warning(conditionMessage(res$error))
        }
    } else {
        res <- list(value = NULL)
    }

    if (control$rm_mirt_internal) {
        try(silent = TRUE, expr = {
            res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@den <- function() {}
            res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@safe_den <- function() {}
            res$value@Call <- call("mirt")
        })
    }

    out <- list(mirt = res$value, error = res$error, warning = res$warning, spec = spec)
    class(out) <- c("irtree_fit", class(out))
    return(out)
}

#' Prepare a mirt Model
#'
#' This is an internal function used by [irtree_fit_mirt()]. It receives its
#' inputs from the model object and the data set and returns a
#' [mirt::mirt.model] object.
#'
#' @inheritParams irtree_fit_mirt
#' @return A list with four elements. `mirt_string` is the [mirt::mirt.model]
#'   object; `itemtype` and `values` are used as arguments for [mirt::mirt()];
#'   `lambda` is the modified lambda matrix from the `object`-argument.
#' @export
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
        itemtype <- ifelse(lambda$loading == "@1", "Rasch", "2PL")
    } else if (object$class == "grm") {
        itemtype <- "graded"
    } else {
        stop("Only model classes Tree and GRM are implemented for mirt.")
    }
    return(out1 = list(mirt_string = mirt_string,
                       itemtype    = itemtype,
                       lambda      = lambda))

}

#' Control Aspects of Fitting a Model in mirt
#'
#' This function should be used to generate the `control` argument of the
#' [`fit()`][fit.irtree_model] function.
#'
#' @param rm_mirt_internal Logical. [mirt::mirt()] returns a lot of
#'   information including two functions that can take up a huge amount of space
#'   (https://github.com/philchalmers/mirt/issues/147#issue-352032654). These
#'   two functions are removed from the output if `rm_mirt_internal =
#'   TRUE`.
#' @param control List of arguments passed to argument `control` of
#'   [mirt::mirt()]. See examples below.
#' @param technical List of arguments passed to argument `technical` of
#'   [mirt::mirt()]. See examples below.
#' @param ... Other arguments passed to [mirt::mirt()].
#' @return A list with one element for every argument of `control_mirt()`.
#' @inheritParams mirt::mirt
#' @examples
#' control_mirt(SE = FALSE,
#'              quadpts = 15,
#'              control = list(),
#'              technical = list(NCYCLES = 500),
#'              TOL = .0001)
#' @export
control_mirt <- function(rm_mirt_internal = TRUE,
                         SE = TRUE,
                         quadpts = NULL,
                         control = list(),
                         technical = list(NCYCLES = NULL),
                         ...) {

    ctrl <- c(as.list(environment()), list(...))

    checkmate::qassert(rm_mirt_internal, "B1")
    checkmate::qassert(SE, "B1")
    checkmate::assert_int(quadpts, null.ok = TRUE, lower = 3)
    checkmate::qassert(control, "l")
    checkmate::qassert(technical, "l")

    return(ctrl)
}

mirt_constrain_loadings <- function(object = NULL, mirt_values = NULL) {
    if (!any(grepl("@", object$loading))) {
        return(mirt_values)
    }
    my_values <- object$lambda[, c("item", "theta", "loading")]

    my_values$est <- !grepl("@", my_values$loading)
    my_values$value <- as.numeric(sub("@|[*]", "", my_values$loading))
    my_values$name <- factor(my_values$theta, labels = paste0("a", seq_len(object$S)))

    for (ii in seq_len(nrow(my_values))) {
        vals <- my_values[ii, ]
        mirt_values <- with(mirt_values,   est[item == vals$item & name == vals$name] <- vals$est)
        mirt_values <- with(mirt_values, value[item == vals$item & name == vals$name] <- vals$value)
    }
    return(mirt_values)
}

mirt_constrain_var_cov <- function(object = NULL,
                                   lambda = NULL,
                                   mirt_values = NULL) {
    # covariances
    tmp1 <- expand.grid(seq_len(object$S), seq_len(object$S))
    cov_names <- apply(tmp1[tmp1$Var1 > tmp1$Var2, ],
                       1,
                       function(x) paste0("COV_",
                                          paste(x, collapse = "")))
    mirt_values[mirt_values$name %in% cov_names, "est"] <- TRUE

    # variances
    var_names <- apply(tmp1[tmp1$Var1 == tmp1$Var2, ],
                       1,
                       function(x) paste0("COV_",
                                          paste(x, collapse = "")))
    var_free <- tapply(lambda$loading, lambda$theta, function(x) any(x == "@1"))

    mirt_values[mirt_values$name %in% var_names[var_free], "est"] <- TRUE

    return(mirt_values)
}
