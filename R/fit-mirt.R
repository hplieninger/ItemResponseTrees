#' Fit an IR-Tree Model using mirt.
#'
#' This function takes a `data` frame and a model `object` and runs the model in mirt.
#'
#' @param link String specifying the link function. Only `logit` is
#'   implemented in mirt.
#' @param rm_mirt_internal Logical. [mirt::mirt()] returns a lot of
#'   information including two functions that can take up a huge amount of space
#'   (https://github.com/philchalmers/mirt/issues/147#issue-352032654). These
#'   two functions are removed from the output if `rm_mirt_internal =
#'   TRUE`.
#' @param ... Other arguments passed to [mirt::mirt()].
#' @inheritParams fit.irtree_model
#' @inheritParams mirt::mirt
#' @return List with two elements. `mirt` contains the mirt output, namely an
#'   object of class [mirt::SingleGroupClass-class]. `spec` contains the input
#'   specifications.
#' @example inst/examples/example-fit.R
#' @export
irtree_fit_mirt <- function(object = NULL,
                            data = NULL,
                            link = "logit",
                            SE = TRUE,
                            verbose = interactive(),
                            rm_mirt_internal = TRUE,
                            ...,
                            .improper_okay = FALSE) {

    link <- match.arg(link)

    checkmate::assert_class(object, "irtree_model")
    assert_irtree_data(data = data, object = object, engine = "mirt")
    data <- tibble::as_tibble(data)

    assert_irtree_equations(object)
    assert_irtree_proper(object, .improper_okay = .improper_okay)

    object$j_names <- sort2(object$j_names, names(data))
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    object$lambda <- object$lambda[order(object$lambda$item, object$lambda$irt), ]


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
        res <- myTryCatch(
            mirt::mirt(data     = pseudoitems,
                       model    = mirt_input$mirt_string,
                       itemtype = mirt_input$itemtype,
                       pars     = mirt_input$values,
                       SE       = SE,
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

    if (rm_mirt_internal) {
        try(silent = TRUE, expr = {
            res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@den <- function() {}
            res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@safe_den <- function() {}
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
                                 all.missing = FALSE, ncols = (object$P)*object$J)

    lambda <- object$lambda

    lambda <- lambda[order(lambda$irt, lambda$item), ]
    lambda$new_name <- names(data)

    # spec$object$lambda <- object$lambda <- lambda

    mirt1 <- split(lambda, lambda$theta)

    mirt2 <- vapply(seq_along(mirt1), function(x) {
        glue::glue_collapse(c(glue::glue("{names(mirt1[x])} = "),
                              # glue::glue(" {mirt1[[x]]$new_name}", ",")
                              glue::glue_collapse(glue::glue("{mirt1[[x]]$new_name}"), sep = ", ")
        ))
    }, FUN.VALUE = "")

    mirt3 <- NULL
    if (object$class == "tree") {
        # if (all(lambda$loading == "*")) {
        #     itemtype <- "2PL"
        # } else if (all(lambda$loading == "@1")) {
        #     itemtype <- "Rasch"
        # } else {
        #     itemtype <- "2PL"
        # }
        itemtype <- ifelse(lambda$loading == "@1", "Rasch", "2PL")
    } else if (object$class == "grm") {
        itemtype <- "graded"
        if (!all(lambda$loading == "*")) {
            tmp0 <- lambda[lambda$loading != "*", ]
            tmp1 <- split(tmp0$new_name, tmp0$theta)
            tmp2 <- vapply(seq_along(tmp1), function(x) paste0("(",
                                                               paste(tmp1[[x]], collapse = ", "),
                                                               ", a", x, ")"), FUN.VALUE = "")
            mirt3 <- paste("CONSTRAIN =", paste(tmp2, collapse = ", "))
        }
    } else {
        stop("Only model classes Tree and GRM are implemented for mirt.")
    }

    mirt_string <- mirt::mirt.model(paste(c(mirt2, mirt3), collapse = "\n"),
                                    itemnames = names(data))

    values <- mirt::mirt(data = data,
                         model = mirt_string,
                         itemtype = itemtype,
                         pars = "values")

    # Free all covariances
    tmp1 <- expand.grid(seq_len(object$S), seq_len(object$S))
    cov_names <- apply(tmp1[tmp1$Var1 > tmp1$Var2, ],
                       1,
                       function(x) paste0("COV_",
                                          paste(x, collapse = "")))
    values[values$name %in% cov_names, "est"] <- TRUE

    # Free all variances of 'Rasch'-items
    var_names <- apply(tmp1[tmp1$Var1 == tmp1$Var2, ],
                       1,
                       function(x) paste0("COV_",
                                          paste(x, collapse = "")))
    var_free <- tapply(lambda$loading, lambda$theta, function(x) all(x == "@1"))

    values[values$name %in% var_names[var_free], "est"] <- TRUE

    ### Fix some loadings in case 'itemtype' is not sufficient ###

    # if (length(unique(lambda$loading)) > 1) {
    #     tmp1 <- data.frame(
    #         item  = gl(object$J, k = object$S, labels = object$j_names),
    #         trait = gl(object$S, k = 1, labels = object$s_names)
    #     )
    #
    #     lambda2 <- dplyr::left_join(tmp1, lambda, by = c("item", "trait"))
    #     lambda2$est <- FALSE
    #     lambda2$est[lambda2$loading == "*"] <- TRUE
    #     lambda2$value <- 0
    #     lambda2$value[!is.na(lambda2$loading)] <- 1
    #
    #     values[grepl("^a\\d+", values$name), "est"]   <- lambda2$est
    #     values[grepl("^a\\d+", values$name), "value"] <- lambda2$value
    # }

    # itemtype <- switch(object$class,
    #     tree = "2PL",
    #     grm  = "graded",
    #     NULL
    # )

    # if (object$class == "tree") {
    #     itemtype <- ifelse(lambda$loading == "@1", "Rasch", "2PL")
    # } else if (object$class == "grm") {
    #     itemtype <- "graded"
    # }

    # tmp1 <- data.frame(
    #     item  = gl(object$J, k = object$S, labels = object$j_names),
    #     trait = gl(object$S, k = 1, labels = object$s_names)
    # )
    #
    # lambda2 <- dplyr::left_join(tmp1, lambda, by = c("item", "trait"))
    # lambda2$est <- FALSE
    # lambda2$est[lambda2$loading == "*"] <- TRUE
    # lambda2$value <- 0
    # lambda2$value[!is.na(lambda2$loading)] <- 1
    #
    # values <- mirt(data = data,
    #                object = mirt_string,
    #                itemtype = itemtype,
    #                pars = "values")
    #
    # values[grepl("^COV",   values$name), "est"]   <- TRUE
    # values[grepl("^a\\d+", values$name), "est"]   <- lambda2$est
    # values[grepl("^a\\d+", values$name), "value"] <- lambda2$value

    # tmp1 <- vapply(object$s_names,
    #                function(x) paste(rep(x, 2), collapse = "*"),
    #                FUN.VALUE = "",
    #                USE.NAMES = FALSE)
    # tmp2 <- clps("*", object$s_names)
    # mirt3 <- paste0("COV = ", clps(", ", c(tmp1, tmp2)))
    #
    # string1 <- paste(c(mirt2, mirt3), collapse = "\n")
    # cat(string1)
    #
    # mirt_string <- mirt.model(string1, itemnames = names(data))

    return(out1 = list(mirt_string = mirt_string,
                       itemtype    = itemtype,
                       lambda      = lambda,
                       values      = values))

}
