#' Fit an IR-Tree Model using mirt.
#'
#' This function takes a `data` frame and a model `object` and runs the model in mirt.
#'
#' @param link String specifying the link function. Only `logit` is
#'   implemented in mirt.
#' @param rm_mirt_internal Logical. [mirt::mirt()] returns a lot of
#'   information including two functions that can take up a huge amount of space
#'   (https://github.com/philchalmers/mirt/issues/147#issue-352032654). These
#'   two functions are removed from the output if \code{rm_mirt_internal =
#'   TRUE}.
#' @param ... Other arguments passed to [mirt::mirt()].
#' @inheritParams fit.irtree_model
#' @inheritParams mirt::mirt
#' @return List with two elements. `mirt` contains the mirt output, namely
#'   an object of class \code{\link[mirt]{SingleGroupClass-class}} . `args`
#'   contains the input specifications.
# @examples
#' @export
irtree_fit_mirt <- function(object = NULL,
                            data = NULL,
                            link = "logit",
                            SE = TRUE,
                            verbose = interactive(),
                            rm_mirt_internal = TRUE,
                            ...
) {

    ellipsis::check_dots_used()

    checkmate::assert_class(object, "irtree_model")
    checkmate::assert_data_frame(data,
                                 # types = "numeric",
                                 all.missing = FALSE, min.rows = 1, min.cols = object$J)
    checkmate::assert_data_frame(data[, names(object$j_names)], types = "integerish",
                                 ncols = object$J)
    # checkmate::assert_set_equal(names(data), y = levels(j_names))
    checkmate::assert_subset(names(object$j_names), choices = names(data))

    object$j_names <- sort2(object$j_names, names(data), x_names = TRUE)
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    object$lambda <- object$lambda[order(object$lambda$item, object$lambda$trait), ]

    # SE <- force(SE)
    # verbose <- force(verbose)

    link <- match.arg(link)

    args <- c(as.list(environment())
              # , list(...)
    )

    if (object$class == "tree") {
        # categ_dat <- as.integer(names(table(as.matrix(data))))
        categ_dat <- unique(unlist(data, use.names = FALSE))
        categ_mod <- as.integer(names(object$expr))
        if (length(sym_diff(categ_dat, categ_mod)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'object' has equations for categories ", clps(", ", categ_mod), "."
                 , call. = FALSE)
        }
        pseudoitems <- irtree_recode(object = object, data = data)
    } else if (object$class == "grm") {
        pseudoitems <- data
    }

    # lambda <- object$lambda
    #
    # lambda <- lambda[order(lambda$trait, lambda$item), ]
    # lambda$new_name <- names(pseudoitems)
    # args$object$lambda <- object$lambda <- lambda
    #
    # mirt1 <- split(lambda, lambda$trait)
    #
    # mirt2 <- vapply(seq_along(mirt1), function(x) {
    #     glue::glue_collapse(c(glue::glue("{names(mirt1[x])} = "),
    #                           # glue::glue(" {mirt1[[x]]$new_name}", ",")
    #                           glue::glue_collapse(glue::glue("{mirt1[[x]]$new_name}"), sep = ", ")
    #     ))
    # }, FUN.VALUE = "")
    #
    # itemtype <- ifelse(lambda$loading == "@1", "Rasch", "2PL")
    #
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
    # mirt_string <- mirt.object(string1, itemnames = names(pseudoitems))

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
            warning(conditionMessage(res$warning), call. = FALSE)
        }
        if (!is.null(res$error)) {
            warning("Error: ", conditionMessage(res$error), call. = FALSE)
        }
    } else {
        res <- list(value = NULL)
    }

    if (rm_mirt_internal) {
        res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@den <- function() {}
        res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@safe_den <- function() {}
    }

    return(list(mirt = res$value, error = res$error, warning = res$warning, args = args))
}

#' Prepare a mirt Model
#'
#' This is an internal function used by \code{\link{irtree_fit_mirt}}. It receives its
#' inputs from the model object and the data set and returns a
#' \code{\link[mirt]{mirt.model}} object.
#'
#' @inheritParams irtree_fit_mirt
#' @return A list with four elements. `mirt_string` is the
#'   \code{\link[mirt]{mirt.model}} object; `itemtype` and `values`
#'   are used as arguments for \code{\link[mirt]{mirt}}; `lambda` is the
#'   modified lambda matrix from the `object`-argument.
# @examples
#' @export
write_mirt_input <- function(object = NULL,
                             data = NULL) {

    checkmate::assert_class(object, "irtree_model")

    checkmate::assert_data_frame(data, types = "integerish", any.missing = TRUE,
                                 all.missing = FALSE, ncols = (object$P)*object$J)

    lambda <- object$lambda

    lambda <- lambda[order(lambda$trait, lambda$item), ]
    lambda$new_name <- names(data)

    # args$object$lambda <- object$lambda <- lambda

    mirt1 <- split(lambda, lambda$trait)

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
            tmp1 <- split(tmp0$new_name, tmp0$trait)
            tmp2 <- vapply(seq_along(tmp1), function(x) paste0("(",
                                                               paste(tmp1[[x]], collapse = ", "),
                                                               ", a", x, ")"), FUN.VALUE = "")
            mirt3 <- paste("CONSTRAIN =", paste(tmp2, collapse = ", "))
        }
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
    var_free <- tapply(lambda$loading, lambda$trait, function(x) all(x == "@1"))

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

#' Retrieve Estimates From mirt
#'
#' This function takes the output from \code{\link[mirt]{mirt}} and returns the
#' parameter estimates in a convenient way.
#'
#' @param results An object of class \code{\link[mirt]{SingleGroupClass-class}}
#'   as returned from \code{\link{irtree_fit_mirt}}.
#' @param ... Further arguments passed to \code{\link[mirt]{fscores}}
#' @inheritParams extract_mplus_output
#' @inheritParams mirt::fscores
#' @return A list of parameter estimates and model fit information.
# @examples
#' @export
#' @importClassesFrom mirt SingleGroupClass
# @importMethodsFrom mirt fscores
#' @export
extract_mirt_output <- function(results = NULL,
                                object = NULL,
                                method = "MAP",
                                class = NULL,
                                ...) {

    checkmate::assert_class(results, "SingleGroupClass")

    checkmate::assert_class(object, "irtree_model", null.ok = TRUE)

    e2 <- new.env()
    # e2$lv_names <-

    if (!is.null(object)) {
        e2$class <- object$class
    } else {
        checkmate::assert_choice(class, choices = c("tree", "grm"))
        e2$class <- class
    }

    # fscores <- mirt::fscores(results, method = method, ...)

    # lambda <- object$lambda

    # lv_names <- unstd[unstd$paramHeader == "Variances", "param"]

    cf <- mirt::coef(results, printSE = FALSE, simplify = TRUE)

    sigma <- cf$cov
    sigma[upper.tri(sigma)] <- t(sigma)[upper.tri(sigma)]

    cormat <- cov2cor(sigma)

    personpar_est <- as.data.frame(mirt::fscores(results, method = method, ...))

    itempar <- list()

    if (e2$class == "tree") {
        betapar <- data.frame(param = rownames(cf$items),
                              est = cf$items[, "d"])
        tmp1 <- tidyr::separate(betapar, "param", c("time", "item"), sep = "_")
        itempar$beta <- reshape(tmp1, direction = "wide", idvar = "item")
        names(itempar$beta) <- sub("^est[.]", "", names(itempar$beta))

        alphapar <- data.frame(param = rownames(cf$items),
                               cf$items[, grepl("a\\d+", colnames(cf$items))])
        tmp1 <- tidyr::separate(alphapar, "param", c("time", "item"), sep = "_")
        tmp1$est <- NA
        for (ii in seq_along(unique(tmp1$time))) {
            tmp1[tmp1$time == unique(tmp1$time)[ii], "est"] <-
                tmp1[tmp1$time == unique(tmp1$time)[ii], 2 + ii]
        }
        itempar$alpha <- reshape(dplyr::select(tmp1, .data$time, .data$item, .data$est),
                                direction = "wide", idvar = "item")
        names(itempar$alpha) <- sub("^est[.]", "", names(itempar$alpha))

    } else if (e2$class == "grm") {
        itempar$beta <- data.frame(
            param = rownames(cf$items),
            cf$items[, grepl("^d\\d+", colnames(cf$items)), drop = FALSE])

        itempar$alpha <- data.frame(
            param = rownames(cf$items),
            cf$items[, grepl("a\\d+", colnames(cf$items)), drop = FALSE])
        itempar <- lapply(itempar, `rownames<-`, NULL)
    }

    summaries <- mirt::anova(results)
    summaries$converged <- mirt::extract.mirt(results, "converged")
    summaries$secondordertest <- mirt::extract.mirt(results, "secondordertest")

    out <- list(
        person    = list(personpar_est = personpar_est
                         # , personpar_se  = personpar_se
                         ),
        item      = itempar,
        sigma     = sigma,
        cormat    = cormat,
        summaries = summaries
        # , warnings  = results$warnings
        # , errors    = results$errors
    )

    return(out)
}
