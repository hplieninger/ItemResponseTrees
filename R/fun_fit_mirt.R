#' Fit an IR-Tree Model using mirt
#'
#' This function takes a data frame and a model string and runs the model in mirt.
#'
#' @param link Character string specifying the link function. Only \code{logit}
#'   is implemented in mirt.
#' @param rm_mirt_internal Logical. \code{\link[mirt]{mirt}} returns a lot of
#'   information including two functions that can take up a huge amount of space
#'   (https://github.com/philchalmers/mirt/issues/147#issue-352032654). These
#'   two functions are removed from the output if \code{rm_mirt_internal =
#'   TRUE}.
#' ... Other arguments passed to \code{\link[mirt]{mirt}}.
#' @inheritParams fit_tree_mplus
#' @inheritParams mirt::mirt
#' @return List with two elements. \code{mirt} contains the mirt output, namely
#'   an object of class \code{\link[mirt]{SingleGroupClass-class}} . \code{args}
#'   contains the input specifications.
# @examples
#' @export
fit_tree_mirt <- function(data = NULL,
                          model = NULL,
                          dir = ".",
                          R = 1,
                          link = "logit",
                          SE = TRUE,
                          verbose = FALSE,
                          run = TRUE,
                          rm_mirt_internal = TRUE,
                          ...
                          ) {

    if (!inherits(model, "tree_model")) {
        model <- tree_model(model = model)
    }
    # checkmate::assert_data_frame(data, types = "integerish",
    #                              all.missing = FALSE, ncols = model$J)
    checkmate::assert_data_frame(data,
                                 # types = "numeric",
                                 all.missing = FALSE, min.rows = 1, min.cols = model$J)
    checkmate::assert_data_frame(data[, model$j_names], types = "integerish",
                                 ncols = model$J)
    # checkmate::assert_set_equal(names(data), y = levels(j_names))
    checkmate::assert_subset(model$j_names, choices = names(data))

    model$j_names <- sort2(model$j_names, names(data))
    model$lambda$item <- factor(model$lambda$item, levels = model$j_names)
    model$lambda <- model$lambda[order(model$lambda$item, model$lambda$trait), ]

    # SE <- force(SE)
    # verbose <- force(verbose)

    link <- match.arg(link)

    checkmate::assert_directory_exists(dir)

    args <- c(as.list(environment())
              # , list(...)
    )

    if (model$class == "tree") {
        # categ_dat <- as.integer(names(table(as.matrix(data))))
        categ_dat <- unique(unlist(data, use.names = FALSE))
        categ_mod <- as.integer(names(model$expr))
        if (length(sym_diff(categ_dat, categ_mod)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'model' has equations for categories ", clps(", ", categ_mod), "."
                 , call. = FALSE)
        }
        pseudoitems <- recode_data(model = model, data = data)
    } else if (model$class == "grm") {
        pseudoitems <- data
    }

    # lambda <- model$lambda
    #
    # lambda <- lambda[order(lambda$trait, lambda$item), ]
    # lambda$new_name <- names(pseudoitems)
    # args$model$lambda <- model$lambda <- lambda
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
    # tmp1 <- vapply(model$s_names,
    #                function(x) paste(rep(x, 2), collapse = "*"),
    #                FUN.VALUE = "",
    #                USE.NAMES = FALSE)
    # tmp2 <- clps("*", model$s_names)
    # mirt3 <- paste0("COV = ", clps(", ", c(tmp1, tmp2)))
    #
    # string1 <- paste(c(mirt2, mirt3), collapse = "\n")
    # cat(string1)
    #
    # mirt_string <- mirt.model(string1, itemnames = names(pseudoitems))

    mirt_input <- write_mirt_input(model = model, data = pseudoitems)

    if (run) {
        res <- myTryCatch(
            mirt::mirt(data     = pseudoitems,
                       model    = mirt_input$mirt_string,
                       itemtype = mirt_input$itemtype,
                       pars     = mirt_input$values,
                       SE       = SE,
                       verbose  = verbose,
                       ...))
        if (!is.null(res$warning)) {
            warning(conditionMessage(res$warning), call. = F)
        }
        if (!is.null(res$error)) {
            warning("Error: ", conditionMessage(res$error), call. = F)
        }
    } else {
        res <- list(value = NULL)
    }

    # tmp1 <- attr(pseudoitems, "mapping_matrix")
    # tmp1 <- tmp1[, !is.element(colnames(tmp1), "cate"), drop = FALSE]
    # tmp2 <- sapply(seq_len(ncol(tmp1)), function(x) paste0("  pseudoitem", x, ":  ",
    #                                               clps(, ifelse(is.na(tmp1[, x]), "-", tmp1[, x]))))
    # tmp3 <- glue::glue("{'  '}#Parameters:  N = {nrow(data)}; J = {model$J}; K = {model$K}; \\
    #                    P = {model$P}; S = {model$S};")
    # tmp4 <- paste0("  Items:        ", clps(, model$j_names))
    # TITLE <- paste(c(paste0("  Tree Model;   R = ", R), tmp3, tmp4, tmp2), collapse = "\n")

    # if (run) {
    #     invisible(
    #         capture.output(
    #             res <- mirt::mirt
    #         ))
    #
    #     wrn1 <- res$warnings
    #     if (length(wrn1) > 0) {
    #         sapply(wrn1, function(x) warning("Mplus error: ", clps(, x), call. = FALSE))
    #     }
    #     err1 <- res$errors
    #     if (length(err1) > 0) {
    #         sapply(err1, function(x) warning("Mplus error: ", clps(, x), call. = FALSE))
    #     }
    # } else {
    #     res <- NULL
    # }

    if (rm_mirt_internal) {
        res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@den <- function() {}
        res$value@ParObjects$pars[[length(res$value@ParObjects$pars)]]@safe_den <- function() {}
    }

    return(list(mirt = res$value, error = res$error, warning = res$warning, args = args))
}

#' Prepare a mirt Model
#'
#' This is an internal function used by \code{\link{fit_tree_mirt}}. It receives its
#' inputs from the model and the data set and returns a
#' \code{\link[mirt]{mirt.model}} object.
#'
#' @inheritParams fit_tree_mirt
#' @return A list with four elements. \code{mirt_string} is the
#'   \code{\link[mirt]{mirt.model}} object; \code{itemtype} and \code{values}
#'   are used as arguments for \code{\link[mirt]{mirt}}; \code{lambda} is the
#'   modified lambda matrix from the \code{model}-argument.
# @examples
#' @export
write_mirt_input <- function(model = NULL,
                             data = NULL) {

    if (!inherits(model, "tree_model")) {
        model <- tree_model(model = model)
        class <- model$class
    }

    checkmate::assert_data_frame(data, types = "integerish", any.missing = TRUE,
                                 all.missing = FALSE, ncols = (model$P)*model$J)

    lambda <- model$lambda

    lambda <- lambda[order(lambda$trait, lambda$item), ]
    lambda$new_name <- names(data)

    # args$model$lambda <- model$lambda <- lambda

    mirt1 <- split(lambda, lambda$trait)

    mirt2 <- vapply(seq_along(mirt1), function(x) {
        glue::glue_collapse(c(glue::glue("{names(mirt1[x])} = "),
                              # glue::glue(" {mirt1[[x]]$new_name}", ",")
                              glue::glue_collapse(glue::glue("{mirt1[[x]]$new_name}"), sep = ", ")
        ))
    }, FUN.VALUE = "")

    mirt3 <- NULL
    if (model$class == "tree") {
        # if (all(lambda$loading == "*")) {
        #     itemtype <- "2PL"
        # } else if (all(lambda$loading == "@1")) {
        #     itemtype <- "Rasch"
        # } else {
        #     itemtype <- "2PL"
        # }
        itemtype <- ifelse(lambda$loading == "@1", "Rasch", "2PL")
    } else if (model$class == "grm") {
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
    tmp1 <- expand.grid(seq_len(model$S), seq_len(model$S))
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
    #         item  = gl(model$J, k = model$S, labels = model$j_names),
    #         trait = gl(model$S, k = 1, labels = model$s_names)
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

    # itemtype <- switch(model$class,
    #     tree = "2PL",
    #     grm  = "graded",
    #     NULL
    # )

    # if (model$class == "tree") {
    #     itemtype <- ifelse(lambda$loading == "@1", "Rasch", "2PL")
    # } else if (model$class == "grm") {
    #     itemtype <- "graded"
    # }

    # tmp1 <- data.frame(
    #     item  = gl(model$J, k = model$S, labels = model$j_names),
    #     trait = gl(model$S, k = 1, labels = model$s_names)
    # )
    #
    # lambda2 <- dplyr::left_join(tmp1, lambda, by = c("item", "trait"))
    # lambda2$est <- FALSE
    # lambda2$est[lambda2$loading == "*"] <- TRUE
    # lambda2$value <- 0
    # lambda2$value[!is.na(lambda2$loading)] <- 1
    #
    # values <- mirt(data = data,
    #                model = mirt_string,
    #                itemtype = itemtype,
    #                pars = "values")
    #
    # values[grepl("^COV",   values$name), "est"]   <- TRUE
    # values[grepl("^a\\d+", values$name), "est"]   <- lambda2$est
    # values[grepl("^a\\d+", values$name), "value"] <- lambda2$value

    # tmp1 <- vapply(model$s_names,
    #                function(x) paste(rep(x, 2), collapse = "*"),
    #                FUN.VALUE = "",
    #                USE.NAMES = FALSE)
    # tmp2 <- clps("*", model$s_names)
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
#'   as returned from \code{\link{fit_tree_mirt}}.
#' @param ... Further arguments passed to \code{\link[mirt]{fscores}}
#' @inheritParams fit_tree_mplus
#' @inheritParams mirt::fscores
#' @return A list of parameter estimates and model fit information.
# @examples
#' @export
#' @importClassesFrom mirt SingleGroupClass
# @importMethodsFrom mirt fscores
#' @export
extract_mirt_output <- function(results = NULL,
                                model = NULL,
                                method = "MAP",
                                ...) {

    if (!inherits(model, "tree_model")) {
        model <- tree_model(model)
    }

    # fscores <- mirt::fscores(results, method = method, ...)

    lambda <- model$lambda

    # lv_names <- unstd[unstd$paramHeader == "Variances", "param"]

    cf <- mirt::coef(results, printSE = FALSE, simplify = TRUE)

    sigma <- cf$cov
    sigma[upper.tri(sigma)] <- t(sigma)[upper.tri(sigma)]

    # sig1 <- cf$GroupPars[, grep("^COV_", colnames(cf$GroupPars))]
    # sigma <- matrix(NA, model$S, model$S)
    # sigma[lower.tri(sigma, TRUE)] <- sig1
    # sigma[upper.tri(sigma)] <- t(sigma)[upper.tri(sigma)]

    cormat <- cov2cor(sigma)

    personpar_est <- mirt::fscores(results, method = method, ...)
    # personpar_se  <-
    colnames(personpar_est) <- model$lv_names
    # colnames(personpar_se)  <- model$s_names

    itempar <- list()

    if (model$class == "tree") {
        betapar <- cf$items[, "d"]
        tmp1 <- cbind(lambda[, c("item", "trait")], "est" = betapar)
        itempar$beta <- reshape2::dcast(tmp1, item ~ trait, value.var = "est")[, -1]

        tmp1 <- cf$items[, grepl("a\\d+", colnames(cf$items))]

        if (sum(tmp1 == 0) == model$J * model$P) {
            tmp2 <- cbind(lambda[, c("item", "trait")], "est" = tmp1[tmp1 != 0])
            itempar$alpha <- reshape2::dcast(tmp2, item ~ trait, value.var = "est")[, -1]
        } else {
            itempar$alpha <- tmp1
        }
    } else if (model$class == "grm") {
        betapar <- cf$items[, grepl("^d\\d+", colnames(cf$items))]

        itempar$alpha <- cf$items[, grepl("a\\d+", colnames(cf$items))]
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
