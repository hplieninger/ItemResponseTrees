#' Retrieve estimates From Mplus.
#'
#' This function takes the output from Mplus as returned from
#' [irtree_fit_mplus()] and returns the estimates in a convenient way.
#'
#' @param results A list as returned from [irtree_fit_mplus()].
#' @param object A description of the user-specified model. See
#'   [irtree_model] for more information.
#' @param class String specifying which class of model was fit
#' @param .errors2messages Logical indicating whether errors should be converted
#'   to messages
#' @return A list of parameter estimates, model fit information
#'   (`summaries`), `warnings`, `errors`.
#'
#' @name extract_mplus_output-deprecated
#' @usage extract_mplus_output(results = NULL, object = NULL, class = NULL,
#'   .errors2messages = FALSE)
#' @seealso [ItemResponseTrees-deprecated]
#' @keywords internal
NULL

#' @rdname ItemResponseTrees-deprecated
#' @usage NULL
#' @section `extract_mplus_output()`:
#' This function is deprecated. Use `glance()`, `tidy()`, and `augment()`
#'   instead.
#'
#' @export
#' @keywords internal
extract_mplus_output <- function(results = NULL, # nocov start
                                 object = NULL,
                                 class = NULL,
                                 .errors2messages = FALSE) {
    .Deprecated("tidy.irtree_fit")

    checkmate::assert_class(results, "mplus.model")

    checkmate::assert_class(object, "irtree_model", null.ok = TRUE)

    tmp1 <- vapply(results$errors, function(x) {
        any(stringr::str_detect(x,
                                "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY"))},
        FUN.VALUE = logical(1))

    if (any(tmp1)) {
        if (.errors2messages) {
            lapply(results$errors[cumsum(tmp1) > 0], function(x) message("Mplus error: ", clps(" ", x), call. = FALSE))
        } else {
            stop("Mplus error: ", clps(" ", unlist(results$errors[cumsum(tmp1) > 0])), call. = FALSE)
        }
    }

    e2 <- new.env()
    e2$lv_names <-
        as.character(
            na.omit(
                stringr::str_extract(results$input$object, "\\w+(?=\\s*BY)")))
    if (!is.null(object)) {
        e2$class <- object$class
    } else {
        checkmate::assert_choice(class, choices = c("tree", "grm"))
        e2$class <- class
    }

    results$parameters <- lapply(results$parameters, function(x) {
        x$se <- ifelse(x$pval == 999, NA, x$se)
        x$est_se <- ifelse(x$pval == 999, NA, x$est_se)
        x$pval <- ifelse(x$pval == 999, NA, x$pval)
        return(x)
    })

    unstd <- results[["parameters"]][["unstandardized"]]
    class(unstd) <- "data.frame"

    if (!(is.null(results[["savedata"]]) | length(results[["savedata"]]) == 0)) {

        fscores <- results[["savedata"]]

        fscore_cols1 <- is.element(toupper(names(fscores)),
                                   toupper(e2$lv_names))
        fscore_cols2 <- is.element(toupper(names(fscores)),
                                   toupper(paste0(e2$lv_names, "_SE")))
        personpar_est <- fscores[, fscore_cols1, drop = FALSE]
        personpar_se  <- fscores[, fscore_cols2, drop = FALSE]

    } else {
        personpar_est <- NULL
        personpar_se  <- NULL
    }

    if (!(is.null(results[["tech4"]][["latCovEst"]]))) {

        sigma  <- results[["tech4"]][["latCovEst"]]
        cormat <- results[["tech4"]][["latCorEst"]]

        if (!is.null(e2$lv_names)) {
            sigma  <-  sigma[toupper(e2$lv_names), toupper(e2$lv_names), drop = FALSE]
            cormat <- cormat[toupper(e2$lv_names), toupper(e2$lv_names), drop = FALSE]
        }

    } else {
        sigma  <- NULL
        cormat <- NULL
    }


    if (!is.null(e2$lambda$new_name)) {
        alphapar    <- unstd[tolower(unstd$param) %in%        tolower(e2$lambda$new_name), , drop = FALSE]
        betapar     <- unstd[tolower(unstd$param) %in% paste0(tolower(e2$lambda$new_name), "$1"), , drop = FALSE]
    } else {
        alphapar    <- unstd[grep("[.]BY$", unstd$paramHeader), , drop = FALSE]
        betapar     <- unstd[unstd$paramHeader == "Thresholds", , drop = FALSE]
    }
    alphapar$param <- factor(alphapar$param, levels = unique(alphapar$param))
    rownames(alphapar) <- NULL
    rownames(betapar) <- NULL

    itempar <- list()

    if (e2$class == "tree") {

        betapar <- tidyr::separate(betapar, "param", c("traititem", "threshold"),
                                   sep = "\\$", extra = "merge")
        betapar <- tidyr::separate(betapar, "traititem", c("trait", "item"),
                                   sep = "_", extra = "merge", fill = "right")
        betapar$trait <- factor(betapar$trait, levels = unique(betapar$trait))
        betapar$item  <- factor(betapar$item, levels = unique(betapar$item))

        itempar$beta <- reshape(
            dplyr::select(betapar, .data$trait, .data$item, .data$est),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$beta) <- sub("^est[.]", "", names(itempar$beta))
        itempar$beta_se <- reshape(
            dplyr::select(betapar, .data$trait, .data$item, .data$se),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$beta_se) <- sub("^se[.]", "", names(itempar$beta_se))

        alphapar <- tidyr::separate(alphapar, "param", c("trait", "item"),
                                    sep = "_", extra = "merge", fill = "right")
        alphapar$trait <- factor(alphapar$trait, levels = unique(alphapar$trait))
        alphapar$item  <- factor(alphapar$item, levels = unique(alphapar$item))

        itempar$alpha <- reshape(
            dplyr::select(alphapar, .data$trait, .data$item, .data$est),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$alpha) <- sub("^est[.]", "", names(itempar$alpha))
        itempar$alpha_se <- reshape(
            dplyr::select(alphapar, .data$trait, .data$item, .data$se),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$alpha_se) <- sub("^se[.]", "", names(itempar$alpha_se))

    } else if (e2$class == "grm") {

        tmp1 <- tidyr::separate(betapar, "param", c("item", "threshold"),
                                sep = "\\$")

        itempar$beta <- reshape(
            dplyr::select(tmp1, .data$item, .data$threshold, .data$est),
            direction = "wide",
            idvar = "item",
            timevar = "threshold"
        )
        names(itempar$beta) <- sub("^est[.]", "b", names(itempar$beta))

        itempar$beta_se <- reshape(
            dplyr::select(tmp1, .data$item, .data$threshold, .data$se),
            direction = "wide",
            idvar = "item",
            timevar = "threshold"
        )
        names(itempar$beta_se) <- sub("^se[.]", "b", names(itempar$beta_se))

        itempar$alpha <- dplyr::select(alphapar, item = .data$param, .data$est)
        itempar$alpha_se <- dplyr::select(alphapar, item = .data$param, .data$se)

        itempar <- lapply(itempar, function(x) {
            x$item <- factor(x$item, unique(alphapar$param))
            x[order(x$item), ]
        })
    }

    out <- list(
        person     = list(personpar_est = personpar_est,
                          personpar_se  = personpar_se),
        item       = itempar,
        sigma      = sigma,
        cormat     = cormat,
        summaries  = results$summaries,
        warnings   = results$warnings,
        errors     = results$errors,
        parameters = results$parameters
    )

    return(out)
} # nocov end
