#' @title Retrieve estimates from mirt.
#'
#' @description This function takes the output from [irtree_fit_mirt()] and
#'   returns the parameter estimates in a convenient way.
#'
#' @param results An object of class \code{\link[mirt]{SingleGroupClass-class}}
#'   as returned from [irtree_fit_mirt()].
#' @param object A description of the user-specified model. See
#'   [irtree_model] for more information.
#' @param method Passed to [mirt::fscores()].
#' @param class String specifying which class of model was fit.
#' @param ... Passed to [mirt::fscores()].
#' @return A list of parameter estimates and model fit information.
#'
#' @name extract_mirt_output-deprecated
#' @usage extract_mirt_output(results = NULL, object = NULL, method = "MAP",
#'   class = NULL, ...)
#' @seealso [ItemResponseTrees-deprecated]
#' @keywords internal
NULL

#' @rdname ItemResponseTrees-deprecated
#' @usage NULL
#' @section `extract_mirt_output()`:
#' This function is deprecated. Use `glance()`, `tidy()`, and `augment()`
#'   instead.
#'
#' @export
extract_mirt_output <- function(results = NULL, # nocov start
                                object = NULL,
                                method = "MAP",
                                class = NULL,
                                ...) {

    .Deprecated("tidy.irtree_fit")

    checkmate::assert_class(results, "SingleGroupClass")

    checkmate::assert_class(object, "irtree_model", null.ok = TRUE)

    e2 <- new.env()

    if (!is.null(object)) {
        e2$class <- object$class
    } else {
        checkmate::assert_choice(class, choices = c("tree", "grm"))
        e2$class <- class
    }

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
    )

    return(out)
} # nocov end
