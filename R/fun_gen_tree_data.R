#' Generate Data From an IR-Tree Model
#'
#' This function generates data from an IR-tree model.
#'
#' @param N Integer, the number of persons.
#' @param sigma Either a matrix or a function that returns a matrix. This matrix
#'   is the variance-covariance matrix of the person parameters that is passed
#'   to \code{\link[MASS]{mvrnorm}}. Note that the order of the person
#'   parameters is taken from the section Processes in the \code{model} (see
#'   \code{\link{tree_model}}).
#' @param itempar Either a list or a function that returns a list. The list has
#'   an element \code{beta} and an element \code{alpha}. Each of these is a
#'   matrix of item parameters. Note theat the order of items (rows) is taken from the
#'   section Items and the order of processes (columns) is taken from the
#'   section Processes in the \code{model} (see \code{\link{tree_model}}).
#' @param K Integer, number of categories. Needed only not defined by the
#'   equations in \code{model}.
#' @inheritParams fit_tree_mplus
#' @return A list with element \code{X} containing the data and an
#'   element \code{args} containing the true parameter values etc.
# @examples
#' @export
# @import MASS
# @import dplyr
# @import reshape2

gen_tree_data <- function(model = NULL,
                          N = NULL,
                          sigma = NULL,
                          itempar = NULL,
                          link = c("probit", "logit"),
                          K = NULL) {

    link <- match.arg(link)

    args <- c(as.list(environment())
              # , list(...)
    )

    # args <- list(model = model,
    #              model2 = model2,
    #              N = N,
    #              # sigma = sigma,
    #              # itempar = itempar,
    #              link = link)

    link <- switch(link,
                   probit = setNames("pnorm", link),
                   logit  = setNames("plogis", link))

    # if (is.null(model2)) {
    #     model2 <- tree_model(model = model)
    # }
    if (!inherits(model, "tree_model")) {
        model <- tree_model(model = model)
    }

    # if (any(ls() %in% names(model))) {
    #     stop("Internal error, please contact mantainer.")
    # }
    # list2env(model[names(model) != "string"], envir = environment())

    S <- model$S
    s_names <- model$s_names
    J <- model$J
    J2 <- length(model$items)
    j_names <- model$j_names
    P <- model$P
    p_names <- model$p_names
    # K <- model$K
    if (is.null(model$K)) {
        checkmate::qassert(K, "X1[2,)")
    } else {
        checkmate::qassert(model$K, "X1[2,)")
        K <- model$K
    }
    lambda <- model$lambda
    subtree <- model$subtree
    expr <- model$expr

    if (is.function(sigma)) {
        FUN <- match.fun(sigma)
        args$sigma_fun <- FUN
        sigma <- FUN()
    }
    args$sigma <- sigma

    checkmate::assert_matrix(sigma, mode = "numeric", any.missing = FALSE,
                             nrows = S, ncols = S)

    if (is.function(itempar)) {
        FUN <- match.fun(itempar)
        # environment(FUN) <- environment()
        args$itempar_fun <- FUN
        itempar <- FUN()
    }
    args$itempar <- itempar

    checkmate::assert_list(itempar, types = "numeric", any.missing = FALSE,
                           len = 2,
                           names = "named")
    checkmate::assert_names(names(itempar), permutation.of = c("beta", "alpha"))

    checkmate::assert_matrix(itempar$beta, mode = "numeric", any.missing = FALSE,
                             nrows = J2, ncols = P)
    checkmate::assert_matrix(itempar$alpha, mode = "numeric", any.missing = FALSE,
                             nrows = J2, ncols = P)

    ### Parse item parameters ###

    beta_names  <- paste0("beta",  1:P)
    alpha_names <- paste0("alpha", 1:P)
    # theta_names <- paste0("theta", 1:S)
    theta_names <- paste0("theta", 1:P)

    betas  <- data.frame(item = factor(model$items, levels = model$items),
                         setNames(data.frame(itempar$beta), nm = beta_names))
    alphas <- data.frame(item = factor(model$items, levels = model$items),
                         setNames(data.frame(itempar$alpha), nm = alpha_names))

    ### Data generation ###

    # Make a 'long' data frame with N*J2*K rows.
    # Add the thetas and the betas in columns
    # Combine them, i.e., pnorm(theta - beta)
    # Apply model equation for each categ, i.e., p1*(1-p2)

    dat1 <- data.frame(
        pers = gl(N, J2*K, length = J2*K*N),
        item = gl(J2, K,   length = J2*K*N, labels = model$items),
        cate = gl(K, 1,    length = J2*K*N)
    )

    tmp1 <- MASS::mvrnorm(ifelse(N == 1, 1.001, N), mu = rep(0, S), Sigma = sigma)
    colnames(tmp1) <- s_names
    args$personpar <- personpar <- data.frame(pers = gl(N, 1), tmp1)

    dat2 <- dplyr::left_join(dat1, personpar, by = "pers")

    # From dat2 to dat6:
    # Apply subtree-structure such that each item loads on the correct theta,
    # e.g., if t = t1 + t2

    dat3 <- reshape2::melt(dat2, id.vars = c("pers", "item", "cate"), variable.name = "trait")

    dat5 <- dplyr::inner_join(dat3, lambda, by = c("item", "trait"))

    for (ii in seq_len(nrow(subtree))) {
        tmpx <- gsub(subtree[ii, 2], subtree[ii, 1], levels(dat5$trait))
        levels(dat5$trait) <- tmpx
    }

    # dat5$trait <- factor(dat5$trait, levels = levels(dat5$trait),
    #                      labels = paste0(levels(dat5$trait), "i"))
    dat5$trait <- factor(dat5$trait, levels = levels(dat5$trait),
                         labels = theta_names)

    dat6 <- reshape2::dcast(dat5, pers + item + cate ~ trait, value.var = "value")

    # Add item parameters and calculate probabilities

    dat7 <- dplyr::left_join(dat6, betas, by = "item")
    dat7 <- dplyr::left_join(dat7, alphas, by = "item")

    # for (ii in seq_len(P)) {
    #     dat7[, as.character(p_names[ii])] <- do.call(link,
    #                                    list(dat7[, paste0(p_names[ii], "i")] -
    #                                             dat7[, paste0(p_names[ii], "j")]))
    # }
    for (ii in seq_len(P)) {
        dat7[, as.character(p_names[ii])] <- do.call(link,
                                                     list(dat7[, alpha_names[ii]]*(
                                                         dat7[, theta_names[ii]] -
                                                             dat7[, beta_names[ii]])))
    }

    dat8 <- split(dat7, dat7$cate)

    for (ii in seq_along(dat8)) {
        dat8[[ii]] <- within(dat8[[ii]], prob <- eval(expr[[names(dat8)[ii]]]))
    }

    probs <- unsplit(dat8, dat7$cate)

    prob_item_sum <- aggregate(prob ~ pers + item, data = probs, sum)$prob
    if (!isTRUE(all.equal(prob_item_sum, rep(1, N*J2)))) {
        # stop("Probabilities do not sum to 1 within each person-item combination. ",
        #      "Make sure to provide model equations that define a proper IR-tree model", call. = FALSE)
        rlang::abort(
            paste("Probabilities do not sum to 1 within each person-item combination.",
                  "Make sure to provide model equations that define a proper IR-tree model"),
            type = "improper_model")
    }

    dat10 <- aggregate(prob ~ pers + item,
                       data = probs,
                       function(x) which(rmultinom(n = 1, size = 1, prob = x) == 1))

    X <- reshape2::dcast(dat10, pers ~ item, value.var = "prob")[, -1]

    return(list(data = X, args = args))

}

#' Recode Data Set Into Pseudoitems
#'
#' This function takes a data set with polytomous items and an IR-tree model and
#' returns the recoded items, the so-called pseudoitems.
#'
#' @inheritParams fit_tree_mplus
#' @param keep Logical indicating whether to append the original items to the
#'   data frame of the generated pseudoitems
#' @return Data frame
# @examples
#' @export
# @import checkmate
# @import reshape2
# @import dplyr
recode_data <- function(model = NULL,
                        # model2 = NULL,
                        data = NULL,
                        keep = FALSE) {

    if (!inherits(model, "tree_model")) {
        model <- tree_model(model = model)
    }

    # if (any(ls() %in% names(model))) {
    #     stop("Internal error, please contact mantainer.")
    # }
    # list2env(model[names(model) != "string"], envir = environment())

    # S <- model$S
    # s_names <- model$s_names
    J <- model$J
    J2 <- length(model$items)
    j_names <- model$j_names
    P <- model$P
    p_names <- model$p_names
    K <- model$K
    # lambda <- model$lambda
    # subtree <- model$subtree
    # expr <- model$expr
    equations <- model$equations

    # checkmate::assert_data_frame(data,
    #                              # types = "numeric",
    #                              all.missing = FALSE, min.rows = 1, min.cols = J)
    # checkmate::assert_data_frame(data[, j_names], types = "integerish",
    #                              ncols = J)
    # # checkmate::assert_set_equal(names(data), y = levels(j_names))
    # checkmate::assert_subset(j_names, choices = names(data))

    ### mapping matrix for pseudoitems ###

    mapping_matrix <- matrix(NA, K, P, dimnames = list(NULL, p_names))
    mapping_matrix <- cbind(cate = 1:K, mapping_matrix)

    for (ii in seq_along(p_names)) {
        pseudoitem <- ifelse(sapply(equations[2,], grepl, pattern = p_names[ii], perl = TRUE),
                             no = NA,
                             yes = ifelse(sapply(equations[2, ],
                                                 grepl,
                                                 pattern = paste0("(?<![-])", p_names[ii]),
                                                 perl = TRUE),
                                          yes = 1L, no = 0L))
        mapping_matrix[, p_names[ii]] <- pseudoitem
    }

    dat10 <- reshape2::melt(cbind(pers = seq_len(nrow(data)), data[, model$items]),
                            id.vars = "pers",
                            variable.name = "item",
                            value.name = "cate")
    dat10$item <- factor(dat10$item, levels = j_names)

    dat11 <- dplyr::left_join(dat10, data.frame(mapping_matrix), by = "cate")
    # dat11 <- dplyr::left_join(dat10, data.frame(mapping_matrix), by = c("prob" = "cate"))

    dat12 <- reshape2::melt(dat11, id.vars = c("pers", "item"), measure.vars = p_names)

    X2 <- reshape2::dcast(dat12, pers ~ variable + item, value.var = "value")[, -1]

    # Make names no longer than 8 chars for Mplus
    names(X2) <- substr(names(X2), 1, 8)
    ii <- 7
    while (length(unique(names(X2))) != ncol(X2)) {
        tmp1 <- make.unique(substr(names(X2), 1, ii), sep = "_")
        names(X2) <- substr(tmp1, 1, 8)
        ii <- ii - 1
    }

    if (keep) {
        X3 <- cbind(X2, data)
    } else {
        X3 <- cbind(X2, data[, !names(data) %in% model$items, drop = FALSE])
    }

    attr(X3, "mapping_matrix") <- mapping_matrix

    attr(X3, "pseudoitem_names") <- names(X2)

    return(X3)
}
