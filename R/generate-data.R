#' Generate Data From an IR-Tree Model
#'
#' This function generates data from an IR-tree model.
#'
#' @param N Integer, the number of persons.
#' @param sigma Either a matrix or a function that returns a matrix. This matrix
#'   is the variance-covariance matrix of the person parameters that is passed
#'   to [MASS::mvrnorm()]. Note that the order of the person
#'   parameters is taken from the section Processes in the model `object` (see
#'   [irtree_model()]).
#' @param itempar Either a list or a function that returns a list. The list has
#'   an element `beta` and an element `alpha`. Each of these is a
#'   matrix of item parameters. Note theat the order of items (rows) is taken from the
#'   section Items and the order of processes (columns) is taken from the
#'   section Processes in the `model` (see \code{\link{irtree_model}}).
#' @param link Character. Link function to use.
#' @param K Integer, number of categories. Needed only not defined by the
#'   equations in `model`.
#' @inheritParams fit.irtree_model
#' @return A list with element `X` containing the data and an
#'   element `args` containing the true parameter values etc.
#' @export
irtree_sim_data <- function(object = NULL,
                            N = NULL,
                            sigma = NULL,
                            itempar = NULL,
                            link = c("probit", "logit"),
                            K = NULL) {

    link <- match.arg(link)

    args <- c(as.list(environment())
              # , list(...)
    )

    link <- switch(link,
                   probit = setNames("pnorm", link),
                   logit  = setNames("plogis", link))

    object <- irtree_model(object)

    S <- object$S
    J <- object$J
    j_names <- object$j_names
    P <- object$P
    p_names <- object$p_names
    if (is.null(object$K)) {
        checkmate::qassert(K, "X1[2,)")
    } else {
        checkmate::qassert(object$K, "X1[2,)")
        K <- object$K
    }
    lambda <- object$lambda
    subtree <- object$subtree
    expr <- object$expr

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
        args$itempar_fun <- FUN
        itempar <- FUN()
    }
    args$itempar <- itempar

    checkmate::assert_list(itempar, types = "numeric", any.missing = FALSE,
                           len = 2,
                           names = "named")
    checkmate::assert_names(names(itempar), permutation.of = c("beta", "alpha"))

    checkmate::assert_matrix(itempar$beta, mode = "numeric", any.missing = FALSE,
                             nrows = J, ncols = P)
    checkmate::assert_matrix(itempar$alpha, mode = "numeric", any.missing = FALSE,
                             nrows = J, ncols = P)

    ### Parse item parameters ###

    beta_names  <- paste0("beta",  1:P)
    alpha_names <- paste0("alpha", 1:P)
    theta_names <- paste0("theta", 1:P)

    betas  <- data.frame(item = factor(j_names, levels = j_names),
                         setNames(data.frame(itempar$beta), nm = beta_names))
    alphas <- data.frame(item = factor(j_names, levels = j_names),
                         setNames(data.frame(itempar$alpha), nm = alpha_names))

    ### Data generation ###

    # Make a 'long' data frame with N*J*K rows.
    # Add the thetas and the betas in columns
    # Combine them, i.e., pnorm(theta - beta)
    # Apply model equation for each categ, i.e., p1*(1-p2)

    dat1 <- data.frame(
        pers = gl(N, J*K, length = J*K*N),
        item = gl(J, K,   length = J*K*N, labels = j_names),
        cate = gl(K, 1,    length = J*K*N)
    )

    tmp1 <- MASS::mvrnorm(ifelse(N == 1, 1.001, N), mu = rep(0, S), Sigma = sigma)
    colnames(tmp1) <- object$lv_names
    args$personpar <- personpar <- data.frame(pers = gl(N, 1), tmp1)

    dat2 <- dplyr::left_join(dat1, personpar, by = "pers")

    # From dat2 to dat6:
    # Apply subtree-structure such that each item loads on the correct theta,
    # e.g., if t = t1 + t2

    # dat3 <- reshape2::melt(dat2, id.vars = c("pers", "item", "cate"), variable.name = "trait")
    # tidyr::pivot_long(dat2, -c("pers", "item", "cate"), names_to = "trait")
    dat3 <- reshape(dat2, direction = "long",
                    idvar = c("pers", "item", "cate"),
                    varying = list(which(!is.element(names(dat2), c("pers", "item", "cate")))),
                    times = setdiff(names(dat2), c("pers", "item", "cate")),
                    timevar = "trait", v.names = "value")
    dat3$trait <- factor(dat3$trait, unique(dat3$trait))
    rownames(dat3) <- NULL

    dat5 <- dplyr::inner_join(dat3, lambda, by = c("item", "trait"))

    for (ii in seq_len(nrow(subtree))) {
        tmpx <- gsub(subtree[ii, 2], subtree[ii, 1], levels(dat5$trait))
        levels(dat5$trait) <- tmpx
    }

    dat5$trait <- factor(dat5$trait, levels = levels(dat5$trait),
                         labels = theta_names)

    # dat6 <- reshape2::dcast(dat5, pers + item + cate ~ trait, value.var = "value")
    # dat6 <- tidyr::spread(dplyr::select(dat5, pers, item, cate, trait, value),
    #                       trait, value)
    dat6 <- reshape(
        dplyr::select(dat5, .data$pers, .data$item, .data$cate, .data$trait, .data$value),
        direction = "wide",
        idvar = c("pers", "item", "cate"),
        v.names = "value",
        timevar = "trait")
    names(dat6) <- sub("^value[.]", "", names(dat6))

    # Add item parameters and calculate probabilities

    dat7 <- dplyr::left_join(dat6, betas, by = "item")
    dat7 <- dplyr::left_join(dat7, alphas, by = "item")

    for (ii in seq_len(P)) {
        dat7[, as.character(p_names[ii])] <- do.call(link,
                                                     list(dplyr::pull(dat7, alpha_names[ii])*(
                                                          dplyr::pull(dat7, theta_names[ii]) -
                                                          dplyr::pull(dat7, beta_names[ii]))))
    }

    dat8 <- split(dat7, dat7$cate)

    for (ii in seq_along(dat8)) {
        dat8[[ii]] <- within(dat8[[ii]], "prob" <- eval(expr[[names(dat8)[ii]]]))
    }

    probs <- dplyr::bind_rows(dat8)

    prob_item_sum <- aggregate(prob ~ pers + item, data = probs, sum)$prob
    if (!isTRUE(all.equal(prob_item_sum, rep(1, N*J)))) {
        rlang::abort(
            paste("Probabilities do not sum to 1 within each person-item combination.",
                  "Make sure to provide model equations that define a proper IR-tree model"),
            .subclass = "improper_model")
    }

    dat10 <- aggregate(prob ~ pers + item,
                       data = probs,
                       function(x) which(rmultinom(n = 1, size = 1, prob = x) == 1))

    # X <- reshape2::dcast(dat10, pers ~ item, value.var = "prob")[, -1]
    # tidyr::pivot_wide(dat10, names_from = "item", values_from = "prob")
    X <- reshape(dat10, direction = "wide", idvar = "pers", timevar = "item")
    X <- dplyr::select(X, -.data$pers)
    names(X) <- sub("^prob[.]", "", names(X))

    return(list(data = X, args = args))

}

#' Recode Data Set Into Pseudoitems.
#'
#' This function takes a data set with polytomous items and an IR-tree model and
#' returns the recoded items, the so-called pseudoitems.
#'
#' @inheritParams fit.irtree_model
#' @param keep Logical indicating whether to append the original items to the
#'   data frame of the generated pseudoitems
#' @return Data frame
# @examples
#' @export
irtree_recode <- function(object = NULL,
                          data = NULL,
                          keep = FALSE) {

    object <- irtree_model(object)

    j_names <- object$j_names
    p_names <- object$p_names
    mapping_matrix <- object$mapping_matrix

    # data: polytomous items in wide format
    # PIs1: binary pseudoitems in wide format
    #
    # dat2: reshape data to long format
    # dat3: original responses 1,2,3,... are in column 'cate'. This is
    # left_joined with the mapping matrix such that the original polytomous
    # response is recoded into P binary pseudoitems.
    # dat4: pseudoitems P are wide, reshape to long format
    # PIs1: cast the pseudoitems back to wide format; this data frame has P*J columns
    # PIs2: cbind pseudoitems and original polytomous responses

    # dat2 <- reshape2::melt(cbind(pers = seq_len(nrow(data)), data[, names(j_names)]),
    #                         id.vars = "pers",
    #                         variable.name = "item",
    #                         value.name = "cate")
    # dat2$item <- factor(dat2$item, levels = names(j_names), labels = j_names)
    tmp1 <- data.frame(pers = seq_len(nrow(data)), data[, names(j_names)])
    dat2 <- reshape(tmp1, direction = "long",
                    idvar = "pers",
                    varying = list(which(names(tmp1) != "pers")),
                    times = names(data[, names(j_names)]),
                    timevar = "item", v.names = "cate")
    dat2$item <- factor(dat2$item, levels = names(j_names), labels = j_names)
    rownames(dat2) <- NULL

    dat3 <- dplyr::left_join(dat2, data.frame(mapping_matrix), by = "cate")

    # dat4 <- reshape2::melt(dat3, id.vars = c("pers", "item"), measure.vars = p_names)
    dat4 <- reshape(dat3, direction = "long",
                    idvar = c("pers", "item"),
                    drop = "cate",
                    varying = list(which(!is.element(names(dat3), c("pers", "item", "cate")))),
                    times = setdiff(names(dat3), c("pers", "item", "cate")),
                    timevar = "variable", v.names = "value")
    rownames(dat4) <- NULL

    # dat5 <- split(dat4, dat4$variable)
    #
    # for (ii in seq_along(dat5)) {
    #     dat5[[ii]] <- dplyr::filter(dat5[[ii]], item %in%
    #                                  names(object$irt_items[[names(dat5)[ii]]]))
    # }
    # dat6 <- dplyr::bind_rows(dat5)

    # PIs1 <- reshape2::dcast(dat4, pers ~ variable + item, value.var = "value")[, -1]
    tmp1 <- tidyr::unite(dat4, "time", c("variable", "item"))
    tmp2 <- reshape(tmp1, direction = "wide", idvar = "pers")
    PIs1 <- dplyr::select(tmp2, -.data$pers)
    names(PIs1) <- sub("^value[.]", "", names(PIs1))

    # # Make names no longer than 8 chars for Mplus
    # names(PIs1) <- substr(names(PIs1), 1, 8)
    # ii <- 7
    # while (length(unique(tolower(names(PIs1)))) != ncol(PIs1)) {
    #     tmp1 <- make.unique(substr(names(PIs1), 1, ii), sep = "_")
    #     names(PIs1) <- substr(tmp1, 1, 8)
    #     ii <- ii - 1
    # }

    if (keep) {
        PIs2 <- cbind(PIs1, data)
    } else {
        PIs2 <- cbind(PIs1, data[, !names(data) %in% names(j_names), drop = FALSE])
    }

    # attr(PIs2, "mapping_matrix") <- mapping_matrix

    # attr(PIs2, "pseudoitem_names") <- names(PIs1)

    return(PIs2)
}
