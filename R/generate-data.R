#' Generate data
#'
#' This function generates data from an ItemResponseTrees model.
#'
#' @param N Integer, the number of persons.
#' @param sigma Either a matrix or a function that returns a matrix. This matrix
#'   is the variance-covariance matrix of the person parameters that is passed
#'   to [MASS::mvrnorm()]. Note that the order of the person
#'   parameters is taken from the section Processes in the model `object` (see
#'   [irtree_model]).
#' @param theta Optional numeric matrix of person parameters with one row per person and
#'   one column per dimension (i.e., `object$S`). If provided, this overrides
#'   `N` and `sigma`.
#' @param itempar Either a list or a function that returns a list. The list has
#'   an element `beta` and an element `alpha`. Each of these is a
#'   matrix of item parameters. Note that the order of items (rows) is taken from the
#'   section Items and the order of processes (columns) is taken from the
#'   section Processes in the `model` (see [irtree_model]).
#' @param link Character. Link function to use.
#' @param na_okay Logical indicating whether variables with unobserved response
#'   categories are permitted. If `FALSE`, rejection sampling
#'   is used to ensure that all categories are observed.
#' @param skip Logical. Some features of the [irtree_model] syntax,
#'   which are available for model fitting (e.g., `Addendum`), are not
#'   implemented for data generation. Those parts of the model are ignored if
#'   `skip = TRUE`.
#' @inheritParams fit.irtree_model
#' @return A list with element `data` containing the data and an
#'   element `spec` containing the true parameter values etc.
#' @example inst/examples/example-generate-data.R
#' @export
irtree_gen_data <- function(object = NULL,
                            N = NULL,
                            sigma = NULL,
                            theta = NULL,
                            itempar = NULL,
                            link = c("logit", "probit"),
                            na_okay = TRUE,
                            skip = FALSE
) {

    checkmate::assert_class(object, "irtree_model")

    .must_have(object, "addendum", FALSE, skip = skip)

    link <- match.arg(link)

    if (object$class == "tree") {
        out <- irtree_gen_tree(object = object,
                               N = N,
                               sigma = sigma,
                               theta = theta,
                               itempar = itempar,
                               link = link,
                               na_okay = na_okay)
    } else if (object$class == "pcm") {
        out <- irtree_gen_pcm(object = object,
                              N = N,
                              sigma = sigma,
                              theta = theta,
                              itempar = itempar,
                              link = link,
                              na_okay = na_okay,
                              skip = skip)
    } else {
        stop("Class ", object$class, " not implemented in irtree_gen_data().", call. = FALSE)
    }
    return(out)
}

#' Generate data from an IR-tree model
#'
#' This function generates data from an IR-tree model.
#'
#' @inheritParams irtree_gen_data
#' @return A list with element `data` containing the data and an
#'   element `spec` containing the true parameter values etc.
#' @keywords internal
irtree_gen_tree <- function(object = NULL,
                            N = NULL,
                            sigma = NULL,
                            theta = NULL,
                            itempar = NULL,
                            link = c("logit", "probit"),
                            na_okay = TRUE) {

    link <- match.arg(link)

    spec <- c(as.list(environment()))
    spec$J <- object$J

    link <- switch(link,
                   probit = setNames("pnorm", link),
                   logit  = setNames("plogis", link))

    S <- object$S
    J <- object$J
    j_names <- object$j_names
    P <- object$P
    p_names <- levels(object$lambda$mpt)
    checkmate::qassert(object$K, "X1[2,)")
    K <- object$K
    lambda <- object$lambda
    expr <- object$expr

    checkmate::assert_int(N, lower = 1, null.ok = !is.null(theta))

    if (is.function(sigma)) {
        FUN <- match.fun(sigma)
        spec$sigma_fun <- FUN
        sigma <- FUN()
    }
    spec$sigma <- sigma

    checkmate::assert_matrix(sigma, mode = "numeric", any.missing = FALSE,
                             nrows = S, ncols = S, null.ok = !is.null(theta))

    if (!is.null(theta)) {
        spec$theta <- theta <- data.matrix(theta, rownames.force = FALSE)
        N <- nrow(theta)
    }
    checkmate::assert_matrix(theta, mode = "numeric", min.rows = 1,
                             ncols = object$S, null.ok = !is.null(sigma))

    if (is.function(itempar)) {
        FUN <- match.fun(itempar)
        spec$itempar_fun <- FUN
        itempar <- FUN()
    } else {
        itempar <- lapply(itempar, data.matrix, rownames.force = FALSE)
    }
    spec$itempar <- itempar

    checkmate::assert_list(itempar, types = "numeric", any.missing = FALSE,
                           len = 2,
                           names = "named")
    checkmate::assert_names(names(itempar), permutation.of = c("beta", "alpha"))

    checkmate::assert_matrix(itempar$beta, mode = "numeric",
                             nrows = J, ncols = P)
    checkmate::assert_matrix(itempar$alpha, mode = "numeric",
                             nrows = J, ncols = P)

    ### Parse item parameters ###

    beta_names  <- paste0("beta",  1:P)
    alpha_names <- paste0("alpha", 1:P)

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
        pers  = gl(N, J*K, length = J*K*N),
        item  = gl(J, K,   length = J*K*N, labels = j_names),
        categ = gl(K, 1,   length = J*K*N, labels = object$k_names)
    )

    if (is.null(theta)) {
        theta <- MASS::mvrnorm(ifelse(N == 1, 1.001, N), mu = rep(0, S), Sigma = sigma)
    }
    colnames(theta) <- levels(lambda$theta)
    spec$personpar <- theta
    spec$theta <- NULL

    dat2 <- dplyr::left_join(dat1,
                             data.frame(pers = gl(N, 1), theta),
                             by = "pers")

    # From dat2 to dat6:
    # Apply subtree-structure such that each item loads on the correct theta,
    # e.g., if t = t1 + t2

    dat3 <- tidyr::pivot_longer(
        dat2, -c("pers", "item", "categ"), names_to = "theta",
        names_ptypes = list(theta = factor(levels = unique(object$latent_names$theta))))

    dat5 <- dplyr::inner_join(dat3, lambda, by = c("item", "theta"))

    dat6 <- tidyr::pivot_wider(dat5, id_cols = .data$pers:.data$categ,
                               names_from = "mpt", values_from = "value")

    if (any(is.na(dat6))) {
        tmp1 <- which(is.na(dat6[, p_names]), arr.ind = TRUE)
        stop("Problem in 'model': Every process must be measured by all items. ",
             "However, item '", dat6[tmp1[1, 1], "item"], "' is not listed for ",
             "process '", p_names[tmp1[1, 2]], "', for example.", call. = FALSE)
    }

    # Add item parameters and calculate probabilities

    dat7 <- dplyr::left_join(dat6, betas, by = "item")
    dat7 <- dplyr::left_join(dat7, alphas, by = "item")

    for (ii in seq_len(P)) {
        dat7[, as.character(p_names[ii])] <- do.call(link,
                                                     list(dplyr::pull(dat7, alpha_names[ii])*(
                                                         dplyr::pull(dat7, p_names[ii]) -
                                                             dplyr::pull(dat7, beta_names[ii]))))
    }

    dat8 <- split(dat7, dat7$categ)

    for (ii in seq_along(dat8)) {
        dat8[[ii]] <- within(dat8[[ii]], "prob" <- eval(expr[[names(dat8)[ii]]]))
    }

    probs <- dplyr::bind_rows(dat8)
    p_return <- dplyr::select(probs, c("pers", "item", "categ", "prob")) %>%
        dplyr::arrange(.data$pers, .data$item, .data$categ)

    prob_item_sum <- dplyr::summarize(
        dplyr::group_by(probs, .data$pers, .data$item),
        p = sum(.data$prob))$p
    if (!isTRUE(all.equal(prob_item_sum, rep(1, N*J)))) {
        rlang::abort(
            paste("Probabilities do not sum to 1 within each person-item combination.",
                  "Make sure to provide model equations that define a proper IR-tree model."),
            .subclass = "improper_model")
    }

    f1 <- function(x) object$k_names[rmultinom(n = 1, size = 1, prob = x) == 1]
    dat10 <- dplyr::group_by(probs, .data$pers, .data$item) %>%
        dplyr::summarize(p = f1(.data$prob)) %>%
        dplyr::ungroup()

    X <- tidyr::pivot_wider(dat10, .data$pers, names_from = "item", values_from = "p") %>%
        dplyr::select(-.data$pers)

    if (!na_okay) {
        ii <- 0
        while (!.check_all_categ_observed(X, object$K)) {
            dat10 <- dplyr::group_by(probs, .data$pers, .data$item) %>%
                dplyr::summarize(p = f1(.data$prob)) %>%
                dplyr::ungroup()
            X <- tidyr::pivot_wider(dat10, .data$pers, names_from = "item", values_from = "p") %>%
                dplyr::select(-.data$pers)
            ii <- ii + 1
            if (ii >= 25) stop("Could not generate data without missing categories.")
        }
    }

    return(list(data = tibble::as_tibble(X),
                probs = p_return, spec = spec))

}

#' Recode data into pseudoitems
#'
#' This function takes a data set with polytomous items and an `irtree_model`
#' and returns the recoded items, the so-called pseudoitems.
#'
#' @inheritParams fit.irtree_model
#' @param keep Logical indicating whether to append the original items to the
#'   data frame of the generated pseudoitems
#' @param mapping_matrix Matrix of so-called pseudo-items, optional and
#'   overwritten by `object` if present. The observed response categories must
#'   appear in the first column. The other columns contain the pseudo-items and
#'   each entry may be either `1`, `0`, or `NA`. The first column name must be
#'   `categ`, and the other names (related to the pseudo-items) are used to
#'   construct the names of the returned data frame.
#' @return Data frame
#' @examples
#' m1 <- "
#' IRT:
#' t BY x1;
#' e BY x1;
#' m BY x1;
#' Equations:
#' 1 = (1-m)*(1-t)*e
#' 2 = (1-m)*(1-t)*(1-e)
#' 3 = m
#' 4 = (1-m)*t*(1-e)
#' 5 = (1-m)*t*e
#' Class:
#' Tree
#' "
#' model1 <- irtree_model(m1)
#' dat <- data.frame(x1 = 1:5)
#' irtree_recode(model1, dat, keep = TRUE)
#'
#' irtree_recode(data = dat,
#'               mapping_matrix = cbind(categ = 1:5,
#'                                      m = c(0, 0, 1, 0, 0),
#'                                      t = c(1, 1, NA, 0, 0),
#'                                      e = c(1, 0, NA, 0, 1)))
#' @export
irtree_recode <- function(object = NULL,
                          data = NULL,
                          keep = FALSE,
                          mapping_matrix = NULL) {

    checkmate::assert_class(object, "irtree_model",
                            null.ok = !is.null(mapping_matrix))
    checkmate::assert_data_frame(data)

    if (is.null(object)) {
        mapping_matrix <- data.matrix(mapping_matrix)
        checkmate::assert_matrix(mapping_matrix, col.names = "strict")
        checkmate::assert_names(colnames(mapping_matrix)[1],
                                identical.to = "categ",
                                what = "colnames")
        checkmate::assert_subset(na.omit(unique(unlist(data))),
                                 mapping_matrix[, 1])
        checkmate::qassert(mapping_matrix[, -1], "x[0, 1]")

        j_names <- names(data)
        mpt_names <- p_names <- colnames(mapping_matrix[, -1])
    } else {
        j_names <- object$j_names
        p_names <- object$p_names
        mapping_matrix <- object$mapping_matrix
        mpt_names <- unique(object$latent_names$mpt)
    }

    data <- dplyr::mutate_at(data, j_names, ~`attributes<-`(.x, NULL))

    # data: polytomous items in wide format
    # PIs1: binary pseudoitems in wide format
    #
    # dat2: reshape data to long format
    # dat3: original responses 1,2,3,... are in column 'categ'. This is
    # left_joined with the mapping matrix such that the original polytomous
    # response is recoded into P binary pseudoitems.
    # dat4: pseudoitems P are wide, reshape to long format
    # PIs1: cast the pseudoitems back to wide format; this data frame has P*J columns
    # PIs2: cbind pseudoitems and original polytomous responses

    dat1 <- data.frame(pers = seq_len(nrow(data)), data[, j_names, drop = FALSE])
    dat2 <- tidyr::pivot_longer(
        dat1, cols = -.data$pers, names_to = "item", values_to = "categ",
        names_ptypes = list(item = factor(levels = j_names)))

    dat3 <- dplyr::left_join(dat2, data.frame(mapping_matrix), by = "categ") %>%
        dplyr::select(-.data$categ)

    dat4 <-
        tidyr::pivot_longer(
            data = dat3,
            cols = -(.data$pers:.data$item),
            names_to = "variable",
            names_ptypes = list(
                variable = factor(
                    levels = mpt_names))) %>%
        dplyr::arrange(.data$pers, .data$variable, .data$item)

    PIs1 <- tidyr::pivot_wider(dat4, "pers", names_from = c("variable", "item"),
                               values_from = "value") %>%
        dplyr::select(-.data$pers) %>%
        dplyr::mutate_all(~ vctrs::vec_cast(., integer()))

    if (keep) {
        PIs2 <- cbind(PIs1, data)
    } else {
        PIs2 <- cbind(PIs1, data[, !names(data) %in% j_names, drop = FALSE])
    }
    class(PIs2) <- class(data)

    return(PIs2)
}

.check_all_categ_observed <- function(data, K) {
    tmp1 <- apply(data, 2, function(x) {
        table(factor(x, seq(min(data), length.out = K)))
    })

    return(!any(tmp1 == 0))
}
