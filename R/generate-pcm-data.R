#' Generate Data From a Partial Credit Model
#'
#' This function generates data from a (multidimensional) PCM
#'
#' @inheritParams irtree_gen_data
#' @return A list with element `data` containing the data and an
#'   element `spec` containing the true parameter values etc.
#' @examples
#' m1 <- "
#' IRT:
#' t BY x1@1;
#' e BY x1@1;
#' m BY x1@1;
#' Weights:
#' t = c(0, 1, 2, 3, 4)
#' e = c(1, 0, 0, 0, 1)
#' m = c(0, 0, 1, 0, 0)
#' Class:
#' PCM
#' "
#' model1 <- irtree_model(m1)
#' dat <- irtree_gen_pcm(model1, N = 5, sigma = diag(3),
#'                       itempar = list(beta = matrix(rnorm(4), 1, 4)))
#'
#' m2 <- "
#' IRT:
#' t BY x1@1, x2@1, x3@1, x4@1, x5@1;
#' Weights:
#' t = c(0, 1, 2, 3, 4)
#' Class:
#' PCM
#' "
#' model2 <- irtree_model(m2)
#' dat <- irtree_gen_pcm(model2, N = 5, sigma = diag(1),
#'                       itempar = list(beta = matrix(sort(rnorm(4*5)), 5, 4)))
#' @export
irtree_gen_pcm <- function(object = NULL,
                           N = NULL,
                           sigma = NULL,
                           theta = NULL,
                           itempar = NULL,
                           link = "logit",
                           na_okay = TRUE,
                           skip = FALSE
    ) {

    spec <- c(as.list(environment()))
    spec$J <- object$J

    match.arg(link)

    S <- object$S
    J <- object$J
    j_names <- object$j_names
    K <- object$K

    # INPUT CHECKING ----------------------------------------------------------

    .must_have(object, "constraints", FALSE, skip = skip)
    .must_have(object, "addendum", FALSE, skip = skip)

    if (!isTRUE(all(unlist(object$irt_loadings) == "@1"))) {
        stop("2Pl is not implemented for class PCM.")
    }

    checkmate::assert_int(N, lower = 1, null.ok = !is.null(theta))
    if (!is.null(theta)) {
        spec$theta <- theta <- data.matrix(theta, rownames.force = FALSE)
        N <- nrow(theta)
    }
    checkmate::assert_matrix(theta, mode = "numeric", min.rows = 1,
                             ncols = object$S, null.ok = !is.null(sigma))

    if (is.function(sigma)) {
        FUN <- match.fun(sigma)
        spec$sigma_fun <- FUN
        sigma <- FUN()
    }
    spec$sigma <- sigma

    checkmate::assert_matrix(sigma, mode = "numeric", any.missing = FALSE,
                             nrows = S, ncols = S, null.ok = !is.null(theta))

    if (is.function(itempar)) {
        FUN <- match.fun(itempar)
        spec$itempar_fun <- FUN
        itempar <- FUN()
    }
    checkmate::assert_list(itempar, types = "numeric", any.missing = FALSE,
                           len = 1, # len = 2,
                           names = "named")
    checkmate::assert_names(names(itempar), subset.of = "beta"
                            # permutation.of = c("beta", "alpha")
                            )
    itempar <- lapply(itempar, data.matrix, rownames.force = FALSE)
    spec$itempar <- itempar
    checkmate::assert_matrix(itempar$beta, nrows = J, ncols = K - 1)
    # checkmate::assert_matrix(itempar$alpha, nrows = J, ncols = P)

    # THETA --------------------------------------------------------------------
    # [DIM x N] MATRIX

    if (is.null(theta)) {
        theta <- MASS::mvrnorm(ifelse(N == 1, 1.001, N), mu = rep(0, S), Sigma = sigma)
    }
    colnames(theta) <- object$latent_names$theta
    spec$personpar <- theta
    spec$theta <- NULL
    theta <- Matrix::Matrix(t(theta))

    # THRESHOLDS ---------------------------------------------------------------
    # [ITEMS*(CATEG-1) x N] MATRIX

    thres <- Matrix::Matrix(as.vector(t(itempar$beta)),
                    nrow = J * (K - 1), ncol = N)

    # B-MATRIX -----------------------------------------------------------------
    # [ITEMS*CATEG x DIM] MATRIX

    B <- .make_tam_B(object, array = FALSE)

    # A-MATRIX -----------------------------------------------------------------
    # [ITEMS*CATEG x ITEMS*(CATEG-1)] MATRIX

    A <- diag(1, K - 1)
    A[lower.tri(A)] <- 1
    A <- rbind(0, A)
    A <- Matrix::bdiag(replicate(J, A, simplify = FALSE))

    # IRT MODEL -> DATA --------------------------------------------------------
    num <- exp(B %*% theta - A %*% thres)
    num <- array(num, dim = c(K, J, N))
    den <- array(rep(colSums(num), each = K), dim = c(K, J, N))
    p   <- num / den

    checkmate::qassert(p, "N[0,1]")
    checkmate::assert_integerish(apply(p, 2:3, sum), lower = .9, upper = 1.1,
                                 any.missing = FALSE, len = N*J)

    f1 <- function(x) {
        storage.mode(x) <- "logical"
        apply(x, c(2:3), which)
    }

    dat <-
        tibble::as_tibble(
            setNames(
                as.data.frame(
                    t(f1(
                        apply(p, 2:3, rmultinom, n = 1, size = 1)))),
                j_names))

    if (!na_okay) {
        ii <- 0
        while (!.check_all_categ_observed(dat, object$K)) {
            dat <-
                tibble::as_tibble(
                    setNames(
                        as.data.frame(
                            t(f1(
                                apply(p, 2:3, rmultinom, n = 1, size = 1)))),
                        j_names))
            ii <- ii + 1
            if (ii >= 25) stop("Could not generate data without missing categories.")
        }
    }

    p_return <- data.frame(
        pers = gl(N, J*K, length = J*K*N),
        item = gl(J, K,   length = J*K*N, labels = j_names),
        cate = gl(K, 1,    length = J*K*N),
        prob = matrix(p, ncol = 1)[, 1]
    )

    return(list(data = dat, probs = p_return, spec = spec))
}
