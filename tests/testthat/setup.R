gen_itempar_boeck <- function(J = 10, loading = 1) {
    # pnorm(m + 2*ss)
    # curve(truncnorm::dtruncnorm(qnorm(x), mean = m, sd = ss, a = m - 2*ss, b = m + 2*ss),
    #       xlim = c(0, 1))

    # mrs
    m <- qnorm(.7)
    ss <- sqrt(.2)
    i1 <- truncnorm::rtruncnorm(J, mean = m, sd = ss,
                                a = m - 2 * ss, b = m + 2 * ss)
    # ers
    m <- qnorm(.7)
    ss <- sqrt(.2)
    i2 <- truncnorm::rtruncnorm(J, mean = m, sd = ss,
                                a = m - 2 * ss, b = m + 2 * ss)

    # trt
    m <- qnorm(.5)
    ss <- sqrt(.4)
    i3 <- truncnorm::rtruncnorm(J, mean = m, sd = ss,
                                a = m - 2 * ss, b = m + 2 * ss)

    out <- list()
    out$beta <- cbind(i1, i2, i3)

    if (all(loading == 1)) {
        out$alpha <- matrix(1, J, 3)
    } else {
        tmp1 <- runif(J * 3, .5, 1.5)
        fixed <- grepl("@", loading)
        fix_load <- sub("@", "", loading)
        tmp1[fixed] <- as.numeric(fix_load[fixed])
        out$alpha <- matrix(tmp1, J, 3)
    }

    return(out)
}


gen_sigma_boeck <- function(sigma = NULL, df = 50, vars = NULL) {

    if (is.null(sigma)) {
        sigma  <- matrix(c(1, .4, 0,
                           .4, 1, 0,
                           0, 0, 1), 3, 3, byrow = TRUE)
    }

    sig1 <- rWishart(1, df = df, Sigma = sigma)[, , 1]

    if (is.null(vars)) {
        vars <- c(rgamma(2, shape = 51,  scale = .01),
                  rgamma(1, shape = 101, scale = .01))
        sds <- sqrt(vars)
    }

    sig2 <- diag(sds) %*% cov2cor(sig1) %*% diag(sds)

    return(sig2)

    # ggplot(reshape2::melt(apply(foo, 3, diag)), aes(x = value)) +
    #     geom_posterior(ci_width = 0.95, draw_sd = F) +
    #     # geom_density() +
    #     facet_wrap(~ Var1, scales = "free_x")
    #
    # ggplot(reshape2::melt(apply(foo, 3, cov2cor)[c(2, 3, 6), ]), aes(x = value)) +
    #     # geom_density() +
    #     geom_posterior(ci_width = 0.95, draw_sd = F) +
    #     facet_wrap(~ Var1, scales = "free_x")
    #
    # curve(dgamma(x = x, shape = 51, scale = .01), xlim = c(0, 2))
    # curve(dgamma(x = x, shape = 101, scale = .01), xlim = c(0, 2))
}
