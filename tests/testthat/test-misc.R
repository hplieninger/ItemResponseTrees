test_that("fit functions have same arguments", {
    tmp1 <- formalArgs(fit.irtree_model)
    expect_equal(tmp1[!tmp1 %in% c("...", "engine")],
                 formalArgs(irtree_fit_mplus))
    expect_equal(formalArgs(irtree_fit_mplus),
                 formalArgs(irtree_fit_mirt))
    expect_equal(formalArgs(irtree_fit_mplus),
                 formalArgs(irtree_fit_tam))
})

test_that("rtruncatednorm works as expected", {
    n <- sample(100, 1)
    m <- rnorm(1, sd = 5)
    ll <- m - runif(1, max = 5)
    ul <- m + runif(1, max = 5)
    x <- rtruncatednorm(n, m, 1, ll, ul)
    checkmate::test_numeric(x, lower = ll, upper = ul, any.missing = FALSE, len = n)
})
