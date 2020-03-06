test_that("fit functions have same arguments", {
    tmp1 <- formalArgs(fit.irtree_model)
    expect_equal(tmp1[!tmp1 %in% c("...", "engine")],
                 formalArgs(irtree_fit_mplus))
    expect_equal(formalArgs(irtree_fit_mplus),
                 formalArgs(irtree_fit_mirt))
    expect_equal(formalArgs(irtree_fit_mplus),
                 formalArgs(irtree_fit_tam))

    expect_error(fit.irtree_model(a = 1), "The ... are currently not used")
    expect_error(.stop_not_implemented(), "is not implemented")
})

test_that("rtruncatednorm() works as expected", {
    n <- sample(100, 1)
    m <- rnorm(1, sd = 5)
    ll <- m - runif(1, max = 5)
    ul <- m + runif(1, max = 5)
    x <- rtruncatednorm(n, m, 1, ll, ul)
    checkmate::expect_numeric(x, lower = ll, upper = ul, any.missing = FALSE, len = n)
})

test_that("irtree_create_template() works as expected", {
    expect_message(irtree_create_template("a"), "Equations")
})

test_that("has_namespace", {
    expect_error(has_namespace("H"), "packages are missing")
})

test_that("assert_nchar", {
    expect_error(assert_nchar("abcd", 3), "must have at most")
})
