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
    expect_message(irtree_create_template(data.frame(a = 1)), "Equations")
})

test_that("has_namespace", {
    expect_error(has_namespace("H"), "packages are missing")
})

test_that("assert_nchar", {
    expect_error(assert_nchar("abcd", 3), "must have at most")
})

test_that("expect_integers_in_irtree_model() works correctly", {
    success <- list(k_names = 1L:2L,
                    mapping_matrix = matrix(1L, 2, 2))
    fail1 <- within(success, k_names <- c(1.0, 2))
    fail2 <- within(success, mapping_matrix[1, 1] <- 1.0)
    expect_success(expect_integers_in_irtree_model(success))
    expect_failure(expect_integers_in_irtree_model(fail1))
    expect_failure(expect_integers_in_irtree_model(fail2))
})
