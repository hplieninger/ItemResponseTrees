testthat::skip_if_not_installed("xml2")
if (requireNamespace('spelling', quietly = TRUE))
    spelling::spell_check_test(vignettes = TRUE, error = FALSE,
                               skip_on_cran = TRUE)
