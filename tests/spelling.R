if (requireNamespace('spelling', quietly = TRUE) &
    length(find.package("xml2", verbose = FALSE, quiet = TRUE)) > 0)
    spelling::spell_check_test(vignettes = TRUE, error = FALSE,
                               skip_on_cran = TRUE)
