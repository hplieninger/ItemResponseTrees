test_that("fit functions have same arguments", {
    tmp1 <- formalArgs(fit.irtree_model)
    expect_equal(tmp1[!tmp1 %in% c("...", "engine")],
                 formalArgs(irtree_fit_mplus))
    expect_equal(formalArgs(irtree_fit_mplus),
                 formalArgs(irtree_fit_mirt))
    expect_equal(formalArgs(irtree_fit_mplus),
                 formalArgs(irtree_fit_tam))
})
