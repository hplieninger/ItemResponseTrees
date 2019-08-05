##### Models #####

m1 <- "
# Comment
IRT:
a BY X1@1, X2@1, X3@1, X4@1, X5@1;
b BY X1@1, X2@1, X3@1, X4@1, X5@1;

Equations:
1 = (1-a)
2 = a*(1-b)
3 = a*b

Class:
Tree
"

m2 <- "
IRT:
# a BY Comfort, Work, Future, Benefit;
a BY Work@1, Comfort@1, Future@1, Benefitvar;
# a BY Comfort, Work, Benefitvar, Future;

Class:
GRM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

##### Data #####

flag1 <- TRUE
ii <- 0
while (flag1) {
    ii <- ii + 1
    X <- irtree_sim_data(object = model1, N = 100,
                         sigma = diag(model1$S),
                         itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                                        alpha = matrix(1, model1$J, model1$P)))
    flag1 <- any(!vapply(lapply(X$data, unique), function(x) length(x) == model1$K, FUN.VALUE = T))
    if (ii > 50) {
        stop("Data generation failed")
    }
}

data(Science, package = "mirt")
ScienceNew <- Science
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

##### Fit #####

res1 <- fit(data = X$data,
            engine = "mirt",
            object = model1,
            SE = FALSE,
            method = "QMCEM",
            quadpts = 1000,
            verbose = FALSE)
# summ1 <- extract_mirt_output(res1$mirt, object = model1)

res2 <- fit(data = ScienceNew,
            engine = "mirt",
            object = model2,
            SE = FALSE,
            method = "QMCEM",
            quadpts = 1000,
            verbose = FALSE)

res2x <- mirt::mirt(ScienceNew, 1, "graded", SE = FALSE,
                    method = "QMCEM", quadpts = 1000, verbose = FALSE,
                    constrain = list(c(1, 5, 9)))

# summ2 <- extract_mirt_output(res2$mirt, class = model2$class)

##### Tests #####

test_that("irtree_model() works", {
    expect_s3_class(model1, "irtree_model")
    expect_s3_class(model2, "irtree_model")
})

test_that("irtree_fit_mirt() works", {
    expect_s4_class(res1$mirt, "SingleGroupClass")
    expect_s4_class(res2$mirt, "SingleGroupClass")
    expect_equal(mirt::coef(res2$mirt), mirt::coef(res2x))
})

test_that("Methods work for output of irtree_fit_mirt()", {
    expect_condition(capture.output(print(res1)), NA)
    expect_condition(capture.output(summary(res1)), NA)
    expect_condition(capture.output(coef(res1)), NA)
})

# test_that("extract_mirt_output() works for Tree", {
#
#     expect_s3_class(summ1$summaries, "mirt_df")
#
#     lapply(summ1$person, checkmate::expect_data_frame,
#            any.missing = FALSE,
#            nrows = nrow(X$data), ncols = model1$S,
#            info = "Tested expect_data_frame(summ1$person)")
#     checkmate::expect_list(summ1$item, types = "list", any.missing = FALSE, len = 2)
#     lapply(summ1$item, checkmate::expect_data_frame,
#            min.cols = 2, max.cols = NULL, nrows = model1$J,
#            info = "Tested expect_data_frame(summ1$item)")
#     checkmate::expect_set_equal(names(summ1$item), c("beta", "alpha"))
#     checkmate::expect_matrix(summ1$sigma, mode = "numeric", all.missing = FALSE,
#                              nrows = model1$P, ncols = model1$P)
#     checkmate::expect_matrix(summ1$cormat, mode = "numeric", all.missing = FALSE,
#                              nrows = model1$P, ncols = model1$P)
# })
#
# test_that("extract_mirt_output() works for GRM", {
#
#     expect_s3_class(summ2$summaries, "mirt_df")
#
#     mirtcoef <- mirt::coef(res2x)[names(ScienceNew)] %>%
#         lapply(data.frame) %>%
#         dplyr::bind_rows() %>%
#         dplyr::select(-a1)
#
#     expect_equal(summ2$item$beta[, 2:4], mirtcoef)
#
#     lapply(summ2$person, checkmate::expect_data_frame,
#            any.missing = FALSE,
#            nrows = nrow(ScienceNew), ncols = model2$S,
#            info = "Tested expect_data_frame(summ2$person)")
#     checkmate::expect_list(summ2$item, types = "list", any.missing = FALSE, len = 2)
#     lapply(summ2$item, checkmate::expect_data_frame,
#            min.cols = 2, max.cols = NULL, nrows = model2$J,
#            info = "Tested expect_data_frame(summ2$item)")
#     checkmate::expect_set_equal(names(summ2$item), c("beta", "alpha"))
#     checkmate::expect_matrix(summ2$sigma, mode = "numeric", all.missing = FALSE,
#                              nrows = model2$P, ncols = model2$P)
#     checkmate::expect_matrix(summ2$cormat, mode = "numeric", all.missing = FALSE,
#                              nrows = model2$P, ncols = model2$P)
# })

# Tidiers -----------------------------------------------------------------

# From vignette at https://broom.tidyverse.org/articles/adding-tidiers.html

skip_if_not_installed("modeltests")

data(column_glossary, package = "modeltests")

test_that("tidy.irtree_fit()", {

    td1 <- tidy(res1)
    td2 <- tidy(res2)

    modeltests::check_tidy_output(subset(td1, select = -effect))
    modeltests::check_tidy_output(subset(td2, select = -effect))

    modeltests::check_dims(td1, 26, 4)  # optional but a good idea
    modeltests::check_dims(td2, 18, 4)  # optional but a good idea

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td1, term, estimate))
    tmp2 <- tmp1[["COV_21"]]/sqrt(tmp1[["COV_11"]])/sqrt(tmp1[["COV_22"]])
    expect_equal(tmp2, tmp1[["COR_b.a"]], tolerance = .002)

    tmp1 <- tibble::deframe(select(td2, term, estimate))
    expect_equal(tmp1[["MEAN_1"]], 0)
    expect_equal(tmp1[["COV_11"]], 1)

    checkmate::expect_numeric(td1$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td2$std.error, lower = 0, finite = TRUE)

})

test_that("glance.irtree_fit()", {

    gl1 <- glance(res1)
    gl2 <- glance(res2)

    modeltests::check_glance_outputs(gl1, gl2, strict = TRUE)

    ### Own tests ###

    expect_equal(pull(gl1, nobs), nrow(X$data))
    expect_equal(pull(gl2, nobs), nrow(ScienceNew))

})

test_that("implementation of augment.irtree_fit()", {

    # skip_if(TRUE)

    modeltests::check_augment_function(
        augment.irtree_fit, res1, data = X$data, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res2, data = ScienceNew, strict = FALSE
    )

    ag1 <- augment(res1)
    ag2 <- augment(res2)

    modeltests::check_dims(ag1, nrow(X$data), ncol(X$data) + model1$S*2)
    modeltests::check_dims(ag2, nrow(ScienceNew), ncol(ScienceNew) + model2$S*2)

    checkmate::expect_numeric(ag1$.se.fitF1, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.se.fitF2, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.se.fitF1, lower = 0, finite = TRUE, all.missing = FALSE)

})
