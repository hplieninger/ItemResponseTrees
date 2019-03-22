context("mirt: Simple models")

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
a BY Work, Comfort, Future, Benefitvar;
# a BY Comfort, Work, Benefitvar, Future;

Class:
GRM
"

model1 <- tree_model(m1)
model2 <- tree_model(m2)

##### Data #####

flag1 <- TRUE
ii <- 0
while (flag1) {
    ii <- ii + 1
    X <- gen_tree_data(model = model1, N = 100,
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

res1 <- fit_tree_mirt(data = X$data,
                     model = model1,
                     SE = FALSE,
                     method = "QMCEM",
                     quadpts = 1000)
summ1 <- extract_mirt_output(res1$mirt, class = model1$class)

res2 <- fit_tree_mirt(data = ScienceNew,
                      model = model2,
                      SE = FALSE,
                      method = "QMCEM",
                      quadpts = 1000)
# res2 <- fit_tree_mirt(data = Science,
#                       model = model2,
#                       SE = FALSE,
#                       method = "QMCEM",
#                       quadpts = 1000)
res2x <- mirt::mirt(ScienceNew, 1, "graded", SE = FALSE,
                    method = "QMCEM", quadpts = 1000, verbose = FALSE)

summ2 <- extract_mirt_output(res2$mirt, class = model2$class)

##### Tests #####

test_that("tree_model() works", {
    expect_s3_class(model1, "tree_model")
    expect_s3_class(model2, "tree_model")
})

test_that("fit_tree_mirt() works", {
    expect_s4_class(res1$mirt, "SingleGroupClass")
    expect_s4_class(res2$mirt, "SingleGroupClass")
    expect_equal(mirt::coef(res2$mirt), mirt::coef(res2x))
})

test_that("extract_mirt_output() works for Tree", {

    expect_s3_class(summ1$summaries, "mirt_df")

    lapply(summ1$person, checkmate::expect_data_frame,
           any.missing = FALSE,
           nrows = nrow(X$data), ncols = model1$S,
           info = "Tested expect_data_frame(summ1$person)")
    checkmate::expect_list(summ1$item, types = "list", any.missing = FALSE, len = 2)
    lapply(summ1$item, checkmate::expect_data_frame,
           min.cols = 2, max.cols = NULL, nrows = model1$J,
           info = "Tested expect_data_frame(summ1$item)")
    checkmate::expect_set_equal(names(summ1$item), c("beta", "alpha"))
    checkmate::expect_matrix(summ1$sigma, mode = "numeric", all.missing = FALSE,
                             nrows = model1$P, ncols = model1$P)
    checkmate::expect_matrix(summ1$cormat, mode = "numeric", all.missing = FALSE,
                             nrows = model1$P, ncols = model1$P)
})

test_that("extract_mirt_output() works for GRM", {

    expect_s3_class(summ2$summaries, "mirt_df")

    mirtcoef <- mirt::coef(res2x)[names(ScienceNew)] %>%
        lapply(data.frame) %>%
        dplyr::bind_rows() %>%
        dplyr::select(-a1)

    expect_equal(summ2$item$beta[, 2:4], mirtcoef)

    lapply(summ2$person, checkmate::expect_data_frame,
           any.missing = FALSE,
           nrows = nrow(ScienceNew), ncols = model2$S,
           info = "Tested expect_data_frame(summ2$person)")
    checkmate::expect_list(summ2$item, types = "list", any.missing = FALSE, len = 2)
    lapply(summ2$item, checkmate::expect_data_frame,
           min.cols = 2, max.cols = NULL, nrows = model2$J,
           info = "Tested expect_data_frame(summ2$item)")
    checkmate::expect_set_equal(names(summ2$item), c("beta", "alpha"))
    checkmate::expect_matrix(summ2$sigma, mode = "numeric", all.missing = FALSE,
                             nrows = model2$P, ncols = model2$P)
    checkmate::expect_matrix(summ2$cormat, mode = "numeric", all.missing = FALSE,
                             nrows = model2$P, ncols = model2$P)
})
