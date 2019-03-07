context("test-fit-mplus")

m1 <- "
# Comment
 IRT:
b BY X1@1, X2@1, X3@1, X4@1, X10@1;
a BY X1@1, X2@1, X3@1, X4@1, X10@1;

 Equations:
1 = (1-a)
2 = a*(1-b)
3 = a*b
Class:
Tree

Addendum:
a WITH b@0;
a WITH y1;
"

m2 <- "
IRT:
# a BY Comfort@1, Work, Future, Benefit;
a BY Work, Comfort@1, Future, Benefit;

Class:
GRM
"

model1 <- tree_model(m1)
model2 <- tree_model(m2)

test_that("tree_model() works", {
    expect_s3_class(model1, "tree_model")
})

flag1 <- TRUE
while (flag1) {
    X <- gen_tree_data(model = model1, N = 100,
                       sigma = diag(model1$S),
                       itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                                      alpha = matrix(1, model1$J, model1$P)))
    flag1 <- any(!vapply(lapply(X$data, unique), function(x) length(x) == model1$K, FUN.VALUE = T))
}

df1 <- data.frame(X$data, y1 = rnorm(100))
df1 <- data.frame(X$data[, rev(names(X$data))], y1 = rnorm(100))

res1 <- fit_tree_mplus(data = df1,
                      model = model1,
                      file_name = basename(tempfile()),
                      dir = tempdir(),
                      run = T,
                      integration_points = 7,
                      analysis_list = list(LOGCRITERION = ".01"),
                      .warnings2messages = TRUE)

test_that("fit_tree_mplus() works for Tree", {
    expect_s3_class(res1$mplus, "mplus.model")
})

data(Science, package = "mirt")

res2 <- fit_tree_mplus(data = Science,
                      model = model2,
                      file_name = basename(tempfile()),
                      dir = tempdir(),
                      run = T,
                      integration_points = 7)

test_that("fit_tree_mplus() works for GRM", {
    expect_s3_class(res2$mplus, "mplus.model")
    checkmate::expect_data_frame(res2$mplus$parameters$unstandardized, any.missing = FALSE,
                                 nrows = 17, ncols = 6)
})

counts <- Science %>%
    purrr::map_dfr(~table(factor(., levels = 1:4))) %>%
    dplyr::mutate(category = 1:4) %>%
    reshape2::melt(value.name = "count", id.vars = "category")
counts$variable <- toupper(as.character(counts$variable))
test_that("Items correctly exported to Mplus", {
    expect_equal(res2$mplus$sampstat$proportions.counts[order(counts$variable, counts$category), names(counts)],
                 counts[order(counts$variable, counts$category), ],
                 check.attributes = FALSE)
})

# extract_mplus_output ----------------------------------------------------

summ1 <- extract_mplus_output(res1$mplus, m1)

test_that("extract_mplus_output() works for Tree", {
    expect_s3_class(summ1$summaries, "mplus.summaries")
    expect_s3_class(summ1$warnings, "mplus.warnings")
    expect_s3_class(summ1$errors, "mplus.errors")
    lapply(summ1$person, checkmate::expect_data_frame,
           any.missing = FALSE,
           nrows = nrow(df1), ncols = model1$P,
           info = "Tested expect_data_frame(summ1$person)")
    checkmate::expect_list(summ1$item, types = "list", any.missing = FALSE, len = 4)
    lapply(summ1$item, checkmate::expect_data_frame,
           min.cols = 2, max.cols = NULL, nrows = model1$J,
           info = "Tested expect_data_frame(summ1$item)")
    expect_equal(summ1$item$beta[summ1$item$beta$item == "X10", "B"],
                 res1$mplus$parameters$unstandardized[res1$mplus$parameters$unstandardized$param == "B_X10$1", "est"])
    checkmate::expect_set_equal(names(summ1$item), c("beta", "beta_se", "alpha", "alpha_se"))
    checkmate::expect_matrix(summ1$sigma, mode = "numeric", all.missing = FALSE,
                             nrows = model1$P, ncols = model1$P)
    checkmate::expect_matrix(summ1$cormat, mode = "numeric", all.missing = FALSE,
                             nrows = model1$P, ncols = model1$P)
})

summ2 <- extract_mplus_output(res2$mplus, model2)

test_that("extract_mplus_output() works for GRM", {
    expect_s3_class(summ2$summaries, "mplus.summaries")
    expect_s3_class(summ2$warnings, "mplus.warnings")
    expect_s3_class(summ2$errors, "mplus.errors")
    lapply(summ2$person, checkmate::expect_data_frame,
           any.missing = FALSE,
           nrows = nrow(Science), ncols = model2$P,
           info = "Tested expect_data_frame(summ2$person)")
    checkmate::expect_list(summ2$item, types = "list", any.missing = FALSE, len = 4)
    lapply(summ2$item, checkmate::expect_data_frame,
           all.missing = FALSE,
           min.cols = 2, max.cols = NULL, nrows = model2$J,
           info = "Tested expect_data_frame(summ2$item)")
    checkmate::expect_set_equal(names(summ2$item), c("beta", "beta_se", "alpha", "alpha_se"))
    checkmate::expect_matrix(summ2$sigma, mode = "numeric", any.missing = FALSE,
                             nrows = 1, ncols = model2$P)
    checkmate::expect_matrix(summ2$cormat, mode = "numeric", any.missing = FALSE,
                             nrows = 1, ncols = model2$P)
})
