context("test-fit-mplus")

# m1 <- "
# # Comment
# IRT
# t1 BY X1@1, x2@1, X3@1;
# T2 BY X4@1, X5@1, X6@1;
# e  BY X1@1, x2@1, X3@1, X4@1, X5@1
#  X6@1;
# m  BY X1@1, x2@1, X3@1, X4@1, X5@1,
# X6@1;
#
# Items:
# X1, x2, X3, X4, X5,
#   X6
#
# Subtree:
# t = t1 + T2
#
# Equations
# 1 = (1-m)*(1-t)*e
# 2 = (1-m)*(1-t)*(1-e)
# 3 = m
# 4 = (1-m)*t*(1-e)
# 5 = (1-m)*t*e
#
# Processes:
# m, e, t1, T2
#
# Class:
# Tree
#
# "

m1 <- "
# Comment
IRT
a BY X1@1, X2@1, X3@1, X4@1, X10@1;
b BY X1@1, X2@1, X3@1, X4@1, X10@1;

Items:
X1, X2, X3, X4,
X10
Equations
1 = (1-a)
2 = a*(1-b)
3 = a*b

Processes:
a, b

Class:
Tree

Addendum:
a WITH b@0;
a WITH y1;
"

m2 <- "
IRT
a BY Comfort@1, Work, Future, Benefit;

Items:
Comfort, Work, Future, Benefit

# Equations
# 1 = (1-a)
# 2 = a*(1-b)
# 3 = a*b

Processes:
a

Class:
GRM
"

model <- tree_model(m1)

test_that("tree_model() works", {
    expect_s3_class(model, "tree_model")
})

flag1 <- TRUE
while (flag1) {
    X <- gen_tree_data(model = model, N = 100,
                       sigma = diag(model$S),
                       itempar = list(beta = matrix(rnorm(model$J*model$P), model$J, model$P),
                                      alpha = matrix(1, model$J, model$P)))
    flag1 <- any(!vapply(lapply(X$data, unique), function(x) length(x) == model$K, FUN.VALUE = T))
}

# X <- gen_tree_data(model = model, N = 100,
#                    sigma = diag(model$S),
#                    itempar = list(beta = matrix(rnorm(model$J*model$P), model$J, model$P),
#                                   alpha = matrix(1, model$J, model$P)))

df1 <- data.frame(X$data, y1 = rnorm(100))

res <- fit_tree_mplus(data = df1,
                      model = model,
                      file_name = basename(tempfile()),
                      dir = tempdir(),
                      run = T,
                      integration_points = 7,
                      analysis_list = list(LOGCRITERION = ".01"),
                      .warnings2messages = TRUE)

test_that("fit_tree_mplus() works for Tree", {
    # expect_is(res$mplus, "list")
    expect_s3_class(res$mplus, "mplus.model")
})

data(Science, package = "mirt")

res <- fit_tree_mplus(data = Science,
                      model = m2,
                      file_name = basename(tempfile()),
                      dir = tempdir(),
                      run = T,
                      integration_points = 7)

test_that("fit_tree_mplus() works for GRM", {
    expect_s3_class(res$mplus, "mplus.model")
    checkmate::expect_data_frame(res$mplus$parameters$unstandardized, any.missing = FALSE,
                                 nrows = 17, ncols = 6)
})

res2 <- extract_mplus_output(res$mplus, m2)

test_that("extract_mplus_output() works for GRM", {
    expect_s3_class(res2$summaries, "mplus.summaries")
    expect_s3_class(res2$warnings, "mplus.warnings")
    expect_s3_class(res2$errors, "mplus.errors")
    checkmate::expect_data_frame(res2$person$personpar_est, any.missing = FALSE,
                                 nrows = nrow(Science), ncols = 1)
    checkmate::expect_list(res2$item, types = "list", any.missing = FALSE, len = 4)
    checkmate::expect_set_equal(names(res2$item), c("beta", "beta_se", "alpha", "alpha_se"))
    checkmate::expect_matrix(res2$sigma, mode = "numeric", any.missing = FALSE,
                             nrows = 1, ncols = 1)
    checkmate::expect_matrix(res2$cormat, mode = "numeric", any.missing = FALSE,
                             nrows = 1, ncols = 1)
})

