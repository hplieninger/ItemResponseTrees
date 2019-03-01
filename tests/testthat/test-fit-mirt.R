context("test-fit-mirt.R")

# source("test-functions.R")
# source_test_helpers()

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
a BY X1@1, X2@1, X3@1, X4@1, X5@1;
b BY X1@1, X2@1, X3@1, X4@1, X5@1;

Items:
X1, X2, X3, X4, X5

Equations
1 = (1-a)
2 = a*(1-b)
3 = a*b

Processes:
a, b

Class:
Tree
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

res <- fit_tree_mirt(data = X$data,
                     model = model,
                     # dir = here::here("misc"),
                     run = T,
                     method = "QMCEM",
                     quadpts = 1000)

test_that("fit_tree_mirt() works", {
    expect_s4_class(res$mirt, "SingleGroupClass")
})
