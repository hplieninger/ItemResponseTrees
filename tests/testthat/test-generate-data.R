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

model1 <- irtree_model(m1)

N <- 1
J <- model1$J

X <- irtree_gen_data(object = model1,
                     N = N + 42,
                     sigma = diag(model1$S),
                     theta = matrix(0, N, model1$S),
                     itempar = list(beta = matrix(rnorm(J*model1$P), J, model1$P),
                                    alpha = matrix(1, J, model1$P)))

test_that("irtree_gen_data() works if theta is provided", {
    checkmate::expect_data_frame(X$data, nrows = N, ncols = J)
    checkmate::qexpectr(X$data, "X[1,3]")
    checkmate::expect_data_frame(X$probs, nrows = N*J*3, ncols = 4)
    checkmate::qexpect(X$probs$prob, "N[0,1]")
})

X <- irtree_gen_data(object = model1,
                     N = N,
                     sigma = diag(model1$S),
                     itempar = list(beta = matrix(rnorm(J*model1$P), J, model1$P),
                                    alpha = matrix(1, J, model1$P)))

test_that("irtree_gen_data() works if theta is not provided", {
    checkmate::expect_data_frame(X$data, nrows = N, ncols = J)
    checkmate::qexpectr(X$data, "X[1,3]")
    checkmate::expect_data_frame(X$probs, nrows = N*J*3, ncols = 4)
    checkmate::qexpect(X$probs$prob, "N[0,1]")
})
