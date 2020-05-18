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
# Comment
IRT:
a BY X1@1;

Weights:
a = seq(0, 9)

Class:
PCM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

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

X <- irtree_gen_data(
    object = model1,
    N = N,
    sigma = function(x) diag(model1$S),
    itempar = function(x) {
        list(beta = matrix(rnorm(J*model1$P), J, model1$P),
                   alpha = matrix(1, J, model1$P))
    })

test_that("irtree_gen_data() works if theta is not provided", {
    checkmate::expect_data_frame(X$data, nrows = N, ncols = J)
    checkmate::qexpectr(X$data, "X[1,3]")
    checkmate::expect_data_frame(X$probs, nrows = N*J*3, ncols = 4)
    checkmate::qexpect(X$probs$prob, "N[0,1]")
})

test_that("irtree_recode() works as expected", {
    df1 <- irtree_recode(model1, X$data)
    tmp1 <- 5
    df2 <- irtree_recode(
        data = data.frame(a = sample(tmp1),
                          b = sample(tmp1)),
        mapping_matrix = matrix(c(1:tmp1, c(0, 0, 1, 1, 1)),
                                tmp1, 2,
                                dimnames = list(NULL, c("cate", "bar"))))
    checkmate::expect_data_frame(df1, types = "integer", nrows = N,
                                 ncols = model1$J*model1$S,
                                 col.names = "strict")
    checkmate::expect_set_equal(names(df1), model1$lambda$new_name,
                                ordered = TRUE)

    checkmate::expect_data_frame(df2, types = "integer", nrows = tmp1,
                                 ncols = 2,
                                 col.names = "strict")
})

test_that("irtree_gen_data() errors if some categories not observed", {
    expect_error(
        irtree_gen_data(object = model1,
                        N = 2,
                        sigma = diag(model1$S),
                        itempar = list(beta = matrix(rnorm(J*model1$P), J, model1$P),
                                       alpha = matrix(1, J, model1$P)),
                        na_okay = FALSE),
        "without missing categories"
    )
    expect_error(
        irtree_gen_data(object = model2,
                        N = model2$K - 1,
                        sigma = diag(model2$S),
                        link = "logit",
                        itempar = list(beta = matrix(rnorm(model2$J*(model2$K - 1)),
                                                     model2$J, model2$K - 1)),
                        na_okay = FALSE),
        "without missing categories"
    )
})
