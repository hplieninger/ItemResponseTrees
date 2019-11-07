m1 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1;
# b BY x1@1, x2@1, x3@1, x4@1;

Equations:
1 = 1- a
2 = a
# 2 = a*(1-b)
# 3 = a*b

Class:
Tree
"

m2 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1;

Weights:
a = c(0, 1)

Class:
PCM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

# tmp1 <- irtree_gen_data(model1, N = 100, sigma = diag(1),
#                         itempar = list(beta = matrix(sort(rnorm(model1$J*model1$P)), model1$J, model1$P),
#                                        alpha = matrix(1, model1$J, model1$P)))

##### Fit #####

R <- 2

res1 <- irtree_sim(gen_model = model1,
                   # fit_model = gen_model,
                   N = 250,
                   sigma = function(x) diag(1),
                   itempar = function(x) list(beta = matrix(sort(rnorm(model1$J*model1$P)), model1$J, model1$P),
                                              alpha = matrix(1, model1$J, model1$P)),
                   link = "logit",
                   engine = "mirt",
                   save_rdata = FALSE,
                   R = seq_len(R),
                   in_memory = "reduced",
                   .na_okay = FALSE,
                   dots = list(tidy = list(difficulty = TRUE)))

res2 <- irtree_sim(gen_model = model1,
                   fit_model = list(model1, model2),
                   N = 250,
                   sigma = function(x) diag(1),
                   itempar = function(x) list(beta = matrix(sort(rnorm(model1$J*model1$P)), model1$J, model1$P),
                                              alpha = matrix(1, model1$J, model1$P)),
                   link = "logit",
                   engine = "tam",
                   save_rdata = FALSE,
                   R = 1,
                   in_memory = "reduced",
                   .na_okay = FALSE,
                   dots = list(fit = list(control = list(snodes = 1000))))

##### Tests #####


test_that("irtree_sim() works", {

    res11 <- res1[[sample(R, 1)]]
    res21 <- res2[[1]]

    checkmate::expect_list(res1, len = R)
    checkmate::expect_set_equal(names(res1), c("sim1", "sim2"))
    checkmate::expect_list(res2, len = 1)
    checkmate::expect_set_equal(names(res2), "sim1")

    checkmate::expect_list(res11, len = 2)
    checkmate::expect_set_equal(names(res11),
                                c("fits", "spec"))

    checkmate::expect_list(res11$fits$m1, len = 4)
    checkmate::expect_set_equal(names(res11$fits$m1),
                                c("fit", "glanced", "tidied", "augmented"))
    checkmate::expect_set_equal(names(res21$fits), c("m1", "m2"))
    checkmate::expect_set_equal(names(res21$fits[[sample(2, 1)]]),
                                c("fit", "glanced", "tidied", "augmented"))

    expect_s3_class(res11$fits$m1$fit, "irtree_fit")
    expect_s3_class(res21$fits[[sample(2, 1)]]$fit, "irtree_fit")

    modeltests::check_dims(res11$fits$m1$augmented,
                           res11$spec$N, model1$S*3)

    modeltests::check_glance_outputs(res11$fits$m1$glanced, strict = TRUE)

    skip_if_not_installed("modeltests")

    data(column_glossary, package = "modeltests")

    modeltests::check_tidy_output(
        subset(
            res11$fits$m1$tidied, select = -effect))

})