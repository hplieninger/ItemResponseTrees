skip_on_cran()

m1 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1;
# b BY x1@1, x2@1, x3@1, x4@1;

Equations:
0 = 1- a
1 = a
# 2 = a*(1-b)
# 3 = a*b

Class:
Tree
"

m2 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1;

Weights:
a = 0:1

Class:
PCM
"

m3 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1;

Class:
GRM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)

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
                   in_memory = "everything",
                   na_okay = FALSE,
                   control = control_mirt(SE = FALSE, technical = list(NCYCLES = 200)))

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
                   na_okay = FALSE,
                   control = control_tam(control = list(snodes = 1000)))

##### Tests #####

data(column_glossary, package = "modeltests")

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

    modeltests::check_tidy_output(res11$fits$m1$tidied)

})

test_that("irtree_sim() works when writing to disc", {
    expect_warning(
        irtree_sim(gen_model = model1,
                   N = 250,
                   sigma = function(x) diag(1),
                   itempar = function(x) list(beta = matrix(sort(rnorm(model1$J*model1$P)), model1$J, model1$P),
                                              alpha = matrix(1, model1$J, model1$P)),
                   link = "logit",
                   engine = "mirt",
                   save_rdata = FALSE,
                   R = 1,
                   in_memory = "nothing",
                   na_okay = FALSE,
                   control = control_mirt(SE = FALSE, technical = list(NCYCLES = 200)))
    )
})

test_that("irtree_sim() works with mplus", {

    run <- (MplusAutomation::mplusAvailable() == 0)

    res3 <- irtree_sim(gen_model = model1,
                       # fit_model = gen_model,
                       N = 250,
                       sigma = function(x) diag(1),
                       itempar = function(x) list(beta = matrix(sort(rnorm(model1$J*model1$P)), model1$J, model1$P),
                                                  alpha = matrix(1, model1$J, model1$P)),
                       link = "logit",
                       engine = "mplus",
                       save_rdata = FALSE,
                       R = seq_len(R),
                       in_memory = "reduced",
                       na_okay = FALSE,
                       control = control_mplus(run = run, warnings2messages = TRUE))

    res31 <- res3[[sample(R, 1)]]

    checkmate::expect_list(res3, len = R)
    checkmate::expect_set_equal(names(res3), c("sim1", "sim2"))

    checkmate::expect_list(res31, len = 2)
    checkmate::expect_set_equal(names(res31),
                                c("fits", "spec"))

    checkmate::expect_list(res31$fits$m1, len = 4)
    checkmate::expect_set_equal(names(res31$fits$m1),
                                c("fit", "glanced", "tidied", "augmented"))

    expect_s3_class(res31$fits$m1$fit, "irtree_fit")

    modeltests::check_dims(res31$fits$m1$augmented,
                           res31$spec$N, model1$S*ifelse(run, 3, 1))

    skip_if_not(run)

    modeltests::check_glance_outputs(res31$fits$m1$glanced, strict = TRUE)

    skip_if_not_installed("modeltests")

    modeltests::check_tidy_output(res31$fits$m1$tidied)

})
