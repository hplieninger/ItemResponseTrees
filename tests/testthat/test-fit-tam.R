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
a BY Work@1, Comfort@1, Future@1, Benefitvar@1;

Class:
PCM

Weights:
a = c(0, 1, 2, 3)
"

m3 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1;
b BY x1@1, x2@1, x3@1, x4@1;

Class:
PCM

Weights:
a = c(0, 1, 2, 3, 4)
b = c(1, 0, 0, 0, 1)
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)

##### Data #####

data1 <- irtree_gen_data(
    object = model1, N = 100,
    sigma = diag(model1$S),
    itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                   alpha = matrix(1, model1$J, model1$P)),
    .na_okay = FALSE)

data(Science, package = "mirt")
ScienceNew <- Science - 1
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

data3 <- irtree_gen_data(
    object = model3, N = 100,
    link = "logit",
    sigma = diag(model3$S),
    itempar = list(beta = matrix(sort(rnorm(model3$J*model3$P)), model3$J, model3$K - 1)),
    .na_okay = FALSE)

##### Fit #####

control_list <- list(snodes = 1000)

res1 <- fit(data = data1$data,
            engine = "tam",
            object = model1,
            control = control_list,
            verbose = FALSE)

res2 <- fit(data = ScienceNew,
            engine = "tam",
            object = model2,
            control = control_list,
            verbose = FALSE,
            .set_min_to_0 = TRUE)

res2x <- TAM::tam.mml(resp = ScienceNew, irtmodel = "PCM",
                      control = control_list,
                      verbose = FALSE)

res3 <- fit(data = data3$data,
            engine = "tam",
            object = model3,
            control = control_list,
            verbose = FALSE,
            .set_min_to_0 = TRUE)

##### Tests #####

test_that("irtree_model() works", {
    expect_s3_class(model1, "irtree_model")
    expect_s3_class(model2, "irtree_model")
    expect_s3_class(model3, "irtree_model")
})

test_that("irtree_fit_tam() works", {
    expect_s3_class(res1$tam, "tam.mml")
    expect_s3_class(res2$tam, "tam.mml")
    expect_s3_class(res3$tam, "tam.mml")
    # expect_equal(res2$tam$xsi, res2x$xsi)
})

test_that("Methods work for output of irtree_fit_tam()", {
    expect_condition(capture.output(print(res1)), NA)
    expect_condition(capture.output(summary(res1)), NA)
    expect_condition(capture.output(coef(res1)), NA)
})

# Tidiers -----------------------------------------------------------------

# From vignette at https://broom.tidyverse.org/articles/adding-tidiers.html

skip_if_not_installed("modeltests")

data(column_glossary, package = "modeltests")

test_that("tidy.irtree_fit()", {

    td1 <- tidy(res1)
    td2 <- tidy(res2)
    td3 <- tidy(res3)

    modeltests::check_tidy_output(subset(td1, select = -effect))
    modeltests::check_tidy_output(subset(td2, select = -effect))
    modeltests::check_tidy_output(subset(td3, select = -effect))

    modeltests::check_dims(td1, 14, 4)  # optional but a good idea
    modeltests::check_dims(td2, 13, 4)  # optional but a good idea
    modeltests::check_dims(td3, 20, 4)  # optional but a good idea

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td1, term, estimate))
    tmp2 <- tmp1[["COV_12"]]/sqrt(tmp1[["COV_11"]])/sqrt(tmp1[["COV_22"]])
    expect_equal(tmp2, tmp1[["COR_12"]], tolerance = .002)

    checkmate::expect_numeric(td1$estimate, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(td2$estimate, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(td3$estimate, finite = TRUE, any.missing = FALSE)

    checkmate::expect_numeric(td1$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td2$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td3$std.error, lower = 0, finite = TRUE)

})

test_that("glance.irtree_fit()", {

    gl1 <- glance(res1)
    gl2 <- glance(res2)
    gl3 <- glance(res3)

    modeltests::check_glance_outputs(gl1, gl2, gl3, strict = TRUE)

    ### Own tests ###

    expect_equal(pull(gl1, nobs), nrow(data1$data))
    expect_equal(pull(gl2, nobs), nrow(ScienceNew))
    expect_equal(pull(gl3, nobs), nrow(data3$data))

})

test_that("implementation of augment.irtree_fit()", {

    modeltests::check_augment_function(
        augment, res1, data = data1$data, strict = FALSE
    )
    modeltests::check_augment_function(
        augment, res2, data = ScienceNew, strict = FALSE
    )
    modeltests::check_augment_function(
        augment, res3, data = data3$data, strict = FALSE
    )

    ag1 <- augment(res1)
    ag2 <- augment(res2)
    ag3 <- augment(res3)

    modeltests::check_dims(ag1, nrow(data1$data), ncol(data1$data) + model1$S*2)
    modeltests::check_dims(ag2, nrow(ScienceNew), ncol(ScienceNew) + model2$S*2)
    modeltests::check_dims(ag3, data3$spec$N,     data3$spec$J + model3$S*2)

    checkmate::expect_numeric(ag1$.fitted.Dim1, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(ag1$.fitted.Dim2, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(ag2$.fitted,      finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.fitted.Dim1, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(ag3$.fitted.Dim2, finite = TRUE, any.missing = FALSE)

    checkmate::expect_numeric(ag1$.se.fit.Dim1, lower = 0, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(ag1$.se.fit.Dim2, lower = 0, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(ag2$.se.fit, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.se.fit.Dim1, lower = 0, finite = TRUE, any.missing = FALSE)
    checkmate::expect_numeric(ag3$.se.fit.Dim2, lower = 0, finite = TRUE, any.missing = FALSE)

})

