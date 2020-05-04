##### Models #####

m1 <- "
# Comment
IRT:
a BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;
b BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;

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
a BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;
b BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;

Class:
PCM

Weights:
a = c(0, 1, 2, 3, 4)
b = c(1, 0, 0, 0, 1)
"

m4 <- "
IRT:
a1 BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;
a2 BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;

Class:
PCM

Weights:
a = c(0, 1, 2, 3, 4)

Constraints:
a = a1 | a2
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)
model4 <- irtree_model(m4)

##### Data #####

set.seed(123)

data1 <- irtree_gen_data(
    object = model1, N = 200,
    sigma = diag(model1$S),
    itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                   alpha = matrix(1, model1$J, model1$P)),
    na_okay = FALSE)
for (ii in seq_len(ncol(data1$data))) {
    data1$data[ii, ii] <- NA
}

data(Science, package = "mirt")
ScienceNew <- Science - 1
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

data3 <- irtree_gen_data(
    object = model3, N = 200,
    link = "logit",
    sigma = function() diag(model3$S),
    itempar = function() {
        list(beta = matrix(sort(rnorm(model3$J*model3$P)), model3$J, model3$K - 1))
    },
    na_okay = FALSE)

##### Fit #####

control_list <- list(snodes = 1000, convD = .01, conv = .001)

res1 <- fit(data = data1$data,
            engine = "tam",
            object = model1,
            control = control_tam(control = control_list,
                                  item.elim = FALSE,
                                  constraint = "cases"),
            verbose = FALSE)

res2 <- fit(data = ScienceNew,
            engine = "tam",
            object = model2,
            control = control_tam(control = control_list),
            verbose = FALSE)

res2x <- TAM::tam.mml(resp = ScienceNew, irtmodel = "PCM",
                      control = control_list,
                      verbose = FALSE)

res3 <- fit(data = data3$data,
            engine = "tam",
            object = model3,
            control = control_tam(set_min_to_0 = TRUE,
                                  control = control_list),
            verbose = FALSE)

##### Tests #####

test_that("TAM errors with GRM or 2PL", {
    model <- "
    IRT:
    a BY var1;
    Weights:
    a = 0:1
    Class:
    PCM
    "
    model <- irtree_model(model)
    expect_error(fit(model, engine = "tam", data.frame(var1 = 0:1)),
                 "2PL is not implemented")
    model <- "
    IRT:
    a BY var1@1;
    Class:
    GRM
    "
    model <- irtree_model(model)
    expect_error(fit(model, engine = "tam", data.frame(var1 = 1)),
                 "Class grm is not implemented")
})

test_that("irtree_model() works", {
    expect_s3_class(model1, "irtree_model")
    expect_s3_class(model2, "irtree_model")
    expect_s3_class(model3, "irtree_model")
    expect_s3_class(model4, "irtree_model")
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

test_that("Passing arguments to TAM works", {
    expect_equal(res1$tam$control[names(control_list)], control_list)
    expect_equal(res2$tam$control[names(control_list)], control_list)
    expect_equal(res3$tam$control[names(control_list)], control_list)
    expect_equal(as.list(res1$tam$CALL)$item.elim, FALSE)
    expect_equal(as.list(res1$tam$CALL)$constraint, "cases")
})

# Tidiers -----------------------------------------------------------------

# From vignette at https://broom.tidyverse.org/articles/adding-tidiers.html

skip_if_not_installed("modeltests")

data(column_glossary, package = "modeltests")

test_that("tidy.irtree_fit()", {

    td1 <- tidy(res1)
    td2 <- tidy(res2)
    td3 <- tidy(res3)

    modeltests::check_tidy_output(td1)
    modeltests::check_tidy_output(td2)
    modeltests::check_tidy_output(td3)

    npar1 <- with(model1, J*S + S*(S+1)/2 + S*(S-1)/2)
    npar2 <- with(model2, J*(K-1) + S*(S+1)/2 + S*(S-1)/2)
    npar3 <- with(model3, J*(K-1) + S*(S+1)/2 + S*(S-1)/2)
    modeltests::check_dims(td1, npar1, 5)
    modeltests::check_dims(td2, npar2, 5)
    modeltests::check_dims(td3, npar3, 5)

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td1, term, estimate))
    tmp2 <- tmp1[["COV_21"]]/sqrt(tmp1[["VAR_1"]])/sqrt(tmp1[["VAR_2"]])
    expect_equal(tmp2, tmp1[["CORR_21"]], tolerance = .002)

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
    ag2 <- augment(res2, method = "WLE")
    ag3 <- augment(res3)

    expect_gt(
        expected = .50,
        min(
            diag(
                subset(
                    cor(data1$spec$personpar, ag1),
                    select = .fitted.Dim1:.fitted.Dim2))))
    expect_gt(
        expected = .90,
        min(
            # diag(
                subset(
                    cor(rowMeans(ScienceNew), ag2),
                    select = .fitted)))

    expect_gt(
        expected = .90,
        min(
            # diag(
                subset(
                    cor(data3$spec$personpar[, 1], ag3),
                    select = .fitted.Dim1)))

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

