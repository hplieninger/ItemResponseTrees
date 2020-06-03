##### Models #####

m1 <- "
# Comment
IRT:
a BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;
b BY X1@1, X2@1, X3@1, X4@1, X5@1, X6@1, X7@1;

Equations:
 1 = a*b
 0 = a*(1-b)
-1 = (1-a)

Class:
Tree
"

m2 <- "
IRT:
a BY Work@1, Comfort@1, Future@1, Benefitvar;

Class:
GRM
"


m3 <- "
IRT:
a BY Work@1, Comfort@1, Future@1, Benefit@1;
b BY Work@1, Comfort@1, Future@1, Benefit@1;
c BY Work@1, Comfort@1, Future@1, Benefit@1;

Equations:
1 = (1-a)
2 = a*(1-b)
3 = a*b*(1-c)
4 = a*b*c

Class:
Tree

Constraints:
a = b
a = c
"

m4 <- "
IRT:
a BY Work@1, Comfort@1;
b BY Future@1, Benefitvar;

Class:
GRM
"

m5 <- "
IRT:
a BY Comfort, Future, Benefit;

Class:
GRM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)
model4 <- irtree_model(m4)
model5 <- irtree_model(m5)

##### Data #####

X <- irtree_gen_data(object = model1, N = 200,
                     sigma = diag(model1$S),
                     itempar = list(beta = matrix(sort(rnorm(model1$J*model1$P)),
                                                  model1$J, model1$P),
                                    alpha = matrix(1, model1$J, model1$P)),
                     na_okay = FALSE)
for (ii in seq_len(ncol(X$data))) {
    X$data[ii, ii] <- NA
}
# Test mix of integers and numerics and also variable label
X$data$X1 <- as.numeric(X$data$X1)
X$data$X2 <- as.integer(X$data$X2)
attr(X$data$X1, "label") <- "random var label"

data(Science, package = "mirt")
ScienceNew <- Science
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

##### Fit #####

ctrl <- control_mirt(SE = FALSE, TOL = .01, quadpts = 31,
                     control = list(trace = 0),
                     technical = list(parallel = FALSE, symmetric = TRUE))

res1 <- fit(data = X$data,
            engine = "mirt",
            object = model1,
            control = ctrl,
            verbose = FALSE)

res2 <- fit(data = ScienceNew,
            engine = "mirt",
            object = model2,
            control = control_mirt(SE = FALSE, TOL = .01),
            verbose = FALSE)
res4 <- fit(data = ScienceNew,
            engine = "mirt",
            object = model4,
            control = control_mirt(SE = FALSE, TOL = .01),
            verbose = FALSE)

res2x <- mirt::mirt(ScienceNew,
                    # 1,
                    "a = 1-4
                    COV = a*a
                    FIXED = (1-3, a1)
                    START = (1-3, a1, 1.0)",
                    "graded", SE = FALSE, TOL = .01,
                    # constrain = list(c(1, 5, 9)),
                    verbose = FALSE)

res3 <- fit(data = Science,
            engine = "mirt",
            object = model3,
            control = control_mirt(SE = FALSE, TOL = .01),
            verbose = FALSE)

res3x <- mirt::mirt(Science, 1, "Tutz", SE = FALSE, TOL = .01, verbose = FALSE)

res5 <- fit(data = subset(Science, select = -Work),
            object = model5,
            control = control_mirt(SE = TRUE, TOL = .01),
            verbose = FALSE)

##### Tests #####

test_that("irtree_model() works", {
    expect_s3_class(model1, "irtree_model")
    expect_s3_class(model2, "irtree_model")
    expect_s3_class(model3, "irtree_model")
    expect_s3_class(model4, "irtree_model")
    expect_s3_class(model5, "irtree_model")
    expect_integers_in_irtree_model(model1)
    expect_integers_in_irtree_model(model2)
    expect_integers_in_irtree_model(model3)
    expect_integers_in_irtree_model(model4)
    expect_integers_in_irtree_model(model5)
})

test_that("irtree_fit_mirt() works", {
    expect_s4_class(res1$mirt, "SingleGroupClass")
    expect_s4_class(res2$mirt, "SingleGroupClass")
    expect_equal(mirt::coef(res2$mirt), mirt::coef(res2x))
    expect_s4_class(res3$mirt, "SingleGroupClass")
    expect_s4_class(res4$mirt, "SingleGroupClass")
    expect_s4_class(res5$mirt, "SingleGroupClass")
})

test_that("Methods work for output of irtree_fit_mirt()", {
    expect_condition(capture.output(print(res1)), NA)
    expect_condition(capture.output(summary(res2)), NA)
    expect_condition(capture.output(coef(res3)), NA)
})

test_that("Passing arguments to mirt works", {
    skip_if_not_installed("mirt", "1.32.1")
    tmp1 <- res1$mirt@Options[names(ctrl)[!names(ctrl) %in% "control"]]
    tmp1$technical <- tmp1$technical[names(ctrl$technical)]
    expect_equal(tmp1,
                 ctrl[!names(ctrl) %in% "control"])
})

# Tidiers -----------------------------------------------------------------

# From vignette at https://broom.tidyverse.org/articles/adding-tidiers.html

test_that("tidy.irtree_fit()", {

    skip_if_not_installed("modeltests")

    data(column_glossary, package = "modeltests")

    expect_error(tidy(res1), "Assertion on 'par_type' failed")
    td1 <- tidy(res1, par_type = "easiness")
    td2 <- tidy(res2, par_type = "difficulty")
    td3 <- tidy(res3, par_type = "easiness")
    td4 <- tidy(res4, par_type = "difficulty")
    td5 <- tidy(res5, par_type = "difficulty")

    modeltests::check_tidy_output(td1)
    modeltests::check_tidy_output(td2)
    modeltests::check_tidy_output(td3)
    modeltests::check_tidy_output(td4)
    modeltests::check_tidy_output(td5)

    n_ipar_1 <- with(model1, J*4 + S + S*(S+1)/2 + S*(S-1)/2)
    n_ipar_2 <- with(model2, J + J*3 + S + S)
    n_ipar_3 <- with(model3, J*3*2 + S + S)
    n_ipar_4 <- with(model4, J*3 + J + S + S*(S+1)/2 + S*(S-1)/2)
    n_ipar_5 <- with(model5, J*4 + S + S)

    modeltests::check_dims(td1, n_ipar_1, 5)
    modeltests::check_dims(td2, n_ipar_2, 5)
    modeltests::check_dims(td3, n_ipar_3, 5)
    modeltests::check_dims(td4, n_ipar_4, 5)
    modeltests::check_dims(td5, n_ipar_5, 5)

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td1, term, estimate))
    tmp2 <- tmp1[["COV_21"]]/sqrt(tmp1[["COV_11"]])/sqrt(tmp1[["COV_22"]])
    expect_equal(tmp2, tmp1[["CORR_21"]], tolerance = .002)

    checkmate::expect_numeric(td1$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td2$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td3$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td4$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td5$std.error[1:(n_ipar_5-2)], lower = 0,
                              finite = TRUE, any.missing = FALSE)

})

test_that("glance.irtree_fit()", {

    skip_if_not_installed("modeltests")

    data(column_glossary, package = "modeltests")

    gl1 <- glance(res1)
    gl2 <- glance(res2)
    gl3 <- glance(res3)
    gl4 <- glance(res4)
    gl5 <- glance(res5)

    modeltests::check_glance_outputs(gl1, gl2, gl3, gl4, gl5, strict = TRUE)

    ### Own tests ###

    expect_equal(pull(gl1, nobs), nrow(X$data))
    expect_equal(pull(gl2, nobs), nrow(ScienceNew))
    expect_equal(pull(gl3, nobs), nrow(Science))
    expect_equal(pull(gl4, nobs), nrow(ScienceNew))
    expect_equal(pull(gl5, nobs), nrow(Science))

    expect_equal(pull(gl1, npar), with(model1, J*S + S*(S+1)/2))
    expect_equal(pull(gl2, npar), with(model2, (J-3) + J*3 + S*(S+1)/2))
    expect_equal(pull(gl3, npar), with(model3, J*3 + S*(S+1)/2))
    expect_equal(pull(gl4, npar), with(model4, (J-3) + J*3 + S*(S+1)/2))
    expect_equal(pull(gl5, npar), with(model5, J*4))

})

test_that("implementation of augment.irtree_fit()", {

    skip_if_not_installed("modeltests")

    # data(column_glossary, package = "modeltests")

    modeltests::check_augment_function(
        augment.irtree_fit, res1, data = X$data, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res2, data = ScienceNew, strict = FALSE
    )

    ag1 <- augment(res1)
    ag2 <- augment(res2)
    ag3 <- augment(res3)
    ag4 <- augment(res4)
    ag5 <- augment(res5)

    modeltests::check_dims(ag1, nrow(X$data), ncol(X$data) + model1$S*2)
    modeltests::check_dims(ag2, nrow(ScienceNew), ncol(ScienceNew) + model2$S*2)
    modeltests::check_dims(ag3, nrow(Science), ncol(Science) + model3$S*2)
    modeltests::check_dims(ag4, nrow(ScienceNew), ncol(ScienceNew) + model4$S*2)
    modeltests::check_dims(ag5, nrow(Science), ncol(Science) - 1 + model5$S*2)

    skip_if_not_installed("mirt", "1.32.1")

    checkmate::expect_numeric(ag1$.fitted.a, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.fitted.b, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.fitted.a, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.fitted.a, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag4$.fitted.a, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag4$.fitted.b, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag5$.fitted.a, finite = TRUE, any.missing = FALSE)

    checkmate::expect_numeric(ag1$.se.fit.a, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.se.fit.b, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.se.fit.a, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.se.fit.a, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag4$.se.fit.a, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag4$.se.fit.b, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag5$.se.fit.a, lower = 0, finite = TRUE, any.missing = FALSE)

    expect_gt(cor(ag3$.fitted.a, mirt::fscores(res3x)), .99)
})
