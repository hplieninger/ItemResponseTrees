##### Models #####

m1 <- "
# Comment
IRT:
a BY X1@1, X2@1, X3@1, X4@1, X5@1;
b BY X1@1, X2@1, X3@1, X4@1, X5@1;

Equations:
 1 = a*b
 0 = a*(1-b)
-1 = (1-a)

Class:
Tree
"

m2 <- "
IRT:
# a BY Comfort, Work, Future, Benefit;
a BY Work@1, Comfort@1, Future@1, Benefitvar;
# a BY Comfort, Work, Benefitvar, Future;

Class:
GRM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

##### Data #####

X <- irtree_gen_data(object = model1, N = 100,
                     sigma = diag(model1$S),
                     itempar = list(beta = matrix(sort(rnorm(model1$J*model1$P)),
                                                  model1$J, model1$P),
                                    alpha = matrix(1, model1$J, model1$P)),
                     .na_okay = FALSE)

data(Science, package = "mirt")
ScienceNew <- Science
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

##### Fit #####

res1 <- fit(data = X$data,
            engine = "mirt",
            object = model1,
            SE = FALSE,
            method = "QMCEM",
            quadpts = 1000,
            verbose = FALSE)

res2 <- fit(data = ScienceNew,
            engine = "mirt",
            object = model2,
            SE = FALSE,
            method = "QMCEM",
            quadpts = 1000,
            verbose = FALSE)

res2x <- mirt::mirt(ScienceNew, 1, "graded", SE = FALSE,
                    method = "QMCEM", quadpts = 1000, verbose = FALSE,
                    constrain = list(c(1, 5, 9)))

##### Tests #####

test_that("irtree_model() works", {
    expect_s3_class(model1, "irtree_model")
    expect_s3_class(model2, "irtree_model")
})

test_that("irtree_fit_mirt() works", {
    expect_s4_class(res1$mirt, "SingleGroupClass")
    expect_s4_class(res2$mirt, "SingleGroupClass")
    expect_equal(mirt::coef(res2$mirt), mirt::coef(res2x))
})

test_that("Methods work for output of irtree_fit_mirt()", {
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
    td2 <- tidy(res2, difficulty = TRUE)

    modeltests::check_tidy_output(subset(td1, select = -effect))
    modeltests::check_tidy_output(subset(td2, select = -effect))

    modeltests::check_dims(td1, 26, 4)  # optional but a good idea
    modeltests::check_dims(td2, 18, 4)  # optional but a good idea

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td1, term, estimate))
    tmp2 <- tmp1[["COV_21"]]/sqrt(tmp1[["COV_11"]])/sqrt(tmp1[["COV_22"]])
    expect_equal(tmp2, tmp1[["COR_a.b"]], tolerance = .002)

    tmp1 <- tibble::deframe(select(td2, term, estimate))
    expect_equal(tmp1[["MEAN_1"]], 0)
    expect_equal(tmp1[["COV_11"]], 1)

    checkmate::expect_numeric(td1$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td2$std.error, lower = 0, finite = TRUE)

})

test_that("glance.irtree_fit()", {

    gl1 <- glance(res1)
    gl2 <- glance(res2)

    modeltests::check_glance_outputs(gl1, gl2, strict = TRUE)

    ### Own tests ###

    expect_equal(pull(gl1, nobs), nrow(X$data))
    expect_equal(pull(gl2, nobs), nrow(ScienceNew))

})

test_that("implementation of augment.irtree_fit()", {

    modeltests::check_augment_function(
        augment.irtree_fit, res1, data = X$data, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res2, data = ScienceNew, strict = FALSE
    )

    ag1 <- augment(res1)
    ag2 <- augment(res2)

    modeltests::check_dims(ag1, nrow(X$data), ncol(X$data) + model1$S*2)
    modeltests::check_dims(ag2, nrow(ScienceNew), ncol(ScienceNew) + model2$S*2)

    checkmate::expect_numeric(ag1$.se.fitF1, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.se.fitF2, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.se.fitF1, lower = 0, finite = TRUE, all.missing = FALSE)

})
