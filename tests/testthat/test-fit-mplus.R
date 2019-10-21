##### Models #####

m1 <- "
# Comment
 IRT:
b BY X1@1, X2@1, X3@1, X4longname@1, X10@1;
aprocessvar BY X1@1, X2@1, X3@1, X4longname@1, X10@1;

 Equations:
1 = (1-aprocessvar)
2 = aprocessvar*(1-b)
3 = aprocessvar*b
Class:
Tree

Addendum:
aprocessvar WITH b@0;
aprocessvar WITH y1;
"

m2 <- "
IRT:
# a BY Comfort@1, Work, Future, Benefit;
a BY Work, Comfort@1, Future, Benefitvar;

Class:
GRM
"

m3 <- "
IRT:
a BY Comfort@1, Work, Future;

Class:
GRM

Addendum:
a WITH Benefit;
"

m4 <- "
IRT:
b BY X1, X2, X3, X4*2, X10;
a BY X1, X2, X3, X4*2, X10;

Equations:
1 = (1-a)
2 = a*(1-b)
3 = a*b

Class:
Tree

Addendum:
a WITH b@0;
a WITH y1;
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)


##### Data #####

X <- irtree_gen_data(object = model1, N = 100,
                     sigma = diag(model1$S),
                     itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                                    alpha = matrix(1, model1$J, model1$P)),
                     .na_okay = FALSE)
tmp1 <- names(model1$j_names)
names(tmp1) <- model1$j_names
names(X$data) <- stringr::str_replace_all(names(X$data), tmp1)
df1 <- sample(data.frame(X$data, y1 = rnorm(100)))

data(Science, package = "mirt")
ScienceNew <- Science
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

# counts <- ScienceNew %>%
#     purrr::map_dfr(~table(factor(., levels = 1:4))) %>%
#     dplyr::mutate(category = 1:4) %>%
#     reshape2::melt(value.name = "count", id.vars = "category")
# counts$variable <- toupper(as.character(counts$variable))

counts <- ScienceNew %>%
    lapply(function(x) data.frame(table(factor(x, 1:4)))) %>%
    tibble::enframe(name = "variable") %>%
    tidyr::unnest(value) %>%
    transmute(category = as.integer(Var1),
              variable = toupper(variable), count = Freq)

##### Fit #####

run <- (MplusAutomation::mplusAvailable() == 0)

res1 <- fit(data = df1,
            verbose = FALSE,
            engine = "mplus",
            object = model1,
            quadpts = 6,
            analysis_list = list(LOGCRITERION = ".01",
                                 COVERAGE = "0"),
            .warnings2messages = TRUE,
            run = run)

res2 <- fit(data = ScienceNew,
            verbose = FALSE,
            engine = "mplus",
            object = model2,
            quadpts = "MONTECARLO(500)",
            analysis_list = list(LOGCRITERION = ".01",
                                 COVERAGE = "0"),
            run = run)

res3 <- fit(data = Science,
            verbose = FALSE,
            engine = "mplus",
            object = model3,
            quadpts = "GAUSS(6)",
            analysis_list = list(LOGCRITERION = ".01",
                                 COVERAGE = "0"),
            run = run)

test_that("Provide starting values",{
    skip_if(TRUE)

    flag1 <- TRUE
    while (flag1) {
        X4 <- irtree_gen_data(object = model4, N = 100,
                              sigma = diag(model4$S),
                              itempar = list(beta = matrix(rnorm(model4$J*model4$P), model4$J, model4$P),
                                             alpha = matrix(1, model4$J, model4$P)))
        flag1 <- any(!vapply(lapply(X4$data, unique), function(x) length(x) == model4$K, FUN.VALUE = T))
    }
    tmp1 <- names(model4$j_names)
    names(tmp1) <- model4$j_names
    names(X4$data) <- stringr::str_replace_all(names(X4$data), tmp1)
    df4 <- sample(data.frame(X4$data, y1 = rnorm(100)))

    model4 <- irtree_model(m4)
    res4 <- fit(data = df4,
                verbose = FALSE,
                engine = "mplus",
                object = model4,
                file_name = basename(tempfile()),
                dir = tempdir(),
                run = T,
                quadpts = 7)
})

##### Tests #####

skip_if_not(MplusAutomation::mplusAvailable() == 0)

test_that("irtree_fit_mplus() works for Tree", {

    expect_s3_class(res1$mplus, "mplus.model")
    checkmate::expect_data_frame(res1$mplus$parameters$unstandardized,
                                 any.missing = FALSE,
                                 nrows = 26, ncols = 6)
})

test_that("irtree_fit_mplus() works for GRM", {

    expect_s3_class(res2$mplus, "mplus.model")

    checkmate::expect_data_frame(res2$mplus$parameters$unstandardized,
                                 any.missing = FALSE,
                                 nrows = 17, ncols = 6)
    expect_equal(res2$mplus$sampstat$proportions.counts[, "count"],
                 counts[, "count", drop = TRUE], check.attributes = FALSE)
})

test_that("Methods work for output of irtree_fit_mplus()", {

    expect_condition(capture.output(print(res1)), NA)
    expect_condition(capture.output(summary(res1)), NA)
    expect_condition(capture.output(coef(res1)), NA)
})

# Tidiers -----------------------------------------------------------------

# From vignette at https://broom.tidyverse.org/articles/adding-tidiers.html

skip_if_not_installed("modeltests")

data(column_glossary, package = "modeltests")
data(argument_glossary, package = "modeltests")

test_that("irtree_fit tidier arguments", {

    modeltests::check_arguments(tidy.irtree_fit)
    modeltests::check_arguments(glance.irtree_fit)
    modeltests::check_arguments(augment.irtree_fit, strict = FALSE)
})

test_that("tidy.irtree_fit()", {

    td1 <- tidy(res1)
    td2 <- tidy(res2)
    td3 <- tidy(res3)

    modeltests::check_tidy_output(subset(td1, select = -effect))
    modeltests::check_tidy_output(subset(td2, select = -effect))
    modeltests::check_tidy_output(subset(td3, select = -effect))

    modeltests::check_dims(td1, 28, 5)  # optional but a good idea
    modeltests::check_dims(td2, 17, 5)  # optional but a good idea
    modeltests::check_dims(td3, 17, 5)  # optional but a good idea

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td3, term, estimate))
    tmp2 <- tmp1[["A<->BENEFIT"]]/sqrt(tmp1[["BENEFIT<->BENEFIT"]])/sqrt(tmp1[["A<->A"]])
    expect_equal(tmp2, tmp1[["COR_A<->BENEFIT"]], tolerance = .002)

    tmp1 <- dplyr::filter(td3, grepl("Thresholds", term)) %>%
        select(3) %>%
        as.data.frame
    tmp2 <- dplyr::filter(td2, grepl("Thresholds", term)) %>%
        select(3) %>%
        slice(1:9) %>%
        as.data.frame
    expect_equal(tmp1, tmp2, tolerance = .002)

    checkmate::expect_numeric(td1$p.value, lower = 0, upper = 1, finite = TRUE)
    checkmate::expect_numeric(td2$p.value, lower = 0, upper = 1, finite = TRUE)
    checkmate::expect_numeric(td3$p.value, lower = 0, upper = 1, finite = TRUE)
    checkmate::expect_numeric(td1$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td2$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td3$std.error, lower = 0, finite = TRUE)

})

test_that("glance.irtree_fit()", {

    gl1 <- glance(res1)
    gl2 <- glance(res2)
    gl3 <- glance(res3)

    modeltests::check_glance_outputs(gl1, gl2, strict = TRUE)

    ### Own tests ###

    expect_equal(pull(gl1, nobs), nrow(df1))
    expect_equal(pull(gl2, nobs), nrow(Science))

})

test_that("augment.irtree_fit()", {

    modeltests::check_augment_function(
        augment.irtree_fit, res1, data = df1, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res2, data = ScienceNew, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res3, data = Science, strict = FALSE
    )

    ### Own tests ###

    ag1 <- augment(res1)
    ag2 <- augment(res2)
    ag3 <- augment(res3)

    modeltests::check_dims(ag1, nrow(df1), ncol(df1) + model1$S*2)
    modeltests::check_dims(ag2, nrow(Science), ncol(ScienceNew) + model2$S*2)
    modeltests::check_dims(ag3, nrow(Science), ncol(Science) + model2$S*2)

    checkmate::expect_numeric(ag1$.se.fitAB, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.se.fitBAP, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.se.fitA, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.se.fitA, lower = 0, finite = TRUE, all.missing = FALSE)

})
