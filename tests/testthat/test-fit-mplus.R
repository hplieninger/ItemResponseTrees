context("Mplus: Simple models")

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

model1 <- tree_model(m1)
model2 <- tree_model(m2)
model3 <- tree_model(m3)


##### Data #####

flag1 <- TRUE
while (flag1) {
    X <- gen_tree_data(model = model1, N = 100,
                       sigma = diag(model1$S),
                       itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                                      alpha = matrix(1, model1$J, model1$P)))
    flag1 <- any(!vapply(lapply(X$data, unique), function(x) length(x) == model1$K, FUN.VALUE = T))
}
tmp1 <- names(model1$j_names)
names(tmp1) <- model1$j_names
names(X$data) <- stringr::str_replace_all(names(X$data), tmp1)
df1 <- sample(data.frame(X$data, y1 = rnorm(100)))

data(Science, package = "mirt")
ScienceNew <- Science
names(ScienceNew) <- sub("Benefit", "Benefitvar", names(ScienceNew))

counts <- ScienceNew %>%
    purrr::map_dfr(~table(factor(., levels = 1:4))) %>%
    dplyr::mutate(category = 1:4) %>%
    reshape2::melt(value.name = "count", id.vars = "category")
counts$variable <- toupper(as.character(counts$variable))

##### Fit #####

if (MplusAutomation::mplusAvailable() == 0) {
    res1 <- fit_tree_mplus(data = df1,
                           model = model1,
                           file_name = basename(tempfile()),
                           dir = tempdir(),
                           run = T,
                           integration_points = 7,
                           analysis_list = list(LOGCRITERION = ".01"),
                           .warnings2messages = TRUE)
    summ1a <- extract_mplus_output(res1$mplus, model1)
    summ1b <- extract_mplus_output(res1$mplus, m1)
    summ1c <- extract_mplus_output(res1$mplus, class = "tree")

    res2 <- fit_tree_mplus(data = ScienceNew,
                           model = model2,
                           file_name = basename(tempfile()),
                           dir = tempdir(),
                           run = T,
                           integration_points = 7)

    summ2a <- extract_mplus_output(res2$mplus, model2)
    summ2b <- extract_mplus_output(res2$mplus, m2)
    summ2c <- extract_mplus_output(res2$mplus, class = "grm")

    res3 <- fit_tree_mplus(data = Science,
                           model = model3,
                           file_name = basename(tempfile()),
                           dir = tempdir(),
                           run = T,
                           integration_points = 7)
}

test_that("Provide starting values",{
    skip_if(TRUE)

    flag1 <- TRUE
    while (flag1) {
        X4 <- gen_tree_data(model = model4, N = 100,
                            sigma = diag(model4$S),
                            itempar = list(beta = matrix(rnorm(model4$J*model4$P), model4$J, model4$P),
                                           alpha = matrix(1, model4$J, model4$P)))
        flag1 <- any(!vapply(lapply(X4$data, unique), function(x) length(x) == model4$K, FUN.VALUE = T))
    }
    tmp1 <- names(model4$j_names)
    names(tmp1) <- model4$j_names
    names(X4$data) <- stringr::str_replace_all(names(X4$data), tmp1)
    df4 <- sample(data.frame(X4$data, y1 = rnorm(100)))

    model4 <- tree_model(m4)
    res4 <- fit_tree_mplus(data = df4,
                           model = model4,
                           file_name = basename(tempfile()),
                           dir = tempdir(),
                           run = T,
                           integration_points = 7)
})

##### Tests #####

test_that("fit_tree_mplus() works for Tree", {
    skip_if_not(MplusAutomation::mplusAvailable() == 0)

    expect_s3_class(res1$mplus, "mplus.model")
    checkmate::expect_data_frame(res1$mplus$parameters$unstandardized,
                                 any.missing = FALSE,
                                 nrows = 26, ncols = 6)
})

test_that("fit_tree_mplus() works for GRM", {
    skip_if_not(MplusAutomation::mplusAvailable() == 0)

    expect_s3_class(res2$mplus, "mplus.model")

    checkmate::expect_data_frame(res2$mplus$parameters$unstandardized,
                                 any.missing = FALSE,
                                 nrows = 17, ncols = 6)
    expect_equal(res2$mplus$sampstat$proportions.counts[, "count"],
                 counts[, "count"], check.attributes = FALSE)
})

test_that("extract_mplus_output() works for Tree", {
    skip_if_not(MplusAutomation::mplusAvailable() == 0)

    expect_equal(summ1a, summ1b)
    expect_equal(summ1a, summ1c)

    expect_s3_class(summ1a$summaries, "mplus.summaries")
    expect_s3_class(summ1a$warnings, "mplus.warnings")
    expect_s3_class(summ1a$errors, "mplus.errors")

    lapply(summ1a$person, checkmate::expect_data_frame,
           any.missing = FALSE,
           nrows = nrow(df1), ncols = model1$P,
           info = "Tested expect_data_frame(summ1a$person)")
    checkmate::expect_list(summ1a$item, types = "list", any.missing = FALSE, len = 4)
    lapply(summ1a$item, checkmate::expect_data_frame,
           min.cols = 2, max.cols = NULL, nrows = model1$J,
           info = "Tested expect_data_frame(summ1a$item)")
    expect_equal(summ1a$item$beta[summ1a$item$beta$item == "X3", "AB"],
                 res1$mplus$parameters$unstandardized[res1$mplus$parameters$unstandardized$param == "AB_X3$1", "est"])
    checkmate::expect_set_equal(names(summ1a$item), c("beta", "beta_se", "alpha", "alpha_se"))
    checkmate::expect_matrix(summ1a$sigma, mode = "numeric", all.missing = FALSE,
                             nrows = model1$P, ncols = model1$P)
    checkmate::expect_matrix(summ1a$cormat, mode = "numeric", all.missing = FALSE,
                             nrows = model1$P, ncols = model1$P)
})


test_that("extract_mplus_output() works for GRM", {
    skip_if_not(MplusAutomation::mplusAvailable() == 0)

    expect_equal(summ2a, summ2b)
    expect_equal(summ2a, summ2c)

    expect_s3_class(summ2a$summaries, "mplus.summaries")
    expect_s3_class(summ2a$warnings, "mplus.warnings")
    expect_s3_class(summ2a$errors, "mplus.errors")

    lapply(summ2a$person, checkmate::expect_data_frame,
           any.missing = FALSE,
           nrows = nrow(ScienceNew), ncols = model2$P,
           info = "Tested expect_data_frame(summ2a$person)")
    checkmate::expect_list(summ2a$item, types = "list", any.missing = FALSE, len = 4)
    lapply(summ2a$item, checkmate::expect_data_frame,
           all.missing = FALSE,
           min.cols = 2, max.cols = NULL, nrows = model2$J,
           info = "Tested expect_data_frame(summ2a$item)")
    checkmate::expect_set_equal(names(summ2a$item), c("beta", "beta_se", "alpha", "alpha_se"))
    checkmate::expect_matrix(summ2a$sigma, mode = "numeric", any.missing = FALSE,
                             nrows = 1, ncols = model2$P)
    checkmate::expect_matrix(summ2a$cormat, mode = "numeric", any.missing = FALSE,
                             nrows = 1, ncols = model2$P)
})
