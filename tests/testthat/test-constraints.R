##### Data #####

skip_on_cran()

data("jackson")

df1 <- select(jackson, starts_with("E")[1:10], starts_with("C")[1:10]) %>%
    sample_n(300)

##### Model #####

m1 <- "
IRT:
a1 BY E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1, E10@1;
a2 BY C1@1, C2@1, C3@1, C4@1, C5@1, C6@1, C7@1, C8@1, C9@1, C10@1;
e  BY E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1, E10@1, C1@1, C2@1, C3@1, C4@1,
      C5@1, C6@1, C7@1, C8@1, C9@1, C10@1;
m  BY E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1, E10@1, C1@1, C2@1, C3@1, C4@1,
      C5@1, C6@1, C7@1, C8@1, C9@1, C10@1;

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Class:
Tree

Constraints:
t = a1 | a2
e = m
"

m2 <- "
IRT:
a1 BY E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1, E10@1;
a2 BY C1@1, C2@1, C3@1, C4@1, C5@1, C6@1, C7@1, C8@1, C9@1, C10@1;

Weights:
t = 0:4

Class:
PCM

Constraints:
t = a1 | a2
"

m3 <- "
IRT:
a1 BY E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1, E10@1;
a2 BY C1@1, C2@1, C3@1, C4@1, C5@1, C6@1, C7@1, C8@1, C9@1, C10@1;

Class:
GRM

Constraints:
t = a1 | a2
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)

##### Fit #####

verbose <- FALSE

res11 <- fit(data = df1,
             engine = "tam",
             object = model1,
             control = control_tam(
                 control = list(snodes = 1000, convD = .01, conv = .001)),
             verbose = verbose)

res12 <- fit(data = df1,
             engine = "mirt",
             object = model1,
             control = control_mirt(SE = FALSE, TOL = .01, quadpts = 10),
             verbose = verbose)

res21 <- fit(data = df1,
             engine = "tam",
             object = model2,
             control = control_tam(set_min_to_0 = TRUE,
                                   control = list(snodes = 1000, convD = .01, conv = .001)),
             verbose = verbose)

res31 <- fit(data = df1,
             engine = "mirt",
             object = model3,
             control = control_mirt(SE = FALSE, TOL = .01, quadpts = 10),
             verbose = verbose)

##### Tests #####

test_that("Model constraints work", {

    expect_s3_class(res11, "irtree_fit")
    expect_s3_class(res12, "irtree_fit")
    expect_s3_class(res21, "irtree_fit")
    expect_s3_class(res31, "irtree_fit")

    expect_s3_class(res11$tam,  "tam.mml")
    expect_s4_class(res12$mirt, "SingleGroupClass")
    expect_s3_class(res21$tam,  "tam.mml")
    expect_s4_class(res31$mirt,  "SingleGroupClass")

    ag1 <- augment(res11)
    ag2 <- augment(res12)

    expect_gt(min(diag(cor(select(ag1, matches(".fitted.")),
                           select(ag2, matches(".fitted."))))), .95)

    gl11 <- glance(res11)
    gl12 <- glance(res12)
    gl21 <- glance(res21)
    gl31 <- glance(res31)

    expect_equal(gl11$n.factors, model1$S)
    expect_equal(gl12$n.factors, model1$S)
    expect_equal(gl21$n.factors, model2$S)
    expect_equal(gl31$n.factors, model3$S)

    skip_if_not(MplusAutomation::mplusAvailable() == 0)

    res13 <- fit(data = df1,
                 engine = "mplus",
                 object = model1,
                 verbose = verbose,
                 link = "logit",
                 control = control_mplus(
                     quadpts = 5,
                     analysis_list = list(LOGCRITERION = ".01",
                                          COVERAGE = "0"),
                     warnings2messages = TRUE))

    res32 <- fit(data = df1,
                 engine = "mplus",
                 object = model3,
                 verbose = verbose,
                 control = control_mplus(
                     quadpts = 5,
                     analysis_list = list(LOGCRITERION = ".01",
                                          COVERAGE = "0"),
                     warnings2messages = TRUE))

    expect_s3_class(res13, "irtree_fit")
    expect_s3_class(res32, "irtree_fit")

    expect_s3_class(res13$mplus,  "mplus.model")
    expect_s3_class(res32$mplus,  "mplus.model")

    ag3 <- augment(res13)

    expect_gt(min(diag(cor(select(ag1, matches(".fitted.")),
                           select(ag3, matches(".fitted."))))), .95)

    gl13 <- glance(res13)
    gl32 <- glance(res32)

    expect_equal(gl13$n.factors, model1$S)
    expect_equal(gl32$n.factors, model3$S)
})
