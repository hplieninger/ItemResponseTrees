skip_if_not_installed("sirt")

##### Data #####

data("data.big5.qgraph", package = "sirt")

df0 <- as_tibble(data.big5.qgraph)
df1 <- select(df0, starts_with("E")[1:10], starts_with("C")[1:10])

##### Model #####

m1 <- "
IRT:
a1 BY E2@1, E7@1, E12@1, E17@1, E22@1, E27@1, E32@1, E37@1, E42@1, E47@1;
a2 BY C5@1, C10@1, C15@1, C20@1, C25@1, C30@1, C35@1, C40@1, C45@1, C50@1;
e  BY E2@1, E7@1, E12@1, E17@1, E22@1, E27@1, E32@1, E37@1, E42@1, E47@1, C5@1, C10@1, C15@1, C20@1, C25@1, C30@1, C35@1, C40@1, C45@1, C50@1;
m  BY E2@1, E7@1, E12@1, E17@1, E22@1, E27@1, E32@1, E37@1, E42@1, E47@1, C5@1, C10@1, C15@1, C20@1, C25@1, C30@1, C35@1, C40@1, C45@1, C50@1;

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
a1 BY E2@1, E7@1, E12@1, E17@1, E22@1, E27@1, E32@1, E37@1, E42@1, E47@1;
a2 BY C5@1, C10@1, C15@1, C20@1, C25@1, C30@1, C35@1, C40@1, C45@1, C50@1;
# b1 BY E2@1, E7@1, E12@1, E17@1, E22@1, E27@1, E32@1, E37@1, E42@1, E47@1;
# b2 BY C5@1, C10@1, C15@1, C20@1, C25@1, C30@1, C35@1, C40@1, C45@1, C50@1;

Weights:
t = 0:4
# e = c(1, 0, 0, 0, 1)

Class:
PCM

Constraints:
t = a1 | a2
# e = b1 | b2
"

m3 <- "
IRT:
a1 BY E2@1, E7@1, E12@1, E17@1, E22@1, E27@1, E32@1, E37@1, E42@1, E47@1;
a2 BY C5@1, C10@1, C15@1, C20@1, C25@1, C30@1, C35@1, C40@1, C45@1, C50@1;

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
             control = list(snodes = 1000, convD = .01, conv = .001),
             verbose = verbose)

res12 <- fit(data = df1,
             engine = "mirt",
             object = model1,
             TOL = .01,
             SE = FALSE,
             quadpts = 10,
             verbose = verbose)

res21 <- fit(data = df1,
             engine = "tam",
             object = model2,
             control = list(snodes = 1000, convD = .01, conv = .001),
             verbose = verbose,
             .set_min_to_0 = TRUE)

res31 <- fit(data = df1,
             engine = "mirt",
             object = model3,
             TOL = .01,
             SE = FALSE,
             quadpts = 10,
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

    expect_true(.99 < min(diag(cor(select(ag1, matches(".fitted.")),
                                   select(ag2, matches(".fitted."))))))

    skip_if_not(MplusAutomation::mplusAvailable() == 0)

    res13 <- fit(data = df1,
                 engine = "mplus",
                 object = model1,
                 quadpts = 5,
                 link = "logit",
                 analysis_list = list(LOGCRITERION = ".01",
                                      COVERAGE = "0"),
                 verbose = verbose,
                 .warnings2messages = TRUE)

    res32 <- fit(data = df1,
                 engine = "mplus",
                 object = model3,
                 quadpts = 5,
                 analysis_list = list(LOGCRITERION = ".01",
                                      COVERAGE = "0"),
                 verbose = verbose,
                 .warnings2messages = TRUE)

    expect_s3_class(res13, "irtree_fit")
    expect_s3_class(res32, "irtree_fit")

    expect_s3_class(res13$mplus,  "mplus.model")
    expect_s3_class(res32$mplus,  "mplus.model")

    ag3 <- augment(res13)

    expect_true(.99 < min(diag(cor(select(ag1, matches(".fitted.")),
                                   select(ag3, matches(".fitted."))))))
})
