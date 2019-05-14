##### Data #####

if (requireNamespace("lme4", quietly = TRUE)) {
    data(VerbAgg, package = "lme4")

    dat_1 <- VerbAgg %>%
        dplyr::mutate_at("resp", as.integer) %>%
        reshape(direction = "wide",
                idvar = c("id", "Anger", "Gender"),
                timevar = "item", v.names = "resp",
                drop = c("btype", "situ", "mode", "r2"))
    names(dat_1) <- sub("^resp[.]", "", names(dat_1))
    # dat_1 <- dplyr::mutate_at(VerbAgg, "resp", as.integer)
    # dat_1 <- reshape2::dcast(dat_1, id + Anger + Gender ~ item, value.var = "resp")
    dat_1x <- dat_1

    names(dat_1x)[names(dat_1x) == "S4wantCurse"] <- "S4WantCurse"
    names(dat_1x)[-(1:3)] <-
        stringr::str_replace_all(names(dat_1x)[-(1:3)],
                                 c("^S" = "", "Do" = "D", "Want" = "W",
                                   "Curse" = "Cu", "Scold" = "Sc", "Shout" = "Sh"))
    names(dat_1x)[-(1:3)] <- gsub("([1-4])(\\w{3})", "\\2\\1", names(dat_1x)[-(1:3)])
}

##### Model 1 #####

m1 <- "

# Model with 2 Processes 0-1-1 and NA-0-1
# Model with 4 LVs with constraints, such that processes are identical within
# Do-items and within Want-items

IRT:
AW BY WCu1, WSc1, WSh1, WCu2, WSc2, WSh2, WCu3, WSc3, WSh3, WCu4, WSc4, WSh4;
BW BY WCu1, WSc1, WSh1, WCu2, WSc2, WSh2, WCu3, WSc3, WSh3, WCu4, WSc4, WSh4;

AD BY DCu1, DSc1, DSh1, DCu2, DSc2, DSh2, DCu3, DSc3, DSh3, DCu4, DSc4, DSh4;
BD BY DCu1, DSc1, DSh1, DCu2, DSc2, DSh2, DCu3, DSc3, DSh3, DCu4, DSc4, DSh4;

Equations:
1 = (1- A)
2 = A * (1  - B)
3 = A * B

Subtree:
B = BW + BD
A = AW + AD

Constraints:
AW = BW
AD = BD

Class:
Tree
"

##### Model 2 #####

tmp1 <- "IRT:"
tmp2 <- c("WeakWant   BY S1WantCurse, S1WantScold, S1WantShout, S2WantCurse, S2WantScold, S2WantShout, S3WantCurse, S3WantScold, S3WantShout, S4wantCurse, S4WantScold, S4WantShout;",
          "StrongWant BY S1WantCurse, S1WantScold, S1WantShout, S2WantCurse, S2WantScold, S2WantShout, S3WantCurse, S3WantScold, S3WantShout, S4wantCurse, S4WantScold, S4WantShout;",
          "WeakDO     BY S1DoCurse, S1DoScold, S1DoShout, S2DoCurse, S2DoScold, S2DoShout, S3DoCurse, S3DoScold, S3DoShout, S4DoCurse, S4DoScold, S4DoShout;",
          "StrongDo   BY S1DoCurse, S1DoScold, S1DoShout, S2DoCurse, S2DoScold, S2DoShout, S3DoCurse, S3DoScold, S3DoShout, S4DoCurse, S4DoScold, S4DoShout;")
tmp3 <- "
Equations:
1 = (1- Weak)
2 = Weak * (1  - Strong)
3 = Weak * Strong

Class:
Tree

Subtree:
"
tmp4 <- c("Strong = StrongWant + StrongDo",
          "Weak = WeakWant + WeakDO")
tmp5 <- "Constraints:"
# tmp6 <- c("WeakWant = StrongWant",
#           "WeakDO = StrongDo")

tmp6 <- sample(list(c("WeakWant", "WeakDO"), c("StrongWant", "StrongDo"))) %>%
    {paste(.[[1]], .[[2]], sep = " = ")}

m2 <- paste(c(tmp1, sample(tmp2), tmp3, sample(tmp4), tmp5, sample(tmp6)), collapse = "\n")


model1 <- tree_model(m1)
model2 <- tree_model(m2)


test_that("tree_model() works", {
    expect_s3_class(model1, "tree_model")
    expect_s3_class(model2, "tree_model")
})

##### Fit #####

test_that("Model constraints work independently of names", {
    skip_if_not(MplusAutomation::mplusAvailable() == 0)
    skip_if_not_installed("lme4")

    rx <- sample(24)

    res1 <- fit_tree_mplus(data = dat_1x[names(dat_1x) %in% names(model1$j_names)][rx],
                           model = model1,
                           file_name = basename(tempfile()),
                           dir = tempdir(),
                           run = TRUE,
                           integration_points = 7,
                           analysis_list = list(LOGCRITERION = ".01"),
                           .warnings2messages = TRUE)
    res2 <- fit_tree_mplus(data = dat_1[names(dat_1) %in% names(model2$j_names)][rx],
                           model = model2,
                           file_name = basename(tempfile()), dir = tempdir(),
                           run = TRUE, integration_points = 7,
                           analysis_list = list(LOGCRITERION = ".01"),
                           .warnings2messages = TRUE)
    sum1 <- extract_mplus_output(res1$mplus, model1)
    sum2 <- extract_mplus_output(res2$mplus, model2)

    tmp1 <- sum(abs(sum1$item$beta[, 2:3] - sum2$item$beta[, 2:3]))
    tmp2 <- sum(abs(sum1$item$beta[, 2:3] - sum2$item$beta[, 3:2]))

    expect_s3_class(res1$mplus, "mplus.model")
    expect_s3_class(res2$mplus, "mplus.model")
    expect_equal(sum1$summaries$AIC, sum2$summaries$AIC, tolerance = .1)
    if (tmp1 < tmp2) {
        expect_equal(sum1$item$beta[, 2:3], sum2$item$beta[, 2:3], tolerance = .01,
                     check.attributes = FALSE)
    } else {
        expect_equal(sum1$item$beta[, 2:3], sum2$item$beta[, 3:2], tolerance = .01,
                     check.attributes = FALSE)
    }
})

# Deprecated --------------------------------------------------------------

# m5c <- "
#
# # Model with 2 Processes 0-1-1 and NA-0-1
# # Model with 4 LVs with constraints, such that processes are identical within
# # Do-items and within Want-items
#
# IRT:
# WeakWant   BY S1WantCurse, S1WantScold, S1WantShout, S2WantCurse, S2WantScold, S2WantShout, S3WantCurse, S3WantScold, S3WantShout, S4wantCurse, S4WantScold, S4WantShout;
# StrongWant BY S1WantCurse, S1WantScold, S1WantShout, S2WantCurse, S2WantScold, S2WantShout, S3WantCurse, S3WantScold, S3WantShout, S4wantCurse, S4WantScold, S4WantShout;
#
# WeakDO     BY S1DoCurse, S1DoScold, S1DoShout, S2DoCurse, S2DoScold, S2DoShout, S3DoCurse, S3DoScold, S3DoShout, S4DoCurse, S4DoScold, S4DoShout;
# StrongDo   BY S1DoCurse, S1DoScold, S1DoShout, S2DoCurse, S2DoScold, S2DoShout, S3DoCurse, S3DoScold, S3DoShout, S4DoCurse, S4DoScold, S4DoShout;
#
# Equations:
# 1 = (1- Weak)
# 2 = Weak * (1  - Strong)
# 3 = Weak * Strong
#
# Subtree:
# Strong = StrongWant + StrongDo
# Weak = WeakWant + WeakDO
#
# Constraints:
# WeakWant = StrongWant
# WeakDO = StrongDo
#
# Class:
# Tree
# "
