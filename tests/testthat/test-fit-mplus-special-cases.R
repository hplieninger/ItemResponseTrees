context("test-fit-mplus-special-cases")

dat_bfi <- get(data(bfi, package = "psych"))

keys.list <-
    list(agree=c("-A1","A2","A3","A4","A5"),conscientious=c("C1","C2","C3","-C4","-C5"),
         extraversion=c("-E1","-E2","E3","E4","E5"),neuroticism=c("N1","N2","N3","N4","N5"),
         openness = c("O1","-O2","O3","O4","-O5"))

tmp1 <- sub("-", "", unlist(keys.list)[grep("^\\-", unlist(keys.list))])

dat_bfi[, tmp1] <- 7 - dat_bfi[, tmp1]

# set.seed(123)
dat_1 <- dat_bfi[sample(nrow(bfi), 200), sample(names(dat_bfi[, 1:10]))]

m1 <- "
IRT:
fac_a BY A1@1, A2, A3, A4, A5;
fac_c BY C1@1, C2, C3, C4, C5;

Items:
A1, A2, A3, A4, A5, C1, C2, C3, C4, C5

# Equations:
# 1 = (1-a)
# 2 = a*(1-b)
# 3 = a*b

Processes:
fac_a, fac_c

Class:
GRM
"

model <- tree_model(m1)

test_that("tree_model() works", {
    expect_s3_class(model, "tree_model")
})

file1 <- tempfile()
dirname(file1)

counts <- dat_1 %>%
    purrr::map_dfr(~table(factor(., levels = 1:6))) %>%
    dplyr::mutate(category = 1:6) %>%
    reshape2::melt(value.name = "count", id.vars = "category")
counts$variable <- as.character(counts$variable)


res <- fit_tree_mplus(data = dat_1,
                      model = model,
                      file_name = basename(file1),
                      dir = dirname(file1),
                      run = T,
                      integration_points = 7,
                      showOutput = TRUE,
                      analysis_list = list(LOGCRITERION = ".1",
                                           RLOGCRITERION = ".0001",
                                           MCONVERGENCE = ".1",
                                           CONVERGENCE = ".0001"),
                      .warnings2messages = TRUE)

test_that("fit_tree_mplus() works for Tree", {
    # expect_is(res$mplus, "list")
    expect_s3_class(res$mplus, "mplus.model")
})

test_that("Items correctly exproted to Mplus", {
    expect_equal(res$mplus$sampstat$proportions.counts[order(counts$variable, counts$category), names(counts)],
                 counts[order(counts$variable, counts$category), ],
                 check.attributes = FALSE)
})


# Variable names with _ ---------------------------------------------------

dat_2 <- dat_1
names(dat_2) <- sub("([A-C])", "\\1_", names(dat_2))
names(dat_2) <- sub("4", "11", names(dat_2))
names(dat_2) <- sub("5", "12", names(dat_2))

# m2 <- "
# IRT:
# fac_a BY A_1@1, A_2, A_3, A_11, A_12;
# fac_c BY C_1@1, C_2, C_3, C_11, C_12;
#
# Items:
# A_1, A_2, A_3, A_11, A_12, C_1, C_2, C_3, C_11, C_12
#
# # Equations:
# # 1 = (1-a)
# # 2 = a*(1-b)
# # 3 = a*b
#
# Processes:
# fac_a, fac_c
#
# Class:
# GRM
# "
#
# model2 <- tree_model(m2)
#
# test_that("tree_model() works", {
#     expect_s3_class(model2, "tree_model")
# })
#
# file2 <- tempfile()
#
# counts2 <- dat_2 %>%
#     purrr::map_dfr(~table(factor(., levels = 1:6))) %>%
#     dplyr::mutate(category = 1:6) %>%
#     reshape2::melt(value.name = "count", id.vars = "category")
# counts2$variable <- as.character(counts2$variable)
#
#
# res2 <- fit_tree_mplus(data = dat_2,
#                       model = model2,
#                       file_name = basename(file2),
#                       dir = dirname(file2),
#                       run = T,
#                       integration_points = 7,
#                       showOutput = TRUE,
#                       analysis_list = list(LOGCRITERION = ".1",
#                                            RLOGCRITERION = ".0001",
#                                            MCONVERGENCE = ".1",
#                                            CONVERGENCE = ".0001"),
#                       .warnings2messages = TRUE)
#
# test_that("fit_tree_mplus() works for Tree", {
#     # expect_is(res$mplus, "list")
#     expect_s3_class(res2$mplus, "mplus.model")
# })
#
# test_that("Items correctly exproted to Mplus", {
#     expect_equal(res2$mplus$sampstat$proportions.counts[order(counts2$variable, counts$category), names(counts)],
#                  counts2[order(counts2$variable, counts$category), ],
#                  check.attributes = FALSE)
# })

# IR-Tree -----------------------------------------------------------------

dat_3 <- dplyr::mutate_all(dat_2, car::recode, recodes = "4=3; 5=4; 6=5")

m3 <- "
IRT:
fac_a BY A_1@1, A_2, A_3, A_11, A_12;
fac_c BY C_1@1, C_2, C_3, C_11, C_12;
e     BY A_3@1, C_3@1, A_11@1, C_2@1, C_1@1, A_1@1, C_12@1, C_11@1, A_2@1, A_12@1;
m     BY A_3@1, C_3@1, A_11@1, C_2@1, C_1@1, A_1@1, C_12@1, C_11@1, A_2@1, A_12@1;

Subtree:
t = fac_a + fac_c

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Processes:
m, e, fac_a, fac_c

Items:
A_1, A_2, A_3, A_11, A_12, C_1, C_2, C_3, C_11, C_12

Class:
Tree
"


model3 <- tree_model(m3)

test_that("tree_model() works", {
    expect_s3_class(model3, "tree_model")
})

file3 <- tempfile()

counts3 <- dat_3 %>%
    purrr::map_dfr(~table(factor(., levels = 1:6))) %>%
    dplyr::mutate(category = 1:6) %>%
    reshape2::melt(value.name = "count", id.vars = "category")
counts3$variable <- as.character(counts3$variable)


res3 <- fit_tree_mplus(data = dat_3,
                       model = model3,
                       file_name = basename(file3),
                       dir = dirname(file3),
                       run = T,
                       integration_points = 7,
                       showOutput = TRUE,
                       analysis_list = list(LOGCRITERION = ".1",
                                            RLOGCRITERION = ".0001",
                                            MCONVERGENCE = ".1",
                                            CONVERGENCE = ".0001"),
                       .warnings2messages = TRUE)

test_that("fit_tree_mplus() works for Tree", {
    # expect_is(res$mplus, "list")
    expect_s3_class(res3$mplus, "mplus.model")
})

test_that("Items correctly exproted to Mplus", {
    expect_equal(res3$mplus$sampstat$proportions.counts[order(counts3$variable, counts$category), names(counts)],
                 counts3[order(counts3$variable, counts$category), ],
                 check.attributes = FALSE)
})
