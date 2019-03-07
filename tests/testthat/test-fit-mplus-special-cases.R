context("test-fit-mplus-special-cases")

skip_if_not_installed("psych", minimum_version = NULL)

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

res1 <- fit_tree_mplus(data = dat_1,
                      model = model,
                      file_name = basename(file1),
                      dir = dirname(file1),
                      run = T,
                      integration_points = "MONTECARLO(500)",
                      showOutput = FALSE,
                      analysis_list = list(
                          LOGCRITERION = ".1",
                          RLOGCRITERION = ".01",
                          MCONVERGENCE = ".1",
                          CONVERGENCE = ".01"),
                      .warnings2messages = TRUE)


test_that("fit_tree_mplus() works for Tree", {
    # expect_is(res1$mplus, "list")
    expect_s3_class(res1$mplus, "mplus.model")
})

test_that("Items correctly exported to Mplus", {
    expect_equal(res1$mplus$sampstat$proportions.counts[order(counts$variable, counts$category), names(counts)],
                 counts[order(counts$variable, counts$category), ],
                 check.attributes = FALSE)
})

summ1 <- extract_mplus_output(res1$mplus, m1)


# Variable names with _ ---------------------------------------------------

dat_2 <- dat_1
names(dat_2) <- sub("([A-C])", "\\1_", names(dat_2))
names(dat_2) <- sub("4", "11", names(dat_2))
names(dat_2) <- sub("5", "12", names(dat_2))

# IR-Tree -----------------------------------------------------------------

dat_3 <- dat_bfi[sample(nrow(bfi), 200), 1:15] %>%
    dplyr::mutate_all(car::recode, recodes = "4=3; 5=4; 6=5")

set.seed(123)
names(dat_3) <- sample(paste0("itm_", 1:15))

# paste0(names(dat_3)[1:5], collapse = ", ") %>% cat
# paste0(names(dat_3)[6:10], collapse = ", ") %>% cat
# paste0(names(dat_3)[11:15], collapse = ", ") %>% cat
# paste0("itm_", 1:15, "@1", collapse = ", ") %>% cat
# paste0("itm_", 1:15, collapse = ", ") %>% cat
# paste0(names(dat_3), collapse = ", ") %>% cat

m3 <- "
IRT:
fac_a BY itm_5@1, itm_12, itm_6, itm_11, itm_14;
fac_c BY itm_1@1, itm_15, itm_8, itm_4, itm_3;
fac_e BY itm_9@1, itm_2, itm_13, itm_7, itm_10;
e     BY itm_1@1, itm_2@1, itm_3@1, itm_4@1, itm_5@1, itm_6@1, itm_7@1, itm_8@1, itm_9@1, itm_10@1, itm_11@1, itm_12@1, itm_13@1, itm_14@1, itm_15@1;
m     BY itm_1@1, itm_2@1, itm_3@1, itm_4@1, itm_5@1, itm_6@1, itm_7@1, itm_8@1, itm_9@1, itm_10@1, itm_11@1, itm_12@1, itm_13@1, itm_14@1, itm_15@1;

Subtree:
t = fac_a + fac_c + fac_e

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Class:
Tree
"


model3 <- tree_model(m3)

test_that("tree_model() works", {
    expect_s3_class(model3, "tree_model")
})

# Long variable names -----------------------------------------------------

m4 <- "
# Comment
IRT:
NodeProb1 BY Variable1@1, Variable2@1, Variable3@1, Variable10@1, Variable101@1;
NodeProb11 BY Variable1@1, Variable2@1, Variable3@1;
NodeProb12 BY Variable10@1, Variable101@1;

Equations:
1 = (1-NodeProb1)
2 = NodeProb1*(1-NodeProb10)
3 = NodeProb1*NodeProb10

Subtree:
NodeProb10 = NodeProb11 + NodeProb12

Class:
Tree

Addendum:
NodeProb1 WITH NodeProb11@0;
NodeProb1 WITH y1;
"

model4 <- tree_model(m4)

test_that("tree_model() works", {
    expect_s3_class(model4, "tree_model")
})
