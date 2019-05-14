m1 <- "
IRT:
t1 BY X1@1, x2@1, X3@1;
T2 BY X4@1, X5@1, X6@1;
e  BY X1@1, x2@1, X3@1, X4@1, X5@1,
 X6@1;
m  BY X1@1, x2@1, X3@1, X4@1, X5@1,
X6@1;

Subtree:
t = t1 + T2

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m*t*e
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Class:
Tree

"

test_that("Improper MPT equation gives warning", {
  expect_warning(tree_model(m1))
})

m2 <- "
IRT:
process_t BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
process_e  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;

Equations:
1 = (1-process_e)
2 = process_e*(1-process_t)
3 = process_e*process_t

Class:
Tree
"

dat <- setNames(data.frame(matrix(1:4, 10, 6)), paste0("x", 1:6))

test_that("Mismatch of categories between model and data throws error", {
    expect_error(fit_tree_mirt(dat, m2))
})

m3 <- "
IRT:
a BY X1, X2;
b BY X1, X2;

Equations:
1 = (1-a)
2 = (a)*(1-b)
3 = a*bb

Class:
Tree
"

test_that("Mismatch of names between IRT and Equations throws error", {
    expect_error(tree_model(m3))
})

m4 <- "
IRT:
a BY X1, X2;
Equations:
1 = (1-a)
2 = (a)
Class:
TREE
"

l4 <- vector("list", length = 0)

l4 <- c(l4, sub("IRT:", "IRT", m4))
l4 <- c(l4, sub("IRT:", "ITR:", m4))
l4 <- c(l4, gsub("Equations|TREE", "GRM", m4))
l4 <- c(l4, gsub("Equations:", "", m4))
l4 <- c(l4, gsub("TREE", "tre", m4))
l4 <- c(l4, paste0(m4, "cLass:", collapse = "\n"))
l4 <- c(l4, gsub("X1", "x-1", m4))
l4 <- c(l4, gsub("X1, X2", "X1 X2", m4))
l4 <- c(l4, gsub("BY", "bye", m4))
l4 <- c(l4, gsub("BY", "bye", m4))
l4 <- c(l4, sub("IRT:", "ITR", m4))

l4 <- c(l4, "
IRT:
a BY X1, X2
b BY X1, X2;
Equations:
1 = (1-a)
2 = (a)*(1-b)
3 = a*b
Class:
Tree
")

test_that("Misspecified model throws error", {
    for (ii in seq_along(l4)) {
        expect_error(tree_model(l4[[ii]]), info = paste0("Problem in l4[[", ii, "]]."))
    }
})

