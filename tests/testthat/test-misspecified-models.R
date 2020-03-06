
# Typos in Model String ---------------------------------------------------

models <- list()

("ITR:
a BY X1, X2;
Equations:
1 = (1-a)
2 = (a)
Class:
TREE") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2;
GRM:
1 = (1-a)
2 = (a)
Class:
GRM") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2;

1 = (1-a)
2 = (a)
Class:
TREE") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2;
Equations:
1 = (1-a)
2 = (a)
Class:
TREE
cLass:") %>%
    append(x = models) -> models

("IRT:
a BY X1 X2;
Equations:
1 = (1-a)
2 = (a)
Class:
TREE") %>%
    append(x = models) -> models

("IRT:
a bye X1, X2;
Equations:
1 = (1-a)
2 = (a)
Class:
TREE") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2
b BY X1, X2;
Equations:
1 = (1-a)
2 = (a)*(1-b)
3 = a*b
Class:
Tree") %>%
    append(x = models) -> models

("
IRT:
a BY X1, X2;
b BY X1, X2;

Equations:
1 = (1-a)
2 = (a)*(1-b)
3 = a*bb

Class:
Tree
")%>%
    append(x = models) -> models

test_that("Malformed equation throws an error", {
    for (ii in seq_along(models)) {
        expect_error(irtree_model(models[[!!ii]]),
                     "Problem in 'model'")
    }
})

models <-
    ("IRT:
a BY X1, X2;
Equations:
1 = (1-a)
2 = (a)
Class:
tre")

test_that("Typo in class", {
    for (ii in seq_along(models)) {
        expect_error(irtree_model(models[[!!ii]]),
                     "Assertion on 'Class' failed")
    }
})

models <-
("IRT:
a BY x-1, X2;
Equations:
1 = (1-a)
2 = (a)
Class:
TREE")


models <- append(models,
("IRT:
a.1 BY X1, X2;
b BY X1, X2;
Equations:
1 = (1-a.1)
2 = (a.1)*(1-b)
3 = a.1*b
Class:
Tree"))

test_that("Illegal variable names", {
    for (ii in seq_along(models)) {
        expect_error(irtree_model(models[[!!ii]]),
                     "Must comply to pattern")
    }
})

# Typos etc. in Equation --------------------------------------------------

models <- list()

("IRT:
a BY X1, X2;
b BY X1, X2;
c BY X1, X2;

Equations:
# improper, illegal character
1 = (1-a)
2 = (a)*(1-b)
3 = c^2

Class:
Tree") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2;
b BY X1, X2;

Equations:
# improper even though sum to 1
# minus not preceeded by '1'
1 = a + b
2 = 1 - a - b

Class:
Tree") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2;
b BY X1, X2;
c BY X1, X2;

Equations:
# forgotten multiplication
1 = a
2 = (1-a)b
3 = (1-a)(1-b)

Class:
Tree") %>%
    append(x = models) -> models

("IRT:
a BY X1, X2;
b BY X1, X2;

Equations:
# improper: minus not preceeded by '1'
1 = a
2 = -a

Class:
Tree") %>%
    append(x = models) -> models

# The following model is in principle a valid IR-tree model. However, generating
# the pseudoitems is impossible, because actually three pseudoitems are needed
# even though only two parameters are specified.
# Estimating such a model is possible if specified differently, namely, using
# constraints.
("IRT:
t  BY X1@1, x2@1, X3@1, X4@1, X5@1, X6@1;
e  BY X1@1, x2@1, X3@1, X4@1, X5@1, X6@1;

Equations:
# used 'e' twice instead of 'e' and 'm'
1 = (1-e)*(1-t)*e
2 = (1-e)*(1-t)*(1-e)
3 = e
4 = (1-e)*t*(1-e)
5 = (1-e)*t*e

Class:
Tree") %>%
    append(x = models) -> models

test_that("Malformed equation throws an error", {
    for (ii in seq_along(models)) {
        expect_error(irtree_model(models[[!!ii]]),
                     "Each model equation may either contain")
    }
})

# Mismatch of categories between model and data ---------------------------

models <- "
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
models <- irtree_model(models)

dat <- setNames(data.frame(matrix(1:4, 10, 6)), paste0("x", 1:6))

test_that("Mismatch of categories between model and data throws error", {
    expect_error(irtree_fit_mirt(models, dat), "has equations for categories")
})


# Mixture models ----------------------------------------------------------

m_mixture <- "
IRT:
a BY X1@1, X2@1;
b BY X1@1, X2@1;
c BY X1@1, X2@1;

Equations:
# proper but mixture
# fail in fit
1 = c + (1-c)*a
2 = (1-c)*(1-a)*b
3 = (1-c)*(1-a)*(1-b)

Class:
Tree"

test_that("Proper mixture model fails in fit()", {
    expect_message(model1 <- irtree_model(m_mixture), "mixture model")
    expect_s3_class(model1, "irtree_model")

    d1 <- irtree_gen_data(
        model1, N = 100, sigma = diag(3),
        itempar = list(beta = matrix(0, 2, 3),
                       alpha = matrix(1, 2, 3)))
    expect_error(fit(model1, d1$data, "mirt"), "mixture model")
    expect_error(fit(model1, d1$data, "mplus",
                     control = control_mplus(run = FALSE)), "mixture model")
    expect_error(fit(model1, d1$data, "tam"), "mixture model")
})

# Improper Model ----------------------------------------------------------

m1 <- "
IRT:
t1 BY X1@1, X2@1, X3@1;
T2 BY X4@1, X5@1, X6@1;
e  BY X1@1, X2@1, X3@1, X4@1, X5@1,
 X6@1;
m  BY X1@1, X2@1, X3@1, X4@1, X5@1,
X6@1;

Constraints:
t = t1 | T2

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m*t
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Class:
Tree"

test_that("Improper MPT equation throws warning (and error in fit)", {
    df1 <- as.data.frame(replicate(6, sample(5, 10, T), F),
                         col.names = paste0("X", 1:6))

    expect_warning(model1 <- irtree_model(m1),
                   "Equations do not constitute a proper model")
    expect_error(fit(model1, df1, engine = "mirt"), "improper model")
    expect_error(fit(model1, df1, engine = "tam"), "improper model")
    expect_error(fit(model1, df1, engine = "mplus",
                     control = control_mplus(run = FALSE)), "improper model")

})

# PCM ---------------------------------------------------------------------

m6a <- "
IRT:
a BY X1, X2;
b BY X1, X2;

Weights:
a = 1:2
b = 1:3

Class:
PCM
"

m6b <- "
IRT:
a BY X1, X2;

Weights:
a = 0:2

Class:
PCM
"

m6c <- "
IRT:
a BY X1, X2;

Weights:
a = 0:3

Class:
PCM
"

m6d <- "
IRT:
a BY X1, X2;

Weights:
b = 0:3

Class:
PCM
"

dat6b  <- setNames(data.frame(matrix(1:4, 6, 2)), paste0("X", 1:2))
dat6c  <- setNames(data.frame(matrix(1:4, 6, 2)), paste0("X", 1:2))
dat6c2 <- setNames(data.frame(matrix(0:4, 5, 2)), paste0("X", 1:2))

test_that("Misspecified PCM model syntax throw errors", {
    expect_error(irtree_model(m6a), "Assertion on 'weights'")
    expect_error(fit(irtree_model(m6b), dat6b,  engine = "tam"),
                 "Minimum of data is not equal to zero")
    expect_error(fit(irtree_model(m6c), dat6c,  engine = "tam"),
                 "Minimum of data is not equal to zero")
    expect_error(fit(irtree_model(m6c), dat6c2, engine = "tam"),
                 "'data' has categories")
    expect_error(irtree_model(m6d), "Problem in 'model'")
})
