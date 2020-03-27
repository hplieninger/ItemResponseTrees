# IR-Tree Model -----------------------------------------------------------

m1 <- "
Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

IRT:
t BY x1, x2, x3;
e BY x1, x2, x3;
m BY x1, x2, x3;

Class:
Tree
"

model1 <- irtree_model(m1)

dat1 <- irtree_gen_data(model1, N = 5, sigma = diag(3),
                        itempar = list(beta = matrix(rnorm(9), 3, 3),
                                       alpha = matrix(1, 3, 3)))
dat1$data

# Partial Credit Model ----------------------------------------------------

m2 <- "
IRT:
t BY x1@1, x2@1, x3@1;
e BY x1@1, x2@1, x3@1;
m BY x1@1, x2@1, x3@1;

Weights:
t = c(0, 1, 2, 3, 4)
e = c(1, 0, 0, 0, 1)
m = c(0, 0, 1, 0, 0)

Class:
PCM
"
model2 <- irtree_model(m2)
dat2 <- irtree_gen_data(model2, N = 5, sigma = diag(3),
                       itempar = list(beta = matrix(sort(rnorm(12)), 3, 4)))
dat2$data

m3 <- "
IRT:
t BY x1@1, x2@1, x3@1;

Weights:
t = 0:4

Class:
PCM
"

model3 <- irtree_model(m3)

dat3 <- irtree_gen_data(model3, N = 5, sigma = diag(1),
                       itempar = list(beta = matrix(sort(rnorm(12)), 3, 4)))
dat3$data
