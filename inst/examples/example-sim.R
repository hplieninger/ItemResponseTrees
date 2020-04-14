\donttest{
# Running these examples may take a while

m1 <- "
Equations:
1 = 1-a
2 = a*(1-b)
3 = a*b

IRT:
a BY x1@1, x2@1, x3@1, x4@1, X5@1, X6@1, X7@1;
b BY x1@1, x2@1, x3@1, x4@1, X5@1, X6@1, X7@1;

Class:
Tree
"

m2 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1, X5@1, X6@1, X7@1;

Class:
GRM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

set.seed(2413)
res <- irtree_sim(
    ### Data generation ###
    gen_model = model1,
    link = "logit",
    N = 500,
    sigma = function(x) diag(2),
    itempar = function(x) list(
        beta = matrix(sort(runif(model1$J*model1$P, -2, 2)),
                      model1$J, model1$P),
        alpha = matrix(1, model1$J, model1$P)),
    na_okay = FALSE,

    ### Estimation ###
    fit_model = list(model1, model2),
    engine = "mirt",
    control = control_mirt(SE = FALSE),
    par_type = "difficulty",

    ### Replications ###
    R = 2,
    save_rdata = FALSE

    ### Optional parallelization ###
    # plan = "multiprocess",
    # plan_args = list(workers = 2)
)

tab1 <- matrix(NA, 0, 4, dimnames = list(NULL, c("iter", "model", "AIC", "BIC")))

for (ii in seq_along(res)) {
    for (jj in seq_along(res[[ii]]$fits)) {
        IC <- res[[ii]]$fits[[jj]]$glanced
        tab1 <- rbind(tab1, c(ii, jj, IC$AIC, IC$BIC))
    }
}
tab1
#>      iter model      AIC      BIC
#> [1,]    1     1 6906.398 6978.046
#> [2,]    1     2 7005.481 7068.700
#> [3,]    2     1 6810.548 6882.196
#> [4,]    2     2 6886.484 6949.703
}
