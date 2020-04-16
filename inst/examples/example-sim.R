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
    save_rdata = FALSE,

    ### Optional parallelization ###
    plan = "multiprocess",
    plan_args = list(workers = future::availableCores() - 1)
)

tab1 <- matrix(NA, 0, 4, dimnames = list(NULL, c("Rep", "Model", "AIC", "BIC")))

for (ii in seq_along(res)) {
    for (jj in seq_along(res[[ii]]$fits)) {
        IC <- res[[ii]]$fits[[jj]]$glanced
        tab1 <- rbind(tab1, c(ii, jj, round(IC$AIC, -1), round(IC$BIC, -1)))
    }
}
tab1
#>      Rep Model  AIC  BIC
#> [1,]   1     1 6900 6970
#> [2,]   1     2 7000 7060
#> [3,]   2     1 6810 6880
#> [4,]   2     2 6880 6940
}
