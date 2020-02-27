\donttest{
m1 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1, X5@1, X6@1, X7@1, X8@1, X9@1, X10@1;
b BY x1@1, x2@1, x3@1, x4@1, X5@1, X6@1, X7@1, X8@1, X9@1, X10@1;

Equations:
1 = 1-a
2 = a*(1-b)
3 = a*b

Class:
Tree
"

m2 <- "
IRT:
a BY x1@1, x2@1, x3@1, x4@1, X5@1, X6@1, X7@1, X8@1, X9@1, X10@1;

Class:
GRM
"

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

res1 <- irtree_sim(
    ### Data generation ###
    gen_model = model1,
    N = 500,
    link = "logit",
    sigma = function(x) diag(2),
    itempar = function(x) list(
        beta = matrix(sort(runif(model1$J*model1$P, -2, 2)),
                      model1$J, model1$P),
        alpha = matrix(1, model1$J, model1$P)),
    na_okay = FALSE,

    ### Estimation ###
    fit_model = list(model1, model2),
    engine = "mirt",
    dots = list(fit = list(SE = FALSE),
                tidy = list(difficulty = TRUE)),

    ### Replications ###
    R = 2,
    save_rdata = FALSE,

    ### Parallelization ###
    plan = "multiprocess",
    future_args = list(workers = 2)
)

library("tidyr")
library("dplyr")
library("tibble")

enframe(res1, "repl", "res") %>%
    hoist(res, m1 = list("fits", "m1", "glanced"), m2 = list("fits", "m2", "glanced")) %>%
    pivot_longer(cols = c(m1, m2), names_to = "model", values_to = "glanced") %>%
    unnest_wider(col = glanced) %>%
    select(repl, model, AIC, BIC)
#> # A tibble: 4 x 4
#>   repl  model   AIC   BIC
#>   <chr> <chr> <dbl> <dbl>
#> 1 sim1  m1    9607. 9704.
#> 2 sim1  m2    9773. 9866.
#> 3 sim2  m1    9507. 9603.
#> 4 sim2  m2    9676. 9768.
}
