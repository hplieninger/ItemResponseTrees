Getting started with IR-trees
================

[![Build
Status](https://travis-ci.org/hplieninger/ItemResponseTrees.svg?branch=master)](https://travis-ci.org/hplieninger/ItemResponseTrees)
[![codecov](https://codecov.io/gh/hplieninger/ItemResponseTrees/branch/master/graph/badge.svg)](https://codecov.io/gh/hplieninger/ItemResponseTrees)

## ItemResponseTrees

ItemResponseTrees is an R package that allows to fit IR-tree models in
Mplus, mirt, or TAM. If you’re unfamiliar with IR-trees, the paper of
[Böckenholt (2012)](https://dx.doi.org/10.1037/a0028111) is a good
starting point. If you’re familiar with the class of IR-tree models,
this tutorial will get you started. The package automates some of the
hassle of IR-tree modeling by means of a consistent syntax. This allows
new users to quickly adopt this model class, and this allows experienced
users to fit many complex models effortlessly.

The package can be installed as follows:

``` r
remotes::install_github("hplieninger/ItemResponseTrees")
```

## Example Data

Herein, an illustrative example will be shown using a popular IR-tree
model for 5-point items (Böckenholt, 2012). The model is applied to a
Big Five data set from the sirt package, more precisely to eight
extraversion items.

``` r
library("ItemResponseTrees")
library("dplyr")
```

``` r
data("data.big5.qgraph", package = "sirt")

df0 <- as_tibble(data.big5.qgraph)
df1 <- select(df0, starts_with("E")[5:12])
```

## Defining the Model

The model is defined by the following tree diagram. The model equations
can be directly derived from the diagram: The probability for a specific
category is given by multiplying all parameters along the branch that
lead to that category (see also Böckenholt, 2012; Plieninger & Heck,
2018). For example, the branch leading to Category 5 is comprised of the
parameters (1-m), t, and e. The resulting five equations are shown in
the right part of the figure.

<img src="tools/ecn-model.png" width="80%" style="border:0px;display: block;  margin-left: auto; margin-right: auto;" />

In the ItemResponseTrees package, a model is defined using a specific
syntax that consists mainly of three parts.

1.  `Equations:` Herein, the model equation for each response category
    is listed in the format `cat = p1 * (1-p2)`, where `cat` is one of
    the observed responses (e.g., 1, …, 5). Furthermore, `p1` is a
    freely chosen parameter label, and I’ve chosen `t`, `e`, and `m`
    below corresponding to the diagram above.
2.  `IRT:` The parameters in the `Equations` (and also those in the
    figure above) actually correspond to latent variables of an IRT
    model. These latent variables are measured using a number of
    items/variables, and this is specified in this section using the
    same parameter labels as in `Equations`.  
    The format for this section is highly similar to the MODEL statement
    in Mplus: a semicolon is used after each definition; loadings
    (discrimination parameters) can be fixed using `@`. The syntax below
    fixes all loadings corresponding to dimensions *e* and *m* to 1
    corresponding to a 1PL or Rasch model, whereas all loadings
    corresponding to dimension *t* are freely estimated (i.e.,
    2PL-structure).
3.  `Class:` Can be either `Tree` for an IR-tree model or `GRM` for a
    graded response model.

The function `irtree_create_template()` may be used to create a
model-string template, which can be copy-pasted and modified.

``` r
# irtree_create_template(names(df1))

m1 <- "
# IR-tree model for 5-point items (Böckenholt, 2012)

IRT:
t  BY E22,   E27,   E32,   E37,   E42,   E47,   E52,   E57;
e  BY E22@1, E27@1, E32@1, E37@1, E42@1, E47@1, E52@1, E57@1;
m  BY E22@1, E27@1, E32@1, E37@1, E42@1, E47@1, E52@1, E57@1;

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Class:
Tree
"
```

In case of a graded response model, only two sections need to be
specified.

``` r
m2 <- "
# Graded response model

IRT:
t  BY E22,   E27,   E32,   E37,   E42,   E47,   E52,   E57;

Class:
GRM
"
```

Further information about the model syntax is provided in
`?irtree_model()`.

## Fitting the model

The ItemResponseTrees package provides wrapper functions for Mplus (via
the
[MplusAutomation](https://cran.r-project.org/package=MplusAutomation)
package), for the [mirt](https://cran.r-project.org/package=mirt)
package, and for the [TAM](https://cran.r-project.org/package=TAM)
package. To fit a model, the model string as defined above has to be
converted into an object of class `irtree_model` using the function
`irtree_model()`. Then, the model can be `fit()` as follows:

``` r
model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

# Using 8 quadrature points to increase speed; see ?mirt::mirt for details
fit1 <- fit(model1, data = df1, engine = "mirt", quadpts = 8)
fit2 <- fit(model2, data = df1, engine = "mirt", quadpts = 8)
```

## Results

### Model Fit

Information about model fit is obtained via `glance()`. As seen below,
the IR-tree model has 37 freely estimated parameters (3 x 8 thresholds +
7 loadings + 3 variances + 3 covariances). The GRM has 40 estimated
parameters (4 x 8 thresholds + 8 loadings). (Of course, this comparison
is a little bit unfair, because the IR-tree model is much more flexible
in terms of dimensionality/“random effects” even though it is less
flexible with respect to the thresholds/“fixed effects”.)

For the present data, the IR-tree model outperforms the GRM according to
AIC and BIC, and thus one may conclude that response styles are present
in these data.

``` r
glance(fit1)
#> # A tibble: 1 x 11
#>     AIC    BIC  AICc logLik converged iterations estimator  npar  nobs n.factors
#>   <dbl>  <dbl> <dbl>  <dbl> <lgl>          <int> <chr>     <int> <int>     <int>
#> 1 9911. 10066. 9917. -4918. TRUE              41 EM           37   500         3
#> # ... with 1 more variable: ngroups <int>

list("tree" = fit1, "grm" = fit2) %>% 
    purrr::map_dfr(glance, .id = "Model")
#> # A tibble: 2 x 12
#>   Model    AIC    BIC   AICc logLik converged iterations estimator  npar  nobs
#>   <chr>  <dbl>  <dbl>  <dbl>  <dbl> <lgl>          <int> <chr>     <int> <int>
#> 1 tree   9911. 10066.  9917. -4918. TRUE              41 EM           37   500
#> 2 grm   10004. 10172. 10011. -4962. TRUE              30 EM           40   500
#> # ... with 2 more variables: n.factors <int>, ngroups <int>
```

### Parameter Estimates

The parameter estimates are obtained via `tidy()`. For the IR-tree
model, this returns a tibble with 60 rows containing the 40 free
parameters, 17 fixed loadings, and 3 correlations (in addition to the 3
covariances for better interpretation). Below, the
loading/discrimination parameters pertaining to parameter *t* are shown,
which vary between 0.18 and 6.78. The loadings pertaining to parameter
*e* and *m* were fixed to one and thus no standard error is returned for
these. The latent correlations are shown below as well, and these show
the typical pattern of a negative correlation between *e* and *m*.

``` r
tidy(fit1, difficulty = TRUE)
#> # A tibble: 60 x 4
#>    effect term     estimate std.error
#>    <chr>  <chr>       <dbl>     <dbl>
#>  1 fixed  t_E22.a1    0.468     0.202
#>  2 fixed  t_E27.a1    1.07      0.319
#>  3 fixed  t_E32.a1    1.34      0.430
#>  4 fixed  t_E37.a1    6.78     40.4  
#>  5 fixed  t_E42.a1    0.687     0.200
#>  6 fixed  t_E47.a1    0.802     0.234
#>  7 fixed  t_E52.a1    0.184     0.177
#>  8 fixed  t_E57.a1    1.23      0.386
#>  9 fixed  e_E22.a2    1        NA    
#> 10 fixed  e_E27.a2    1        NA    
#> # ... with 50 more rows
tail(tidy(fit1, difficulty = TRUE), 3)
#> # A tibble: 3 x 4
#>   effect   term    estimate std.error
#>   <chr>    <chr>      <dbl>     <dbl>
#> 1 ran_pars COR_t.e   0.336         NA
#> 2 ran_pars COR_t.m  -0.0840        NA
#> 3 ran_pars COR_e.m  -0.321         NA
```

### Factor scores

The factor scores or person parameter estimates are obtained via
`augment()`. This returns a tibble comprised of the data set and the
factor scores (plus respective standard errors) for the three dimensions
*t* (F1), *e* (F2), and *m* (F3).\[1\]

The correlation of the scores for the target trait (extraversion in this
case) between the IR-tree model and the GRM is considerably lower than
1.

``` r
augment(fit1)
#> # A tibble: 500 x 14
#>      E22   E27   E32   E37   E42   E47   E52   E57 .fittedF1 .fittedF2 .fittedF3
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>     <dbl>     <dbl>
#>  1     4     5     5     5     2     5     5     5    0.618     1.85      -0.964
#>  2     4     4     3     3     3     2     3     4   -0.416    -1.29       1.30 
#>  3     5     4     4     5     2     3     4     5    0.269     0.444     -0.186
#>  4     4     5     5     3     4     2     4     4    0.153     0.0242    -0.123
#>  5     4     2     5     4     3     2     2     5   -0.426    -0.116     -0.110
#>  6     3     4     4     4     2     3     4     4   -0.0482   -1.30       0.518
#>  7     5     5     5     5     5     4     3     5    1.12      2.24      -0.462
#>  8     2     4     4     3     4     3     5     4    0.102    -0.598      0.426
#>  9     3     5     5     4     4     3     4     5    0.645     0.519      0.280
#> 10     3     5     4     3     4     3     5     5    0.624     0.616      0.692
#> # ... with 490 more rows, and 3 more variables: .se.fitF1 <dbl>,
#> #   .se.fitF2 <dbl>, .se.fitF3 <dbl>

# augment(fit2)

cor(augment(fit1)$.fittedF1, augment(fit2)$.fittedF1)
#> [1] 0.758345
```

1.  The order corresponds to the order of appearance in the section
    `IRT` of the model string.
