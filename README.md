Getting started with IR-trees
================

[![R build
status](https://github.com/hplieninger/ItemResponseTrees/workflows/R-CMD-check/badge.svg)](https://github.com/hplieninger/ItemResponseTrees/actions)
[![codecov](https://codecov.io/gh/hplieninger/ItemResponseTrees/branch/master/graph/badge.svg)](https://codecov.io/gh/hplieninger/ItemResponseTrees)
[![Say
Thanks\!](https://img.shields.io/badge/Say%20Thanks-!-1EAEDB.svg)](https://saythanks.io/to/plieninger@uni-mannheim.de)

## ItemResponseTrees

ItemResponseTrees is an R package that allows to fit IR-tree models in
mirt, Mplus, or TAM. If you’re unfamiliar with IR-trees, the paper of
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
Big Five data set from Jackson (2012), more precisely to seven
conscientiousness items.

``` r
library("ItemResponseTrees")
library("dplyr")
```

``` r
data("jackson")
set.seed(1432)

df1 <- select(jackson, paste0("C", 1:7)) %>% 
    sample_n(250)
df1
#> # A tibble: 250 x 7
#>       C1    C2    C3    C4    C5    C6    C7
#>    <int> <dbl> <int> <dbl> <int> <dbl> <int>
#>  1     4     5     5     4     4     5     5
#>  2     3     3     3     5     3     2     3
#>  3     3     1     5     2     3     2     5
#>  4     5     2     4     4     5     4     5
#>  5     4     4     5     4     2     4     4
#>  6     4     3     4     4     2     3     5
#>  7     5     2     4     5     2     3     4
#>  8     3     3     4     2     2     3     3
#>  9     3     3     3     2     4     2     3
#> 10     5     5     5     5     4     5     5
#> # ... with 240 more rows
```

## Defining the Model

The model is defined by the following tree diagram. The model equations
can be directly derived from the diagram: The probability for a specific
category is given by multiplying all parameters along the branch that
lead to that category (see also Böckenholt, 2012; Plieninger & Heck,
2018). For example, the branch leading to Category 5 is comprised of the
parameters (1-*m*), *t*, and *e*. The resulting five equations are shown
in the right part of the figure.

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
t  BY  C1,   C2,   C3,   C4,   C5,   C6,   C7;
e  BY  C1@1, C2@1, C3@1, C4@1, C5@1, C6@1, C7@1;
m  BY  C1@1, C2@1, C3@1, C4@1, C5@1, C6@1, C7@1;

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
t  BY  C1,   C2,   C3,   C4,   C5,   C6,   C7;

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

# The following control parameters are lax but increase speed
ctrl <- control_mirt(quadpts = 8, TOL = .01)

fit1 <- fit(model1, data = df1, engine = "mirt", control = ctrl)
fit2 <- fit(model2, data = df1, engine = "mirt", control = ctrl)
```

## Results

### Model Fit

Information about model fit is obtained via `glance()`. As seen below,
the IR-tree model has 33 freely estimated parameters (3 x 7 thresholds +
7 loadings + 2 variances + 3 covariances). The GRM has 35 estimated
parameters (4 x 7 thresholds + 7 loadings). (Of course, this comparison
is a little bit unfair, because the IR-tree model is much more flexible
in terms of dimensionality/“random effects” even though it is less
flexible with respect to the thresholds/“fixed effects”.)

For the present data, the IR-tree model slightly outperforms the GRM
according to AIC and BIC, and thus one may conclude that response styles
are present in these data.

``` r
glance(fit1)
#> # A tibble: 1 x 11
#>      AIC    BIC   AICc logLik converged iterations estimator  npar  nobs
#>    <dbl>  <dbl>  <dbl>  <dbl> <lgl>          <int> <chr>     <int> <int>
#> 1 13459. 13649. 13469. -6685. TRUE              12 EM           45   500
#> # ... with 2 more variables: n.factors <int>, ngroups <int>

list("tree" = fit1, "grm" = fit2) %>% 
    purrr::map_dfr(glance, .id = "Model")
#> # A tibble: 2 x 12
#>   Model    AIC    BIC   AICc logLik converged iterations estimator  npar  nobs
#>   <chr>  <dbl>  <dbl>  <dbl>  <dbl> <lgl>          <int> <chr>     <int> <int>
#> 1 tree  13459. 13649. 13469. -6685. TRUE              12 EM           45   500
#> 2 grm   13466. 13677. 13477. -6683. TRUE               5 EM           50   500
#> # ... with 2 more variables: n.factors <int>, ngroups <int>
```

### Parameter Estimates

The parameter estimates are obtained via `tidy()`. For the IR-tree
model, this returns a tibble with 54 rows (pertaining to the fixed and
estimated parameters). Below, the seven loading/discrimination
parameters `t_*.a1` pertaining to parameter *t* are shown. The loadings
pertaining to parameter *e* (`e_*.a2`) and *m* (`m_*.a3`) were fixed to
one and thus no standard error is returned for these. The latent
correlations are shown below as well, and these show the typical pattern
of a negative correlation between *e* and *m*.

``` r
tidy(fit1, par_type = "difficulty")
#> # A tibble: 72 x 4
#>    effect term     estimate std.error
#>    <chr>  <chr>       <dbl>     <dbl>
#>  1 fixed  t_E1.a1      1.94     0.231
#>  2 fixed  t_E2.a1      2.08     0.203
#>  3 fixed  t_E3.a1      2.14     0.253
#>  4 fixed  t_E4.a1      2.27     0.216
#>  5 fixed  t_E5.a1      2.39     0.290
#>  6 fixed  t_E6.a1      2.43     0.419
#>  7 fixed  t_E7.a1      2.02     0.194
#>  8 fixed  t_E8.a1      1.62     0.185
#>  9 fixed  t_E9.a1      1.55     0.160
#> 10 fixed  t_E10.a1     2.28     0.274
#> # ... with 62 more rows

tail(tidy(fit1, par_type = "difficulty"), 3)
#> # A tibble: 3 x 4
#>   effect   term    estimate std.error
#>   <chr>    <chr>      <dbl>     <dbl>
#> 1 ran_pars COR_t.e  -0.163         NA
#> 2 ran_pars COR_t.m   0.0312        NA
#> 3 ran_pars COR_e.m  -0.569         NA
```

### Factor scores

The factor scores or person parameter estimates are obtained via
`augment()`. This returns a tibble comprised of the data set and the
factor scores (plus respective standard errors) for the three dimensions
*t* (F1), *e* (F2), and *m* (F3).\[1\]

The correlation of the scores for the target trait (conscientiousness in
this case) between the IR-tree model and the GRM indicates that the
models differ in this respect even though not drastically.

``` r
augment(fit1)
#> # A tibble: 500 x 16
#>       E1    E2    E3    E4    E5    E6    E7    E8    E9   E10 .fittedF1
#>    <int> <dbl> <int> <dbl> <int> <dbl> <int> <dbl> <int> <dbl>     <dbl>
#>  1     3     2     3     3     3     3     3     4     4     4    0.673 
#>  2     5     4     4     5     4     5     5     4     5     4    1.58  
#>  3     1     2     2     1     2     1     1     1     1     1   -1.68  
#>  4     4     4     4     4     4     4     4     3     4     4    1.58  
#>  5     3     3     3     3     3     4     3     2     4     2    0.0203
#>  6     2     1     2     1     2     1     2     1     2     2   -1.63  
#>  7     4     4     4     4     4     4     4     4     4     4    1.68  
#>  8     3     4     4     2     4     4     4     3     4     3    0.622 
#>  9     2     5     5     4     2     4     3     4     4     4    0.640 
#> 10     2     2     3     1     4     2     2     1     1     1   -0.994 
#> # ... with 490 more rows, and 5 more variables: .fittedF2 <dbl>,
#> #   .fittedF3 <dbl>, .se.fitF1 <dbl>, .se.fitF2 <dbl>, .se.fitF3 <dbl>

cor(augment(fit1)$.fittedF1, augment(fit2)$.fittedF1)
#> [1] 0.9293013
```

1.  The order corresponds to the order of appearance in the section
    `IRT` of the model string.
