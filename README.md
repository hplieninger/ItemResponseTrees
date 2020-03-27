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
install.packages("remotes")
remotes::install_github("hplieninger/ItemResponseTrees", upgrade = "always")
```

## Example Data

Herein, an illustrative example will be shown using a popular IR-tree
model for 5-point items (Böckenholt, 2012). The model is applied to a
Big Five data set from Jackson (2012), more precisely to nine
conscientiousness items.

``` r
library("ItemResponseTrees")

data("jackson")

set.seed(9701)
df1 <- jackson[sample(nrow(jackson), 321), paste0("E", 1:9)]
df1
#> # A tibble: 321 x 9
#>       E1    E2    E3    E4    E5    E6    E7    E8    E9
#>    <int> <dbl> <int> <dbl> <int> <dbl> <int> <dbl> <int>
#>  1     3     3     3     3     3     3     2     3     3
#>  2     3     5     3     3     5     5     5     1     5
#>  3     4     4     3     3     4     3     5     4     2
#>  4     4     3     5     4     3     4     4     3     4
#>  5     3     1     4     1     2     4     2     3     4
#>  6     1     2     3     2     2     2     2     2     2
#>  7     4     5     4     3     4     4     4     3     3
#>  8     3     5     3     4     4     5     3     5     4
#>  9     2     2     3     4     2     1     2     2     2
#> 10     2     4     4     2     3     4     2     3     3
#> # ... with 311 more rows
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

<!-- end list -->

``` r
# Use irtree_create_template() to create a model-string template.
# This may also come handy if you prefer to provide the mapping matrix 
#   (i.e., pseudoitems) rather than the model equations
irtree_create_template(df1, mapping_matrix = NULL)
```

In the following, the model string for the desired IR-tree model for the
nine items is specified and saved as `m1`.

``` r
m1 <- "
# IR-tree model for 5-point items (Böckenholt, 2012)

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

IRT:
t  BY  E1,   E2,   E3,   E4,   E5,   E6,   E7,   E8,   E9;
e  BY  E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1;
m  BY  E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1;

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
t  BY  E1,   E2,   E3,   E4,   E5,   E6,   E7,   E8,   E9;

Class:
GRM
"
```

Subsequently, the function `irtree_model()` needs to be called, which
takes a model string such as `m1` or `m2` as its sole argument. The
resulting objects `model1` and `model2` of class `irtree_model` contain
all the necessary information for fitting the model. Furthermore, one
may inspect specific elements, for example, the pseudoitems contained in
the mapping matrix.

Further information on creating model strings is provided in
`?irtree_model()`.

``` r
model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

model1$mapping_matrix
#>      cate  t  e m
#> [1,]    1  0  1 0
#> [2,]    2  0  0 0
#> [3,]    3 NA NA 1
#> [4,]    4  1  0 0
#> [5,]    5  1  1 0
```

## Fitting the model

Then, the model can be `fit()` using one of three different engines. The
ItemResponseTrees package supports the engines
[mirt](https://cran.r-project.org/package=mirt),
[TAM](https://cran.r-project.org/package=TAM), and Mplus (via the
[MplusAutomation](https://cran.r-project.org/package=MplusAutomation)
package). Additional arguments for the engine, for example, details of
the algorithms, can be specified via the `control` argument.

``` r
# mirt can be used with an EM algorithm (the default) or, for example, with the
# MH-RM algorithm, which seems a little bit faster here.
# See ?mirt::mirt for details.
ctrl <- control_mirt(method = "MHRM")

fit1 <- fit(model1, data = df1, engine = "mirt", control = ctrl)
fit2 <- fit(model2, data = df1, engine = "mirt", control = ctrl)
```

## Results

The easiest way to access the information stored in `fit1` and `fit2` is
via the functions `glance()`, `tidy()`, and `augment()` (that come from
the [broom](https://broom.tidyverse.org/) package, which is part of the
tidyverse).

### Model Fit

Information about model fit is obtained via `glance()`. As seen below,
the IR-tree model has 41 freely estimated parameters (3 x 9 thresholds +
9 loadings + 2 variances + 3 covariances). The GRM has 45 estimated
parameters (4 x 9 thresholds + 9 loadings). (Of course, this comparison
is a little bit unfair, because the IR-tree model is much more flexible
in terms of dimensionality/“random effects” even though it is less
flexible with respect to the thresholds/“fixed effects”.)

For the present data, the IR-tree model slightly outperforms the GRM
according to AIC and BIC, and thus one may conclude that response styles
are present in these data.

``` r
glance(fit1)
#> # A tibble: 1 x 11
#>     AIC   BIC  AICc logLik converged iterations estimator  npar  nobs n.factors
#>   <dbl> <dbl> <dbl>  <dbl> <lgl>          <int> <chr>     <int> <int>     <int>
#> 1 7660. 7815. 7672. -3789. TRUE             369 MHRM         41   321         3
#> # ... with 1 more variable: ngroups <int>

rbind(glance(fit1), glance(fit2))
#> # A tibble: 2 x 11
#>     AIC   BIC  AICc logLik converged iterations estimator  npar  nobs n.factors
#>   <dbl> <dbl> <dbl>  <dbl> <lgl>          <int> <chr>     <int> <int>     <int>
#> 1 7660. 7815. 7672. -3789. TRUE             369 MHRM         41   321         3
#> 2 7686. 7856. 7701. -3798. TRUE              84 MHRM         45   321         1
#> # ... with 1 more variable: ngroups <int>
```

### Parameter Estimates

The parameter estimates are obtained via `tidy()`. For the IR-tree
model, this returns a tibble with 66 rows (pertaining to the fixed and
estimated parameters). Below, the nine threshold/difficulty parameters
`t_E*.d` pertaining to parameter *t* are shown plus the threshold of
pseudoitem `e_E1`.

The latent variances, covariances, and correlations are shown below as
well, and these show the typical pattern of a negative correlation
between *e* and *m*.\[1\]

``` r
tidy(fit1, par_type = "difficulty")
#> # A tibble: 66 x 5
#>    parameter component term   estimate std.error
#>    <chr>     <chr>     <chr>     <dbl>     <dbl>
#>  1 Threshold <NA>      t_E1.d    1.08      0.574
#>  2 Threshold <NA>      t_E2.d   -1.07      0.652
#>  3 Threshold <NA>      t_E3.d   -1.34      0.383
#>  4 Threshold <NA>      t_E4.d   -0.208     0.563
#>  5 Threshold <NA>      t_E5.d   -1.81      0.522
#>  6 Threshold <NA>      t_E6.d   -2.12      0.632
#>  7 Threshold <NA>      t_E7.d    0.622     0.540
#>  8 Threshold <NA>      t_E8.d    0.636     0.448
#>  9 Threshold <NA>      t_E9.d   -0.178     0.351
#> 10 Threshold <NA>      e_E1.d    0.744     0.189
#> # ... with 56 more rows

tail(tidy(fit1, par_type = "difficulty"), 9)
#> # A tibble: 9 x 5
#>   parameter component term    estimate std.error
#>   <chr>     <chr>     <chr>      <dbl>     <dbl>
#> 1 Var       t         COV_11     1       NA     
#> 2 Var       e         COV_22     2.08     0.279 
#> 3 Var       m         COV_33     0.888    0.134 
#> 4 Cov       <NA>      COV_21     0.258    0.0954
#> 5 Cov       <NA>      COV_31    -0.101    0.0852
#> 6 Cov       <NA>      COV_32    -1.12     0.161 
#> 7 Corr      <NA>      CORR_21    0.179   NA     
#> 8 Corr      <NA>      CORR_31   -0.107   NA     
#> 9 Corr      <NA>      CORR_32   -0.826   NA
```

### Factor scores

The factor scores or person parameter estimates are obtained via
`augment()`. This returns a tibble comprised of the data set and the
factor scores (plus respective standard errors) for the three dimensions
*t* (F1), *e* (F2), and *m* (F3).

The correlation of the scores for the target trait (extraversion in this
case) between the IR-tree model and the GRM indicates that the models
differ in this respect even though not drastically.

``` r
augment(fit1)
#> # A tibble: 321 x 15
#>       E1    E2    E3    E4    E5    E6    E7    E8    E9 .fittedF1 .fittedF2
#>    <int> <dbl> <int> <dbl> <int> <dbl> <int> <dbl> <int>     <dbl>     <dbl>
#>  1     3     3     3     3     3     3     2     3     3   -0.743     -2.82 
#>  2     3     5     3     3     5     5     5     1     5    0.560      1.62 
#>  3     4     4     3     3     4     3     5     4     2    0.809     -0.812
#>  4     4     3     5     4     3     4     4     3     4    1.16      -0.706
#>  5     3     1     4     1     2     4     2     3     4   -0.634     -0.291
#>  6     1     2     3     2     2     2     2     2     2   -1.42      -0.807
#>  7     4     5     4     3     4     4     4     3     3    1.10      -0.808
#>  8     3     5     3     4     4     5     3     5     4    1.13       0.237
#>  9     2     2     3     4     2     1     2     2     2   -0.905     -0.767
#> 10     2     4     4     2     3     4     2     3     3   -0.0666    -1.51 
#> # ... with 311 more rows, and 4 more variables: .fittedF3 <dbl>,
#> #   .se.fitF1 <dbl>, .se.fitF2 <dbl>, .se.fitF3 <dbl>

cor(augment(fit1)$.fittedF1, augment(fit2)$.fittedF1)
#> [1] 0.9021495
```

1.  The order of the processes corresponds to the order of appearance in
    the section `IRT` of the model string. Thus, the order here is *t*,
    *e*, *m*, such that `COV_33` is the variance of person parameters for
    *m*, and `CORR_32` is the correlation between *m* and *e*. Likewise,
    in the output of `augment(fit1)` shown herein, `F1` corresponds to
    *t* etc.
