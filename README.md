
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/hplieninger/ItemResponseTrees.svg?branch=master)](https://travis-ci.org/hplieninger/ItemResponseTrees)
[![Coverage
status](https://codecov.io/gh/hplieninger/ItemResponseTrees/branch/master/graph/badge.svg)](https://codecov.io/github/hplieninger/ItemResponseTrees?branch=master)

# ItemResponseTrees

This package allows users to define IR-tree models and estimate them
using either Mplus or the R package mirt.

## Installation

You can install the latest version of ItemResponseTrees using devtools:

``` r
devtools::install_github("hplieninger/ItemResponseTrees")
```

## Example

This is an example of specifying the IR-tree model for items with five
categories proposed by Böckenholt (2012) as well as DeBoeck and Partchev
(2012). The user specifies the model in a string comprised of different
parts.

  - In the section **IRT**, it is specified which items in the data set
    load on which latent variable. In the present example, there are
    latent variables for extreme responding (e), midpoint responding
    (m), and the target trait (t). Each latent variable is measured by
    all five items in the model.
  - In the section **Equations**, the model equations for all five
    response categories are specified. Herein, for example, the
    probability to respond with 5 depends on t, e, and (1-m).
  - In the section **Class**, it is specified whether a Tree or a GRM
    model is specified.

For details about the model specification, see `?tree_model`

``` r
m1 <- "
IRT:
t  BY x1@1, x2@1, x3@1, x4@1, x5@1;
e  BY x1@1, x2@1, x3@1, x4@1, x5@1;
m  BY x1@1, x2@1, x3@1, x4@1, x5@1;

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

Class:
Tree
"
model1 <- tree_model(m1)
res1   <- fit_tree_mplus(data = df, model = model1)
sum1   <- extract_mplus_output(res1$mplus, model = model1)
```
