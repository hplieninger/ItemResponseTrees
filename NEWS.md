# ItemResponseTrees 0.2.4

* Refactor condition handling (e.g., replace `myTryCatch()` with `tryCatch.W.E()`)
* `sym_diff()` was replaced with `sets::set_symdiff()`, and `sort2()` was removed.

# ItemResponseTrees 0.2.3

* Minor, internal changes in the documentation

# ItemResponseTrees 0.2.2

* Modified package Description field for CRAN
* Modified vignette, `install.packages` was commented out.
* Credit to Jackson for the `jackson` data and Dontas for `sort2()` is now given via appropriate attribution rather than usage of `\author`.

# ItemResponseTrees 0.2.1

* Modified package Title field for CRAN

# ItemResponseTrees 0.2.0

## Breaking changes

* In the model syntax, the section *Subtree* is deprecated and was merged with section *Constraints*.
* Long variable names are no longer automatically replaced. They are now accepted for mirt and TAM, but no longer for Mplus.
* `logit` is now the default link function everywhere.
* `mirt` is now the default engine in `fit()`.

## Improvements

* More complex models using constraints (that were only available for Mplus) are now implemented for mirt and TAM.
* The new function `irtree_create_template()` helps to create a model string.
* Data set `jackson` (Big Five questionnaire) was added.
* Many internal changes to increase robustness.
* Controlling features of the engine in `fit()` is now done via a single `control` argument (rather than `...`), and helper functions `control_<engine>()` make that painless.
* The documentation was improved.
* The output of `tidy()` was enhanced (added columns `parameter` and `component`), and the argument `par_type` is now required for mirt.

# ItemResponseTrees 0.1.1

* Add wrapper functions for the TAM package
* Add the multidimensional partial credit model (not an IR-tree model); works only in TAM

# ItemResponseTrees 0.1

* First stable version
