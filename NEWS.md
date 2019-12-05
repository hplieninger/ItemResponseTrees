# Current version

## Breaking changes

* In the model syntax, the section *Subtree* is deprecated and was merged with section *Constraints*.
* Long variable names are no longer automatically replaced. They are now accepted for mirt and TAM, but no longer for Mplus.

## Improvements

* More complex models using constraints (that were only available for Mplus) are now implemented for mirt and TAM.
* The new function `irtree_create_template()` helps to create a model string.

# ItemResponseTrees 0.1.1

* Add wrapper functions for the TAM package
* Add the multidimensional partial credit model (not an IR-tree model); works only in TAM

# ItemResponseTrees 0.1

* First stable version
