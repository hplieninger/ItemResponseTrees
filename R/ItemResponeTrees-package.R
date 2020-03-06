#' IR-Tree Modeling in mirt, Mplus, or TAM
#'
#' ItemResponseTrees is an R package that allows to fit IR-tree models in Mplus
#' or mirt. The package automates some of the hassle of IR-tree modeling by
#' means of a consistent syntax. This allows new users to quickly adopt this
#' model class, and this allows experienced users to fit many complex models
#' effortlessly.
#'
#' @docType package
#' @name ItemResponseTrees-package
#' @aliases ItemResponseTrees
#' @author Hansjoerg Plieninger
#'
#' @import stats
#' @importFrom utils capture.output packageVersion
#' @importFrom methods formalArgs
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
