#' Fitting and Simulating IR-Tree Models
#'
#' Item response tree (IR-tree) models are a class of item response models that
#' reparameterize polytomous responses using a processing tree structure.
#' They have been used to study response styles but are suitable for other
#' applications as well.
#' They can be fit using standard IRT software, and this package uses either the
#' proprietary software 'Mplus' or the R package 'mirt' as a backend.
#' Functions are provided to generate data, to fit a model, and to run
#' simulation studies.
#'
#' @docType package
#' @name ItemResponseTrees-package
#' @aliases ItemResponseTrees
#' @author Hansjoerg Plieninger
#'
#' @importFrom stats setNames aggregate as.formula na.omit rmultinom cov2cor reshape
#' @importFrom utils capture.output
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#'
# @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
