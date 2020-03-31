#' IR-Tree Modeling in mirt, Mplus, or TAM
#'
#' Item response tree (IR-tree) models are a class of item response theory (IRT)
#' models that assume that the responses to polytomous items can best be
#' explained by multiple psychological processes (e.g., Böckenholt, 2012,
#' \url{https://dx.doi.org/10.1037/a0028111}). The package 'ItemResponseTrees'
#' allows to fit such IR-tree models in
#' [mirt](https://cran.r-project.org/package=mirt),
#' [Mplus](https://cran.r-project.org/package=MplusAutomation), or
#' [TAM](https://cran.r-project.org/package=TAM). The package automates some of
#' the hassle of IR-tree modeling by means of a consistent syntax. This allows
#' new users to quickly adopt this model class, and this allows experienced
#' users to fit many complex models effortlessly.
#'
#' @references Böckenholt, U. (2012). Modeling multiple response processes in
#'   judgment and choice. *Psychological Methods*, *17*(4), 665–678.
#'   https://doi.org/10.1037/a0028111
#' @references Plieninger, H. (2020). Developing and applying IR-tree models:
#'   Guidelines, caveats, and an extension to multiple groups. *Organizational
#'   Research Methods*. Advance online publication.
#'   https://doi.org/10.1177/1094428120911096
#'
#' @docType package
#' @name ItemResponseTrees-package
#' @aliases ItemResponseTrees
#' @author Hansjörg Plieninger
#'
#' @import stats
#' @importFrom utils capture.output packageVersion
#' @importFrom methods formalArgs
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#'
#' @encoding UTF-8
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
