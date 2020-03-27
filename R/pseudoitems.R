#' Pseudo-Items
#'
#' @description
#'
#' IR-tree models can be fit on the basis of so-called pseudo-items. To this
#' end, the original, polytomous items are recoded into binary pseudo-items.
#' Whether a pseudo-item is coded as `1`, `0`, or `NA` depends on the model
#' equations (e.g., Böckenholt, 2012; Plieninger, 2020).
#'
#' The ItemResponseTrees package internally works with pseudo-items as well.
#' However, the user has to specify the model equations rather than the
#' pseudo-items in the [irtree_model] syntax. Internally, the original responses
#' are recoded on the basis of the model supplied by the user by the function
#' [irtree_recode()]. This function may also be used directly if desired.
#'
#' As an alternative to specifying the model equations themselves, users may
#' also use the function [irtree_create_template()] with a mapping matrix (that
#' specifies the structure of the pseudo-items) to generate the model equations
#' automatically.
#'
#' @examples
#' # Mapping matrix for data with three response categories:
#' (mm <- cbind(cat = 0:2,
#'              p1 = c(0,  1, 1),
#'              p2 = c(NA, 0, 1)))
#' #>      cat p1 p2
#' #> [1,]   0  0 NA
#' #> [2,]   1  1  0
#' #> [3,]   2  1  1
#'
#' irtree_create_template(data.frame(x1 = 0:2, x2 = 0:2),
#'                        mapping_matrix = mm)
#' #>
#' #> m1 <- "
#' #> Equations:
#' #> 0 = (1-p1)
#' #> 1 = p1*(1-p2)
#' #> 2 = p1*p2
#' #>
#' #> IRT:
#' #> p1 BY x1@1, x2@1;
#' #> p2 BY x1@1, x2@1;
#' #>
#' #> Class:
#' #> Tree
#' #> "
#' #>
#'
#' @references Böckenholt, U. (2012). Modeling multiple response processes in
#'   judgment and choice. *Psychological Methods*, *17*(4), 665–678.
#'   https://doi.org/10.1037/a0028111
#' @references Plieninger, H. (2020). Developing and applying IR-tree models:
#'   Guidelines, caveats, and an extension to multiple groups. *Organizational
#'   Research Methods*. Advance online publication.
#'   https://doi.org/10.1177/1094428120911096
#' @name pseudoitems
#' @encoding UTF-8
NULL
