#' ItemResponseTrees model syntax
#'
#' The ItemResponseTrees model syntax describes the statistical model. The
#' function `irtree_model()` turns a user-defined model string into an object of
#' class `irtree_model` that represents the full model as needed by the package.
#'
#' @section Overview of the Model Syntax:
#'
#' \enumerate{
#'   \item The `model` string must contain at least the sections **IRT**,
#'     **Class**, and (if class is tree) **Equations**.
#'   \item Section headings must appear on a separate line ending with a colon (:).
#'   \item The model may contain empty lines and commented lines, which begin
#'     with `#` (do not use inline comments).
#'   \item Line breaks are only allowed in section **IRT**.
#' }
#' Details for all the required and optional sections of the `model` string are
#' given in the following.
#'
#' ## IRT
#'
#'   The `model` must contain a section with heading **IRT**. Therein, the IRT
#'   structure of the model is described in a way resembling the MODEL part of
#'   an Mplus input file. It has a structure of \code{LV BY item1*, item2@1},
#'   where `LV` is the name of the latent variable/parameter/process, and
#'   `item` is the name of the observed variable in the data set, which
#'   is followed by the loading. The loading may either be fixed (e.g., to 1)
#'   using \code{@1} or it may be set free using `*` or omitting the
#'   loading completely.
#'
#'   Each measurement model (i.e., the LV and its items) must appear on a
#'   separate line ending with a semicolon. Items must be separated by
#'   commas. Line breaks are allowed. For example:
#'
#'   ```
#'   IRT:
#'   t BY x1, x2, x3, x4, x5, x6;
#'   e BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#'   m BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#'   ```
#'
#' ## Equations
#'
#'   The `model` must contain a section with heading **Equations** if **Class** is
#'   Tree.
#'   Therein, the model equations are described.
#'   They have a structure similar to `Cat = p1*(1-p2)`, where `Cat`
#'   is any observed response category in the data set.
#'   The names of the parameters must be equal to those of the latent variables
#'   in the section **IRT** (combined with **Constraints** if specified).
#'
#'   The equations may contain only products and not sums.
#'   That is, it is not possible to estimate genuine mixture models as, for
#'   example, in the package [mpt2irt](https://github.com/hplieninger/mpt2irt).
#'
#'   Each equation must appear on a separate, non-broken line. For example:
#'
#'   ```
#'   Equations:
#'   1 = (1-m)*(1-t)*e
#'   2 = (1-m)*(1-t)*(1-e)
#'   3 = m
#'   4 = (1-m)*t*(1-e)
#'   5 = (1-m)*t*e
#'   ```
#'
#' ## Class
#'
#'   The `model` must contain a section with heading **Class** to specify the
#'   type/class of IRT model to use.
#'   Currently, may be either `Tree`, `GRM`, or `PCM`. For example:
#'
#'   ```
#'   Class:
#'   Tree
#'   ```
#'
#' ## Constraints
#'
#'   The `model` may contain a section with heading **Constraints** to specify
#'   equality constraints of latent variables.
#'   Constraints may be useful for multidimensional questionnaires to link
#'   **IRT** and **Equations** in a specific way.
#'   Or, latent variables in **IRT** may be constrained to equality.
#'
#' ### Constraints in order to link sections IRT and Equations
#'
#'   A process in the model equations (section **Equations**) may correspond to
#'   multiple latent variables (section **IRT**).
#'   For example, when analyzing a Big Five data set, one may wish to specify only
#'   one extremity process *e* for all items but multiple target traits *t*, namely,
#'   one for each of the five scales.
#'   In such a case, the section **Equations** would list only the parameter *t*,
#'   while the section **IRT** would list the parameters *t1*, ..., *t5*.
#'
#'   In the framework of MPT, one would think of such a situation in terms of
#'   multiple albeit similar trees with specific parameter contraints across
#'   trees.
#'   For example, one would use one tree for each Big Five scale and fix the
#'   response style parameters to equality across trees but not the target trait
#'   parameters.
#'
#'   Each line in this section has a structure of `Param = LV |
#'   LV`, where `Param` is the name of the process used only in section
#'   **Equations** and `LV` it the name of the process used only in
#'   section **IRT**.
#'   Use one line for each definition. For example:
#'
#'   ```
#'   Constraints:
#'   t = t1 | t2 | t3 | t4 | t5
#'   ```
#'
#' ### Constraints within section IRT
#'
#'   For example, in a sequential model as proposed by Tutz as well as Verhelst,
#'   one would specify two processes for a 3-point item. The first process would
#'   correspond to a pseudoitem of `0-1-1` and the second process to a
#'   pseudoitem of `NA-0-1`.
#'   However, the latent variables corresponding to these processes would
#'   typically be assumed to be equal and need thus be constrained accordingly.
#'
#'   Each line in this section has a structure of `LV1 = LV2`, where
#'   `LV1` and `LV2` are the names of the latent variables used in
#'   section **IRT**.
#'   Use one line for each definition. For example:
#'
#'   ```
#'   Constraints:
#'   LV1 = LV2
#'   LV1 = LV3
#'   ```
#'
#' ## Addendum
#'
#'   The `model` may contain a section with heading **Addendum** if
#'   `engine = "mplus"` is used for estimation.
#'   Any code in this section is directly pasted in the `MODEL` section of
#'   the Mplus input file.
#'   Use a semicolon at the end of each line; lines must not exceed 90 characters.
#'   Note that the addendum is ignored in [irtree_gen_data()]. For example:
#'
#'   ```
#'   Addendum:
#'   e WITH t@0;
#'   m WITH t@0;
#'   ```
#'
#' ## Weights
#'
#'   The `model` may contain a section with heading **Weights** if model
#'   **Class** is PCM.
#'   This allows to specify (uni- and) multidimensional partial credit models.
#'   They have been proposed, for example, by Wetzel and Carstensen (2017), as
#'   an alternative to IR-tree models.
#'   Note that fitting these models is only implemented for `engine = "tam"`.
#'
#'   Each line in this section has a structure of `LV = weights`, where `LV` is
#'   the name of the processes used in section **IRT**.
#'   `weights` must be valid R code, namely, a vector of weights (see, e.g.,
#'   Table 1 in Wetzel & Carstensen, 2017, or Table 2 in Falk & Cai, 2015).
#'   Use one line for each definition. For example:
#'
#'   ```
#'   Weights:
#'   t = c(0, 1, 2, 3, 4)
#'   e = c(1, 0, 0, 0, 1)
#'   m = c(0, 0, 1, 0, 0)
#'   ```
#'
#' @param model String with a specific structure as described below.
#' @return List of class `irtree_model`. It contains the information extracted
#'   from parsing `model`. Side note: The returned list contains an element
#'   `mappping_matrix` that contains the pseudoitems. This information is
#'   instructive, and it might be used as an input to the `dendrify()`
#'   function of the irtrees package.
#' @examples
#' m1 <- "
#' # Comment
#' IRT:
#' t1 BY x1@1, x2*, x3*;
#' t2 BY x4@1, x5*, x6*;
#' e  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#' m  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#'
#' Constraints:
#' t = t1 | t2
#'
#' Equations:
#' 1 = (1-m)*(1-t)*e
#' 2 = (1-m)*(1-t)*(1-e)
#' 3 = m
#' 4 = (1-m)*t*(1-e)
#' 5 = (1-m)*t*e
#'
#' Class:
#' Tree
#' "
#'
#' model <- irtree_model(m1)
#' @export
irtree_model <- function(model = NULL) {

    if (inherits(model, "irtree_model")) {
        return(model)
    } else {
        checkmate::assert_string(model, min.chars = 10)
    }

    # out1 <- list(string = model)
    e1 <- new.env()
    # Remove white space and emtpy lines at beginning and end
    model <- sub("^\\s*", "", model)
    model <- sub("\\s*$", "", model)
    e1$string <- model

    model2 <- trimws(strsplit(model, "\\n+")[[1]])
    model2 <- model2[nchar(model2) > 0]
    model2 <- model2[!grepl("^#", model2)]

    ### Extract suparts of 'model' ###

    model_list <- list("irt" = NULL,
                       "equations" = NULL,
                       "class" = NULL,
                       "addendum" = NULL,
                       "constraints" = NULL,
                       "weights" = NULL)
    # Check Headings in 'model'
    headings <-
        na.omit(
            stringr::str_extract(tolower(model2),
                                 paste0(# "(?i)^",
                                        names(model_list), ".*", collapse = "|")))
    # No duplicated headings
    if (any(
        duplicated(
            stringr::str_replace(headings, "\\:", ""),
            incomparables = NA))) {
        stop("Problem in 'model': Each heading may appear at most once. Problem with: ",
             clps(", ", "'",
                  headings[duplicated(stringr::str_replace(headings, "\\:", ""), incomparables = NA)],
                  "'", sep = ""),
             call. = FALSE)
    }

    # All headings must end with a colon
    if (any(stringr::str_detect(headings, ".*(?<!\\:)$"))) {
        stop("Problem in 'model': All headings must end with a colon (:). Problem with: ",
             clps(", ", "'",
                  na.omit(stringr::str_extract(headings, ".*(?<!\\:)$")), "'", sep = ""),
             call. = FALSE)
    }

    # Only headings may end with a colon
    tmp1 <- setdiff(na.omit(stringr::str_extract(tolower(model2), ".*\\:$")), headings)
    if (length(tmp1) > 0) {
        stop("Problem in 'model': Only headings may end with a colon (:). Problem with:\n",
             clps("; ", "'", tmp1, "'", sep = ""),
             call. = FALSE)
    }

    posx <- c(grep(paste(names(model_list), collapse = "|"), model2, ignore.case = TRUE), length(model2) + 1)

    flag <- integer()

    for (ii in seq_along(model_list)) {
        tmp1 <- is.element(tolower(model2), paste0(names(model_list)[ii], ":"))
        if (any(tmp1, na.rm = TRUE)) {
            model_list[[ii]] <- model2[(1 + which(tmp1)):(posx[1 + which(posx == which(tmp1))] - 1)]
        } else {
            flag <- c(flag, ii)
        }
    }
    model_list[flag] <- NULL

    .must_have(model_list, "irt", .name = "IRT")

    ##### Class #####

    e1$class <- tolower(stringr::str_extract(model_list$class, "\\w+"))
    checkmate::assert_choice(e1$class, choices = c("tree", "grm", "pcm"), .var.name = "Class")

    if (e1$class == "tree") {
        .must_have(model_list, "equations", .class = "Tree")
        .must_have(model_list, "weights", FALSE, .class = "Tree")
    } else if (e1$class == "grm") {
        e1$K <- NA_integer_
        .must_have(model_list, "equations", FALSE, .class = "GRM")
        .must_have(model_list, "weights", FALSE, .class = "GRM")
    } else if (e1$class == "pcm") {
        .must_have(model_list, "equations", FALSE, .class = "PCM")
        .must_have(model_list, "weights", .class = "PCM")
    } else .stop_not_implemented()

    ##### IRT #####

    irtree_model_irt(model_list = model_list, e1 = e1)

    ##### Equations #####

    irtree_model_equations(model_list = model_list, e1 = e1)

    ##### Weights #####

    irtree_model_weights(model_list, e1)

    ##### Constraints #####

    irtree_model_constraints(model_list, e1)

    ##### Mapping Matrix #####

    irtree_model_mapping(e1 = e1)

    ##### Addendum #####

    irtree_model_addendum(model_list, e1)

    ##### Lambda Matrix #####

    lambda <- tibble::enframe(e1$irt_items, "irt", "item")[2:1] %>%
        dplyr::left_join(e1$latent_names, by = "irt") %>%
        tidyr::unnest(.data$item) %>%
        dplyr::mutate(item = factor(.data$item, levels = e1$j_names)) %>%
        dplyr::mutate(
            dplyr::across(
                tidyselect::all_of(c("irt", "mpt", "theta")),
                ~ factor(.x, unique(.x))))
    lambda$loading <- ifelse(is.na(unlist(e1$irt_loadings)), "*", unlist(e1$irt_loadings))

    if (e1$class == "tree") {
        lambda$new_name <- glue::glue_data(lambda, "{mpt}_{item}")
    } else if (e1$class %in% c("grm", "pcm")) {
        lambda$new_name <- glue::glue_data(lambda, "{item}")
    }
    lambda$mplus <- glue::glue_data(lambda, "{new_name}{loading}")

    lambda$label <- paste0("a", 1:nrow(lambda))

    e1$lambda <- lambda

    e1$proper_model <- TRUE

    out1 <- as.list(e1)
    out1 <- out1[order(names(out1))]
    class(out1) <- c("irtree_model", "list")

    tmp1 <- environment()

    ##### Test if probabilities sum to 1 #####

    if (!is.null(model_list$equations)) {
        tryCatch(
            irtree_gen_data(
                object = out1, N = 1, sigma = diag(e1$S),
                itempar = list(beta  = matrix(stats::rnorm(e1$J*e1$P), e1$J, e1$P),
                               alpha = matrix(stats::runif(e1$J*e1$P, .5), e1$J, e1$P)),
                skip = TRUE),
            improper_model = function(cnd) {
                with(tmp1, out1$proper_model <- FALSE)
                rlang::warn(
                    paste("Equations do not constitute a proper model because",
                          "they do not sum to 1. "),
                    .subclass = "improper_model")
            })
    }
    return(out1)
}

#' @export
print.irtree_model <- function(x, ...) {
    cat(x$string, "\n", ...)
}
