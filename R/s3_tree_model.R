#' ItemResponseTree Model Syntax
#'
#' The ItemResponseTree model syntax describes an IR-tree model. The function
#' `irtree_model` turns a user-defined model string into a list that
#' represents the full model as needed by the package.
#'
#' @section Model:
#'
#' \enumerate{
#'   \item The `model` string must contain at least the sections IRT, Class, and (if class is tree) Equations.
#'   \item Section headings must appear on a seperate line ending with a colon (:).
#'   \item The model may contain empty lines and comments, which begin with `#`.
#'   \item Line breaks are only allowed in section IRT.
#' }
#'
#' @section IRT:
#'
#'   The `model` must contain a section with heading IRT. Therein, the IRT
#'   structure of the model is described in a way resembling the MODEL part of
#'   an Mplus input file. It has a structure of `LV BY item1*, item2@1`,
#'   where `LV` is the name of the latent variable/parameter/process,
#'   `item` is the name of the observed variable in the data set and this
#'   is followed by the loading. The loading may either be fixed (e.g., to 1)
#'   using `@1` or it may be set free using `*` or omitting the
#'   loading completely.
#'
#'   Each measurement model (i.e., the LV and its items) must appear on a
#'   seperate line ending with a semicolon. Items must be seperated by
#'   commas. Linebreaks are allowed. For example:
#'
#'   \preformatted{
#'   IRT:
#'   t  BY x1, x2, x3, x4, x5, x6;
#'   e  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#'   m  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;}
#'
#' @section Equations:
#'
#'   The `model` must contain a section with heading Equations if Class is
#'   Tree.
#'   Therein, the model equations are described.
#'   They have a structure similar to `Cat = p1*(1-p2)`, where `Cat`
#'   is any observed response category in the data set.
#'   The names of the parameters must be equal to those of the latent variables
#'   in the section IRT (combined with Subtree if specified).
#'
#'   The equations may contain only products and not sums.
#'   That is, it is not possible to estimate genuine mixture models as, for
#'   example, in the package [mpt2irt](https://github.com/hplieninger/mpt2irt).
#'
#'   Each equation must appear on a separate, non-broken line. For example:
#'
#'   \preformatted{
#'   Equations:
#'   1 = (1-m)*(1-t)*e
#'   2 = (1-m)*(1-t)*(1-e)
#'   3 = m
#'   4 = (1-m)*t*(1-e)
#'   5 = (1-m)*t*e}
#'
# @section Processes:
#
#   The `model` must contain a section with heading Processes.
#   It is essentially a listing of all processes/parameters/latent variables
#   present in section IRT in the correct order.
#   That is, the first process in that listing is the first column in the
#   matrix of person parameters etc.
#   This section does not contain any new information wrt to the model but
#   simply safeguards against ambiguities in the model syntax.
#
#   It is advised to choose relatively short names for the processes and items,
#   because Mplus allows only names of up to eight characters. For example:
#
#   \preformatted{
#   Processes:
#   e, m, t}
#
# @section Items:
#
#   The `model` must contain a section with heading Items.
#   It is essentially a listing of all observed variables present in the data
#   set in the desired order.
#   That is, the first, second, ... item in that listing is the first, second,
#   ... row in the matrix of item parameters etc.
#   This section does not contain any new information wrt to the model but
#   simply safeguards against ambiguities in the model syntax.
#
#   It is advised to choose relatively short names for the processes and items,
#   because Mplus allows only names of up to eight characters. For example:
#
#   \preformatted{
#   Items:
#   x1, x2, x3, x4, x5}
#'
#' @section Subtree:
#'
#'   The `model` may contain a section with heading Subtree.
#'   This is necessary if a process in the model equations (section Equations)
#'   may correspond to different latent variables (section IRT).
#'   For example, when analyzing a Big Five data set, one may wish to specify
#'   only one extremity process e for all items but multiple target traits t,
#'   namely, one for each of the five scales.
#'   In such a case, the section Equations would list only the parameter t,
#'   while the section IRT would list the parameters t1, ..., t5.
#'
#'   In the framework of MPT, one would think of such a situation in terms of
#'   multiple albeit similar trees with specific parameter contraints across
#'   trees.
#'   For example, one would use one tree for each Big Five scale and fix the
#'   response style parameters to equality across trees but not the target trait
#'   parameters.
#'
#'   Each line in this section has a structure of `process = subprocess +
#'   subprocess`, where `process` is the name of the process used only in
#'   section Equations and `subprocess` it the name of the process used only in
#'   section IRT.
#'   Use one line for each definition. For example:
#'
#'   \preformatted{
#'   Subtree:
#'   t = t1 + t2 + t3 + t4 + t5}
#'
#' @section Class:
#'
#'   The `model` must contain a section with heading Class to specify the
#'   type/class of IRT model to use.
#'   Currently, may be either `Tree` or `GRM` (graded response model). For
#'   example:
#'
#'   \preformatted{
#'   Class:
#'   Tree}
#'
#' @section Constraints:
#'
#'   The `model` may contain a section with heading Constraints to specify
#'   equality constraints of latent variables.
#'   For example, in a sequential model as proposed by Tutz as well as Verhelst,
#'   one would specify two processes for a 3-point item. The first process would
#'   correspond to a pseudoitem of `0-1-1` and the second process to a
#'   pseudoitem of `NA-0-1`.
#'   However, the latent variables corresponding to these processes would
#'   typically be assumed to be equal and need thus be constrained accordingly.
#'
#'   Each line in this section has a structure of `LV1 = LV2`, where
#'   `LV1` and `LV2` are the names of the latent variables used in
#'   section IRT.
#'   Use one line for each definition. For example:
#'
#'   \preformatted{
#'   Constraints:
#'   LV1 = LV2
#'   LV1 = LV3}
#'
#' @section Addendum:
#'
#'   The `model` may contain a section with heading Addendum if
#'   `backend = "mplus"` is used for estimation.
#'   Any code in this section is directly pasted in the `MODEL` section of
#'   the Mplus input file.
#'   Use a semicolon at the end of each line; lines must not exceed 90 characters.
#'   Note that the addendum is ignored in [irtree_sim_data()]. For example:
#'
#'   \preformatted{
#'   Addendum:
#'   e WITH t@0;
#'   m WITH t@0;}
#'
#' @param model String with a specific structure as described below.
#' @return List of class `irtree_model`. It contains the information extracted
#'   from parsing `model`. Side note: The returned list contains an element
#'   `mappping_matrix` that contains the pseudoitems. This information is
#'   instructive, and it might be used as an input to the [irtrees::dendrify()]
#'   function of the [irtrees::irtrees-package] package.
#' @examples
#' m1 <- "
#' # Comment
#' IRT:
#' t1 BY x1@1, x2*, x3*;
#' t2 BY x4@1, x5*, x6*;
#' e  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#' m  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#'
#' Subtree:
#' t = t1 + t2
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
    e1$string <- model

    model2 <- trimws(strsplit(model, "\\n+")[[1]])
    model2 <- model2[nchar(model2) > 0]
    model2 <- model2[!grepl("^#", model2)]

    ### Extract suparts of 'model' ###

    model_list <- list("irt" = NULL, "equations" = NULL,
                       # "processes" = NULL,
                       "subtree" = NULL,
                       # "items" = NULL,
                       "class" = NULL, "addendum" = NULL,
                       "constraints" = NULL)
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
        # tmp1 <- grepl(paste0("^", names(model_list)[ii], ":$"), model2, ignore.case = TRUE)
        tmp1 <- is.element(tolower(model2), paste0(names(model_list)[ii], ":"))
        if (any(tmp1, na.rm = TRUE)) {
            model_list[[ii]] <- model2[(1 + which(tmp1)):(posx[1 + which(posx == which(tmp1))] - 1)]
        } else {
            flag <- c(flag, ii)
        }
    }
    model_list[flag] <- NULL

    if (is.null(model_list$irt)) {
        stop("Argument 'model' must contain a part with heading 'IRT'.", call. = FALSE)
    }

    ##### Class #####

    e1$class <- tolower(stringr::str_extract(model_list$class, "\\w+"))
    checkmate::assert_choice(e1$class, choices = c("tree", "grm"), .var.name = "Class")

    if (e1$class == "tree" & is.null(model_list$equations)) {
        stop("Argument 'model' must contain a part with heading 'Equations'.", call. = FALSE)
    }

    ##### IRT #####

    irtree_model_irt(model_list = model_list, e1 = e1)

    ##### Equations #####

    irtree_model_equations(model_list = model_list, e1 = e1)

    ##### Items #####

    irtree_model_items(e1 = e1)

    ##### Subtree #####

    irtree_model_subtree(model_list = model_list, e1 = e1)

    ##### Mapping Matrix #####

    irtree_model_mapping(e1 = e1)

    ##### Addendum #####

    irtree_model_addendum(model_list, e1)

    ##### Constraints #####

    irtree_model_constraints(model_list, e1)

    ##### Labels for Items and Processes #####

    # Mplus allows names of max 8 characters.
    # Thus, data provided by user exceeding this limit have to be recoded.
    # Additionally, in the creation of the pseudoitems, names of LVs and names
    # of items are concatenated making the names even longer.
    # Therefore, if names are too long, the LVs are renamed. If names are still
    # too long, the items are renamed as well. This is done in a new model_list
    # 'model_list_new', which is subsequently processed to get everything right
    # such as the equations/expressions and the parts passed to Mplus.

    model_list_new <- model_list

    ##### _Create New Names for Items #####

    if (e1$class == "tree") {
        flag1 <- 7 < sum(c(max(c(nchar(e1$p_names), nchar(e1$lv_names))),
                           max(nchar(e1$j_names))))
    } else if (e1$class == "grm") {
        flag1 <- 8 < max(nchar(e1$j_names))
    }

    if (flag1) {

        tmp1 <- floor(log10(e1$J)) + 1
        tmp2 <- gsub("\\d+$", "", e1$j_names)
        tmp2[tmp2 == ""] <- "V"
        tmp3 <- 8
        if (e1$class == "tree") {
            tmp3 <- 7 - min(c(3,
                              max(c(nchar(e1$p_names), nchar(e1$lv_names)))))
        }
        j_names_new <- paste0(substr(tmp2, 1, tmp3 - tmp1),
                              # 1:e1$J,
                              formatC(1:e1$J, width = tmp1, format = "d", flag = "0"))

        names(j_names_new) <- e1$j_names
        j_names_new2 <- j_names_new
        names(j_names_new2) <- paste0("(?<!\\w)", e1$j_names, "(?!\\w)")

        model_list_new$irt <-
            stringr::str_replace_all(model_list_new$irt, j_names_new2)
        if (!is.null(model_list$addendum)) {
            model_list_new$addendum <-
                stringr::str_replace_all(model_list_new$addendum, j_names_new2)
        }
    } else {
        j_names_new <- e1$j_names
    }

    ##### _Create New Names for LVs #####

    if (e1$class == "tree") {
        # flag2 <- 7 < sum(c(max(c(nchar(e1$p_names), nchar(e1$lv_names))),
        #                    max(nchar(e1$j_names))))
        flag2 <- 7 < sum(c(max(c(nchar(e1$p_names), nchar(e1$lv_names))),
                           max(nchar(j_names_new))))
    } else if (e1$class == "grm") {
        flag2 <- 8 < max(nchar(e1$lv_names))
    }

    if (flag2) {
        if (e1$S > 26) {
            stop("Fatal error, please contact package maintainer. ",
                 "Renaming of names of processes only implemented for < 27 processes.")
        }

        lv_names_new <- e1$lv_names
        p_names_new <- e1$p_names
        for (ii in seq_len(nrow(e1$subtree))) {
            lv_names_new <- gsub(e1$subtree[ii, 2], e1$subtree[ii, 1], lv_names_new)
        }
        tmp1 <- factor(lv_names_new, levels = unique(lv_names_new))

        for (ii in seq_along(levels(tmp1))) {
            nsubp <- sum(tmp1 == levels(tmp1)[ii])
            p_names_new[p_names_new == levels(tmp1)[ii]] <-
                # paste0(LETTERS[ii], substr(p_names_new[ii], 1, 2))
                paste0(LETTERS[ii],
                       substr(p_names_new[p_names_new == levels(tmp1)[ii]],
                              1, 2))

            if (nsubp == 1) {
                lv_names_new[tmp1 == levels(tmp1)[ii]] <-
                    paste0(LETTERS[ii], substr(levels(tmp1)[ii], 1, 2))
            } else if (nsubp > 1) {
                lv_names_new[tmp1 == levels(tmp1)[ii]] <-
                    paste0(LETTERS[ii],
                           seq_len(nsubp),
                           substr(levels(tmp1)[ii], 1, 1))
            }
        }

        lv_names_new2 <- lv_names_new
        names(lv_names_new2) <- paste0("(?<!\\w)", names(lv_names_new2), "(?!\\w)")
        p_names_new2 <- p_names_new
        names(p_names_new2) <- paste0("(?<!\\w)", names(p_names_new2), "(?!\\w)")

        model_list_new$irt <-
            stringr::str_replace_all(model_list_new$irt, lv_names_new2)
        if (!is.null(model_list$equations)) {
            model_list_new$equations <-
                stringr::str_replace_all(model_list_new$equations, p_names_new2)
        }
        if (!is.null(model_list$subtree)) {
            model_list_new$subtree <-
                stringr::str_replace_all(model_list_new$subtree, p_names_new2)
            model_list_new$subtree <-
                stringr::str_replace_all(model_list_new$subtree, lv_names_new2)
        }
        if (!is.null(model_list$addendum)) {
            model_list_new$addendum <-
                stringr::str_replace_all(model_list_new$addendum, lv_names_new2)
        }
        if (!is.null(model_list$constraints)) {
            model_list_new$constraints <-
                stringr::str_replace_all(model_list_new$constraints, lv_names_new2)
        }
    } else {
        lv_names_new <- e1$lv_names
        p_names_new  <- e1$p_names
    }

    # ### Create New Names for Items ###
    #
    # if (e1$class == "tree" & flag1) {
    #     flag2 <- 7 < sum(c(max(c(nchar(p_names_new), nchar(lv_names_new))),
    #                        max(nchar(e1$j_names))))
    # } else if (e1$class == "grm") {
    #     flag2 <- 8 < max(nchar(e1$j_names))
    # }
    #
    # if (flag2) {
    #
    #     tmp1 <- floor(log10(e1$J)) + 1
    #     tmp2 <- gsub("\\d+", "", e1$j_names)
    #     tmp2[tmp2 == ""] <- "V"
    #     j_names_new <- paste0(substr(tmp2, 1, 4 - tmp1), 1:e1$J)
    #
    #     names(j_names_new) <- e1$j_names
    #     j_names_new2 <- j_names_new
    #     names(j_names_new2) <- paste0("(?<!\\w)", e1$j_names, "(?!\\w)")
    #
    #     model_list_new$irt <-
    #         stringr::str_replace_all(model_list_new$irt, j_names_new2)
    #     if (!is.null(model_list$addendum)) {
    #         model_list_new$addendum <-
    #             stringr::str_replace_all(model_list_new$addendum, j_names_new2)
    #     }
    # }

    ##### _Update Everything Based on New model_list #####

    if (any(c(flag1, flag2))) {

        irtree_model_irt(model_list_new, e1)
        irtree_model_equations(model_list_new, e1)
        # irtree_model_items(model_list_new, e1)    # not necessary
        irtree_model_subtree(model_list_new, e1)
        irtree_model_addendum(model_list_new, e1)
        irtree_model_constraints(model_list_new, e1)
        irtree_model_mapping(e1 = e1)

        # Update *_names seperately such that names of the character vectors are the old names
        e1$j_names  <- j_names_new
        e1$lv_names <- lv_names_new
        e1$p_names  <- p_names_new
        tmp1 <- setNames(names(j_names_new), nm = j_names_new)
        e1$irt_items <- lapply(e1$irt_items, function(x) {
            names(x) <- stringr::str_replace_all(names(x), tmp1)
            x
        })
        e1$covariates <- setdiff(e1$covariates, c(e1$j_names, e1$lv_names))
    }

    ##### Lambda Matrix #####

    # lambda <- reshape2::melt(e1$irt_items, value.name = "item")
    lambda <- dplyr::bind_rows(
        lapply(
            seq_along(e1$irt_items),
            function(x) {
                data.frame(
                    item = e1$irt_items[[x]],
                    trait = names(e1$irt_items)[x],
                    row.names = NULL, stringsAsFactors = FALSE)
                }))
    lambda$item <- factor(lambda$item, levels = e1$j_names)
    # names(lambda) <- sub("L1", "trait", names(lambda))
    lambda$trait <- factor(lambda$trait, levels = e1$lv_names)
    lambda$loading <- ifelse(is.na(unlist(e1$irt_loadings)), "*", unlist(e1$irt_loadings))

    lambda$p <- lambda$trait
    if (nrow(e1$subtree) > 0) {
        tmp1 <- as.character(e1$subtree$trait)
        names(tmp1) <- e1$subtree$facet
        levels(lambda$p) <- stringr::str_replace_all(levels(lambda$trait), tmp1)
    }
    lambda$p <- factor(lambda$p, levels = e1$p_names)

    lambda <- lambda[order(lambda$p, lambda$trait, lambda$item), ]
    lambda$trait <- factor(lambda$trait, levels = unique(lambda$trait))

    if (e1$class == "tree") {
        lambda$new_name <- glue::glue_data(lambda, "{p}_{item}")
    } else if (e1$class == "grm") {
        lambda$new_name <- glue::glue_data(lambda, "{item}")
    }
    lambda$mplus <- glue::glue_data(lambda, "{new_name}{loading}")

    lambda$label <- paste0("a", 1:nrow(lambda))

    e1$lv_names <- sort2(e1$lv_names, levels(lambda$trait))

    # tmp1 <- aggregate(loading ~ trait, data = lambda,
    #                   function(x) any(grepl(x = x, pattern = "@\\d+", perl = TRUE)))
    # if (any(tmp1$loading == FALSE)) {
    #     message("At least one loading for each trait must be fixed im Mplus, (e.g., @1). ",
    #          "Please fix the following: ",
    #          paste(tmp1[!tmp1$loading, "trait"], collapse = ", "), ".")
    # }

    e1$lambda <- lambda
    e1$proper_model <- TRUE

    out1 <- as.list(e1)
    out1 <- out1[order(names(out1))]
    class(out1) <- c("irtree_model", "list")

    tmp1 <- environment()

    ##### Test if probabilities sum to 1 #####

    if (!is.null(model_list$equations)) {
        tryCatch(irtree_sim_data(object = out1, N = 1, sigma = diag(e1$S),
                                 itempar = list(beta  = matrix(stats::rnorm(e1$J*e1$P), e1$J, e1$P),
                                                alpha = matrix(stats::rnorm(e1$J*e1$P), e1$J, e1$P))
                                 # , K = ifelse(is.null(e1$K), NULL, e1$K)
                                 ),
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
    cat(x$string, ...)
}
