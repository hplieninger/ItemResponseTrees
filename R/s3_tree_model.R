#' ItemResponseTree Model Syntax
#'
#' The ItemResponseTree model syntax describes an IR-tree model. The function
#' \code{tree_model} turns a user-defined model string into a list that
#' represents the full model as needed by the package.
#'
#' @section IRT:
#'
#'   The \code{model} must contain a section with heading IRT. Therein, the IRT
#'   structure of the model is described in a way resembling the MODEL part of
#'   an Mplus input file. It has a structure of \code{LV BY item@1 item*}, where
#'   \code{LV} is the name of the latent variable/parameter/process, \code{item}
#'   is the name of the observed variable in the data set and this is followed
#'   by the loading. The loading may either be fixed (e.g., to 1) using
#'   \code{@1} or it may be set free using \code{*}.
#'
#'   Each measurement model (i.e., the LV and its items) must appear on a
#'   seperate line ending with a semicolon. Items must be seperated by
#'   whitespace. Linebreaks are allowed.
#'
#'   \preformatted{
#'   ## Example
#'   IRT:
#'   t  BY x1@1, x2*, x3*, x4*, x5*, x6*;
#'   e  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;
#'   m  BY x1@1, x2@1, x3@1, x4@1, x5@1, x6@1;}
#'
#' @section Equations:
#'
#'   The \code{model} must contain a section with heading Equations if Class is
#'   Tree.
#'   Therein, the model equations are described.
#'   They have a structure similar to \code{Cat = p1*(1-p2)}, where \code{Cat}
#'   is any observed response category in the data set.
#'   The names of the parameters must be equal to those of the latent variables
#'   in the section IRT.
#'
#'   The equations may contain only products and not sums.
#'   That is, it is not possible to estimate genuine mixture models as, for
#'   example, in the package mpt2irt.
#'
#'   Each equation must appear on a separate, non-broken line.
#'
#'   \preformatted{
#'   ## Example
#'   Equations:
#'   1 = (1-m)*(1-t)*e
#'   2 = (1-m)*(1-t)*(1-e)
#'   3 = m
#'   4 = (1-m)*t*(1-e)
#'   5 = (1-m)*t*e}
#'
#' @section Processes:
#'
#'   The \code{model} must contain a section with heading Processes.
#'   It is essentially a listing of all processes/parameters/latent variables
#'   present in section IRT in the correct order.
#'   That is, the first process in that listing is the first column in the
#'   matrix of person parameters etc.
#'   This section does not contain any new information wrt to the model but
#'   simply safeguards against ambiguities in the model syntax.
#'
#'   It is advised to choose relatively short names for the processes and items,
#'   because Mplus allows only names of up to eight characters.
#'
#'   \preformatted{
#'   ## Example
#'   Processes:
#'   e, m, t}
#'
#' @section Items:
#'
#'   The \code{model} must contain a section with heading Items.
#'   It is essentially a listing of all observed variables present in the data
#'   set in the desired order.
#'   That is, the first, second, ... item in that listing is the first, second,
#'   ... row in the matrix of item parameters etc.
#'   This section does not contain any new information wrt to the model but
#'   simply safeguards against ambiguities in the model syntax.
#'
#'   It is advised to choose relatively short names for the processes and items,
#'   because Mplus allows only names of up to eight characters.
#'
#'   \preformatted{
#'   ## Example
#'   Items:
#'   x1, x2, x3, x4, x5}
#'
#' @section Subtree:
#'
#'   The \code{model} may contain a section with heading Subtree.
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
#'   Each line in this section has a structure of \code{process = subprocess +
#'   subprocess}, where \code{process} is the name of the process used only in
#'   section Equations and \code{subprocess} it the name of the process used only in
#'   section IRT.
#'   Use one line for each definition.
#'
#'   \preformatted{
#'   ## Example
#'   Subtree:
#'   t = t1 + t2 + t3 + t4 + t5}
#'
#' @section Class:
#'
#'   The \code{model} must contain a section with heading Class to specify the
#'   type/class of IRT model to use.
#'   Currently, may be either \code{Tree} or \code{GRM} (graded response model).
#'
#'   \preformatted{
#'   ## Example
#'   Class:
#'   Tree}
#'
#' @section Addendum:
#'
#'   The \code{model} may contain a section with heading Addendum if
#'   \code{backend = "mplus"} is used for estimation.
#'   Any code in this section is directly pasted in the \code{MODEL} section of
#'   the Mplus input file.
#'   Use a semicolon at the end of each line; lines must not exceed 90 characters.
#'   Note that the addendum is ignored in \code{\link{gen_tree_data}}.
#'
#'   \preformatted{
#'   ## Example
#'   Addendum:
#'   e WITH t@0;
#'   m WITH t@0;}
#'
#' @param model Character string with a specific structure as described below.
#' @return List of class \code{tree_model}. It contains the information
#'   extracted from parsing \code{model}.
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
#' Processes:
#' m, e, t1, t2
#'
#' Items:
#' x1, x2, x3, x4, x5, x6
#'
#' Class:
#' Tree
#' "
#'
#' model <- tree_model(m1)
#' @export
tree_model <- function(model = NULL) {

    if (inherits(model, "tree_model")) {
        return(model)
    } else {
        checkmate::assert_string(model, min.chars = 10)
    }

    out1 <- list(string = model)

    model2 <- strsplit(model, "\\s*\\n+")[[1]]
    model2 <- model2[nchar(model2) > 0]
    model2 <- model2[!grepl("^#", model2)]

    ### Extract suparts of 'model' ###

    model_list <- list("irt" = NULL, "equations" = NULL, "processes" = NULL, "subtree" = NULL,
                       "items" = NULL, "class" = NULL, addendum = NULL)
    headings <- na.omit(stringr::str_extract(model2,
                                             paste0("(?i)", names(model_list), collapse = "|")))
    if (any(duplicated(headings))) {
        stop("Error in 'model': Each heading may appear at most once. Problem with: ",
             paste(headings[duplicated(headings)], collapse = ", "), call. = FALSE)
    }
    posx <- c(grep(paste(names(model_list), collapse = "|"), model2, ignore.case = TRUE), length(model2) + 1)

    flag <- integer()

    for (ii in seq_along(model_list)) {
        tmp1 <- grepl(names(model_list)[ii], model2, ignore.case = TRUE)
        if (any(tmp1, na.rm = TRUE)) {
            model_list[[ii]] <- model2[(1 + which(tmp1)):(posx[1 + which(posx == which(tmp1))] - 1)]
        } else {
            flag <- c(flag, ii)
        }
    }
    model_list[flag] <- NULL

        if (is.null(model_list$class)) {
        stop("Argument 'model' must contain a part with heading 'Class'.", call. = FALSE)
    }

    ##### Class #####

    out1$class <- class <-
        match.arg(
            tolower(
                stringr::str_extract(model_list$class, "\\w+")),
            choices = c("tree", "grm"))

    if (is.null(model_list$irt)) {
        stop("Argument 'model' must contain a part with heading 'IRT'.", call. = FALSE)
    }
    if (is.null(model_list$equations) & class == "tree") {
        stop("Argument 'model' must contain a part with heading 'Equations'.", call. = FALSE)
    }
    if (is.null(model_list$processes)) {
        stop("Argument 'model' must contain a part with heading 'Processes'.", call. = FALSE)
    }
    if (is.null(model_list$items)) {
        stop("Argument 'model' must contain a part with heading 'Items'.", call. = FALSE)
    }

    ##### IRT #####

    intermediate <- tree_model_irt(model_list = model_list)

                     irt_items    <- intermediate$irt_items
                     irt_loadings <- intermediate$irt_loadings
    out1$S        <- S            <- intermediate$S
    out1$lv_names <- lv_names     <- intermediate$lv_names

    #
    # # irt1 <- strsplit(tolower(model_list$irt), "\\s*by\\s*")
    # # lv_names <- sapply(irt1, `[[`, 1)
    #
    # irt0 <- trimws(strsplit(paste(model_list$irt, collapse = " "), ";")[[1]])
    # missing_sc <- stringr::str_count(irt0, "(?i)\\s+BY\\s+") > 1
    # if (any(missing_sc)) {
    #     stop("Error in model: Every definition in section 'IRT' must end with a ';'. ",
    #          "Problem with:\n", clps("\nS", irt0[missing_sc]), call. = FALSE)
    # }
    #
    # tmp1 <- strsplit(paste(model_list$irt, collapse = " "), "(?i)\\s+BY\\s+")[[1]]
    # missing_BY <- stringr::str_count(tmp1, ";") > 1
    # if (any(missing_BY)) {
    #     stop("Error in model: Every definition in section 'IRT' must contain the keyword 'BY'. ",
    #          "Problem with:\n", clps("\n", tmp1[missing_BY]), call. = FALSE)
    # }
    #
    # irt1 <- vapply(irt0,
    #                function(x)
    #                    strsplit(x, split =  "(?i)\\s+by\\s+", perl = TRUE)[[1]],
    #                FUN.VALUE = character(2), USE.NAMES = FALSE)
    # lv_names <- irt1[1, ]
    #
    # tmp1 <- grepl("[^[:alnum:]_]", x = lv_names, perl = TRUE)
    # # tmp1 <- stringr::str_detect(c("process_1", "process_t2", "process.e"),
    # #                             pattern = "[^[:alnum:][_]]")
    # if (any(tmp1)) {
    #     stop("Variable names may only contain letters, digits, ",
    #          "and the underscore '_': ", clps(", ", lv_names[tmp1]), call. = FALSE)
    # }
    # S <- out1$S <- length(lv_names)
    #
    # # irt2 <- vapply(irt1, `[[`, 2, FUN.VALUE = "")
    # irt4 <- lapply(irt1[2, ],
    #                function(x) strsplit(x, split =  "[^[:alnum:]*]+\\s+|\\s+", perl = TRUE)[[1]])
    # irt5 <- lapply(irt4,
    #                vapply,
    #                stringr::str_extract, pattern = "@\\d+$|[*]$",
    #                FUN.VALUE = "")
    # irt_list <- lapply(irt4,
    #                    vapply,
    #                    sub, pattern = "@\\d+$|[*]$", replacement = "",
    #                    FUN.VALUE = "")
    # names(irt_list) <- lv_names
    #
    # # irt4 <- vapply(irt2, gsub, pattern = paste0(items, collapse = "|"), replacement = "", FUN.VALUE = "")
    #
    # # irt3 <- vapply(irt2, gsub, pattern = "[@]\\d+|[*]", replacement = "", FUN.VALUE = "")
    # # irt_list <- lapply(irt3, function(x) strsplit(x, "[^[:alnum:]]+")[[1]])
    # # names(irt_list) <- lv_names
    #
    #
    # # items <- unique(unlist(irt_list))
    # # J <- args$J <- length(items)

    ##### Equations #####

    if (!is.null(model_list$equations)) {
        intermediate <- tree_model_equations(model_list = model_list)

        out1$equations <- equations <- intermediate$equations
        out1$expr                   <- intermediate$expr
        out1$K                      <- length(intermediate$expr)
    }

    # if (!is.null(model_list$equations)) {
    #     # if (class == "tree") {
    #     out1$equations <- equations <- vapply(
    #         model_list$equations,
    #         function(x) strsplit(x, "\\s*[=]\\s*")[[1]],
    #         FUN.VALUE = character(2), USE.NAMES = FALSE)
    #
    #     eqs2 <- lapply(equations[2, ], function(x) do.call(parse, list(text = x))[[1]])
    #     names(eqs2) <- equations[1, ]
    #     # out1$K <- K <- length(eqs2)
    #     out1$K <- length(eqs2)
    #     out1$expr <- eqs2
    #     # }
    # }

    ##### Dimensions, ordered #####

    out1$s_names <- s_names <- tree_model_dimensions(model_list = model_list,
                                                     lv_names = lv_names)

    # tmp1 <- paste(model_list$processes, collapse = " ")
    #
    # s_names <- strsplit(tmp1, "[^[:alnum:]]+\\s+")[[1]]
    #
    # flag1 <- sym_diff(s_names, lv_names)
    # if (length(flag1) > 0) {
    #     stop("Error in 'model': All processes in 'IRT' must be present in 'Processes' ",
    #          "and vice versa. Problem with ", paste(flag1, collapse = ", "), ".", call. = FALSE)
    # }
    # # else {
    # #     lv_names <- s_names
    # # }
    # out1$s_names <- s_names

    ##### Items, ordered #####

    out1$j_names <- j_names <- tree_model_items(irt_items = irt_items)
    out1$J       <- J       <- length(j_names)

    # out1$j_names <- j_names <- gtools::mixedsort(unique(unlist(irt_items, use.names = F)))
    # out1$J <- J <- length(j_names)
    #
    # tmp1 <- grepl("[^[:alnum:]_]", j_names, perl = TRUE)
    # if (any(tmp1)) {
    #     stop("Variable names may only contain letters, digits, ",
    #          "and the underscore '_'. Problem with: ",
    #          paste(j_names[tmp1], collapse = ", "), call. = FALSE)
    # }

    # tmp1 <- paste(model_list$items, collapse = " ")
    #
    # out1$j_names <- j_names <- strsplit(tmp1, ";\\s*|,\\s*|\\s+")[[1]]
    # out1$J <- J <- length(j_names)
    #
    # tmp1 <- grepl("[^[:alnum:]_]", j_names, perl = TRUE)
    # if (any(tmp1)) {
    #     stop("Variable names may only contain letters, digits, ",
    #          "and the underscore '_': ", paste(j_names[tmp1], collapse = ", "), call. = FALSE)
    # }
    # irt_list <- lapply(irt_list, sort2, j_names)
    # out1$items <- items <- sort2(unique(unlist(irt_list)), j_names)
    # # out1$items <- items <- factor(items, levels = items)
    #
    # # out1$j_names <- j_names <- factor(j_names, levels = j_names)
    # # out1$J <- J <- length(j_names)
    # # if (J != length(unique(unlist(irt_list)))) {
    # #     stop("Error in 'model'. The number of items in the 'IRT'-part does ",
    # #          "not match the number of items in the 'Processes'-part.", call. = FALSE)
    # # }
    #
    # flag1 <- sym_diff(j_names, items)
    # if (length(flag1) > 0) {
    #     if (is.null(model_list$addendum)) {
    #         stop("Error in 'model': All variables in 'IRT' must be present in 'Items' ",
    #              "and vice versa. Problem with ", paste(flag1, collapse = ", "), ".", call. = FALSE)
    #     } else {
    #         flag2 <- vapply(flag1, function(x) {
    #             any(
    #                 stringr::str_detect(string = model_list$addendum, pattern = x))
    #         }, FUN.VALUE = logical(1))
    #         if (!all(flag2)) {
    #             stop("Error in 'model': All variables in 'IRT' must be present in 'Items' ",
    #                  "or 'Addendum' and vice versa. Problem with ",
    #                  paste(flag1[!flag2], collapse = ", "), ".", call. = FALSE)
    #         }
    #     }
    # }

    ##### Subtree #####

    intermediate <- tree_model_subtree(model_list = model_list,
                                       s_names = s_names)

    out1$subtree  <- subtree  <- intermediate$subtree
    out1$p_names  <- p_names <- intermediate$p_name
    out1$P        <- P       <- length(p_names)

    # if (!is.null(model_list$subtree)) {
    #     subtree1 <- vapply(model_list$subtree,
    #                        function(x) strsplit(x, "\\s*[=]\\s*")[[1]],
    #                        FUN.VALUE = character(2))
    #     subtree2 <- subtree1[1, ]
    #     subtree3 <- vapply(subtree1[2, ],
    #                        gsub, pattern = "\\s*[+]\\s*", replacement = "|",
    #                        FUN.VALUE = "")
    #     subtree <- data.frame(trait = subtree2, facet = subtree3, row.names = NULL)
    # } else {
    #     # subtree2 <- character()
    #     subtree <- data.frame()
    # }
    # # tmp1 <- lv_names
    # tmp1 <- s_names
    # for (ii in seq_len(nrow(subtree))) {
    #     tmp1 <- gsub(subtree[ii, 2], subtree[ii, 1], tmp1)
    # }
    # # out1$p_names <- p_names <- factor(unique(tmp1), levels = unique(tmp1))
    # out1$p_names <- p_names <- unique(tmp1)
    # out1$P <- P <- length(p_names)
    #
    # out1$subtree <- subtree

    ##### Labels for Items and Processes #####

    if (7 < sum(c(max(nchar(p_names)),
                  max(nchar(j_names))))) {
        p_names_new <- paste0(LETTERS[1:P], substr(p_names, 1, 2))
        if (P > 26) {
            stop("Fatal error, please contact package maintainer. ",
                 "Renaming of names of processes only implemented for < 27 processes.")
        }
        if (7 < sum(c(max(nchar(p_names_new)),
                      max(nchar(j_names))))) {
            tmp1 <- floor(log10(J)) + 1
            tmp2 <- gsub("\\d+", "", j_names)
            tmp2[tmp2 == ""] <- "V"
            j_names_new <- paste0(substr(tmp2, 1, 4 - tmp1), 1:J)
        }
    }

    ##### Addendum #####

    if (!is.null(model_list$addendum)) {
        out1$addendum <- model_list$addendum
    }

    # names of MPT-parameters
    if (class == "tree") {
        tmp1 <- lapply(out1$equations[2, ],
                       function(x) all.vars(as.formula(paste0("~", x))))
        mpt_names <- unique(unlist(tmp1))

        flag1 <- sym_diff(p_names, mpt_names)
        if (length(flag1) > 0) {
            stop("Error in 'model': All parameters in 'Equations' must be present in 'IRT' ",
                 "combined with 'Subtree 'and vice versa. Problem with ",
                 paste(flag1, collapse = ", "), ".", call. = FALSE)
        }
    }

    # out1$model_list <- model_list

    lambda <- reshape2::melt(irt_list, value.name = "item")
    lambda$item <- factor(lambda$item, levels = items)
    names(lambda) <- sub("L1", "trait", names(lambda))
    lambda$trait <- factor(lambda$trait, levels = s_names)
    lambda$loading <- ifelse(is.na(unlist(irt5)), "*", unlist(irt5))
    lambda <- lambda[order(lambda$trait, lambda$item), ]

    tmp1 <- aggregate(loading ~ trait, data = lambda,
                      function(x) any(grepl(x = x, pattern = "@\\d+", perl = TRUE)))
    if (any(tmp1$loading == FALSE)) {
        message("At least one loading for each trait must be fixed im Mplus, (e.g., @1). ",
             "Please fix the following: ",
             paste(tmp1[!tmp1$loading, "trait"], collapse = ", "), ".")
    }

    out1$lambda <- lambda

    # rm(list = ls()[!ls() %in% c("out1", "S", "J", "P", "model_list")])

    # stopifnot(is.list(out1))
    class(out1) <- c("list", "tree_model")

    ### Test if probabilities add to 1
    if (!is.null(model_list$equations)) {
        J2 <- length(items)
        tryCatch(gen_tree_data(model = out1, N = 1, sigma = diag(S),
                               itempar = list(beta  = matrix(stats::rnorm(J2*P), J2, P),
                                              alpha = matrix(stats::rnorm(J2*P), J2, P)),
                               K = ifelse(is.null(out1$K), NULL, out1$K)),
                 improper_model = function(cnd) {
                     warning("Equations do not constitute a proper model because ",
                             "they do not sum to 1. ", call. = FALSE)
                 })
    }


    # test1 <- myTryCatch(gen_tree_data(model = out1, N = 2, sigma = diag(S),
    #                                   itempar = list(beta  = matrix(rnorm(J*P), J, P),
    #                                                  alpha = matrix(rnorm(J*P), J, P)),
    #                                   K = ifelse(is.null(out1$K), NULL, out1$K)))
    # if (!is.null(test1$warning)) {
    #     warning(conditionMessage(test1$warning), call. = F)
    # }
    # if (!is.null(test1$error)) {
    #     warning("Error: ", conditionMessage(test1$error), call. = F)
    # }

    return(out1)
}

# @param x Object of class \code{tree_model}.
# @inheritDotParams base::print
# @describeIn tree_model Print method for class tree_model
#' @export
print.tree_model <- function(x, ...) {
    cat(x$string, ...)
}
