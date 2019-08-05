irtree_model_irt <- function(model_list = NULL, e1 = new.env()) {

    irt0 <- trimws(strsplit(paste(model_list$irt, collapse = " "), ";")[[1]])
    missing_sc <- stringr::str_count(irt0, "(?i)\\s+BY\\s+") > 1
    if (any(missing_sc)) {
        stop("Problem in model: Every definition in section 'IRT' must end with a ';'. ",
             "Problem with:\n", clps("\n", "'", irt0[missing_sc], "'", sep = ""),
             call. = FALSE)
    }

    tmp1 <- strsplit(paste(model_list$irt, collapse = " "), "(?i)\\s+BY\\s+")[[1]]
    missing_BY <- stringr::str_count(tmp1, ";") > 1
    if (any(missing_BY)) {
        stop("Problem in model: Every definition in section 'IRT' must contain the keyword 'BY'. ",
             "Problem with:\n", clps("\n", tmp1[missing_BY]), call. = FALSE)
    }

    irt1 <- vapply(irt0,
                   function(x)
                       strsplit(x, split =  "(?i)\\s+by\\s+", perl = TRUE)[[1]],
                   FUN.VALUE = character(2), USE.NAMES = FALSE)

    # Aggregate multiple by statements for the same LV, e.g.,
    # F1 by item1, item2;
    # F1 by item3;
    irt2 <- data.frame(t(irt1), stringsAsFactors = FALSE)
    irt2$X1 <- factor(irt2$X1, levels = unique(irt1[1, ]))
    irt3 <- aggregate(. ~ X1, irt2, paste, collapse = ", ")

    lv_names <- irt1[1, ]
    lv_names <- levels(irt3$X1)
    names(lv_names) <- lv_names

    tmp1 <- grepl("[^[:alnum:]_]", x = lv_names, perl = TRUE)
    if (any(tmp1)) {
        stop("Variable names may only contain letters, digits, ",
             "and the underscore '_': ", clps(", ", lv_names[tmp1]), call. = FALSE)
    }
    # S <- out1$S <- length(lv_names)
    S <- length(lv_names)

    # irt4 <- lapply(# irt1[2, ],
    #                irt3$X2,
    #                function(x) strsplit(x, split =  "[^[:alnum:]*]+\\s+|\\s+", perl = TRUE)[[1]])

    irt4 <- stringr::str_split(irt3$X2, ",\\s*")

    if (any(vapply(irt4, length, FUN.VALUE = integer(1)) == 1)) {
        if (any(vapply(irt4, function(x) any(stringr::str_detect(x, pattern = "\\s+")), logical(1)))) {
            stop("Problem in 'model'. Variables in section IRT must be ",
                 "separated by commas.")
        }
    }

    irt_loadings <- lapply(irt4,
                   vapply,
                   stringr::str_extract, pattern = "@\\d+$|[*]$",
                   FUN.VALUE = "")
    irt_items <- lapply(irt4,
                       vapply,
                       sub, pattern = "@\\d+$|[*]$", replacement = "",
                       FUN.VALUE = "")
    irt_items <- lapply(irt_items, function(x) {names(x) <- x; x})
    names(irt_items) <- lv_names
    names(irt_loadings) <- lv_names

    checkmate::assert_character(lv_names, min.chars = 1, any.missing = FALSE,
                                min.len = 1, unique = TRUE, names = "unique")
    lapply(irt_items, checkmate::assert_character,
           min.chars = 1, any.missing = FALSE, min.len = 1,
           unique = TRUE, .var.name = "irt_items")

    rlang::env_bind(e1,
                    irt_items = irt_items,
                    irt_loadings = irt_loadings,
                    S = S,
                    lv_names = lv_names)
}

irtree_model_equations <- function(model_list = NULL, e1 = new.env()) {

    if (is.null(model_list$equations)) {
        return(invisible(NULL))
    }

    e1$equations <- vapply(
        gsub("\\s+", "", model_list$equations),
        # function(x) strsplit(x, "\\s*[=]\\s*")[[1]],
        function(x) strsplit(x, "[=]")[[1]],
        FUN.VALUE = character(2), USE.NAMES = FALSE)

    e1$expr <- lapply(e1$equations[2, ], function(x) do.call(parse, list(text = x))[[1]])
    names(e1$expr) <- e1$equations[1, ]
    e1$K <- length(e1$expr)



    checkmate::assert_matrix(e1$equations, mode = "character",
                             nrows = 2, ncols = e1$K)
    checkmate::assert_character(e1$equations, min.chars = 1)
    checkmate::assert_integerish(as.numeric(e1$equations[1, ]), any.missing = FALSE,
                                 unique = TRUE, .var.name = "lhs of equations")
}

irtree_model_check_equations <- function(equations = NULL, p_names = NULL) {
    checkmate::assert_matrix(equations, mode = "character", min.cols = 2, nrows = 2)

    tmp1 <- stringr::str_extract_all(
        equations[2, , drop = TRUE], pattern = "[:alpha:]+") %>%
        vapply(. %>% duplicated %>% any, FUN.VALUE = TRUE)
    if (any(tmp1)) {
        stop("Each model equation may contain each parameter only once. ",
             "Problem with: ", clps(", ", "'", equations[2, tmp1], "'", sep = ""), ".")
    }

    tmp1 <- stringr::str_split(equations[2, , drop = TRUE], pattern = "[*]") %>%
        lapply(stringr::str_replace, clps("|", p_names), "") %>%
        lapply(stringr::str_replace_all, "\\(|\\)|1-", "") %>%
        vapply(. %>% nchar %>% magrittr::is_greater_than(0) %>% any, FUN.VALUE = TRUE)

    # tmp1 <- stringr::str_split(equations[2, , drop = TRUE], pattern = "[*]") %>%
    #     lapply(stringr::str_replace, "1-", "") %>%
    #     lapply(stringr::str_detect, "\\+|/|-") %>%
    #     vapply(any, TRUE)
    if (any(tmp1)) {
        stop("Each model equation may contain parameters 'p' and '1-p' and ",
             "possibly products thereof but nothing else. Use only equations ",
             "along the lines of a*(1-b). ",
             "Problem with: ", clps(", ", "'", equations[2, tmp1], "'", sep = ""), ".")
    }

}

# irtree_model_dimensions <- function(model_list = NULL, e1 = new.env()) {
#
#     tmp1 <- paste(model_list$processes, collapse = " ")
#
#     s_names <- strsplit(tmp1, "[^[:alnum:]]+\\s+")[[1]]
#
#     flag1 <- sym_diff(s_names, e1$lv_names)
#     if (length(flag1) > 0) {
#         stop("Problem in 'model': All processes in 'IRT' must be present in 'Processes' ",
#              "and vice versa. Problem with ", paste(flag1, collapse = ", "), ".", call. = FALSE)
#     }
#     # return(s_names)
# }

irtree_model_items <- function(e1 = new.env()) {
    # e1$j_names <- gtools::mixedsort(unique(unlist(e1$irt_items, use.names = F)))
    e1$j_names <- unique(unlist(e1$irt_items, use.names = F))
    names(e1$j_names) <- e1$j_names
    e1$J <- length(e1$j_names)

    tmp1 <- grepl("[^[:alnum:]_]", e1$j_names, perl = TRUE)
    if (any(tmp1)) {
        stop("Problem in 'model': Variables must be seperated by commas. ",
             "Variable names may only contain letters, digits, ",
             "and the underscore. Problem with: ",
             paste0("'", e1$j_names[tmp1], "'", collapse = ", "), ".", call. = FALSE)
    }
    checkmate::assert_character(e1$j_names, unique = TRUE, min.chars = 1,
                                any.missing = FALSE, names = "unique")
}

irtree_model_subtree <- function(model_list = NULL, e1 = new.env()) {

    if (!is.null(model_list$subtree)) {
        subtree1 <- vapply(model_list$subtree,
                           function(x) strsplit(x, "\\s*[=]\\s*")[[1]],
                           FUN.VALUE = character(2))
        subtree2 <- subtree1[1, ]
        subtree3 <- vapply(subtree1[2, ],
                           gsub, pattern = "\\s*[+]\\s*", replacement = "|",
                           FUN.VALUE = "")
        subtree <- data.frame(trait = subtree2, facet = subtree3, row.names = NULL)
    } else {
        subtree <- data.frame()
    }
    tmp1 <- e1$lv_names
    for (ii in seq_len(nrow(subtree))) {
        tmp1 <- gsub(subtree[ii, 2], subtree[ii, 1], tmp1)
    }
    e1$p_names <- sort2(unique(tmp1), as.character(subtree$trait), subset = FALSE)
    names(e1$p_names) <- e1$p_names
    e1$P       <- length(e1$p_names)
    e1$subtree <- subtree

    checkmate::assert_character(e1$p_names, unique = TRUE, min.chars = 1,
                                any.missing = FALSE, names = "unique")
    checkmate::assert_data_frame(data.frame(), types = "character",
                                 any.missing = FALSE, max.cols = 2)

    ### names of MPT-parameters ###

    if (e1$class == "tree") {
        mpt_names <-
            all.vars(
                as.formula(
                    paste("~", e1$equations[2, ], collapse = " + ")))

        flag1 <- sym_diff(rlang::env_get(e1, "p_names"), mpt_names)
        if (length(flag1) > 0) {
            stop("Problem in 'model': All parameters in 'Equations' must be present in 'IRT' ",
                 "combined with 'Subtree 'and vice versa. Problem with ",
                 paste(flag1, collapse = ", "), ".", call. = FALSE)
        }
    }
}

irtree_model_addendum <- function(model_list = NULL, e1 = new.env()) {
    if (!is.null(model_list$addendum)) {
        e1$addendum <- model_list$addendum

        # Replace: with, by, on
        tmp1 <- stringr::str_replace_all(e1$addendum, "(?i)(?<!\\w)(with|by|on)(?!\\w)", " ")
        # Replace: () and []
        tmp1 <- stringr::str_replace_all(tmp1, "\\(.+\\)", " ")
        tmp1 <- stringr::str_replace_all(tmp1, "\\[.+\\]", " ")
        # Replace: laodings (@), starting values (*), and thresholds ($)
        ### tmp1 <- stringr::str_replace_all(tmp1,   "@\\s*(\\d+|(\\.\\d+))", " ")
        ### tmp1 <- stringr::str_replace_all(tmp1, "\\*\\s*(\\d+|(\\.\\d+))", " ")
        ### tmp1 <- stringr::str_replace_all(tmp1, "\\(.+\\)", " ")
        tmp1 <- unlist(stringr::str_split(tmp1, "\\b"))
        # Extract variable names, i.e., anything starting with a letter
        tmp1 <- unique(stringr::str_subset(tmp1, "[:alpha:][[:alnum:]_]*"))

        # e1$covariates <- stringr::str_subset(tmp1,
        #                                      paste0("^(",
        #                                             clps("|", c(names(e1$j_names),
        #                                                         names(e1$lv_names))),
        #                                             ")$"),
        #                                      negate = TRUE)
        e1$covariates <- setdiff(tmp1, c(e1$j_names, e1$lv_names))
    } else {
        return(invisible(NULL))
    }
}

irtree_model_constraints <- function(model_list = NULL, e1 = new.env()) {
    if (!is.null(model_list$constraints)) {
        tmp1 <- paste0("^(", clps("|", e1$lv_names),
                       ")=(", clps("|", e1$lv_names), ")$")
        tmp2 <- vapply(model_list$constraints, gsub,
                                 pattern = "\\s+",
                                 replacement = "", FUN.VALUE = character(1))
        if (any(!stringr::str_detect(tmp2, tmp1))) {
            stop("Problem in model: Constraints must be specified in the form of: ",
                 "Name_of_LV = Name_of_LV")
        }
        names(tmp2) <- NULL
        e1$constraints <-
            unlist(
                lapply(strsplit(tmp2, "="),
                       function(x) {y <- x[1]; names(y) <- x[2]; y}))
    } else {
        return(invisible(NULL))
    }
}

irtree_model_mapping <- function(e1 = new.env()) {

    if (!is.null(e1$equations)) {

        mapping_matrix <- matrix(NA, e1$K, e1$P, dimnames = list(NULL, e1$p_names))
        mapping_matrix <- cbind(cate = seq_len(e1$K), mapping_matrix)

        for (ii in seq_along(e1$p_names)) {
            pseudoitem <- ifelse(vapply(e1$equations[2, ],
                                        grepl,
                                        pattern = e1$p_names[ii], perl = TRUE,
                                        FUN.VALUE = logical(1)),
                                 no = NA,
                                 yes = ifelse(vapply(e1$equations[2, ],
                                                     grepl,
                                                     pattern = paste0("(?<!-)", e1$p_names[ii]),
                                                     perl = TRUE,
                                                     FUN.VALUE = logical(1)),
                                              yes = 1L, no = 0L))

            mapping_matrix[, e1$p_names[ii]] <- pseudoitem
            e1$mapping_matrix <- mapping_matrix
        }
    } else {
        return(invisible(NULL))
    }
}
