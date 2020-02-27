# Notation:
# P: Number of different processes in the tree diagram (e.g., P=3 in the
#    classical tree for 5-point items)
# S: Number of latent dimensions (e.g., S=7 for the classical tree for 5-point
#    items applied to Big-5 data)

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

    tmp1 <- grepl("[^[:alnum:]_]", x = lv_names, perl = TRUE)
    if (any(tmp1)) {
        stop("Variable names may only contain letters, digits, ",
             "and the underscore '_': ", clps(", ", lv_names[tmp1]), call. = FALSE)
    }
    S <- length(lv_names)

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
    # irt_items <- lapply(irt_items, function(x) {names(x) <- x; x})
    irt_items <- lapply(irt_items, unname)
    names(irt_items) <- lv_names
    names(irt_loadings) <- lv_names

    checkmate::assert_character(lv_names, min.chars = 1, any.missing = FALSE,
                                min.len = 1, unique = TRUE)
    lapply(irt_items, checkmate::assert_character,
           min.chars = 1, any.missing = FALSE, min.len = 1,
           unique = TRUE, .var.name = "irt_items")

    latent_names <- data.frame(irt   = lv_names,
                               mpt   = lv_names,
                               theta = lv_names,
                               stringsAsFactors = FALSE)

    rlang::env_bind(e1,
                    irt_items = irt_items,
                    irt_loadings = irt_loadings,
                    S = S,
                    latent_names = latent_names)
}

irtree_model_equations <- function(model_list = NULL, e1 = new.env()) {

    if (is.null(model_list$equations)) {
        return(invisible(NULL))
    }

    e1$equations <- vapply(
        gsub("\\s+", "", model_list$equations),
        function(x) strsplit(x, "[=]")[[1]],
        FUN.VALUE = character(2), USE.NAMES = FALSE)

    e1$expr <- lapply(e1$equations[2, ], function(x) do.call(parse, list(text = x))[[1]])
    names(e1$expr) <- e1$equations[1, ]
    e1$K <- length(e1$expr)
    e1$k_names <- as.integer(e1$equations[1, ])
    tmp1 <- diff(sort(e1$k_names))
    if (any(tmp1 != 1)) {
        stop("Categories in the model equations must be consecutive integers.")
    }

    checkmate::assert_matrix(e1$equations, mode = "character",
                             nrows = 2, ncols = e1$K)
    checkmate::assert_character(e1$equations, min.chars = 1)
    checkmate::assert_integerish(as.numeric(e1$equations[1, ]), any.missing = FALSE,
                                 unique = TRUE, .var.name = "lhs of equations")
}

irtree_model_check_equations <- function(equations = NULL, mpt_names = NULL) {
    checkmate::assert_matrix(equations, mode = "character", min.cols = 2, nrows = 2)

    tmp1 <- stringr::str_extract_all(
        equations[2, , drop = TRUE], pattern = "[:alpha:]+") %>%
        vapply(. %>% duplicated %>% any, FUN.VALUE = TRUE)
    if (any(tmp1)) {
        stop("Each model equation may contain each parameter only once. ",
             "Problem with: ", clps(", ", "'", equations[2, tmp1], "'", sep = ""), ".")
    }

    tmp1 <- stringr::str_split(equations[2, , drop = TRUE], pattern = "[*]") %>%
        lapply(stringr::str_replace, clps("|", mpt_names), "") %>%
        lapply(stringr::str_replace_all, "\\(|\\)|1-", "") %>%
        vapply(. %>% nchar %>% magrittr::is_greater_than(0) %>% any, FUN.VALUE = TRUE)

    if (any(tmp1)) {
        stop("Each model equation may contain parameters 'p' and '1-p' and ",
             "possibly products thereof but nothing else. Use only equations ",
             "along the lines of a*(1-b). ",
             "Problem with: ", clps(", ", "'", equations[2, tmp1], "'", sep = ""), ".")
    }

}

irtree_model_items <- function(e1 = new.env()) {
    # e1$j_names <- gtools::mixedsort(unique(unlist(e1$irt_items, use.names = F)))
    e1$j_names <- unique(unlist(e1$irt_items, use.names = F))
    e1$J <- length(e1$j_names)

    tmp1 <- grepl("[^[:alnum:]_]", e1$j_names, perl = TRUE)
    if (any(tmp1)) {
        stop("Problem in 'model': Variables must be seperated by commas. ",
             "Variable names may only contain letters, digits, ",
             "and the underscore. Problem with: ",
             paste0("'", e1$j_names[tmp1], "'", collapse = ", "), ".", call. = FALSE)
    }
    checkmate::assert_character(e1$j_names, unique = TRUE, min.chars = 1,
                                any.missing = FALSE, names = "unnamed")
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

        e1$covariates <- setdiff(tmp1, c(e1$j_names, e1$latent_names$irt))
    } else {
        return(invisible(NULL))
    }
}

irtree_model_constraints <- function(model_list = NULL, e1 = new.env()) {
    if (!is.null(model_list$constraints)) {
        constr <- split(model_list$constraints,
                        ifelse(
                            grepl("\\|", model_list$constraints),
                            "sub", "equ"))
        if (any(grepl("[+]", model_list$constraints))) {
            stop("Section Constraints must not contain the symbol +. ",
                 "See: ?irtree_model.", call. = FALSE)
        }
    } else {
        constr <- NULL
    }

    irtree_model_constr_equ(constr$equ, e1 = e1)
    irtree_model_constr_sub(constr$sub, e1 = e1)

    return(invisible(NULL))
}

irtree_model_constr_equ <- function(equ = NULL, e1 = new.env()) {
    if (!is.null(equ)) {

        lv_names <- unique(e1$latent_names$irt)
        tmp1 <- paste0("^(", clps("|", lv_names),
                       ")=(", clps("|", lv_names), ")$")
        tmp2 <- vapply(equ, gsub,
                       pattern = "\\s+",
                       replacement = "", FUN.VALUE = character(1))
        if (any(!stringr::str_detect(tmp2, tmp1))) {
            stop("Problem in model: Constraints must be specified in the form of: ",
                 "Name_of_LV = Name_of_LV")
        }
        names(tmp2) <- NULL

        f1 <- function(x) {
            y <- x[1]
            names(y) <- paste0("^", x[2], "$")
            return(y)
        }

        e1$constraints <- unlist(lapply(strsplit(tmp2, "="), f1))
        e1$latent_names$theta <- stringr::str_replace_all(
            e1$latent_names$theta,
            e1$constraints)

        e1$S <- length(unique(e1$latent_names$theta))

    } else {
        return(invisible(NULL))
    }
}

irtree_model_constr_sub <- function(sub = NULL, e1 = new.env()) {

    if (!is.null(sub)) {
        subtree1 <- vapply(sub,
                           function(x) strsplit(x, "\\s*[=]\\s*")[[1]],
                           FUN.VALUE = character(2))
        subtree2 <- subtree1[1, ]
        subtree3 <- vapply(subtree1[2, ],
                           gsub, pattern = "\\s*\\|\\s*", replacement = "|",
                           FUN.VALUE = "")
        tmp1 <- strsplit(subtree3, "|", TRUE)[[1]]
        lapply(tmp1,
               checkmate::assert_subset, choices = e1$latent_names$irt,
               .var.name = "Subtree")
        subtree <- data.frame(trait = subtree2, facet = subtree3, row.names = NULL)

        if (e1$class == "pcm") {
            # Apply subtree structure to weights; that is
            # (t = 0:4) together with (t = a1 + a2) becomes
            # (a1 = 0:4, a2 = 0:4)
            e1$weights <- subtree %>%
                dplyr::mutate(trait = as.character(.data$trait)) %>%
                dplyr::right_join(tibble::enframe(e1$weights, "trait", "weights"),
                                  by = "trait") %>%
                tidyr::separate_rows(.data$facet, sep = "\\|") %>%
                dplyr::mutate(facet = dplyr::coalesce(.data$facet, .data$trait)) %>%
                {tibble::deframe(x = .[, 2:3])}
        }
    } else {
        subtree <- data.frame()
    }
    p_names <- e1$latent_names$irt
    for (ii in seq_len(nrow(subtree))) {
        p_names <- gsub(subtree[ii, 2], subtree[ii, 1], p_names)
    }
    e1$latent_names$mpt     <- p_names
    e1$P       <- length(unique(p_names))
    e1$subtree <- subtree

    ### names of MPT-parameters ###

    if (e1$class == "tree") {
        mpt_names <-
            all.vars(
                as.formula(
                    paste("~", e1$equations[2, ], collapse = " + ")))

        flag1 <- sym_diff(p_names, mpt_names)
        if (length(flag1) > 0) {
            stop("Problem in 'model': All parameters in 'Equations' must be present in 'IRT' ",
                 "combined with 'Constraints 'and vice versa. Problem with ",
                 paste(flag1, collapse = ", "), ".", call. = FALSE)

        }
    } else if (e1$class == "pcm") {
        flag1 <- sym_diff(names(e1$weights), e1$latent_names$irt)
        if (length(flag1) > 0) {
            stop("Problem in 'model': All parameters in 'Weights' must be present in 'IRT' ",
                 "combined with 'Constraints 'and vice versa. Problem with ",
                 paste(flag1, collapse = ", "), ".", call. = FALSE)
        }

    }
}

irtree_model_mapping <- function(e1 = new.env()) {

    if (!is.null(e1$equations)) {

        p_names <- unique(e1$latent_names$mpt)

        mapping_matrix <- matrix(NA, e1$K, e1$P,
                                 dimnames = list(NULL, p_names))
        mapping_matrix <- cbind(cate = e1$k_names, mapping_matrix)

        for (ii in seq_len(e1$P)) {
            pseudoitem <- ifelse(vapply(e1$equations[2, ],
                                        grepl,
                                        pattern = p_names[ii], perl = TRUE,
                                        FUN.VALUE = logical(1)),
                                 no = NA,
                                 yes = ifelse(vapply(e1$equations[2, ],
                                                     grepl,
                                                     pattern = paste0("(?<!-)", p_names[ii]),
                                                     perl = TRUE,
                                                     FUN.VALUE = logical(1)),
                                              yes = 1L, no = 0L))

            mapping_matrix[, p_names[ii]] <- pseudoitem
            e1$mapping_matrix <- mapping_matrix
        }
    } else {
        return(invisible(NULL))
    }
}

irtree_model_weights <- function(model_list = NULL, e1 = new.env()) {
    if (is.null(model_list$weights)) {
        return(invisible(NULL))
    }
    w_vec <- unlist(strsplit(model_list$weights, "\\n+"))
    w_vec <- strsplit(w_vec, "\\s?=\\s?")
    weights <- lapply(w_vec, function(x) eval(parse(text = x[2])))
    names(weights) <- vapply(w_vec, `[[`, 1, FUN.VALUE = "a")
    e1$K <- length(weights[[1]])
    e1$k_names <- seq.int(0, length.out = e1$K)
    checkmate::qassertr(weights, paste0("N", e1$K, "(,)"))
    e1$weights <- weights
}
