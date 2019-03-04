tree_model_irt <- function(model_list = NULL) {

    irt0 <- trimws(strsplit(paste(model_list$irt, collapse = " "), ";")[[1]])
    missing_sc <- stringr::str_count(irt0, "(?i)\\s+BY\\s+") > 1
    if (any(missing_sc)) {
        stop("Error in model: Every definition in section 'IRT' must end with a ';'. ",
             "Problem with:\n", clps("\nS", irt0[missing_sc]), call. = FALSE)
    }

    tmp1 <- strsplit(paste(model_list$irt, collapse = " "), "(?i)\\s+BY\\s+")[[1]]
    missing_BY <- stringr::str_count(tmp1, ";") > 1
    if (any(missing_BY)) {
        stop("Error in model: Every definition in section 'IRT' must contain the keyword 'BY'. ",
             "Problem with:\n", clps("\n", tmp1[missing_BY]), call. = FALSE)
    }

    irt1 <- vapply(irt0,
                   function(x)
                       strsplit(x, split =  "(?i)\\s+by\\s+", perl = TRUE)[[1]],
                   FUN.VALUE = character(2), USE.NAMES = FALSE)
    lv_names <- irt1[1, ]

    tmp1 <- grepl("[^[:alnum:]_]", x = lv_names, perl = TRUE)
    if (any(tmp1)) {
        stop("Variable names may only contain letters, digits, ",
             "and the underscore '_': ", clps(", ", lv_names[tmp1]), call. = FALSE)
    }
    # S <- out1$S <- length(lv_names)
    S <- length(lv_names)

    irt4 <- lapply(irt1[2, ],
                   function(x) strsplit(x, split =  "[^[:alnum:]*]+\\s+|\\s+", perl = TRUE)[[1]])
    irt_loadings <- lapply(irt4,
                   vapply,
                   stringr::str_extract, pattern = "@\\d+$|[*]$",
                   FUN.VALUE = "")
    irt_items <- lapply(irt4,
                       vapply,
                       sub, pattern = "@\\d+$|[*]$", replacement = "",
                       FUN.VALUE = "")
    names(irt_items) <- lv_names
    names(irt_loadings) <- lv_names

    return(list(irt_items = irt_items,
                irt_loadings = irt_loadings,
                S = S,
                lv_names = lv_names))

}

tree_model_equations <- function(model_list = NULL) {

    equations <- vapply(
        model_list$equations,
        function(x) strsplit(x, "\\s*[=]\\s*")[[1]],
        FUN.VALUE = character(2), USE.NAMES = FALSE)

    eqs2 <- lapply(equations[2, ], function(x) do.call(parse, list(text = x))[[1]])
    names(eqs2) <- equations[1, ]
    # out1$K <- length(eqs2)
    # out1$expr <- eqs2

    return(list(equations = equations,
                expr = eqs2))
}

tree_model_dimensions <- function(model_list = NULL, lv_names = NULL) {

    tmp1 <- paste(model_list$processes, collapse = " ")

    s_names <- strsplit(tmp1, "[^[:alnum:]]+\\s+")[[1]]

    flag1 <- sym_diff(s_names, lv_names)
    if (length(flag1) > 0) {
        stop("Error in 'model': All processes in 'IRT' must be present in 'Processes' ",
             "and vice versa. Problem with ", paste(flag1, collapse = ", "), ".", call. = FALSE)
    }
    return(s_names)
}

tree_model_items <- function(irt_items = NULL) {
    j_names <- gtools::mixedsort(unique(unlist(irt_items, use.names = F)))
    # out1$J <- J <- length(j_names)

    tmp1 <- grepl("[^[:alnum:]_]", j_names, perl = TRUE)
    if (any(tmp1)) {
        stop("Variable names may only contain letters, digits, ",
             "and the underscore '_'. Problem with: ",
             paste(j_names[tmp1], collapse = ", "), call. = FALSE)
    }
    return(j_names)
}

tree_model_subtree <- function(model_list = NULL,
                               s_names = NULL) {

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
    tmp1 <- s_names
    for (ii in seq_len(nrow(subtree))) {
        tmp1 <- gsub(subtree[ii, 2], subtree[ii, 1], tmp1)
    }
    p_names <- unique(tmp1)

    return(list(subtree = subtree,
                p_names = p_names))
}
