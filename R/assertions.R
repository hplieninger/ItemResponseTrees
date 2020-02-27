assert_irtree_data <- function(data = NULL,
                               object = NULL,
                               engine = NULL,
                               set_min_to_0 = FALSE) {

    ### Is it a data frame? ###

    checkmate::assert_data_frame(data, all.missing = FALSE, min.rows = 1)

    ### Has it the correct colnames? ###

    if (engine == "mplus") {
        checkmate::assert_character(object$covariates, min.chars = 1,
                                    pattern = "^[[:alpha:]][[:alnum:]_]*$",
                                    any.missing = FALSE, unique = TRUE,
                                    null.ok = TRUE, .var.name = "Addendum in object")

        # assert_nchar(object$j_names, 8)
        assert_nchar(object$covariates, 8)
        checkmate::assert_subset(x = c(object$j_names, object$covariates),
                                 choices = names(data),
                                 empty.ok = FALSE, .var.name = "variable names")
    } else {
        checkmate::assert_set_equal(names(data), object$j_names)
    }

    ### Are all vars integers? ###

    checkmate::assert_data_frame(data[object$j_names],
                                 types = "integerish")

    ### Is the range of the vars in line with 'object'? ###

    # tam: is the minimum == 0?
    #      test not necessary for tree, because I recode the data

    categ_dat <- unique(unlist(data[object$j_names], use.names = FALSE))

    if (object$class == "tree") {
        if (length(sym_diff(categ_dat, object$k_names)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'object' has equations for categories ", clps(", ", object$k_names), "."
                 , call. = FALSE)
        }
    } else if (object$class == "pcm" && engine == "tam") {
        if (length(sym_diff(categ_dat, object$k_names)) > 0) {
            if (min(data[object$j_names] != 0, na.rm = TRUE)) {
                stop("Minimum of data is not equal to zero. ",
                     "You should recode your data or set 'set_min_to_0 = TRUE'.",
                     call. = FALSE)
            }
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'object' has weights for categories ", clps(", ", object$k_names), "."
                 , call. = FALSE)
        }
    } else if (object$class == "grm") {
        # No test possible, because 'object' contains no information about the
        # number of categories
    }
}

assert_irtree_equations <- function(object = NULL) {
    if (!is.null(object$equations)) {
        irtree_model_check_equations(object$equations, unique(object$latent_names$mpt))
    }
}

assert_irtree_proper <- function(object = NULL, improper_okay = FALSE) {
    checkmate::qassert(improper_okay, "B1")
    if (improper_okay == FALSE & object$proper_model == FALSE) {
        stop("The model seems to be an improper model. You might set ",
             "'improper_okay' to TRUE, but do this only if you really ",
             "know what you are doing.")
    }
}

check_nchar <- function(x, max.chars = 8, any.missing = FALSE) {
    nchars <- nchar(x)
    if (!isFALSE(any(nchars > max.chars, na.rm = any.missing))) {
        paste("All elements must have at most", max.chars, "characters.",
              "Longest element:", x[which.max(nchars)])
    } else {
        TRUE
    }
}
assert_nchar <- checkmate::makeAssertionFunction(check_nchar)
