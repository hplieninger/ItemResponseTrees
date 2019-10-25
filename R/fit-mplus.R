#' Fit an IR-Tree Model using Mplus
#'
#' This function takes a data frame and a model string and runs the model in Mplus.
#'
#' @param file String naming the file (or path) of the Mplus files and the data
#'   file. Do not provide file endings, those will be automatically appended by
#'   the function.
#' @param quadpts This is passed to argument 'INTEGRATION'. Thus, it may be an
#'   integer specifiying the number of integration points for the Mplus default
#'   of rectangular numerical integration (e.g., `quadpts = 15`). Or it may be a
#'   string, which gives more fine grained control (e.g., `quadpts =
#'   "MONTECARLO(2000)"`).
#' @param estimator String, passed to argument 'ESTIMATOR' in Mplus.
#' @param link String, passed to argument 'LINK' in Mplus. Specifies
#'   the link function.
#' @param cleanup Logical, whether the Mplus files should be removed on exit.
#' @param save_fscores Logical, wheter to save FSCORES or not.
#' @param analysis_list Named list of strings passed to Mplus' argument
#'   ANALYSIS. For example: `analysis_list = list(MITERATIONS = "1000")`.
# @param processors Integer, passed to argument 'PROCESSORS' in Mplus.
#' @param run Logical, whether to indeed run Mplus.
#' @param ... Additional parameters passed to [MplusAutomation::runModels()].
#' @param .warnings2messages Logial, whether Mplus errors and warnings should be
#'   signaled as warnings (the default) or messages.
#' @inheritParams fit.irtree_model
#' @inheritParams MplusAutomation::runModels
#' @inheritParams MplusAutomation::prepareMplusData
#' @return List with two elements. `mplus` contains the Mplus output read into R
#'   via [MplusAutomation::readModels()]. `spec` contains the input
#'   specifications.
#' @examples
#' \donttest{
#' m1 <- "
#' IRT:
#' attitude BY Comfort, Work, Future, Benefit;
#'
#' Class:
#' GRM
#' "
#' model1 <- irtree_model(m1)
#' data(Science, package = "mirt")
#'
#' fit1 <- fit(model1, Science, engine = "mplus")
#' }
#' @export
irtree_fit_mplus <- function(object = NULL,
                             data = NULL,
                             file = tempfile("irtree_"),
                             quadpts = 15,
                             estimator = "MLR",
                             link = c("probit", "logit"),
                             run = TRUE,
                             cleanup = run,
                             save_fscores = TRUE,
                             analysis_list = list(COVERAGE = "0"),
                             verbose = interactive(),
                             replaceOutfile = "always",
                             overwrite = FALSE,
                             ...,
                             .warnings2messages = FALSE) {

    link <- match.arg(link)

    if (run) {
        checkmate::assert_true(MplusAutomation::mplusAvailable() == 0)
    }

    checkmate::assert_list(analysis_list,  types = "character",
                           names = "unique")
    checkmate::qassertr(analysis_list, "S1")
    checkmate::assert_class(object, "irtree_model")

    spec <- c(as.list(environment())
              # , list(...)
    )
    spec$engine <- "mplus"

    checkmate::assert_character(object$covariates, min.chars = 1,
                                pattern = "^[[:alpha:]][[:alnum:]_]*$",
                                any.missing = FALSE, unique = TRUE,
                                null.ok = TRUE, .var.name = "Addendum in object")
    checkmate::assert_subset(x = c(names(object$j_names), object$covariates),
                             choices = names(data),
                             empty.ok = FALSE)
    tmp1 <- checkmate::check_set_equal(names(data),
                                       c(names(object$j_names), object$covariates))
    if (tmp1 != TRUE) {
        rlang::warn(paste0("Assertion on 'names(data)' is suspicious: ",
                           sub("Must", "Expected to", tmp1)),
                    .subclass = "data_has_add_vars")
    }
    check_nchar <- function(x, max.chars = 8, any.missing = TRUE) {
        if (any(nchar(x, allowNA = any.missing) > max.chars)) {
            paste("All elements must have at most", max.chars, "characters")
        } else {
            TRUE
        }
    }
    assert_nchar <- checkmate::makeAssertionFunction(check_nchar)
    assert_nchar(object$j_names, 8)
    assert_nchar(object$covariates, 8)
    checkmate::assert_data_frame(data, all.missing = FALSE, min.rows = 1,
                                 min.cols = object$J + length(object$covariates))
    checkmate::assert_data_frame(data[, names(object$j_names)], types = "integerish",
                                 ncols = object$J)
    data <- tibble::as_tibble(data)
    if (is.numeric(quadpts)) {
        if (quadpts^object$S > 20000) {
            stop("Using that many quadrature points may cause problems.\n",
                 "This error can be disabled by supplying the number of 'quadpts' ",
                 "as a character string (e.g., quadpts = '15').")
        }
    }

    # ellipsis::check_dots_used()

    spec$object$j_names <- object$j_names <- sort2(object$j_names, names(data), TRUE)
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    # CAVE: The following line is super important. It is the only safeguard that
    # assures that the items in the 'MODEL'-statement of the mplus input are in
    # the correct order, namely, that given by the data.
    # This in turn assures that the item thresholds in the output are in that
    # same order---and this is necessary, because otherwise checking the order
    # whilst reading in the output is very cumbersome.
    spec$object$lambda <- object$lambda <- object$lambda[order(object$lambda$trait, object$lambda$item), ]

    ##### file #####

    checkmate::assert_directory_exists(dirname(file), access = "rw")

    dir <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
    file_name <- basename(file)

    data_file <- file.path(dir, paste0(file_name, ".txt"))
    inpu_file <- file.path(dir, paste0(file_name, ".inp"))
    outp_file <- file.path(dir, paste0(file_name, ".out"))
    fsco_file <- file.path(dir, paste0(file_name, ".fsc"))

    if (!overwrite & any(file.exists(data_file, inpu_file, outp_file, fsco_file))) {
        stop("File(s) already exist. Please modify argument 'file' or set 'overwrite' to TRUE.")
    }

    on.exit({
        if (cleanup) {
            suppressWarnings(file.remove(inpu_file, data_file))
            if (run) {
                suppressWarnings(file.remove(outp_file))
                if (save_fscores) {
                    suppressWarnings(file.remove(fsco_file))
                }
            }
        }

    }, add = TRUE)

    ##### Pseudoitems #####

    if (object$class == "tree") {
        categ_dat <- unique(na.omit(unlist(data[, names(object$j_names)], use.names = FALSE)))
        categ_mod <- as.integer(names(object$expr))
        if (length(sym_diff(categ_dat, categ_mod)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'object' has equations for categories ", clps(", ", categ_mod), "."
                 , call. = FALSE)
        }
        pseudoitems <- irtree_recode(object = object, data = data, keep = TRUE)
    } else if (object$class == "grm") {
        pseudoitems <- data
        tmp1 <- object$j_names
        names(tmp1) <- paste0("^", names(tmp1), "$")
        names(pseudoitems) <- stringr::str_replace_all(names(pseudoitems), tmp1)
        # attr(pseudoitems, "pseudoitem_names") <- object$j_names
    } else {
        .stop_not_implemented()
    }

    ##### Mplus Input #####

    tmp1 <- suppressMessages(
        write_mplus_input(
            object        = object,
            # lambda       = object$lambda,
            # addendum     = object$addendum,
            pseudoitems   = pseudoitems,
            data_file     = data_file,
            quadpts       = quadpts,
            estimator     = estimator,
            link          = link,
            save_fscores  = save_fscores,
            fsco_file     = basename(fsco_file),
            # processors    = processors,
            analysis_list = analysis_list))

    mplus_input <- tmp1$mplus_input
    spec$object$lambda <- object$lambda <- tmp1$lambda

    checkmate::assert_class(mplus_input, "mplusObject")

    ##### Mplus Title #####

    if (object$class == "tree") {
        tmp1 <- object$mapping_matrix
        tmp1 <- tmp1[, !is.element(colnames(tmp1), "cate"), drop = FALSE]

        tmp2 <- vapply(seq_len(ncol(tmp1)),
                       function(x) {
                           paste0("  pseudoitem", x, ":  ",
                                  clps(, ifelse(is.na(tmp1[, x]), "-", tmp1[, x])))
                           }, FUN.VALUE = character(1))
    } else if (object$class == "grm") {
        tmp2 <- NULL
    } else {
        .stop_not_implemented()
    }

    # tmp1 <- attr(pseudoitems, "mapping_matrix")
    # tmp1 <- tmp1[, !is.element(colnames(tmp1), "cate"), drop = FALSE]
    #
    # tmp2 <- sapply(seq_len(ncol(tmp1)), function(x) paste0("  pseudoitem", x, ":  ",
    #                                               clps(, ifelse(is.na(tmp1[, x]), "-", tmp1[, x]))))
    tmp3 <- glue::glue("{'  '}#Parameters:  N = {nrow(data)}; J = {object$J}; K = {object$K}; \\
                       P = {object$P}; S = {object$S};")
    tmp4 <- substr(paste0("  Items:        ", clps(, object$j_names)), 1, 90)

    # TITLE <- paste(c(paste0("  Tree Model;   R = ", R), tmp3, tmp4, tmp2), collapse = "\n")
    TITLE <- paste(c("  Tree Model",
                     paste0("  ", file_name),
                     tmp3, tmp4, tmp2),
                   collapse = "\n")

    # mplus_input2 <- MplusAutomation:::update.mplusObject(
    #     mplus_input,
    #     TITLE = TITLE)
    mplus_input$TITLE <- TITLE

    ##### Mplus #####

    # MplusAutomation::createSyntax()
    # MplusAutomation::prepareMplusData()
    # MplusAutomation::runModels()
    # MplusAutomation::readModels()

    body <- MplusAutomation::createSyntax(mplus_input, basename(data_file), check = FALSE)
    cat(body, file = inpu_file, sep = "\n")

    invisible(
        capture.output(
            MplusAutomation::prepareMplusData(pseudoitems,
                                              filename = data_file,
                                              overwrite = overwrite))
        )

    if (run) {
        invisible(
            capture.output(
                MplusAutomation::runModels(inpu_file,
                                           replaceOutfile = replaceOutfile,
                                           showOutput = verbose,
                                           logFile = NULL,
                                           ...)
            ))

        invisible(
            capture.output(
                res <- suppressMessages(
                    MplusAutomation::readModels(outp_file, quiet = TRUE))))

        if (.warnings2messages) {
            # This is useful for testing with testthat because Mplus
            # warnings/errors are often harmless and should not show up in
            # testthat results.
            mywarn <- function(...) {
                message(...)
            }
        } else {
            # This is the default for user-level usage
            mywarn <- function(...) {
                warning(..., call. = FALSE)
            }
        }

        outfiletext <- readLines(outp_file)
        res$converged <- extract_mplus_converged(outfiletext)
        tmp1 <- extract_mplus_warning(outfiletext)
        tmp2 <- class(res$warnings)
        res$warnings <- c(res$warnings, tmp1)
        class(res$warnings) <- tmp2

        wrn1 <- res$warnings
        if (length(wrn1) > 0) {
            sapply(wrn1, function(x) mywarn("Mplus warning: ", clps(, x)))
        }
        err1 <- res$errors
        if (length(err1) > 0) {
            sapply(err1, function(x) mywarn("Mplus error: ", clps(, x)))
        }
    } else {
        res <- NULL
    }

    out <- list(mplus = res, spec = spec)
    class(out) <- c("irtree_fit", class(out))
    return(out)
}

#' Prepare an Mplus Input File
#'
#' This is an internal function used by [irtree_fit_mplus()]. It receives its
#' inputs from the model object and the data set and returns an object of class
#' [MplusAutomation::mplusObject].
#'
#' @param pseudoitems Data frame as returned from [irtree_recode()].
#' @param data_file String, the full file path of the data set.
#' @param fsco_file String, the file name used by Mplus to store the factor scores.
#' @inheritParams irtree_fit_mplus
#' @return A list of of class [MplusAutomation::mplusObject]
#' @export
write_mplus_input <- function(object = object,
                              # lambda = NULL,
                              # addendum = NULL,
                              pseudoitems = NULL,
                              data_file = NULL,
                              fsco_file = NULL,
                              save_fscores = FALSE,
                              quadpts = NULL,
                              estimator = NULL,
                              link = NULL,
                              # processors = 1,
                              analysis_list = list()) {

    lambda <- object$lambda
    # lambda <- lambda[order(lambda$trait, lambda$item), ]

    ##### Apply Constraints #####

    if (!is.null(object$constraints)) {
        lambda$trait <- factor(lambda$trait,
                               levels = levels(lambda$trait),
                               labels = stringr::str_replace_all(
                                   levels(lambda$trait),
                                   object$constraints))

        tmp1 <- object$constraints
        names(tmp1) <- paste0("(?<!\\w)", names(tmp1), "(?!\\w)")

        object$addendum <- stringr::str_replace_all(object$addendum, tmp1)
    }

    ##### MODEL Statement #####

    # lambda$new_name <- attr(pseudoitems, "pseudoitem_names")
    # lambda$mplus <- glue::glue_data(lambda, "{new_name}{loading}")
    #
    # lambda$label <- paste0("a", 1:nrow(lambda))

    mplus1 <- split(lambda, lambda$trait)

    # mplus2 <- vapply(seq_along(mplus1), function(x) {
    #     glue::glue_collapse(c(glue::glue("{names(mplus1[x])} BY"),
    #                           glue::glue(" {mplus1[[x]]$mplus} ({mplus1[[x]]$label})"), ";"),
    #                         sep = "\n")
    # }, FUN.VALUE = "")

    mplus2 <- lapply(seq_along(mplus1), function(x) {
        c(paste(names(mplus1[x]), "BY"),
          paste0("    ", mplus1[[x]]$mplus, " (", mplus1[[x]]$label, ")"),
          ";")
    })

    mplus3 <- paste(
        lapply(
            lapply(c(mplus2, object$addendum),
                   strwrap, width = 89, indent = 2, exdent = 4),
            paste, collapse = "\n"),
        collapse = "\n")

    ##### MODEL CONSTRAINT Statement #####

    model_constr0 <-
        vapply(seq_along(mplus1), function(x) {
            if (any(mplus1[[x]]$loading != "*")) {
                return(character(1))
            }
            glue::glue("0 = ",
                       glue::glue_collapse(glue::glue("{mplus1[[x]]$label}"),
                                           sep = " + "),
                       " - {nrow(mplus1[[x]])};")
        }, FUN.VALUE = "")

    model_constr <- paste(
        lapply(
            lapply(model_constr0[model_constr0 != ""],
                   strwrap, width = 89, indent = 2, exdent = 4),
            paste, collapse = "\n"),
        collapse = "\n")
    if (model_constr == "") {
        model_constr <- NULL
    }

    ##### ANALYSIS Statement #####

    helper1 <- "_-_"
    if (length(analysis_list) > 0) {
        analysis2 <- paste(names(analysis_list), paste0(analysis_list, ";"),
                           sep = " = ", collapse = helper1)
    } else {
        analysis2 <- ""
    }

    ANALYSIS <- glue::glue("ALGORITHM = INTEGRATION EM;",
                           "ESTIMATOR = {estimator};",
                           "INTEGRATION = {quadpts};",
                           "LINK = {link};",
                           # "PROCESSORS = {processors};",
                           analysis2,
                           .sep = helper1)
    ANALYSIS <- strsplit(ANALYSIS, helper1, fixed = TRUE)[[1]]

    ##### SAVEDATA Statement #####

    if (save_fscores) {
        SAVEDATA <- c(glue::glue("FILE = {fsco_file};"),
                      glue::glue("SAVE = FSCORES;"))
    } else {
        SAVEDATA <- NULL
    }

    ##### VARIABLE Statement #####

    # NB: USEVARIABLES is automatically generated using 'mplusObject(autov = T)'
    # NB: mplusObject(usevariables) != USEVARIABLES in Mplus

    if (object$class == "grm") {
        tmp1 <- names(pseudoitems)
    } else if (object$class == "tree") {
        tmp1 <- c(intersect(names(pseudoitems), lambda$new_name), object$covariates)
    } else {
        .stop_not_implemented()
    }

    mp_cat_vars <-
        paste(
            strwrap(
                paste0("CATEGORICAL = ",
                       # paste(attr(pseudoitems,
                       #            "pseudoitem_names"), collapse = " "),
                       clps(" ", lambda$new_name),
                       ";"
                       , "\n\nUSEVARIABLES = ", clps(" ", tmp1), ";"),
                width = 89, indent = 0, exdent = 5),
            collapse = "\n")

    ##### Combine All Statements #####

    # # if keep is TRUE in irtree_recode(), then the 'pseudoitems' also contain the
    # # original items for completeness/debugging/transparency. However, they must
    # # not occur under USEVARIABLES and are therefore excluded in 'rdata'.
    # # Applies not to GRM.
    #
    # if (object$class == "tree") {
    #     rdata <- pseudoitems[, !names(pseudoitems) %in% names(object$j_names)]
    # } else if (object$class == "grm") {
    #     rdata <- pseudoitems
    # }
    # # internal sanity check for debugging
    # checkmate::assert_data_frame(rdata, all.missing = FALSE,
    #                              min.cols = nrow(lambda),
    #                              col.names = "unique")

    mplus_input <- MplusAutomation::mplusObject(
        TITLE = NULL,
        # DATA = NULL,
        VARIABLE = mp_cat_vars,
        # ANALYSIS = NULL,
        ANALYSIS = ANALYSIS,
        MODEL = mplus3,
        OUTPUT = "STDYX TECH1 TECH4 TECH8;",
        SAVEDATA = SAVEDATA,
        # PLOT = NULL,
        # usevariables = names(pseudoitems),
        rdata = pseudoitems,
        # autov = TRUE,
        autov = FALSE,
        MODELCONSTRAINT = model_constr
    )
    mplus_input$usevariables <- names(pseudoitems)

    # mplus_input <- MplusAutomation:::update.mplusObject(
    # mplus_input <- MplusAutomation::update(
    #     mplus_input,
    #     VARIABLE = ~ +"CATEGORICAL = ALL;")

    # cat(MplusAutomation::createSyntax(mplus_input))

    return(list(mplus_input = mplus_input, lambda = lambda))
}

extract_mplus_warning <- function(outfiletext) {
    tmp1 <- grep("ONE OR MORE PARAMETERS WERE FIXED TO AVOID SINGULARITY OF THE",
                 outfiletext)
    if (length(tmp1) == 0) {
        return(list())
    }
    tmp2 <- which("" == outfiletext[tmp1:length(outfiletext)])[1] - 2
    tmp3 <- trimws(outfiletext[tmp1:(tmp1 + tmp2)])
    return(list(tmp3))
}

extract_mplus_converged <- function(outfiletext) {
    any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", outfiletext))
}
