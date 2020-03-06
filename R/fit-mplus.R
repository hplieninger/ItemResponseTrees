#' Fit an IR-Tree Model using Mplus
#'
#' This function takes a data frame and a model string and runs the model in Mplus.
#'
#' @param link String, passed to argument 'LINK' in Mplus. Specifies
#'   the link function.
#' @inheritParams fit.irtree_model
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
irtree_fit_mplus <- function(object = NULL,
                             data = NULL,
                             link = c("logit", "probit"),
                             verbose = interactive(),
                             control = control_mplus(),
                             improper_okay = FALSE
                             ) {

    checkmate::assert_class(object, "irtree_model")
    assert_irtree_data(data = data, object = object, engine = "mplus")
    data <- tibble::as_tibble(data)

    assert_irtree_proper(object, improper_okay = improper_okay)

    assert_nchar(object$lambda$new_name)
    assert_nchar(levels(object$lambda$theta))

    link   <- match.arg(link)
    checkmate::qassert(verbose, "B1")
    checkmate::qassert(control, "l")
    checkmate::assert_names(names(control), must.include = formalArgs(control_mplus))

    spec <- c(as.list(environment()))
    spec$engine <- "mplus"

    if (is.numeric(control$quadpts)) {
        if (control$quadpts^object$S > 20000) {
            stop("Using that many quadrature points may cause problems.\n",
                 "This error can be disabled by supplying the number of 'quadpts' ",
                 "as a character string (e.g., quadpts = '15').")
        }
    }

    spec$object$j_names <- object$j_names <- sort2(object$j_names, names(data))
    object$lambda$item <- factor(object$lambda$item, levels = object$j_names)
    # CAVE: The following line is super important. It is the only safeguard that
    # assures that the items in the 'MODEL'-statement of the mplus input are in
    # the correct order, namely, that given by the data.
    # This in turn assures that the item thresholds in the output are in that
    # same order---and this is necessary, because otherwise checking the order
    # whilst reading in the output is very cumbersome.
    spec$object$lambda <-
        object$lambda <-
        object$lambda[order(object$lambda$irt, object$lambda$item), ]

    ##### file #####

    dir <- normalizePath(dirname(control$file), winslash = "/", mustWork = TRUE)
    file_name <- basename(control$file)

    data_file <- file.path(dir, paste0(file_name, ".txt"))
    inpu_file <- file.path(dir, paste0(file_name, ".inp"))
    outp_file <- file.path(dir, paste0(file_name, ".out"))
    fsco_file <- file.path(dir, paste0(file_name, ".fsc"))

    if (!control$overwrite & any(file.exists(data_file, inpu_file, outp_file, fsco_file))) {
        stop("File(s) already exist. Please modify argument 'file' or set 'overwrite' to TRUE.")
    }

    on.exit({
        if (control$cleanup) {
            suppressWarnings(file.remove(inpu_file, data_file))
            if (control$run) {
                suppressWarnings(file.remove(outp_file))
                if (control$save_fscores) {
                    suppressWarnings(file.remove(fsco_file))
                }
            }
        }

    }, add = TRUE)

    ##### Pseudoitems #####

    if (object$class == "tree") {
        assert_irtree_not_mixture(object)
        pseudoitems <- irtree_recode(object = object, data = data, keep = TRUE)
    } else if (object$class == "grm") {
        pseudoitems <- data
    } else {
        .stop_not_implemented()
    }

    ##### Mplus Input #####

    tmp1 <- suppressMessages(
        write_mplus_input(
            object        = object,
            pseudoitems   = pseudoitems,
            data_file     = data_file,
            quadpts       = control$quadpts,
            estimator     = control$estimator,
            link          = link,
            save_fscores  = control$save_fscores,
            fsco_file     = basename(fsco_file),
            analysis_list = control$analysis_list))

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

    tmp3 <- glue::glue("{'  '}#Parameters:  N = {nrow(data)}; J = {object$J}; K = {object$K}; \\
                       P = {object$P}; S = {object$S};")
    tmp4 <- substr(paste0("  Items:        ", clps(, object$j_names)), 1, 90)

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
                                              overwrite = control$overwrite))
        )

    if (control$run) {
        invisible(
            capture.output(
                MplusAutomation::runModels(
                    inpu_file,
                    replaceOutfile = ifelse(control$overwrite, "always", "never"),
                    showOutput = verbose,
                    logFile = NULL,
                    Mplus_command = control$Mplus_command)
            ))

        invisible(
            capture.output(
                res <- suppressMessages(
                    MplusAutomation::readModels(outp_file, quiet = TRUE))))

        if (control$warnings2messages) {
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
#' @inheritParams control_mplus
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

    ##### MODEL Statement #####

    mplus1 <- split(lambda, lambda$theta)

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

    helper1 <- "\n"
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
                           analysis2,
                           .sep = "\n")

    ##### SAVEDATA Statement #####

    if (save_fscores) {
        SAVEDATA <- c(glue::glue("FILE = {fsco_file};\nSAVE = FSCORES;"))
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
        usevariables = names(pseudoitems),
        rdata = pseudoitems,
        # autov = TRUE,
        autov = FALSE,
        MODELCONSTRAINT = model_constr
    )

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

#' Control Aspects of Fitting a Model in Mplus
#'
#' This function should be used to generate the `control` argument of the
#' [`fit()`][fit.irtree_model] function.
#'
#' @param file String naming the file (or path) of the Mplus files and the data
#'   file. Do not provide file endings, those will be automatically appended.
#' @param quadpts This is passed to argument 'INTEGRATION'. Thus, it may be an
#'   integer specifying the number of integration points for the Mplus default
#'   of rectangular numerical integration (e.g., `quadpts = 15`). Or it may be a
#'   string, which gives more fine grained control (e.g., `quadpts =
#'   "MONTECARLO(2000)"`).
#' @param estimator String, passed to argument 'ESTIMATOR' in Mplus.
#' @param cleanup Logical, whether the Mplus files should be removed on exit.
#' @param save_fscores Logical, whether to save FSCORES or not.
#' @param overwrite Logical value indicating whether data and input (if present)
#'   files should be overwritten.
#' @param analysis_list Named list of strings passed to Mplus' argument
#'   ANALYSIS. See examples below.
# @param processors Integer, passed to argument 'PROCESSORS' in Mplus.
#' @param run Logical, whether to indeed run Mplus.
#' @param warnings2messages Logical, whether Mplus errors and warnings should be
#'   signaled as warnings (the default) or messages.
#' @inheritParams MplusAutomation::runModels
#' @return A list with one element for every argument of `control_mplus()`.
#' @examples
#' \donttest{
#' control_mplus(file = tempfile("irtree_", tmpdir = "."),
#'               quadpts = "GAUSS(10)",
#'               analysis_list = list(COVERAGE = "0",
#'                                    MITERATIONS = "500",
#'                                    MCONVERGENCE = ".001"))
#' }
#' @export
control_mplus <- function(file = tempfile("irtree_"),
                          overwrite = FALSE,
                          cleanup = run,
                          run = TRUE,
                          estimator = "MLR",
                          quadpts = 15,
                          save_fscores = TRUE,
                          analysis_list = list(COVERAGE = "0"),
                          Mplus_command = "Mplus",
                          warnings2messages = FALSE) {

    ctrl <- as.list(environment())

    checkmate::assert_path_for_output(file, overwrite = TRUE)
    checkmate::qassert(overwrite, "B1")
    checkmate::qassert(cleanup, "B1")
    checkmate::qassert(run, "B1")
    if (run) {
        checkmate::assert_true(MplusAutomation::mplusAvailable() == 0)
    }
    checkmate::qassert(save_fscores, "B1")
    checkmate::assert_list(analysis_list,  types = "character", names = "unique")
    checkmate::qassertr(analysis_list, "S1")
    checkmate::qassert(warnings2messages, "B1")

    return(ctrl)
}
