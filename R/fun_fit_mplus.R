#' Fit an IR-Tree Model using Mplus
#'
#' This function takes a data frame and a model string and runs the model in Mplus.
#'
#' @param data Data frame containing only the items.
#' @param model A description of the user-specified model. See
#'   \code{\link{tree_model}} for more information.
#' @param dir String, path name where the Mplus files should be
#'   stored and run; the default corresponds to the working directory
#' @param file_name String, file name of the data file and the Mplus files
# @param R Integer used to index the file names.
#' @param integration_points Integer, passed to argument 'INTEGRATION' in Mplus.
#'   Number of integration points for numerical integration.
#' @param estimator String, passed to argument 'ESTIMATOR' in Mplus.
#' @param link String, passed to argument 'LINK' in Mplus. Specifies
#'   the link function.
#' @param inp_rm Logical, whether the Mplus input file should be removed on exit.
#' @param out_rm Logical, whether the Mplus output file should be removed on exit.
#' @param fsc_rm Logical, whether the Mplus FSCORES file should be removed on exit.
#' @param dat_rm Logical, whether the data file should be removed on exit.
#' @param save_fscores Logical, wheter to save FSCORES or not.
#' @param analysis_list Named list of strings passed to Mplus' argument
#'   ANALYSIS. For example: \code{analysis_list = list(MITERATIONS = "1000")}.
# @param processors Integer, passed to argument 'PROCESSORS' in Mplus.
#' @param run Logical, whether to indeed run Mplus.
#' @param ... Additional parameters passed to \code{\link[MplusAutomation]{runModels}}.
#' @param .warnings2messages Logial, whether Mplus errors and warnings should be
#'   signaled as warnings (the default) or messages.
#' @inheritParams MplusAutomation::runModels
#' @inheritParams MplusAutomation::prepareMplusData
#' @return List with two elements. \code{Mplus} contains the Mplus output read into R via \code{\link[MplusAutomation]{readModels}}. \code{args} contains the input specifications.
# @examples
# \dontrun{
# if(interactive()){
#  #EXAMPLE1
#  }
# }
#' @export
# @import checkmate
# @import glue
# @import MplusAutomation
fit_tree_mplus <- function(data = NULL,
                           model = NULL,
                           dir = ".",
                           # R = 1,
                           file_name = "mplus-tree",
                           integration_points = 15,
                           estimator = "MLR",
                           link = c("probit", "logit"),
                           inp_rm = FALSE,
                           out_rm = FALSE,
                           fsc_rm = FALSE,
                           dat_rm = FALSE,
                           save_fscores = TRUE,
                           # processors = 1,
                           analysis_list = list(),
                           run = TRUE,
                           showOutput = FALSE,
                           replaceOutfile = "always",
                           overwrite = FALSE,
                           ...,
                           .warnings2messages = FALSE) {

    ellipsis::check_dots_used()

    link <- match.arg(link)

    checkmate::assert_true(MplusAutomation::mplusAvailable() == 0)
    checkmate::assert_directory_exists(dir)
    checkmate::assert_list(analysis_list,  types = "character",
                           names = "unique")
    checkmate::qassertr(analysis_list, "S1")

    model <- tree_model(model = model)

    args <- c(as.list(environment())
              # , list(...)
    )

    checkmate::assert_character(model$covariates, min.chars = 1,
                                pattern = "^[[:alpha:]][[:alnum:]_]*$",
                                any.missing = FALSE, unique = TRUE,
                                null.ok = TRUE, .var.name = "Addendum in model")
    checkmate::assert_subset(x = c(names(model$j_names), model$covariates),
                             choices = names(data),
                             empty.ok = FALSE)
    tmp1 <- checkmate::check_set_equal(names(data),
                                       c(names(model$j_names), model$covariates))
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
    assert_nchar(model$j_names, 8)
    assert_nchar(model$covariates, 8)
    # checkmate::assert_data_frame(data, all.missing = FALSE, min.rows = 1, min.cols = model$J)
    checkmate::assert_data_frame(data, all.missing = FALSE, min.rows = 1,
                                 min.cols = model$J + length(model$covariates))
    checkmate::assert_data_frame(data[, names(model$j_names)], types = "integerish",
                                 ncols = model$J)
    # checkmate::assert_subset(names(model$j_names), choices = names(data))

    args$model$j_names <- model$j_names <- sort2(model$j_names, names(data), TRUE)
    model$lambda$item <- factor(model$lambda$item, levels = model$j_names)
    # CAVE: The following line is super important. It is the only safeguard that
    # assures that the items in the 'MODEL'-statement of the mplus input are in
    # the correct order, namely, that given by the data.
    # This in turn assures that the item thresholds in the output are in that
    # same order---and this is necessary, because otherwise checking the order
    # whilst reading in the output is very cumbersome.
    args$model$lambda <- model$lambda <- model$lambda[order(model$lambda$trait, model$lambda$item), ]

    ##### Pseudoitems #####

    if (model$class == "tree") {
        categ_dat <- unique(na.omit(unlist(data[, names(model$j_names)], use.names = FALSE)))
        categ_mod <- as.integer(names(model$expr))
        if (length(sym_diff(categ_dat, categ_mod)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'model' has equations for categories ", clps(", ", categ_mod), "."
                 , call. = FALSE)
        }
        pseudoitems <- recode_data(model = model, data = data, keep = TRUE)
    } else if (model$class == "grm") {
        pseudoitems <- data
        tmp1 <- model$j_names
        names(tmp1) <- paste0("^", names(tmp1), "$")
        names(pseudoitems) <- stringr::str_replace_all(names(pseudoitems), tmp1)
        # attr(pseudoitems, "pseudoitem_names") <- model$j_names
    }

    # data_file <- sprintf("mplus_tree_%05d_data.txt", R)
    # inpu_file <- sprintf("mplus_tree_%05d.inp", R)
    # outp_file <- sub("[.]inp", ".out", inpu_file)
    # fsco_file <- sub("[.]inp", ".fsc", inpu_file)
    data_file <- paste0(file_name, ".txt")
    inpu_file <- paste0(file_name, ".inp")
    outp_file <- paste0(file_name, ".out")
    fsco_file <- paste0(file_name, ".fsc")

    on.exit({
        if (inp_rm) {
            file.remove(file.path(dir, inpu_file))
            if (dat_rm) {
                file.remove(file.path(dir, data_file))
            }
        }
        if (run) {
            if (out_rm) {
                file.remove(file.path(dir, outp_file))
            }
            if (fsc_rm) {
                file.remove(file.path(dir, fsco_file))
            }
        }
    }, add = TRUE)

    ##### Mplus Input #####

    tmp1 <- suppressMessages(
        write_mplus_input(
            model        = model,
            # lambda       = model$lambda,
            # addendum     = model$addendum,
            pseudoitems  = pseudoitems,
            data_file    = data_file,
            integration_points = integration_points,
            estimator     = estimator,
            link          = link,
            save_fscores  = save_fscores,
            fsco_file     = fsco_file,
            # processors    = processors,
            analysis_list = analysis_list))

    mplus_input <- tmp1$mplus_input
    args$model$lambda <- model$lambda <- tmp1$lambda

    checkmate::assert_class(mplus_input, "mplusObject")

    ##### Mplus Title #####

    if (model$class == "tree") {
        tmp1 <- model$mapping_matrix
        tmp1 <- tmp1[, !is.element(colnames(tmp1), "cate"), drop = FALSE]

        tmp2 <- vapply(seq_len(ncol(tmp1)),
                       function(x) {
                           paste0("  pseudoitem", x, ":  ",
                                  clps(, ifelse(is.na(tmp1[, x]), "-", tmp1[, x])))
                           }, FUN.VALUE = character(1))
    } else if (model$class == "grm") {
        tmp2 <- NULL
    }

    # tmp1 <- attr(pseudoitems, "mapping_matrix")
    # tmp1 <- tmp1[, !is.element(colnames(tmp1), "cate"), drop = FALSE]
    #
    # tmp2 <- sapply(seq_len(ncol(tmp1)), function(x) paste0("  pseudoitem", x, ":  ",
    #                                               clps(, ifelse(is.na(tmp1[, x]), "-", tmp1[, x]))))
    tmp3 <- glue::glue("{'  '}#Parameters:  N = {nrow(data)}; J = {model$J}; K = {model$K}; \\
                       P = {model$P}; S = {model$S};")
    tmp4 <- substr(paste0("  Items:        ", clps(, model$j_names)), 1, 90)

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

    body <- MplusAutomation::createSyntax(mplus_input, data_file, check = FALSE)
    if (file.exists(file.path(dir, inpu_file)) & overwrite == FALSE) {
        stop("File already exists: ", file.path(dir, inpu_file), call. = FALSE)
    } else {
        cat(body, file = file.path(dir, inpu_file), sep = "\n")
    }

    invisible(
        capture.output(
            MplusAutomation::prepareMplusData(pseudoitems,
                                              filename = file.path(dir, data_file),
                                              overwrite = overwrite))
        )

    if (run) {
        invisible(
            capture.output(
                MplusAutomation::runModels(file.path(dir, inpu_file),
                                           replaceOutfile = replaceOutfile,
                                           showOutput = showOutput,
                                           logFile = NULL,
                                           ...)
            ))

        res <- MplusAutomation::readModels(file.path(dir, outp_file),
                                           quiet = TRUE)

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
        tmp1 <- extract_mplus_warning(outfiletext)
        res$warnings <- c(res$warnings, tmp1)

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

    invisible(list(mplus = res, args = args))
}

#' Prepare an Mplus Input File
#'
#' This is an internal function used by \code{\link{fit_tree_mplus}}. It receives its
#' inputs from the model and the data set and returns an object of class
#' \code{\link[MplusAutomation]{mplusObject}}.
#'
# @param lambda Matrix as returned from \code{\link{fit_tree_mplus}}.
#' @param pseudoitems Data frame as returned from \code{\link{recode_data}}.
#' @param data_file String, the full file path of the data set.
#' @param fsco_file String, the file used by Mplus to store the factor scores.
# @param addendum String as returned from \code{\link{tree_model}}.
#' @inheritParams fit_tree_mplus
# @importMethodsFrom MplusAutomation update
#' @return A list of of class \code{\link[MplusAutomation]{mplusObject}}
# @examples
#' @export
#' @seealso MplusAutomation::mplusObject
#'
# @import glue
# @import MplusAutomation
write_mplus_input <- function(model = model,
                              # lambda = NULL,
                              # addendum = NULL,
                              pseudoitems = NULL,
                              data_file = NULL,
                              fsco_file = NULL,
                              save_fscores = FALSE,
                              integration_points = NULL,
                              estimator = NULL,
                              link = NULL,
                              # processors = 1,
                              analysis_list = list()) {

    lambda <- model$lambda
    # lambda <- lambda[order(lambda$trait, lambda$item), ]

    ##### Apply Constraints #####

    if (!is.null(model$constraints)) {
        lambda$trait <- factor(lambda$trait,
                               levels = levels(lambda$trait),
                               labels = stringr::str_replace_all(
                                   levels(lambda$trait),
                                   model$constraints))

        tmp1 <- model$constraints
        names(tmp1) <- paste0("(?<!\\w)", names(tmp1), "(?!\\w)")

        model$addendum <- stringr::str_replace_all(model$addendum, tmp1)
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
            lapply(c(mplus2, model$addendum),
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
                           "INTEGRATION = {integration_points};",
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

    if (model$class == "grm") {
        tmp1 <- names(pseudoitems)
    } else if (model$class == "tree") {
        tmp1 <- c(intersect(names(pseudoitems), lambda$new_name), model$covariates)
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

    # # if keep is TRUE in recode_data(), then the 'pseudoitems' also contain the
    # # original items for completeness/debugging/transparency. However, they must
    # # not occur under USEVARIABLES and are therefore excluded in 'rdata'.
    # # Applies not to GRM.
    #
    # if (model$class == "tree") {
    #     rdata <- pseudoitems[, !names(pseudoitems) %in% names(model$j_names)]
    # } else if (model$class == "grm") {
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

#' Retrieve Estimates From Mplus
#'
#' This function takes the output from Mplus as returned from
#' \code{\link[MplusAutomation]{readModels}} and returns the estimates in a
#' convenient way.
#'
#' @param results A list as returned from \code{\link[MplusAutomation]{readModels}}
#' @param class String specifying which class of model was fit
#' @param .errors2messages Logical indicating whether errors should be converted
#'   to messages
#' @inheritParams fit_tree_mplus
#' @return A list of parameter estimates, model fit information
#'   (\code{summaries}), \code{warnings}, \code{errors}.
# @examples
#' @export
extract_mplus_output <- function(results = NULL,
                                 model = NULL,
                                 class = NULL,
                                 .errors2messages = FALSE) {

    checkmate::assert_class(results, "mplus.model")

    if (!is.null(model)) {
        model <- tree_model(model)
    }

    tmp1 <- vapply(results$errors, function(x) {
        any(stringr::str_detect(x,
                                "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY"))},
        FUN.VALUE = logical(1))

    if (any(tmp1)) {
        if (.errors2messages) {
            lapply(results$errors[cumsum(tmp1) > 0], function(x) message("Mplus error: ", clps(" ", x), call. = FALSE))
        } else {
            stop("Mplus error: ", clps(" ", unlist(results$errors[cumsum(tmp1) > 0])), call. = FALSE)
        }
    }
    # if (!is.null(model)) {
    #     model <- tree_model(model)
    #     e2 <- list2env(model)
    # } else {
    #     e2 <- new.env()
    #     # tmp1 <- strsplit(clps(" ", trimws(results$input$model)), "(?<=;)", perl = TRUE)[[1]]
    #     # tmp_list <- list(irt = tmp1)
    #     # tmp_list$irt <- tmp_list$irt[grepl("\\s+by\\s+", tmp_list$irt, ignore.case = T)]
    #     # tree_model_irt(tmp_list, e2)
    #     e2$class <- match.arg(class)
    #
    #     e2$lv_names <- as.character(
    #         na.omit(
    #             stringr::str_extract(results$input$model, "\\w+(?=\\s*BY)")))
    # }

    e2 <- new.env()
    e2$lv_names <-
        as.character(
            na.omit(
                stringr::str_extract(results$input$model, "\\w+(?=\\s*BY)")))
    if (!is.null(model)) {
        e2$class <- model$class
    } else {
        checkmate::assert_choice(class, choices = c("tree", "grm"))
        e2$class <- class
    }

    # replace 999 with NA
    # results$parameters <- purrr::modify(results$parameters, function(d) {
    #     purrr::modify_at(d, c("se", "est_se", "pval"), ~ifelse(d$pval == 999, NA, .x))
    # })
    results$parameters <- lapply(results$parameters, function(x) {
        x$se <- ifelse(x$pval == 999, NA, x$se)
        x$est_se <- ifelse(x$pval == 999, NA, x$est_se)
        x$pval <- ifelse(x$pval == 999, NA, x$pval)
        return(x)
    })

    unstd <- results[["parameters"]][["unstandardized"]]
    class(unstd) <- "data.frame"

    if (!(is.null(results[["savedata"]]) | length(results[["savedata"]]) == 0)) {

        fscores <- results[["savedata"]]

        # tmp1 <- paste0("^", e2$lv_names, "$", collapse = "|")
        # fscore_cols1 <- grepl(tmp1, names(fscores), ignore.case = TRUE)
        # tmp2 <- paste0("^", e2$lv_names, "_SE$", collapse = "|")
        # fscore_cols2 <- grepl(tmp2, names(fscores), ignore.case = TRUE)
        #
        # personpar_est <- fscores[, fscore_cols1, drop = FALSE]
        # personpar_se  <- fscores[, fscore_cols2, drop = FALSE]
        # colnames(personpar_est) <- tolower(names(personpar_est))
        # colnames(personpar_se)  <- tolower(names(personpar_se))

        fscore_cols1 <- is.element(toupper(names(fscores)),
                                   toupper(e2$lv_names))
        fscore_cols2 <- is.element(toupper(names(fscores)),
                                   toupper(paste0(e2$lv_names, "_SE")))
        personpar_est <- fscores[, fscore_cols1, drop = FALSE]
        personpar_se  <- fscores[, fscore_cols2, drop = FALSE]

    } else {
        personpar_est <- NULL
        personpar_se  <- NULL
    }

    if (!(is.null(results[["tech4"]][["latCovEst"]]))) {

        sigma  <- results[["tech4"]][["latCovEst"]]
        cormat <- results[["tech4"]][["latCorEst"]]

        if (!is.null(e2$lv_names)) {
            sigma  <-  sigma[toupper(e2$lv_names), toupper(e2$lv_names), drop = FALSE]
            cormat <- cormat[toupper(e2$lv_names), toupper(e2$lv_names), drop = FALSE]
        }
        # tmp1 <- paste0("^", e2$lv_names, "$", collapse = "|")
        # tmp2 <- grepl(tmp1, colnames(sigma), ignore.case = TRUE)
        # if (sum(tmp2) == e2$S) {
        #     sigma  <-  sigma[tmp2, tmp2, drop = FALSE]
        #     cormat <- cormat[tmp2, tmp2, drop = FALSE]
        # }

    } else {
        sigma  <- NULL
        cormat <- NULL
    }


    if (!is.null(e2$lambda$new_name)) {
        alphapar    <- unstd[tolower(unstd$param) %in%        tolower(e2$lambda$new_name), , drop = FALSE]
        betapar     <- unstd[tolower(unstd$param) %in% paste0(tolower(e2$lambda$new_name), "$1"), , drop = FALSE]
    } else {
        alphapar    <- unstd[grep("[.]BY$", unstd$paramHeader), , drop = FALSE]
        betapar     <- unstd[unstd$paramHeader == "Thresholds", , drop = FALSE]
    }
    alphapar$param <- factor(alphapar$param, levels = unique(alphapar$param))
    rownames(alphapar) <- NULL
    rownames(betapar) <- NULL

    itempar <- list()

    if (e2$class == "tree") {

        betapar <- tidyr::separate(betapar, "param", c("traititem", "threshold"),
                                   sep = "\\$", extra = "merge")
        betapar <- tidyr::separate(betapar, "traititem", c("trait", "item"),
                                   sep = "_", extra = "merge", fill = "right")
        betapar$trait <- factor(betapar$trait, levels = unique(betapar$trait))
        betapar$item  <- factor(betapar$item, levels = unique(betapar$item))

        # itempar$beta    <- reshape2::dcast(betapar, item ~ trait, value.var = "est")
        # itempar$beta_se <- reshape2::dcast(betapar, item ~ trait, value.var = "se")

        itempar$beta <- reshape(
            dplyr::select(betapar, .data$trait, .data$item, .data$est),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$beta) <- sub("^est[.]", "", names(itempar$beta))
        itempar$beta_se <- reshape(
            dplyr::select(betapar, .data$trait, .data$item, .data$se),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$beta_se) <- sub("^se[.]", "", names(itempar$beta_se))

        alphapar <- tidyr::separate(alphapar, "param", c("trait", "item"),
                                    sep = "_", extra = "merge", fill = "right")
        alphapar$trait <- factor(alphapar$trait, levels = unique(alphapar$trait))
        alphapar$item  <- factor(alphapar$item, levels = unique(alphapar$item))

        # itempar$alpha    <- reshape2::dcast(alphapar, item ~ trait, value.var = "est")
        # itempar$alpha  <- tidyr::pivot_wide(alphapar, item, names_from = "trait", values_from = "est")
        # itempar$alpha_se <- reshape2::dcast(alphapar, item ~ trait, value.var = "se")
        # itempar$alpha_se <- tidyr::pivot_wide(alphapar, item, names_from = "trait", values_from = "se")

        itempar$alpha <- reshape(
            dplyr::select(alphapar, .data$trait, .data$item, .data$est),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$alpha) <- sub("^est[.]", "", names(itempar$alpha))
        itempar$alpha_se <- reshape(
            dplyr::select(alphapar, .data$trait, .data$item, .data$se),
            direction = "wide",
            idvar = "item",
            timevar = "trait")
        names(itempar$alpha_se) <- sub("^se[.]", "", names(itempar$alpha_se))

    } else if (e2$class == "grm") {

         tmp1 <- tidyr::separate(betapar, "param", c("item", "threshold"),
                                 sep = "\\$")

         # itempar$beta    <- tidyr::spread(
         #     dplyr::select(tmp1, .data$item, .data$threshold, .data$est),
         #     .data$threshold, .data$est)
         itempar$beta <- reshape(
             dplyr::select(tmp1, .data$item, .data$threshold, .data$est),
             direction = "wide",
             idvar = "item",
             timevar = "threshold"
         )
         names(itempar$beta) <- sub("^est[.]", "b", names(itempar$beta))

         # itempar$beta_se <- tidyr::spread(
         #     dplyr::select(tmp1, .data$item, .data$threshold, .data$se),
         #     .data$threshold, .data$se)
         itempar$beta_se <- reshape(
             dplyr::select(tmp1, .data$item, .data$threshold, .data$se),
             direction = "wide",
             idvar = "item",
             timevar = "threshold"
         )
         names(itempar$beta_se) <- sub("^se[.]", "b", names(itempar$beta_se))


         # tmp2 <- dplyr::select(tmp1, -.data$est_se, -.data$pval, -.data$paramHeader, -.data$param)
         # tmp2$item <- factor(tmp2$item, levels = unique(tmp2$item))
         # tmp4 <- reshape2::recast(tmp2, item + variable ~ threshold, id.var = c("item", "threshold"))
         # tmp5 <- split(tmp4, tmp4$variable)
         # tmp0 <- lapply(tmp5, dplyr::select, -.data$variable)
         # tmp0 <- lapply(tmp0, function(x) {rownames(x) <- NULL; x})
         # itempar$beta <- tmp0$est
         # itempar$beta_se <- tmp0$se

         itempar$alpha <- dplyr::select(alphapar, item = .data$param, .data$est)
         itempar$alpha_se <- dplyr::select(alphapar, item = .data$param, .data$se)

         itempar <- lapply(itempar, function(x) {
             x$item <- factor(x$item, unique(alphapar$param))
             x[order(x$item), ]
             })
    }

    out <- list(
        person     = list(personpar_est = personpar_est,
                          personpar_se  = personpar_se),
        item       = itempar,
        sigma      = sigma,
        cormat     = cormat,
        summaries  = results$summaries,
        warnings   = results$warnings,
        errors     = results$errors,
        parameters = results$parameters
    )

    return(out)
}

extract_mplus_warning <- function(outfiletext) {
    tmp1 <- grep("ONE OR MORE PARAMETERS WERE FIXED TO AVOID SINGULARITY OF THE",
                 outfiletext)
    if (length(tmp1) == 0) {
        return(list(0))
    }
    tmp2 <- which("" == outfiletext[tmp1:length(outfiletext)])[1] - 2
    tmp3 <- trimws(outfiletext[tmp1:(tmp1 + tmp2)])
    return(list(tmp3))
}
