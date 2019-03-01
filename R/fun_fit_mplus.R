#' Fit an IR-Tree Model using Mplus
#'
#' This function takes a data frame and a model string and runs the model in Mplus.
#'
#' @param data Data frame containing only the items.
#' @param model A description of the user-specified model. See
#'   \code{\link{tree_model}} for more information.
#' @param dir Character string of path name, where the Mplus files should be
#'   stored and run; the default corresponds to the working directory.
#' @param file_name Character string, file name of the data file and the Mplus files.
# @param R Integer used to index the file names.
#' @param integration_points Integer, passed to argument 'INTEGRATION' in Mplus.
#'   Number of integration points for numerical integration.
#' @param estimator Character string, passed to argument 'ESTIMATOR' in Mplus.
#' @param link Character string, passed to argument 'LINK' in Mplus. Specifies
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
#' @param .warnings2messages Logial, whether Mplus errors and warnings should be
#'   signaled as warnings (the default) or messages.
#' @inheritParams MplusAutomation::runModels
#' @inheritParams MplusAutomation::prepareMplusData
#' @return List with two elements. \code{Mplus} contains the Mplus output read into R via \code{\link[MplusAutomation]{readModels}}. \code{args} contains the input specifications.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
                           .warnings2messages = FALSE) {

    link <- match.arg(link)

    checkmate::assert_directory_exists(dir)
    checkmate::assert_list(analysis_list,  types = "character",
                           names = "unique")
    checkmate::qassertr(analysis_list, "S1")

    if (!inherits(model, "tree_model")) {
        model <- tree_model(model = model)
    }

    args <- c(as.list(environment())
              # , list(...)
    )

    # pseudoitems <- recode_data(model = model, data = data)
    if (model$class == "tree") {
        categ_dat <- unique(na.omit(unlist(data[, model$j_names], use.names = FALSE)))
        categ_mod <- as.integer(names(model$expr))
        if (length(sym_diff(categ_dat, categ_mod)) > 0) {
            stop("'data' has categories ", clps(", ", sort(categ_dat)),
                 " but 'model' has equations for categories ", clps(", ", categ_mod), "."
                 , call. = FALSE)
        }
        pseudoitems <- recode_data(model = model, data = data, keep = TRUE)
    } else if (model$class == "grm") {
        pseudoitems <- data
        attr(pseudoitems, "pseudoitem_names") <- model$j_names
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

    if (model$class == "tree") {
        tmp1 <- attr(pseudoitems, "mapping_matrix")
        tmp1 <- tmp1[, !is.element(colnames(tmp1), "cate"), drop = FALSE]

        tmp2 <- sapply(seq_len(ncol(tmp1)),
                       function(x) {
                           paste0("  pseudoitem", x, ":  ",
                                  clps(, ifelse(is.na(tmp1[, x]), "-", tmp1[, x])))
                           })
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

    # mplus_input <- MplusAutomation::createSyntax(filename = file.path(dir, data_file))

    ##### Mplus stuff #####

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
                                           logFile = NULL)
            ))

        invisible(
            capture.output(
                res <- MplusAutomation::readModels(file.path(dir, outp_file))
                ))

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
                warning(...)
            }
        }

        wrn1 <- res$warnings
        if (length(wrn1) > 0) {
            sapply(wrn1, function(x) mywarn("Mplus error: ", clps(, x), call. = FALSE))
        }
        err1 <- res$errors
        if (length(err1) > 0) {
            sapply(err1, function(x) mywarn("Mplus error: ", clps(, x), call. = FALSE))
        }
    } else {
        res <- NULL
    }

    # invisible(
    #     capture.output(
    #         res <-
    #             suppressMessages(
    #                 MplusAutomation::mplusModeler(
    #                     mplus_input,
    #                     dataout = data_file,
    #                     # dataout = file.path(dir, data_file),
    #                     modelout = file.path(dir, inpu_file),
    #                     run = run,
    #                     check = FALSE,
    #                     writeData = "always",
    #                     hashfilename = FALSE))))

    invisible(list(mplus = res, args = args))
}

#' Prepare an Mplus Input File
#'
#' This is an internal function used by \code{\link{fit_tree_mplus}}. It receives its
#' inputs from the model and the data set and returns an object of class
#' \code{\link[MplusAutomation]{mplusObject}}.
#'
#' @param lambda Matrix as returned from \code{\link{fit_tree_mplus}}.
#' @param pseudoitems Data frame as returned from \code{\link{recode_data}}.
#' @param data_file Character string naming the full file path of the data set.
#' @param fsco_file Character string naming the file used by Mplus to store the factor scores.
#' @param addendum Character string as returned from \code{\link{tree_model}}.
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

    # CAVE: The following line is super important. It is the only safeguard that
    # assures that the items in the 'MODEL'-statement of the mplus input are in
    # the correct order, namely, that defined in the 'Processes'-section of 'model'.
    # This in turn assures that the item thresholds in the output are in that
    # same order---and this is necessary, because otherwise checking the order
    # whilst reading in the output is very cumbersome.
    lambda <- model$lambda
    lambda <- lambda[order(lambda$trait, lambda$item), ]

    # lambda$new_name <- names(pseudoitems)
    lambda$new_name <- attr(pseudoitems, "pseudoitem_names")
    lambda$mplus <- glue::glue_data(lambda, "{new_name}{loading}")

    mplus1 <- split(lambda, lambda$trait)
    # lapply(mplus1, function(x) glue::glue_collapse(glue::glue("{x$mplus}"), sep = " "))

    mplus2 <- vapply(seq_along(mplus1), function(x) {
        glue::glue_collapse(c(glue::glue("{names(mplus1[x])} BY"),
                              glue::glue(" {mplus1[[x]]$mplus}"), ";"), sep = "")
    }, FUN.VALUE = "")

    mplus3 <- paste(
        lapply(
            lapply(c(mplus2, model$addendum),
                   strwrap, width = 89, indent = 2, exdent = 4),
            paste, collapse = "\n"),
        collapse = "\n")

    # mplus_input <- readChar("mlus_template.txt", file.info("mlus_template.txt")$size)

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
    if (save_fscores) {
        SAVEDATA <- c(glue::glue("FILE = {fsco_file};"),
                      glue::glue("SAVE = FSCORES;"))
    } else {
        SAVEDATA <- NULL
    }

    tmp1 <- names(pseudoitems)
    tmp1 <- tmp1[!tmp1 %in% model$j_names]

    mp_cat_vars <-
        paste(
            strwrap(
                paste0("CATEGORICAL = ",
                       paste(attr(pseudoitems,
                                  "pseudoitem_names"), collapse = " "),
                       ";"
                       , "\n\nUSEVARIABLES = ", clps(" ", tmp1), ";"),
                width = 89, indent = 0, exdent = 5),
            collapse = "\n")

    mplus_input <- MplusAutomation::mplusObject(
        TITLE = NULL,
        # DATA = NULL,
        VARIABLE = mp_cat_vars,
        # ANALYSIS = NULL,
        ANALYSIS = ANALYSIS,
        MODEL = mplus3,
        # MODELCONSTRAINT = NULL,
        OUTPUT = "STDYX TECH1 TECH4 TECH8;",
        SAVEDATA = SAVEDATA,
        # PLOT = NULL,
        # usevariables = names(pseudoitems),
        rdata = pseudoitems,
        autov = TRUE
    )

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
#' @inheritParams fit_tree_mplus
#' @return A list of parameter estimates, model fit information
#'   (\code{summaries}), \code{warnings}, \code{errors}.
# @examples
#' @export
extract_mplus_output <- function(results = NULL,
                                 model = NULL) {

    if (!inherits(model, "tree_model")) {
        model <- tree_model(model)
    }

    # results <- res$fit$mplus

    unstd <- results[["parameters"]][["unstandardized"]]
    unstd[unstd$est_se == 999, "se"] <- NA

    if (!(is.null(results[["savedata"]]) | length(results[["savedata"]]) == 0)) {

        fscores <- results[["savedata"]]

        lv_names <- unstd[unstd$paramHeader == "Variances", "param"]

        personpar_est <- fscores[,     which(names(fscores) %in% lv_names), drop = FALSE]
        personpar_se  <- fscores[, 1 + which(names(fscores) %in% lv_names), drop = FALSE]
        colnames(personpar_est) <- tolower(names(personpar_est))
        colnames(personpar_se)  <- tolower(names(personpar_se))

    } else {
        personpar_est <- NULL
        personpar_se  <- NULL
    }

    lambda <- model$lambda

    # sigma  <- results[["fit"]][["mplus"]][["tech4"]][["latCovEst"]]
    # cormat <- results[["fit"]][["mplus"]][["tech4"]][["latCorEst"]]
    sigma  <- results[["tech4"]][["latCovEst"]]
    cormat <- results[["tech4"]][["latCorEst"]]


    if (!is.null(lambda$new_name)) {
        alphapar    <- unstd[tolower(unstd$param) %in%        tolower(lambda$new_name), , drop = FALSE]
        betapar     <- unstd[tolower(unstd$param) %in% paste0(tolower(lambda$new_name), "$1"), , drop = FALSE]
    } else {
        alphapar    <- unstd[grep("[.]BY$", unstd$paramHeader), , drop = FALSE]
        betapar     <- unstd[unstd$paramHeader == "Thresholds", , drop = FALSE]
    }

    itempar <- list()

    if (model$class == "tree") {

        tmp1 <- cbind(lambda[, c("item", "trait")], betapar[, c("est", "se")])
        itempar$beta    <- reshape2::dcast(tmp1, item ~ trait, value.var = "est")
        itempar$beta_se <- reshape2::dcast(tmp1, item ~ trait, value.var = "se")

        tmp1 <- cbind(lambda[, c("item", "trait")], alphapar[, c("est", "se")])
        itempar$alpha    <- reshape2::dcast(tmp1, item ~ trait, value.var = "est")
        itempar$alpha_se <- reshape2::dcast(tmp1, item ~ trait, value.var = "se")

    } else if (model$class == "grm") {

         # tmp1 <- betapar %>%
         #     tidyr::separate(col = "param",
         #                     into = c("item", "threshold"),
         #                     sep = "\\$") %>%
         #     dplyr::select(-est_se, -pval, -paramHeader) %>%
         #     reshape2::melt(id.vars = c("item", "threshold")) %>%
         #     reshape2::dcast(item + variable ~ threshold) %>%
         #     split(., .$variable) %>%
         #     lapply(dplyr::select, -variable)
         tmp1 <- tidyr::separate(betapar, col = "param",
                             into = c("item", "threshold"),
                             sep = "\\$")
         tmp2 <- dplyr::select(tmp1, -est_se, -pval, -paramHeader)
         # tmp3 <- reshape2::melt(tmp2, id.vars = c("item", "threshold"))
         # tmp4 <- reshape2::dcast(tmp3, item + variable ~ threshold)
         tmp4 <- reshape2::recast(tmp2, item + variable ~ threshold, id.var = c("item", "threshold"))
         tmp5 <- split(tmp4, tmp4$variable)
         tmp0 <- lapply(tmp5, dplyr::select, -variable)
         itempar$beta <- tmp0$est
         itempar$beta_se <- tmp0$se

         itempar$alpha <- dplyr::select(alphapar, item = param, est)
         itempar$alpha_se <- dplyr::select(alphapar, item = param, se)

    }

    out <- list(
        person    = list(personpar_est = personpar_est,
                         personpar_se  = personpar_se),
        item      = itempar,
        sigma     = sigma,
        cormat    = cormat,
        summaries = results$summaries,
        warnings  = results$warnings,
        errors    = results$errors
    )

    return(out)
}
