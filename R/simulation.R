#' Generate IR-Tree Data and Fit Model
#'
#' This function generates data from `gen_model`, subsequently fits all the
#' models in `fit_model`, and returns the results and/or saves them to an
#' external RData file.
#'
#' @param gen_model Object of class `irtree_model` describing the
#'   data-generating model. See [irtree_model] for more information.
#' @param fit_model Object of class `irtree_model` describing the model that
#'   should be fit to the data. May be a list of multiple objects of class
#'   `irtree_model` if different models should be fit to the same data set. See
#'   [irtree_model] for more information.
#' @param save_rdata Logical indicating whether to save the results to an RData
#'   file.
#' @param file String giving the file path used to save the output if
#'   `save_rdata = TRUE`. Note that the file ending is automatically set to
#'   `.rda`. This argument is also passed to [irtree_fit_mplus()] if applicable.
#' @param R Integer used to number the saved output if `save_rdata = TRUE`.
#'   Really only useful when used from [irtree_sim()].
#' @param dots Nested list used to pass further arguments to downstream
#'   methods/functions (i.e., [`fit()`][fit.irtree_model],
#'   [`tidy()`][tidy.irtree_fit]). This is a named list, where the name of
#'   each top-level element corresponds to the respective function. Each
#'   top-level element is again a list, where the names of each element
#'   correspond to the arguments. For example: `dots = list(fit = list(SE =
#'   FALSE), tidy = list(difficulty = TRUE))`.
#' @param .dir Path name that is used to save the results of every run if
#'   `save_rdata = TRUE`.
#' @param reduce_output Logical indicating whether the returned object should be
#'   reduced (i.e., the output of [`fit()`][fit.irtree_model] is removed and
#'   only summary information is retained).
#' @inheritParams irtree_gen_data
#' @inheritParams fit.irtree_model
#' @return List with two elements. The second element `spec` contains various
#'   arguments (such as the data). The first element `fits` is a list with one
#'   element for each `fit_model` that contains the output of
#'   [`fit()`][fit.irtree_model] as well as the elements `glanced`, `tidied`,
#'   and `augmented` (see [glance()], [tidy()], and [augment()]).
#' @seealso [irtree_sim()], and the wrapped functions
#'   [`fit()`][fit.irtree_model] and [irtree_gen_data()].
#' @export
irtree_sim1 <- function(gen_model = NULL,
                        fit_model = gen_model,
                        N = NULL,
                        sigma = NULL,
                        itempar = NULL,
                        link = c("probit", "logit"),
                        engine = c("mirt", "mplus", "tam"),
                        verbose = TRUE,
                        save_rdata = FALSE,
                        file = NULL,
                        R = 1,
                        reduce_output = FALSE,
                        # ...,
                        dots = list(),
                        .dir = tempdir(),
                        .na_okay = TRUE) {

    checkmate::qassert(R, "X1[0,)")
    checkmate::qassert(save_rdata, "B1")
    if (is.null(file)) {
        file <- tempfile(sprintf("irtree%05d_", R), .dir)
    }

    engine <- match.arg(engine)
    link <- match.arg(link)

    X <- irtree_gen_data(object = gen_model,
                         N = N,
                         sigma = sigma,
                         itempar = itempar,
                         link = link)

    tmp1 <- c("N", "J", "link", "personpar",
              "sigma", "sigma_fun",
              "itempar", "itempar_fun")
    tmp2 <- grep(paste(tmp1, collapse = "|"), names(X$spec))

    spec <- X$spec[tmp2]
    spec$data <- X$data
    spec$gen_model <- gen_model
    spec$pid  <- Sys.getpid()
    spec$user <- Sys.getenv(c("USERDOMAIN", "USERNAME"))

    if (checkmate::test_class(fit_model, "irtree_model")) {
        fit_model <- list(fit_model)
    }
    for (ii in seq_along(fit_model)) {
        checkmate::assert_class(fit_model[[ii]], "irtree_model")
    }

    tmp1 <- vapply(fit_model, `[[`, "K", FUN.VALUE = 1)
    if (isTRUE(var(tmp1, na.rm = TRUE) > 0)) {
        stop("Inconsistencies of categories K across fit_model.")
    }

    tmp2 <- vapply(fit_model,
                   function(x) ifelse(is.null(x$k_names), NA, min(x$k_names)),
                   FUN.VALUE = 1)
    if (isTRUE(var(tmp2, na.rm = TRUE) > 0)) {
        stop("Inconsistencies across fit_model. ",
             "Note that the lowest category is '0' for some models, ",
             "which must then be used consistently across models.")
    }

    checkmate::qassert(dots, "L<3")
    checkmate::assert_subset(names(dots), c("fit", "tidy"))

    # Fit ---------------------------------------------------------------------

    fits <- list()

    for (ii in seq_along(fit_model)) {

        mii <- paste0("m", ii)

        do_call_args <- c(list(object = fit_model[[ii]],
                               data = X$data,
                               engine = engine,
                               verbose = verbose,
                               link = link),
                          dots$fit)
        if (engine == "mplus") {
            do_call_args$file <- sprintf("%s_m%1d",
                                         tools::file_path_sans_ext(file), ii)
        } else if (engine == "tam") {
            do_call_args$.set_min_to_0 <- TRUE
        }
        fits[[mii]]$fit <- do.call("fit", do_call_args)

        fits[[mii]]$glanced <- glance(fits[[mii]]$fit)
        # fits[[mii]]$tidied <- tidy(fits[[mii]]$fit)
        fits[[mii]]$tidied <- do.call("tidy",
                                      c(list(x = fits[[mii]]$fit),
                                        dots$tidy))

        tmp1 <- augment(fits[[mii]]$fit)
        fits[[mii]]$augmented <- spec$personpar %>%
            tibble::as_tibble() %>%
            dplyr::bind_cols(dplyr::select(tmp1,
                                           -suppressWarnings(
                                               dplyr::one_of(
                                                   names(fits[[mii]]$fit$spec$data)))))
        fits[[mii]]$fit$spec$data <- NULL
    }

    res <- list()
    res$fits <- fits
    res$spec <- spec

    if (save_rdata) {
        assign(sprintf("res%05d", R), res)
        do.call("save", list(sprintf("res%05d", R),
                             file = paste0(tools::file_path_sans_ext(file), ".rda")))
    }
    if (reduce_output) {
        res$fits <- purrr::map(res$fits, ~purrr::assign_in(.x, list("fit", 1), list()))
    }
    if (save_rdata) {
        return(invisible(res))
    } else {
        return(res)
    }
}

#' Run a Simulation by Generating From and Fitting a Tree Model
#'
#' The function [irtree_sim1()] generates a data set from an IR-tree
#' model and fits one or more models to that data set. Herein, this
#' process is repeated `R` times, and the argument `plan` allows to
#' run the simulation in parallel.
#'
#' @param R Number of replications. Can be either a single number indicating the
#'   number of replications (e.g., `R = 100`), or can be a range (e.g., `R =
#'   1:100`).
#' @param plan Parameter passed as argument `strategy` to [future::plan()]. May
#'   be set to, for example, `multiprocess` in order to run the simulations in
#'   parallel.
#' @param future_args Named list. Parameters passed [future::plan()].
# @param in_memory Logical. If `TRUE`, results are stored in memory and
#   [`return`]ed on exit; if `FALSE`, nothing is stored and [`return`]ed, and this
#   makes only sense in combination with `save_rdata = TRUE`. This latter
#   combination may be suitable for long/large simulations that would otherwise
#   exceed memory.
#' @param in_memory Character string indicating what output should be kept in
#'   memory (note the argument `save_rdata`, which is not affected by
#'   `in_memory`).
#' @param ... Other parameters passed [irtree_sim1()].
#' @return Returns a list of length `R`, where each element is the output of
#'   [irtree_sim1()]. If `in_memory = "nothing"`, returns `NULL`.
#' @inheritParams irtree_sim1
#' @seealso [irtree_sim1()]
#' @example inst/examples/example-sim.R
#' @export
irtree_sim <- function(gen_model = NULL,
                       fit_model = gen_model,
                       N = NULL,
                       sigma = NULL,
                       itempar = NULL,
                       link = c("probit", "logit"),
                       engine = c("mirt", "mplus", "tam"),
                       verbose = FALSE,
                       save_rdata = TRUE,
                       file = NULL,
                       R = 1,
                       plan = NULL,
                       future_args = list(),
                       in_memory = c("reduced", "everything", "nothing"),
                       # ...,
                       dots = list(),
                       .dir = tempdir(),
                       .na_okay = TRUE) {

    time1 <- Sys.time()

    checkmate::assert_function(sigma)
    checkmate::assert_function(itempar)
    checkmate::qassert(R, "X>0[0,)")
    if (length(R) == 1) {
        Rseq <- seq_len(R)
    } else {
        Rseq <- R
    }
    in_memory <- match.arg(in_memory)
    if (save_rdata == FALSE & in_memory == "nothing") {
        warning("You probably want to either write the results to disk ",
                "('save_rdata = TRUE') or/and keep the results in memory ",
                "(see 'in_memory') in order to return them on exit.",
                call. = FALSE)
    }

    oplan <- future::plan()
    on.exit(future::plan(oplan), add = TRUE)

    ### future ###

    do.call(future::plan, args = c(list(strategy = plan), future_args))

    res1 <- listenv::listenv()

    p <- progress::progress_bar$new(
        format = "[:bar] :percent; :elapsedfull",
        # show_after = 30,
        total = length(Rseq), clear = TRUE, incomplete = " ", complete = "-")

    if (in_memory != "nothing") {

        for (rr in Rseq) {

            res1[[which(rr == Rseq)]] <-
                future::future({
                    irtree_sim1(gen_model = gen_model,
                                fit_model = fit_model,
                                N = N,
                                sigma = sigma,
                                itempar = itempar,
                                link = link,
                                engine = engine,
                                verbose = verbose,
                                save_rdata = save_rdata,
                                R = rr,
                                .dir = .dir,
                                reduce_output = switch(in_memory,
                                                       reduced = TRUE, FALSE),
                                dots = dots
                    )
                })

            p$tick()
        }

        res <- lapply(res1, FUN = future::value)

        tmp1 <- difftime(Sys.time(), time1)
        message(sprintf("Time difference of %.1f %s.", tmp1, units(tmp1)))

        names(res) <- paste0("sim", Rseq)
        return(res)

    } else {

        for (rr in Rseq) {

            future::future({
                irtree_sim1(gen_model = gen_model,
                            fit_model = fit_model,
                            N = N,
                            sigma = sigma,
                            itempar = itempar,
                            link = link,
                            engine = engine,
                            verbose = verbose,
                            save_rdata = save_rdata,
                            R = rr,
                            .dir = .dir,
                            dots = dots)
            })

            p$tick()
        }

        tmp1 <- difftime(Sys.time(), time1)
        message(sprintf("Time difference of %.1f %s.", tmp1, units(tmp1)))

        return(invisible(NULL))
    }
}
