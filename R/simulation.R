#' Generate IR-Tree Data and Fit Model
#'
#' This function generates data from \code{gen_model}, subsequently fits the
#' \code{fit_model} in Mplus, and returns the results or saves them to an
#' external RData file.
#'
#' @param gen_model A description of the user-specified model that is used to
#'   generate the data. See \code{\link{irtree_model}} for more information.
#' @param fit_model A description of the user-specified model that is fitted to
#'   the data. See \code{\link{irtree_model}} for more information.
#' @param save_rdata Logical indicating whether to save the results to an RData
#'   file.
#' @param ... Other parameters passed to [fit.irtree_model()].
#' @inheritParams fit.irtree_model
#' @inheritParams irtree_sim_data
#' @inheritParams irtree_fit_mplus
#' @return List of three elements. \code{fit} contains the Mplus output,
#'   \code{gen_args} contains arguments used to generate the data, and
#'   \code{sim_args} contains arguments used to fit the model.
#' @seealso \code{\link{irtree_fit_mplus}}, \code{\link{irtree_sim_data}}
# @examples
# \dontrun{
# if(interactive()){
#  #EXAMPLE1
#  }
# }
#' @export
run_1sim <- function(gen_model = NULL,
                     fit_model = gen_model,
                     N = NULL,
                     sigma = NULL,
                     link = c("probit", "logit"),
                     itempar = NULL,
                     engine = c("mirt", "mplus"),
                     dir = NULL,
                     # R = 1,
                     save_rdata = FALSE,
                     ...) {

    backend <- match.arg(backend)

    gen_model <- irtree_model(gen_model)

    fit_model <- irtree_model(fit_model)

    # fit_model <- force(fit_model)
    #
    # gen_model <- irtree_model(model = gen_model)
    # if (gen_model != fit_model) {
    #     fit_model <- irtree_model(model = fit_model)
    # } else {
    #     fit_model <- gen_model
    # }

    X <- irtree_sim_data(object = gen_model,
                         # model2 = gen_model2,
                         N = N,
                         sigma = sigma,
                         link = link,
                         itempar = itempar)

    tmp1 <- c("N", "link", "personpar",
              "sigma", "sigma_fun",
              "itempar", "itempar_fun")
    tmp2 <- grep(paste(tmp1, collapse = "|"), names(X$args))

    gen_args <- X$args[tmp2]

    sim_args <- list(gen_model  = gen_model,
                     # gen_model2 = gen_model2,
                     fit_model  = fit_model,
                     # fit_model2 = fit_model2,
                     pid = Sys.getpid(),
                     user = Sys.getenv(c("USERDOMAIN", "USERNAME")))

    out <- list(fit = NULL,
                gen_args = gen_args,
                sim_args = sim_args)

    out$fit <- fit(object = fit_model,
                   data = X$data,
                   engine = engine,
                   dir = dir,
                   link = link,
                   # R = R,
                   ...)

    if (engine == "mplus") {
        if (!is.null(out$fit$mplus)) {
            out$fit$est <- extract_mplus_output(object = out$fit$mplus, model = out$fit$args$model)
        }
    } else if (engine == "mirt") {
        if (!is.null(out$fit$mirt)) {
            out$fit$est <- extract_mirt_output(object = out$fit$mirt, model = out$fit$args$model)
        }
    }

    if (save_rdata) {
        save(out, file = file.path(dir, sprintf("%s_tree_%05d.RData", backend, R)))
        invisible(out)
    } else {
        return(out)
    }
}

#' Run a Simulation by Generating From and Fitting a Tree Model
#'
#' The function \code{\link{run_1sim}} generates a data set from an IR-tree
#' model and fits a possibly different model to that data set. Herein, this
#' process is repeated \code{R} times, and the argument \code{plan} allows to
#' run the simulation in parallel.
#'
#' @param R Number of replications
#' @param plan Parameter passed as argument \code{strategy} to
#'   \code{\link[future]{plan}}. May be set to, for example, \code{multisession}
#'   in order to run the simulations in parallel.
#' @param future_args Named list. Parameters passed \code{\link[future]{plan}}.
#' @param ... Other parameters passed \code{\link{run_1sim}}.
#' @inheritParams run_1sim
#' @return XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# @examples
# \dontrun{
# if(interactive()){
#  #EXAMPLE1
#  }
# }
#' @export
run_sim <- function(R = 1,
                    plan = NULL,
                    future_args = list(),
                    gen_model = NULL,
                    fit_model = gen_model,
                    N = NULL,
                    sigma = NULL,
                    link = c("probit", "logit"),
                    itempar = NULL,
                    dir = NULL,
                    save_rdata = FALSE,
                    ...) {

    time1 <- Sys.time()

    checkmate::assert_function(sigma)
    checkmate::assert_function(itempar)

    oplan <- future::plan()
    on.exit(future::plan(oplan), add = TRUE)

    ### future ###

    do.call(future::plan, args = c(list(strategy = plan), future_args))

    res1 <- listenv::listenv()

    p <- progress::progress_bar$new(
        # format = "[:bar] :percent ~:eta remaining",
        format = "[:bar] :current/:total ~:eta remaining",
        total = R, clear = TRUE, incomplete = " ", complete = "-")

    for (rr in seq_len(R)) {

        res1[[rr]] <-
            future::future({
                run_1sim(R = rr,
                         gen_model = gen_model,
                         fit_model = fit_model,
                         N = N,
                         sigma = sigma,
                         link = link,
                         itempar = itempar,
                         dir = dir,
                         save_rdata = save_rdata,
                         ...)
            })

        p$tick()
    }

    res <- lapply(res1, FUN = future::value)

    tmp1 <- difftime(Sys.time(), time1)
    message(sprintf("Time difference of %.1f %s.",
                    tmp1, units(tmp1)))

    names(res) <- paste0("sim", 1:R)
    return(res)
}
