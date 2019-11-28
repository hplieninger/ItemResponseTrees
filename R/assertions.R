assert_irtree_equations <- function(object = NULL) {
    if (!is.null(object$equations)) {
        irtree_model_check_equations(object$equations, unique(object$latent_names$mpt))
    }
}

assert_irtree_proper <- function(object = NULL, .improper_okay = FALSE) {
    if (.improper_okay == FALSE & object$proper_model == FALSE) {
        stop("The model seems to be an improper model. You might set ",
             "'.improper_okay' to TRUE, but do this only if you really ",
             "know what you are doing.")
    }
}
