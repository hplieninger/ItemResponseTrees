# Load packages required for tests ----------------------------------------

library("dplyr")

# Define functions required for tests -------------------------------------

gen_itempar_boeck <- function(J = 10, loading = 1) {

    # mrs
    m <- qnorm(.7)
    ss <- sqrt(.2)
    i1 <- rtruncatednorm(J, mean = m, sd = ss,
                         ll = m - 2 * ss, ul = m + 2 * ss)
    # ers
    m <- qnorm(.7)
    ss <- sqrt(.2)
    i2 <- rtruncatednorm(J, mean = m, sd = ss,
                         ll = m - 2 * ss, ul = m + 2 * ss)

    # trt
    m <- qnorm(.5)
    ss <- sqrt(.4)
    i3 <- rtruncatednorm(J, mean = m, sd = ss,
                         ll = m - 2 * ss, ul = m + 2 * ss)

    out <- list()
    out$beta <- cbind(i1, i2, i3)

    if (all(loading == 1)) {
        out$alpha <- matrix(1, J, 3)
    } else {
        tmp1 <- runif(J * 3, .5, 1.5)
        fixed <- grepl("@", loading)
        fix_load <- sub("@", "", loading)
        tmp1[fixed] <- as.numeric(fix_load[fixed])
        out$alpha <- matrix(tmp1, J, 3)
    }

    return(out)
}

gen_sigma_boeck <- function(sigma = NULL, df = 50, vars = NULL) {

    if (is.null(sigma)) {
        sigma  <- matrix(c(1, .4, 0,
                           .4, 1, 0,
                           0, 0, 1), 3, 3, byrow = TRUE)
    }

    sig1 <- rWishart(1, df = df, Sigma = sigma)[, , 1]

    if (is.null(vars)) {
        vars <- c(rgamma(2, shape = 51,  scale = .01),
                  rgamma(1, shape = 101, scale = .01))
        sds <- sqrt(vars)
    }

    sig2 <- diag(sds) %*% cov2cor(sig1) %*% diag(sds)

    return(sig2)

}

remove_filenames <- function(object = NULL) {
    oclass <- class(object$mplus)
    object$mplus <- lapply(object$mplus, `attr<-`, "filename", NULL)
    object["mplus"] <- lapply(object["mplus"], `attr<-`, "filename", NULL)
    object$mplus$input$title <- NULL
    object$mplus$input$savedata <- NULL
    object$mplus$input$data$file <- NULL
    object$mplus$summaries$Title <- NULL
    object$mplus$summaries$Filename <- NULL
    object$mplus$parameters <- lapply(object$mplus$parameters, `attr<-`, "filename", NULL)
    object$mplus$savedata_info$fileName <- NULL
    object$spec$control$file <- NULL

    class(object$mplus) <- oclass
    return(object)
}
