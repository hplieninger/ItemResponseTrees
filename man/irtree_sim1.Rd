% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{irtree_sim1}
\alias{irtree_sim1}
\title{Generate data from and fit an ItemResponseTrees model}
\usage{
irtree_sim1(
  R = 1,
  gen_model = NULL,
  fit_model = gen_model,
  N = NULL,
  sigma = NULL,
  itempar = NULL,
  link = c("logit", "probit"),
  na_okay = TRUE,
  engine = c("mirt", "mplus", "tam"),
  verbose = TRUE,
  control = NULL,
  improper_okay = FALSE,
  par_type = "difficulty",
  file = NULL,
  dir = tempdir(),
  save_rdata = FALSE,
  reduce_output = FALSE
)
}
\arguments{
\item{R}{Integer used to number the saved output if \code{save_rdata = TRUE}.
Really only useful when used from \code{\link[=irtree_sim]{irtree_sim()}}.}

\item{gen_model}{Object of class \code{irtree_model} describing the
data-generating model. See \link{irtree_model} for more information.}

\item{fit_model}{Object of class \code{irtree_model} describing the model that
should be fit to the data. May be a list of multiple objects of class
\code{irtree_model} if different models should be fit to the same data set. See
\link{irtree_model} for more information.}

\item{N}{Integer, the number of persons.}

\item{sigma}{Either a matrix or a function that returns a matrix. This matrix
is the variance-covariance matrix of the person parameters that is passed
to \code{\link[MASS:mvrnorm]{MASS::mvrnorm()}}. Note that the order of the person
parameters is taken from the section Processes in the model \code{object} (see
\link{irtree_model}).}

\item{itempar}{Either a list or a function that returns a list. The list has
an element \code{beta} and an element \code{alpha}. Each of these is a
matrix of item parameters. Note that the order of items (rows) is taken from the
section Items and the order of processes (columns) is taken from the
section Processes in the \code{model} (see \link{irtree_model}).}

\item{link}{Character. Link function to use.}

\item{na_okay}{Logical indicating whether variables with unobserved response
categories are permitted. If \code{FALSE}, rejection sampling
is used to ensure that all categories are observed.}

\item{engine}{String specifying whether to use mirt, Mplus, or TAM for
estimation.}

\item{verbose}{Logical indicating whether output should be printed to the
console.}

\item{control}{List. The allowed elements of this list depend on the
\code{engine}. Use \code{\link[=control_mirt]{control_mirt()}}, \code{\link[=control_mplus]{control_mplus()}}, or \code{\link[=control_tam]{control_tam()}} for
convenience. Note that the \code{fit()} function does not use \code{...}, but that
you can use the \verb{control_*()} functions to pass additional arguments.}

\item{improper_okay}{Logical indicating whether the model should also be fit
if it is not a proper IR-tree model. Set this only to \code{TRUE} if you really
know what you are doing.}

\item{par_type}{Only used if the fit engine was mirt. Item parameters (or
thresholds) can be either of type \code{easiness} (the mirt default) or
\code{difficulty} (as in Mplus and TAM).}

\item{file}{String giving the file path used to save the output if
\code{save_rdata = TRUE}. Note that the file ending is automatically set to
\code{.rda}. This argument is also passed to \code{\link[=irtree_fit_mplus]{irtree_fit_mplus()}} if applicable.}

\item{dir}{Path name that is used to save the results of every run if
\code{save_rdata = TRUE}.}

\item{save_rdata}{Logical indicating whether to save the results to an RData
file.}

\item{reduce_output}{Logical indicating whether the returned object should be
reduced (i.e., the output of \code{\link[=fit.irtree_model]{fit()}} is removed and
only summary information is retained).}
}
\value{
List with two elements. The second element \code{spec} contains various
arguments (such as the data). The first element \code{fits} is a list with one
element for each \code{fit_model} that contains the output of
\code{\link[=fit.irtree_model]{fit()}} as well as the elements \code{glanced}, \code{tidied},
and \code{augmented} (see \code{\link[=glance]{glance()}}, \code{\link[=tidy]{tidy()}}, and \code{\link[=augment]{augment()}}).
}
\description{
This function generates data from \code{gen_model}, subsequently fits all the
models in \code{fit_model}, and returns the results and/or saves them to an
external RData file.
}
\seealso{
\code{\link[=irtree_sim]{irtree_sim()}}, and the wrapped functions
\code{\link[=fit.irtree_model]{fit()}} and \code{\link[=irtree_gen_data]{irtree_gen_data()}}.
}
\keyword{internal}
