% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aldvmm.getnames.R
\name{aldvmm.getnames}
\alias{aldvmm.getnames}
\title{Creating Names of Parameter Vectors}
\usage{
aldvmm.getnames(X, names, lcoef, lcpar, lcmp, ncmp)
}
\arguments{
\item{X}{a list of design matrices returned by
\ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}}.
\code{'X'} is of length 2 and includes a design matrix for the model of
component distributions and a design matrix for the model of probabilities
of group membership.}

\item{names}{a character vector of names of considered elements
(distributions \code{"beta"}, multinomial logit \code{"delta"} or constant
distribution parameters, i.e. \code{"lnsigma"}). The elements in
\code{'names'} are combined with covariate names in \code{'X'} and
component labels in \code{'lcmp'} to create a vector of names of parameter
vectors.}

\item{lcoef}{a character vector of length 2 with labels of objects including
regression coefficients of component distributions (default \code{"beta"})
and coefficients of probabilities of component membership (default
\code{"delta"}).}

\item{lcpar}{a character vector with the labels of objects including
constant parameters of component distributions (e.g. the standard
deviation of the normal distribution). The length of \code{'lcpar'}
depends on the distribution supplied to \code{'dist'}.}

\item{lcmp}{a character value representing a stub (default \code{"Comp"})
for labeling objects including regression coefficients in different
components (e.g. "Comp1", "Comp2", ...). This label is also used in
summary tables returned by
\ifelse{html}{\code{\link[aldvmm]{summary.aldvmm}}}{\code{summary.aldvmm()}}.}

\item{ncmp}{a numeric value of the number of components that are mixed. The
default value is 2. A value of 1 represents a tobit model with a gap
between 1 and the maximum value in \code{'psi'}.}
}
\value{
a character vector of names of parameter vectors used in
  \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
}
\description{
\ifelse{html}{\code{\link[aldvmm]{aldvmm.getnames}}}{\code{aldvmm.getnames()}}
creates names of parameter vectors used in
\ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}. The order of
the elements in \code{'lcoef'} and \code{'lcpar'} determines the order of
parameters and the structure of summary tables returned by
\ifelse{html}{\code{\link[aldvmm]{summary.aldvmm}}}{\code{summary.aldvmm()}}.
}
