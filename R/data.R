#' @docType data
#' @name utility
#' @title Simulated Example Data of Health State Utilities.
#'
#' @description \ifelse{html}{\code{\link[aldvmm]{utility}}}{\code{utility}} is
#' a simulated data frame including health state utilities and patients' age
#' and sex.
#'
#' @format A data frame with 200 rows and 3 variables:\tabular{ll}{
#'   \code{utility} \tab a utility value \{[-0.594, 0.883], 1\}. \cr \tab \cr
#'   \code{age} \tab Age in years. \cr \tab \cr \code{female} \tab Indicator of
#'   female sex. \cr \tab \cr }
#'
#' @examples
#' set.seed(101010101)
#' utility                 <- data.frame(female = rbinom(size = 1,
#'                                       n      = 200,
#'                                       p      = 0.6))
#' utility[, 'age']        <- stats::rnorm(n    = 200,
#'                                         mean = 50 + utility$female*10,
#'                                         sd   = 15)
#' utility[1:50, 'eq5d']   <- stats::rnorm(n    = 50,
#'                                         mean = 0 - 0.1 *
#'                                               utility[1:50, 'female'] +
#'                                               0.0005 * utility[1:50, 'age'],
#'                                         sd   = 0.1)
#' utility[51:200, 'eq5d'] <- stats::rnorm(n    = 150,
#'                                         mean = 0.5 +
#'                                            0.1 * utility[51:200, 'female'] +
#'                                            0.0001*utility[51:200, 'age'],
#'                                         sd   = 0.2)
#' utility[utility$eq5d<(-0.594), 'eq5d'] <- -0.594
#' utility[utility$eq5d>0.883, 'eq5d'] <- 1
#' hist(utility$eq5d, breaks = 50)
#' 
'utility'
