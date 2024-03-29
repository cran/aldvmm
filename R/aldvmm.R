#' aldvmm: Adjusted Limited Dependent Variable Mixture Models
#'
#' The goal of the package 'aldvmm' is to fit adjusted limited dependent
#' variable mixture models of health state utilities. Adjusted limited
#' dependent variable mixture models are finite mixtures of normal
#' distributions with an accumulation of density mass at the limits, and a gap
#' between 100\% quality of life and the next smaller utility value. The
#' package 'aldvmm' uses the likelihood and expected value functions proposed
#' by Hernandez Alava and Wailoo (2015) <doi: 10.1177/1536867X1501500307> using
#' normal component distributions and a multinomial logit model of
#' probabilities of component membership.
#'
#' @examples data(utility)
#'
#'  fit <- aldvmm(eq5d ~ age + female | 1,
#'                data = utility,
#'                psi = c(0.883, -0.594),
#'                ncmp = 2)
#'
#'  summary(fit)
#'
#'  yhat <- predict(fit)
#'
#' @import numDeriv
#' @import stats
#' @import checkmate
#' @import optimx
#' @import Formula
#' @importFrom sandwich estfun
#' @importFrom lmtest coeftest coefci
#'
#' @docType package
#' @name aldvmm-package
NULL

#' @title Fitting Adjusted Limited Dependent Variable Mixture Models
#'
#' @description The function \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{
#'   \code{aldvmm()}} fits adjusted limited dependent variable mixture models
#'   of health state utilities. Adjusted limited dependent variable mixture
#'   models are finite mixtures of normal distributions with an accumulation of
#'   density mass at the limits, and a gap between 100\% quality of life and
#'   the next smaller utility value. The package \code{aldvmm} uses the
#'   likelihood and expected value functions proposed by Hernandez Alava and
#'   Wailoo (2015) using normal component distributions and a multinomial logit
#'   model of probabilities of component membership.
#' @param formula an object of class "formula" with a symbolic
#'   description of the model to be fitted. The model formula takes the form
#'   \code{y ~ x1 + x2 | x1 + x4}, where the \code{|} delimiter separates the
#'   model for expected values of normal distributions (left) and the
#'   multinomial logit model of probabilities of component membership (right).
#' @param data a data frame, list or environment (or object coercible to a data
#'   frame by 
#'   \ifelse{html}{\code{\link[base]{as.data.frame}}}{\code{base::as.data.frame()}}) 
#'   including data on outcomes and explanatory variables in \code{'formula'}.
#' @param subset an optional numeric vector of row indices of the subset of the model 
#'   matrix used in the estimation. \code{'subset'} can be longer than the 
#'   number of rows in \code{data} and include repeated values for re-sampling 
#'   purposes.
#' @param psi a numeric vector of minimum and maximum possible utility values
#'   smaller than or equal to 1 (e.g. \code{c(-0.594, 0.883)}). The potential
#'   gap between the maximum value and 1 represents an area with zero density
#'   in the value set from which utilities were obtained. The order of the
#'   minimum and maximum limits in \code{'psi'} does not matter.
#' @param ncmp a numeric value of the number of components that are mixed. The
#'   default value is 2. A value of 1 represents a tobit model with a gap
#'   between 1 and the maximum value in \code{'psi'}.
#' @param dist an optional character value of the distribution used in the
#'   components. In this release, only the normal distribution is
#'   available, and the default value is set to \code{"normal"}.
#' @param init.method an optional character value indicating the method for
#'   obtaining initial values. The following values are available:
#'   \code{"zero"}, \code{"random"}, \code{"constant"} and \code{"sann"}. The
#'   default value is \code{"zero"}.
#' @param optim.method an optional character value of one of the following
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}}
#'   methods: \code{"Nelder-Mead"}, \code{"BFGS"}, \code{"CG"},
#'   \code{"L-BFGS-B"}, \code{"nlminb"}, \code{"Rcgmin"}, \code{"Rvmmin"} and
#'   \code{"hjn"}. The default method is \code{"BFGS"}. The method
#'   \code{"L-BFGS-B"} is used when lower and/or upper constraints are set
#'   using \code{'init.lo'} and \code{'init.hi'}. The method \code{"nlm"}
#'   cannot be used in the \code{'aldvmm'} package.
#' @param optim.control an optional list of
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}}
#'   control parameters.
#' @param optim.grad an optional logical value indicating if an analytical
#'   gradient should be used in
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}}
#'   methods that can use this information. The default value is \code{TRUE}.
#'   If \code{'optim.grad'} is set to \code{FALSE}, a finite difference
#'   approximation is used.
#' @param init.est an optional numeric vector of user-defined initial values.
#'   User-defined initial values override the \code{'init.method'} argument.
#'   Initial values have to follow the same order as parameter estimates in the
#'   return value \code{'coef'}.
#' @param init.lo an optional numeric vector of user-defined lower limits for
#'   constrained optimization. When \code{'init.lo'} is not \code{NULL}, the
#'   optimization method \code{"L-BFGS-B"} is used. Lower limits of parameters
#'   have to follow the same order as parameter estimates in the return value
#'   \code{'coef'}.
#' @param init.hi an optional numeric vector of user-defined upper limits for
#'   constrained optimization. When \code{'init.hi'} is not \code{NULL}, the
#'   optimization method \code{"L-BFGS-B"} is used. Upper limits of parameters
#'   have to follow the same order as parameter estimates in the return value
#'   \code{'coef'}.
#' @param se.fit an optional logical value indicating whether standard errors
#'   of fitted values are calculated. The default value is \code{FALSE}.
#' @param model an optional logical value indicating whether the estimation 
#' data frame is returned in the output object. The default value is 
#' \code{TRUE}.
#' @param level a numeric value of the significance level for confidence bands
#'   of fitted values. The default value is 0.95.
#' @param na.action a character value passed to 
#' argument \code{'na.action'} of the function 
#' \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame()}} 
#' in the preparation of the model matrix. The default value is 
#' \code{"na.omit"}.
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{ \code{aldvmm()}} fits
#'   an adjusted limited dependent variable mixture model using the likelihood
#'   and expected value functions from Hernandez Alava and Wailoo (2015).  The
#'   model accounts for latent classes, multi-modality, minimum and maximum
#'   utility values and potential gaps between 1 and the next smaller utility
#'   value.  Adjusted limited dependent variable mixture models combine
#'   multiple component distributions with a multinomial logit model of the
#'   probabilities of component membership. The standard deviations of normal
#'   distributions are estimated and reported as log-transformed values which
#'   enter the likelihood function as exponentiated values to ensure
#'   non-negative values.
#'
#'   The minimum utility and the largest utility smaller than or equal to 1 are
#'   supplied in the argument \code{'psi'}.  The number of
#'   distributions/components that are mixed is set by the argument
#'   \code{'ncmp'}. When \code{'ncmp'} is set to 1 the procedure estimates a
#'   tobit model with a gap between 1 and the maximum utility value in
#'   \code{'psi'}.  The current version only allows finite mixtures of normal
#'   distributions.
#'
#'   The \code{'formula'} object can include a \code{|} delimiter to separate
#'   formulae for expected values in components (left) and the multinomial
#'   logit model of probabilities of group membership (right).  If no \code{|}
#'   delimiter is used, the same formula will be used for expected values in
#'   components and the multinomial logit of the probabilities of component
#'   membership.
#'
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{ \code{aldvmm()}} uses
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}} for
#'   maximum likelihood estimation of model parameters.  The argument
#'   \code{'optim.method'} accepts the following methods: \code{"Nelder-Mead"},
#'   \code{"BFGS"}, \code{"CG"}, \code{"L-BFGS-B"}, \code{"nlminb"},
#'   \code{"Rcgmin"}, \code{"Rvmmin"} and \code{"hjn"}. The default method is
#'   \code{"BFGS"}.  The method \code{"nlm"} cannot be used in
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{ \code{aldvmm()}} because it
#'   requires a different implementation of the likelihood function. The
#'   argument \code{'optim.control'} accepts a list of
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}}
#'   control parameters.  If \code{'optim.grad'} is set to \code{TRUE} the
#'   function
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}} uses
#'   analytical gradients during the optimization procedure for all methods 
#'   that allow for this approach. If \code{'optim.grad'} is set to 
#'   \code{FALSE} or a method cannot use gradients, a finite difference 
#'   approximation is used. The hessian matrix at maximum likelihood parameters 
#'   is approximated numerically using 
#'   \ifelse{html}{\code{\link[numDeriv]{hessian}}}{\code{numDeriv::hessian()}}.
#'
#'   \code{'init.method'} accepts four values of methods for generating initial
#'   values: \code{"zero"}, \code{"random"}, \code{"constant"}, \code{"sann"}.
#'   The method \code{"zero"} sets initial values of all parameters to 0. The
#'   method \code{"random"} draws random starting values from a standard normal
#'   distribution.  The method \code{"constant"} estimates a constant-only
#'   model and uses estimates as initial values of intercepts and standard
#'   errors and 0 for all other parameters.  The method \code{"sann"} estimates
#'   the full model using the simulated annealing optimization method in
#'   \ifelse{html}{\code{\link[stats]{optim}}}{ \code{stats::optim()}} and uses
#'   parameter estimates as initial values.  When user-specified initial values
#'   are supplied in \code{'init.est'}, the argument \code{'init.method'} is
#'   ignored.
#'
#'   By default, \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}
#'   performs unconstrained optimization with upper and lower limits at
#'   \code{-Inf} and \code{Inf}.  When user-defined lower and upper limits are
#'   supplied to \code{'init.lo'} and/or \code{'init.hi'}, these default limits
#'   are replaced with the user-specified values, and the method
#'   \code{"L-BFGS-B"} is used for box-constrained optimization instead of the
#'   user defined \code{'optim.method'}.  It is possible to only set either
#'   maximum or minimum limits.  When initial values supplied to 
#'   \code{'init.est'} or from default methods lie outside the limits, the 
#'   in-feasible values will be set to the limits using the function 
#'   \ifelse{html}{\code{\link[optimx]{bmchk}}}{\code{optimx::bmchk()}}.
#'   
#'   The function \code{aldvmm()} returns the negative log-likelihood, Akaike 
#'   information criterion and Bayesian information criterion. Smaller values 
#'   of these measures indicate better fit.
#'
#'   If \code{'se.fit'} is set to \code{TRUE}, standard errors of fitted values
#'   are calculated using the delta method.  The standard errors of fitted
#'   values in the estimation data set are calculated as \eqn{se_{fit} =
#'   \sqrt{G^{t} \Sigma G}}{se_fit = (t(grad)*\Sigma*grad)^0.5}, where \eqn{G}
#'   is the gradient of a fitted value with respect to changes of parameter
#'   estimates, and \eqn{\Sigma} is the estimated covariance matrix of
#'   parameters (Dowd et al., 2014).  The standard errors of predicted values
#'   in new data sets are calculated as \eqn{se_{pred} = \sqrt{MSE + G^{t}
#'   \Sigma G}}{se_pred = (mse + t(grad)*\Sigma*grad)^0.5}, where
#'   \eqn{MSE}{mse} is the mean squared error of fitted versus observed
#'   outcomes in the original estimation data (Whitmore, 1986).
#'   
#'   The generic function
#'   \ifelse{html}{\code{\link[base]{summary}}}{\code{base::summary()}} can be
#'   used to obtain or print a summary of the results. The generic function
#'   \ifelse{html}{\code{\link[stats]{predict}}}{\code{stats::predict()}} can
#'   be used to obtain predicted values and standard errors of predictions in
#'   new data.

#'
#' @return \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{ \code{aldvmm()}}
#'   returns an object of class "aldvmm". An object of class
#'   "aldvmm" is a list containing the following objects. \item{\code{coef}}{a
#'   numeric vector of parameter estimates.} \item{\code{hessian}}{a numeric 
#'   matrix object with second partial derivatives of the likelihood function.}
#'
#'   \item{\code{cov}}{a numeric matrix object with covariances of parameters.}
#'
#'   \item{\code{n}}{a scalar representing the number of observations that were 
#'   used in the estimation.}
#'   \item{\code{k}}{a scalar representing the number of components that were
#'   mixed.}
#'   \item{\code{df.null}}{an integer value of the residual 
#'   degrees of freedom of a null model including intercepts and standard 
#'   errors.}
#'   \item{\code{df.residual}}{an integer value of the residual 
#'   degrees of freedom..}
#'   \item{\code{iter}}{an integer value of the number of iterations used in 
#'   optimization.}
#'   \item{\code{convergence}}{an integer value indicating convergence. "0" 
#'   indicates successful completion.}
#'   
#'   \item{\code{gof}}{a list including the following elements. \describe{
#'   \item{\code{ll}}{a numeric value of the negative log-likelihood
#'   \eqn{-ll}.} 
#'   \item{\code{aic}}{a numeric value of the Akaike information
#'   criterion \eqn{AIC = 2n_{par} - 2ll}{AIC = 2*npar - 2*ll}.}
#'   \item{\code{bic}}{a numeric value of the Bayesian information criterion
#'   \eqn{BIC = n_{par}*log(n_{obs}) - 2ll}{BIC = npar*log(nobs) - 2*ll}.}
#'   \item{\code{mse}}{a numeric value of the mean squared error \eqn{\sum{(y -
#'   \hat{y})^2}/(n_{obs} - n_{par})}{\sum{(y - \hat{y})^2}/(nobs - npar)}.}
#'   \item{\code{mae}}{a numeric value of the mean absolute error \eqn{\sum{|y
#'   - \hat{y}|}/(n_{obs} - n_{par})}{\sum{|y - \hat{y}|}/(nobs - npar)}.}}}
#'
#'   \item{\code{pred}}{a list including the following elements. \describe{
#'   \item{\code{y}}{a numeric vector of observed outcomes in \code{'data'}.}
#'   \item{\code{yhat}}{a numeric vector of fitted values.} \item{\code{res}}{a
#'   numeric vector of residuals.} \item{\code{se.fit}}{a numeric vector of the
#'   standard error of fitted values.} \item{\code{lower.fit}}{a numeric vector
#'   of 95\% lower confidence limits of fitted values.}
#'   \item{\code{upper.fit}}{a numeric vector of 95\% upper confidence limits
#'   of fitted values} \item{\code{prob}}{a numeric matrix of expected 
#'   probabilities of group membership per individual in \code{'data'}.} }}
#'
#'   \item{\code{init}}{a list including the following elements. \describe{
#'   \item{\code{est}}{a numeric vector of initial parameter estimates.}
#'   \item{\code{lo}}{a numeric vector of lower limits of parameter estimates.}
#'   \item{\code{hi}}{a numeric vector of upper limits of parameter
#'   estimates.}} }
#'
#'   \item{\code{call}}{a character value including the model call captured by
#'   \ifelse{html}{\code{\link[base]{match.call}}}{\code{base::match.call()}}.}
#'   \item{\code{formula}}{an object of class "formula" supplied to argument 
#'   \code{'formula'}.}
#'   \item{\code{terms}}{a list of objects of class "terms" for the 
#'   model of component means ("beta"), probabilities of component membership 
#'   ("delta") and the full model ("full").}
#'   \item{\code{contrasts}}{a nested list of character values showing 
#'   contrasts of factors used in models of component means ("beta") and 
#'   probabilities of component membership ("delta").}
#'
#'   \item{\code{data}}{a data frame created by 
#'   \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame}} 
#'   including estimation data with additional attributes.}
#'   \item{\code{psi}}{a numeric vector with the minimum and maximum utility
#'   below 1 in \code{'data'}.}
#'
#'   \item{\code{dist}}{a character value indicating the used component 
#'   distributions.}
#'
#'   \item{\code{label}}{a list including the following elements. \describe{
#'   \item{\code{lcoef}}{a character vector of labels for objects including
#'   results on distributions (default \code{"beta"}) and the probabilities of
#'   component membership (default \code{"delta"}).} \item{\code{lcpar}}{a
#'   character vector of labels for objects including constant distribution
#'   parameters (default \code{"sigma"} for \code{dist = "normal"}).}
#'   \item{\code{lcmp}}{a character value of the label for objects including
#'   results on different components (default "Comp")} \item{\code{lvar}}{a
#'   list including 2 character vectors of covariate names for model parameters
#'   of distributions (\code{"beta"}) and the multinomial logit
#'   (\code{"delta"}).} } }
#'
#'   \item{\code{optim.method}}{a character value of the used
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}}
#'   method.}
#'   \item{\code{level}}{a numeric value of the confidence level used for 
#'   reporting.}
#'   \item{\code{na.action}}{an object of class "omit" extracted from the 
#'   "na.action" attribute of the data frame created by 
#'   \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame}} 
#'   in the preparation of model matrices.}
#'
#' @references Alava, M. H. and Wailoo, A. (2015) Fitting adjusted limited
#'   dependent variable mixture models to EQ-5D. \emph{The Stata Journal},
#'   \bold{15(3)}, 737--750. \doi{10.1177/1536867X1501500307}
#'
#'   Dowd, B. E., Greene, W. H., and Norton, E. C. (2014) Computation of
#'   standard errors. \emph{Health services research}, \bold{49(2)}, 731--750.
#'   \doi{10.1111/1475-6773.12122}
#'
#'   Whitmore, G. A. (1986) Prediction limits for a univariate normal
#'   observation. \emph{The American Statistician}, \bold{40(2)}, 141--143.
#'   \doi{10.1080/00031305.1986.10475378}
#'
#'
#' @examples data(utility)
#'
#'  fit <- aldvmm(eq5d ~ age + female | 1,
#'                data = utility,
#'                psi = c(0.883, -0.594),
#'                ncmp = 2)
#'
#'  summary(fit)
#'
#'  yhat <- predict(fit)
#'
#' @export

aldvmm <- function(formula, 
                   data, 
                   subset = NULL,
                   psi, 
                   ncmp = 2, 
                   dist = "normal", 
                   optim.method = NULL, 
                   optim.control = list(trace = FALSE),
                   optim.grad = TRUE,
                   init.method = "zero", 
                   init.est = NULL,
                   init.lo = NULL,
                   init.hi = NULL,
                   se.fit = FALSE,
                   model = TRUE,
                   level = 0.95,
                   na.action = "na.omit") {
  
  # Labels
  #-------
  
  # Labels of objects including regression coefficients for component 
  # distributions ("beta") and multinomial logit ("delta")
  lcoef <- c("beta", "delta")
  
  # Names of constant distribution parameters (e.g. lnsigma in dist=="normal")
  if (dist == "normal") {
    lcpar <- c("lnsigma")
  }
  
  # Stub for labeling 1:K components
  lcmp <- "Comp"
  
  # Set optimization method
  #------------------------
  
  # The optimization method will be used in aldvmm.init(), the testing of 
  # initial values and the model fitting.
  
  if (all(init.lo == -Inf) & all(init.hi == Inf) & is.null(optim.method)) {
    # Default optimization method
    optim.method <- "BFGS"
  } else if ((!is.null(init.lo) | !is.null(init.hi))) {
    # Constrained optimization with "L-BFGS-B"
    optim.method <- "L-BFGS-B"
  } else {
    # User-defined optimization method
  }
  
  # Attach gradient function based on user selection
  #-------------------------------------------------
  
  if (optim.grad == TRUE) {
    grd <- aldvmm.gr
  } else {
    grd <- NULL
  }
  
  # Convert data to data.frame object
  #----------------------------------
  
  tryCatch({
    data <- as.data.frame(data)
  }, error = function(e) {
    #message(e)
    stop("'data' cannot be converted to data.frame.")
  })
  
  # Checks
  #-------
  
  aldvmm.check(formula = formula, 
               data = data, 
               subset = subset,
               psi = psi, 
               ncmp = ncmp, 
               dist = dist,
               optim.method = optim.method, 
               optim.grad = optim.grad,
               optim.control = optim.control,
               init.method = init.method, 
               init.est = init.est,
               init.lo = init.lo,
               init.hi = init.hi,
               se.fit = se.fit,
               model = model,
               level = level,
               na.action = na.action,
               lcoef = lcoef,
               lcpar = lcpar,
               lcmp = lcmp)
  
  # Convert formula to "Formula" object
  #------------------------------------
  
  formula <- Formula::Formula(formula)

  # Create model frame
  #-------------------
  
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$formula <- formula
  mf[[1]] <- quote(stats::model.frame)
  data <- eval(mf, parent.frame())
  
  # Make outcome vector
  #--------------------
  
  y <- stats::model.response(data)
  
  # Make list of design matrices
  #-----------------------------
  
  mm <- aldvmm.mm(mf = data,
                  Formula = formula,
                  ncmp = ncmp,
                  lcoef = lcoef)
  
  # Make list of model terms
  #-------------------------
  
  terms <- aldvmm.tm(mf = data,
                     Formula = formula,
                     ncmp = ncmp,
                     lcoef = lcoef)
  
  # Generate initial values
  #------------------------
  
  init <- aldvmm.init(X = mm,
                      y = y,
                      dist = dist,
                      psi = psi,
                      ncmp = ncmp,
                      lcoef = lcoef,
                      lcmp = lcmp,
                      lcpar = lcpar,
                      init.method = init.method,
                      init.est = init.est,
                      init.lo = init.lo,
                      init.hi = init.hi,
                      optim.method = optim.method,
                      optim.control = optim.control,
                      optim.grad = optim.grad)
  
  # Check feasibility of initial values
  #------------------------------------
  
  test <- aldvmm.ll(par = init[["est"]],
                    X = mm,
                    y = y,
                    psi = psi,
                    dist = dist,
                    ncmp = ncmp,
                    lcoef = lcoef,
                    lcmp = lcmp,
                    lcpar = lcpar,
                    optim.method = optim.method)
  
  if (!is.finite(test) | (optim.method == "L-BFGS-B" & test >= 1e+20)) {
    stop("Starting values are not feasible.")
  } 
  
  rm(test)
  
  # Fit model
  #----------
  
  fit <- optimx::optimr(par = init[["est"]],
                        fn = aldvmm.ll,
                        X = mm,
                        y = y,
                        psi = psi,
                        ncmp = ncmp,
                        dist = dist,
                        lcoef = lcoef,
                        lcpar = lcpar,
                        lcmp = lcmp,
                        optim.method = optim.method,
                        lower = init[["lo"]],
                        upper = init[["hi"]],
                        method = optim.method,
                        gr = grd,
                        hessian = FALSE,
                        control = optim.control)
  
  # Obtain covariance matrix, standard errors, sign. levels and conf. limits
  #-------------------------------------------------------------------------
  
  cov <- aldvmm.cv(ll = aldvmm.ll,
                   par = fit[["par"]],
                   X = mm,
                   y = y,
                   psi = psi,
                   ncmp = ncmp,
                   dist = dist,
                   lcoef = lcoef,
                   lcpar = lcpar,
                   lcmp = lcmp,
                   optim.method = optim.method)
  
  # Predict outcomes and probabilities of component membership
  #-----------------------------------------------------------
  
  pred <- aldvmm.pred(par = fit[["par"]],
                      X = mm,
                      y = y,
                      psi = psi,
                      ncmp = ncmp,
                      dist = dist,
                      lcoef = lcoef,
                      lcmp = lcmp,
                      lcpar = lcpar)
  
  # Obtain standard errors of the fit (delta method)
  #-------------------------------------------------
  
  if (se.fit == TRUE) {
    pred.se <- aldvmm.sefit(par = fit[["par"]],
                            yhat = pred[["yhat"]],
                            X = mm,
                            type = "fit",
                            cv = cov[["cv"]],
                            mse = gof[["mse"]],
                            psi = psi,
                            ncmp = ncmp,
                            dist = dist,
                            lcoef = lcoef,
                            lcmp = lcmp,
                            lcpar = lcpar,
                            level = level)
    
  } else {
    pred.se <- list(se.fit = NULL,
                    lower.fit = NULL,
                    upper.fit = NULL)
    
  }
  
  # Calculate degrees of freedom
  #-----------------------------
  
  df.null <- length(y) - 
    ncmp * as.integer(attr(terms[[lcoef[1]]], "intercept") > 0L) - 
    (ncmp > 1) * as.integer(attr(terms[[lcoef[2]]], "intercept") > 0L) -
    ncmp # standard errors lnsigma
  
  df.residual <- length(y) - length(fit$par)
  
  # Assess goodness of fit
  #-----------------------
  
  # Note: Aldvmm.ll returns -log-likelihood
  
  gof <- aldvmm.gof(res = pred[["res"]],
                    par = fit[["par"]],
                    ll = -fit[["value"]])
  
  # Collect output
  #---------------
  
  outlist <- new_aldvmm(fit = fit,
                        cov = cov,
                        y = y,
                        mm = mm,
                        ncmp = ncmp,
                        df.null = df.null,
                        df.residual = df.residual,
                        gof = gof,
                        pred = pred,
                        pred.se = pred.se,
                        init = init,
                        call = match.call(),
                        formula = stats::formula(formula),
                        terms = terms,
                        data = if(model == TRUE) {data} else {NULL},
                        psi = psi,
                        dist = dist,
                        lcoef = lcoef,
                        lcpar = lcpar,
                        lcmp = lcmp,
                        optim.method = optim.method,
                        init.method = init.method,
                        level = level,
                        na.action = attr(data, "na.action"))
  
  return(outlist)
  
}
