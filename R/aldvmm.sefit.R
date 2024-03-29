#' Calculating Standard Errors of Fitted and Predicted Outcomes
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sefit}}}{\code{aldvmm.sefit()}}
#' calculates standard errors of fitted and predicted outcomes using the delta
#' method.
#'
#' @param yhat a numeric vector of predicted outcomes returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm.pred()}}.
#'
#' @param type a character value of either 'fit' or 'pred' indicating whether
#'   the standard error of the fit ('fit') or the standard error of predictions
#'   in new data ('pred') are calculated.
#' @param cv a numeric matrix with covariances/variances of parameter estimates
#'   returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm.cv()}}.
#'
#'
#' @param mse a numeric value of the mean squared error of observed versus
#'   predicted outcomes \eqn{\sum{(y - \hat{y})^2}/(n_{obs} - n_{par})}{\sum{(y
#'   - \hat{y})^2}/(nobs - npar)} for all observations in model matrices
#'   \code{'X'} supplied to
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}.
#'
#'
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sefit}}}{\code{aldvmm.sefit()}}
#' calculates standard errors of fitted values using the delta method. Standard
#' errors of fitted values in the estimation data set are calculated as
#' \eqn{se_{fit} = \sqrt{G^{t} \Sigma G}}{se_fit = (t(grad)*\Sigma*grad)^0.5},
#' where \eqn{G} is the gradient of a fitted value with respect to changes of
#' parameter estimates, and \eqn{\Sigma} is the estimated covariance matrix of
#' parameters (Dowd et al., 2014).  Standard errors of predicted values in new
#' data sets are calculated as \eqn{se_{pred} = \sqrt{MSE + G^{t} \Sigma
#' G}}{se_pred = (mse + t(grad)*\Sigma*grad)^0.5}, where \eqn{MSE}{mse} is the
#' mean squared error of fitted versus observed outcomes in the original
#' estimation data (Whitmore, 1986).  The gradients of fitted values with
#' respect to parameter estimates are approximated numerically using
#' \ifelse{html}{\code{\link[numDeriv]{jacobian}}}{\code{numDeriv::jacobian()}}.
#'
#' @references Whitmore, G. A. (1986). Prediction limits for a univariate
#'   normal observation. The American Statistician, 40(2), 141-143.
#'   https://doi.org/10.1080/00031305.1986.10475378 \cr
#'
#'   Dowd, B. E., Greene, W. H., and Norton, E. C. (2014) Computation of
#'   standard errors. \emph{Health services research}, \bold{49(2)}, 731--750.
#'   \doi{10.1111/1475-6773.12122}
#'
#' @return a named numeric vector of standard errors of fitted or predicted
#'   outcomes. The names of the elements in the vector are identical to the row
#'   names of design matrices in \code{'X'}
#'
#' @export

aldvmm.sefit <- function(par,
                         yhat,
                         X,
                         type,
                         psi,
                         cv,
                         mse = NA,
                         ncmp,
                         dist,
                         level,
                         lcoef,
                         lcmp,
                         lcpar) {

  # Check if yhat has correct names
  #--------------------------------
  
  if (is.null(names(yhat))) {
    stop("'yhat' does not have names.")
  }

  if (!all(rownames(X[[1]]) %in% names(yhat))) {
    stop("Row names of 'newdata' are not subset of names of 'yhat'")
  }
  
  # Check validity of covariance matrix
  #------------------------------------
  
  if (any(is.na(cv))) {
    warning("missing values in covariance matrix: ",
            "No standard errors of the fit obtained\n",
            call. = FALSE)
    return(NULL)
  }
  
  if (any(diag(cv) <= 0)) {
    warning("non-positive diagonals in covariance matrix: ",
            "No standard errors of the fit obtained\n", 
            call. = FALSE)
    return(NULL)
  }
  
  # Calculate jacobian matrix numerically
  #--------------------------------------
  
  jacobian <- numDeriv::jacobian(func = function(z) {
    aldvmm.pred(par   = z,
                X     = X,
                y     = rep(0, nrow(X[[1]])),
                psi   = psi,
                ncmp  = ncmp,
                dist  = dist,
                lcoef = lcoef,
                lcmp  = lcmp,
                lcpar = lcpar)[["yhat"]]
  },
  x = par)
  
  # Calculate standard errors in loop over all observations
  #--------------------------------------------------------
  
  se.fit <- rep(NA, times = nrow(X[[1]]))
  names(se.fit) <- rownames(X[[1]])
  
  if (!(type %in% c("fit", "pred"))) {
    warning("'type' ",
            'is not "fit" or "pred": "pred" is used\n',
            call. = FALSE)
  }
  
  if (type == "fit") {
    for (i in 1:nrow(X[[1]])) {
      se.fit[i] <- sqrt(t(jacobian[i, ]) %*% cv %*% jacobian[i, ])
    }
  } else {
    if (is.na(mse)) {
      warning("'mse' is missing: Standard errors of the fit are generated\n",
              call. = FALSE)
      for (i in 1:nrow(X[[1]])) {
        se.fit[i] <- sqrt(t(jacobian[i, ]) %*% cv %*% jacobian[i, ])
      }
    } else{
      for (i in 1:nrow(X[[1]])) {
        se.fit[i] <- sqrt(mse + t(jacobian[i, ]) %*% cv %*% jacobian[i, ])
      }
    }
  }
  
  # Confidence / prediction interval
  #---------------------------------
  
  ul <- yhat[rownames(X[[1]])] + stats::qnorm((1 + level)/2) * se.fit[rownames(X[[1]])]
  ul[ul > max(psi)] <- 1
  names(ul) <- rownames(X[[1]])
  
  ll <- yhat[rownames(X[[1]])] - stats::qnorm((1 + level)/2) * se.fit[rownames(X[[1]])]
  ll[ll < min(psi)] <- min(psi)
  names(ll) <- rownames(X[[1]])
  
  return(list(se.fit    = se.fit,
              upper.fit = ul,
              lower.fit = ll))
  
}
