test_that("Check covariance function.", {
  
  # Valid covariance matrix
  #------------------------
  
  mm <- matrix(data     = c(rep(1, 4), runif(n = 8)), 
               nrow     = 4, 
               ncol     = 3,
               dimnames = list(NULL,
                               c("(Intercept)", "ind1", "ind2")))
  
  y <- runif(n = 4)
  
  init <- rep(1, ncol(mm))
  
  optim.method <- "BFGS"
  
  test <- function(par, 
                   X = X, 
                   y = y,
                   psi,
                   ncmp,
                   dist,
                   lcoef,
                   lcmp,
                   lcpar,
                   optim.method) {
    sum(stats::pnorm((y - X %*% par)))
  }
  
  cov <- aldvmm.cv(ll = test,
                   par = init,
                   X = mm,
                   y = y,
                   psi = c(0.883, -0.594),
                   ncmp = 2,
                   dist = "normal",
                   lcoef = c("beta", "delta"),
                   lcmp = "Grp",
                   lcpar = c("sigma"),
                   optim.method = optim.method)
  
  testthat::expect(sum(unlist(lapply(cov, function(c) !is.numeric(c)))) == 0,
                   failure_message = "Some elements of list are not numeric."
  )
  testthat::expect(sum(unlist(lapply(cov, function(c) !is.finite(c)))) == 0,
                   failure_message = "Some elements of list are not finite."
  )
  testthat::expect(is.matrix(cov[["cv"]]),
                   failure_message = "Covariance matrix is not matrix."
  )
  testthat::expect(sum(diag(cov[["cv"]]) < 0) == 0,
                   failure_message = 
                     "Covariance matrix includes negative values in diagonal."
  )
  wlength <- unlist(lapply(cov, function(c) sum(dim(c) != length(init))))
  testthat::expect(sum(wlength) == 0,
                   failure_message = 
                     "Some elements of output are of wrong length.")
  rm(wlength)

  # Covariance matrix with non-positive diagonals
  #----------------------------------------------
  
  data(utility)
  
  formula <- eq5d ~ age + female | age + female
  
  psi <- c(0.883, -0.594)
  ncmp <- 2
  
  suppressWarnings({
    fit <- aldvmm(data = utility[1:100, ],
                  formula = formula,
                  psi = psi,
                  ncmp = ncmp,
                  optim.method = "Nelder-Mead")
  })
  
  mm <- aldvmm.mm(mf = stats::model.frame(Formula::Formula(formula), data = utility),
                  Formula = Formula::Formula(formula),
                  ncmp = ncmp,
                  lcoef = fit$label$lcoef)
  
  suppressWarnings(
    testthat::expect_warning(aldvmm.cv(ll = aldvmm.ll,
                                       par = fit$coef,
                                       X = mm,
                                       y = utility$eq5d,
                                       psi = psi,
                                       ncmp = ncmp,
                                       dist = "normal",
                                       lcoef = fit$label$lcoef,
                                       lcmp = fit$label$lcmp,
                                       lcpar = fit$label$lcpar,
                                       optim.method = fit$optim.method))
  )
  
  testthat::expect(all(is.na(cov[["p"]]) == is.na(cov[["se"]])),
                   failure_message = 
                     "Missing p-values do not match missing standard errors.")
  testthat::expect(all(is.na(cov[["z"]]) == is.na(cov[["se"]])),
                   failure_message = 
                     "Missing z-values do not match missing standard errors.")
  testthat::expect(all(is.na(cov[["lower"]]) == is.na(cov[["se"]])),
                   failure_message = 
                     "Missing lower limits do not match missing standard 
                   errors.")
  testthat::expect(all(is.na(cov[["upper"]]) == is.na(cov[["se"]])),
                   failure_message = 
                     "Missing lower limits do not match missing standard 
                   errors.")
  
  # No covariance matrix obtained
  #------------------------------
  
  data(utility)
  
  formula <- eq5d ~ female + age | female + age
  
  psi <- c(0.883, -0.594)
  ncmp <- 2
  
  suppressWarnings({
    fit <- aldvmm(formula = formula,
                  data = utility[1:20, ],
                  psi = psi,
                  ncmp = ncmp,
                  optim.method = "Nelder-Mead")
  })
  
  mm <- aldvmm.mm(mf = stats::model.frame(Formula::Formula(formula), data = utility),
                  Formula = Formula::Formula(formula),
                  ncmp = ncmp,
                  lcoef = fit$label$lcoef)
  
  suppressWarnings(
    testthat::expect_warning(aldvmm.cv(ll = aldvmm.ll,
                                       par = fit$coef,
                                       X = mm,
                                       y = utility$eq5d,
                                       psi = psi,
                                       ncmp = ncmp,
                                       dist = "normal",
                                       lcoef = fit$label$lcoef,
                                       lcmp = fit$label$lcmp,
                                       lcpar = fit$label$lcpar,
                                       optim.method = fit$optim.method))
  )
  
})

