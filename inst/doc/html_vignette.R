## ----setup, eval = TRUE, echo = FALSE, results = 'hide'-----------------------

# Load packages
#--------------

library("aldvmm")
library("xtable")
library("ggplot2")
library("scales")
library("reshape2")
library("kableExtra")

# Load functions
#---------------

source("rep_tab_fit.R")
source("rep_tab_stata.R")
source("est_mhl.R")
source("rep_tab_reg.R")

# Custom ggplot theme
#--------------------

ggplot_theme <- theme(panel.background = element_rect(fill = "white",
                                                      colour = "white",
                                                      #linewidth = 0.5, 
                                                      linetype = "solid"),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.grid.major.y = element_line(#linewidth = 0.25, 
                        linetype = 'dashed', 
                        colour = "grey"),
                      text = element_text(size = 11),
                      plot.margin = margin(10, 10, 0, 10),
                      #axis.line = element_line(linewidth = 0.25),
                      axis.text = element_text(size = 11),
                      #legend.title = element_text(size = 11),
                      legend.title = element_blank(),
                      legend.text = element_text(size = 11),
                      legend.position = 'bottom',
                      legend.direction = "vertical",
                      legend.key = element_blank())

# Figure position HERE
#---------------------

knitr::opts_chunk$set(fig.pos = "!H", out.extra = "")
warnings(file = "./R-warnings.txt")


## ----load_data, eval = FALSE, warning = FALSE, echo = FALSE, results = 'hide'----
#  
#  # List to collect results used in text
#  #-------------------------------------
#  
#  textout <- list()
#  
#  # Download data
#  #--------------
#  
#  temp <- tempfile()
#  download.file(paste0("https://files.digital.nhs.uk/publicationimport",
#                       "/pub11xxx/pub11359/final-proms-eng-apr11-mar12",
#                       "-data-pack-csv.zip"), temp)
#  df <- read.table(unz(description = temp,
#                       filename = 'Hip Replacement 1112.csv'),
#                   sep = ',',
#                   header = TRUE)
#  unlink(temp)
#  rm(temp)
#  
#  # Recode data
#  #------------
#  
#  df <- df[df$AGEBAND != '*' & df$SEX != '*',
#           c('AGEBAND', 'SEX', 'Q2_EQ5D_INDEX', 'HR_Q2_SCORE')]
#  
#  df$eq5d <- df$Q2_EQ5D_INDEX
#  df$hr <- df$HR_Q2_SCORE/10
#  
#  # Select sample
#  #--------------
#  
#  textout[["n"]] <- nrow(df)
#  df <- df[stats::complete.cases(df), ]
#  textout[["ncplt"]] <- nrow(df)
#  
#  set.seed(101010101)
#  df <- df[sample(1:nrow(df), size = nrow(df)*0.3), ]
#  textout[["nspl"]] <- nrow(df)
#  
#  # Population average Oxford Hip Score
#  #------------------------------------
#  
#  textout[["meanhr"]] <- mean(df[, "hr"], na.rm = TRUE)
#  
#  # Save output for text
#  #---------------------
#  
#  save(textout,
#       file = "textout.RData",
#       compress = TRUE)
#  
#  rm(textout)
#  
#  # Save data for plots
#  #--------------------
#  
#  save(df,
#       file = "df.RData",
#       compress = FALSE)
#  
#  rm(df)
#  

## ----calc_fit, eval = FALSE, warning = FALSE, echo = FALSE, results = 'hide'----
#  
#  load("df.RData")
#  
#  #------------------------------------------------------------------------------
#  # Fit model 1
#  #------------------------------------------------------------------------------
#  
#  formula <- eq5d ~ hr | 1
#  
#  # Assess all optimization methods
#  #--------------------------------
#  
#  init.method <- c("zero", "random", "constant", "sann")
#  
#  optim.method <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B",
#                    #"nlm",
#                    "nlminb",
#                    "Rcgmin", "Rvmmin","hjn")
#  
#  fit1_all <- list()
#  
#  for (i in init.method) {
#    for (j in optim.method) {
#  
#      cat(paste0("\n", i, " - ", j, "\n"))
#  
#      start <- Sys.time()
#  
#      set.seed(101010101)
#      fit <- tryCatch({
#        aldvmm::aldvmm(data = df,
#                       formula = formula,
#                       psi = c(0.883, -0.594),
#                       ncmp = 2,
#                       init.method = i,
#                       optim.method = j,
#                       model = FALSE)
#  
#      }, error = function(e) {
#        return(list())
#      })
#  
#      end <- Sys.time()
#  
#      fit1_all[["fit"]][[i]][[j]] <- fit
#      fit1_all[["time"]][[i]][[j]] <- difftime(end, start, units = c("mins"))
#      rm(fit, start, end)
#  
#    }
#  }
#  
#  save(fit1_all,
#       file = "fit1_all.RData",
#       compress = TRUE)
#  
#  rm(fit1_all, init.method, optim.method, i, j)
#  
#  # Default model ("nlminb")
#  #-------------------------
#  
#  # With standard errors of fitted values for comparison to STATA
#  
#  fit1_nlminb <- aldvmm::aldvmm(data = df,
#                                formula = formula,
#                                psi = c(0.883, -0.594),
#                                ncmp = 2,
#                                optim.method = "nlminb",
#                                se.fit = TRUE)
#  
#  save(fit1_nlminb,
#       file = "fit1_nlminb.RData",
#       compress = TRUE)
#  
#  rm(fit1_nlminb)
#  
#  # Constrained optimization
#  #-------------------------
#  
#  # c(0.2358245, 0.1458986, -0.4306492, 0.3134739, 0.7282714, -2.4622704, -1.2480114)
#  
#  init <- c(0,    0,   0,   0,    0,    0,    0.7283)
#  lo   <- c(-Inf, -Inf, -3,  -Inf, -Inf, -3, -Inf)
#  hi   <- c(Inf,  Inf,  Inf, Inf,  Inf,  Inf,  Inf)
#  
#  fit1_cstr <- aldvmm::aldvmm(data = df,
#                              formula = formula,
#                              psi = c(0.883, -0.594),
#                              ncmp = 2,
#                              init.est = init,
#                              init.lo = lo,
#                              init.hi = hi)
#  
#  save(fit1_cstr,
#       file = "fit1_cstr.RData",
#       compress = TRUE)
#  
#  rm(fit1_cstr, init, lo, hi)
#  
#  # Constrained optimization, Hernandez Alava and Wailoo (2015)
#  #-----------------------------------------------------------
#  
#  # With standard errors of fitted values for comparison to STATA
#  
#  init <- c(-.0883435, .2307964, 100, 0, 7.320611, -1.646109, 1.00e-30)
#  lo <- c(-Inf, -Inf, 100, -Inf, -Inf, -Inf, 1e-30)
#  hi <- c(Inf,   Inf, Inf,    0,  Inf,  Inf, Inf)
#  
#  fit1_cstr_stata <- aldvmm::aldvmm(data = df,
#                                    formula = formula,
#                                    psi = c(0.883, -0.594),
#                                    ncmp = 2,
#                                    init.est = init,
#                                    init.lo = lo,
#                                    init.hi = hi,
#                                    se.fit = TRUE)
#  
#  save(fit1_cstr_stata,
#       file = "fit1_cstr_stata.RData",
#       compress = TRUE)
#  
#  rm(fit1_cstr_stata, init, lo, hi)
#  
#  # Constant-only initial values ("nlminb")
#  #----------------------------------------
#  
#  # With standard errors of fitted values for comparison to STATA
#  
#  fit1_cons <- aldvmm::aldvmm(data = df,
#                              formula = formula,
#                              psi = c(0.883, -0.594),
#                              ncmp = 2,
#                              init.method = "constant",
#                              optim.method = "nlminb",
#                              se.fit = TRUE)
#  
#  save(fit1_cons,
#       file = "fit1_cons.RData",
#       compress = TRUE)
#  
#  rm(fit1_cons)
#  
#  # Single-component model
#  #-----------------------
#  
#  fit1_tobit <- aldvmm::aldvmm(data = df,
#                               formula = formula,
#                               psi = c(0.883, -0.594),
#                               ncmp = 1,
#                               optim.method = "nlminb")
#  
#  save(fit1_tobit,
#       file = "fit1_tobit.RData",
#       compress = TRUE)
#  
#  rm(fit1_tobit)
#  rm(formula)
#  
#  #------------------------------------------------------------------------------
#  # Fit model 2
#  #------------------------------------------------------------------------------
#  
#  formula <- eq5d ~ hr | hr
#  
#  # Zero starting values
#  #---------------------
#  
#  fit2 <- aldvmm::aldvmm(data = df,
#                         formula = formula,
#                         psi = c(0.883, -0.594),
#                         ncmp = 2,
#                         optim.method = "nlminb")
#  
#  save(fit2,
#       file = "fit2.RData",
#       compress = TRUE)
#  
#  rm(fit2)
#  
#  # Starting values from Hernandez Alava and Wailoo (2015)
#  #-------------------------------------------------------
#  
#  # With standard errors of fitted values for comparison to STATA
#  
#  init <- c(-.40293118, .30502755, .22614716, .14801581, -.70755741, 0,
#            -1.2632051, -2.4541401)
#  
#  fit2_stata <- aldvmm::aldvmm(data = df,
#                               formula = formula,
#                               psi = c(0.883, -0.594),
#                               ncmp = 2,
#                               init.est = init,
#                               optim.method = "nlminb",
#                               se.fit = TRUE)
#  
#  save(fit2_stata,
#       file = "fit2_stata.RData",
#       compress = TRUE)
#  
#  rm(fit2_stata, init)
#  
#  rm(formula)
#  rm(df)
#  

## ----calc_plot, eval = FALSE, warning = FALSE, echo = FALSE, results = 'hide'----
#  
#  # Load data
#  #----------
#  
#  load("df.RData")
#  load("textout.RData")
#  
#  # Histogram of observed outcomes
#  #-------------------------------
#  
#  plot_hist_obs <- ggplot2::ggplot(df, aes(x = eq5d)) +
#    geom_histogram(binwidth = 0.02, color="black", fill="white") +
#    xlab("EQ-5D-3L utilities") +
#    ylab("Frequency") +
#    scale_y_continuous(labels = scales::comma) +
#    ggplot_theme
#  
#  ggsave("plot_hist_obs.png",
#         width = 6,
#         height = 2.9,
#         dpi = 150)
#  
#  rm(plot_hist_obs)
#  
#  # Histogram of predicted outcomes Model 1, zero starting values, nlminb
#  #---------------------------------------------------------------------------
#  
#  load("fit1_all.RData")
#  
#  plotdf <- data.frame(yhat = fit1_all[["fit"]][["zero"]][["nlminb"]][["pred"]][["yhat"]])
#  
#  plot_hist_pred <- ggplot2::ggplot(plotdf, aes(x = yhat)) +
#    geom_histogram(binwidth = 0.02, color="black", fill="white") +
#    xlab("EQ-5D-3L utilities") +
#    ylab("Frequency") +
#    scale_y_continuous(labels = scales::comma) +
#    ggplot_theme
#  
#  ggsave("plot_hist_pred.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plotdf, plot_hist_pred, fit1_all)
#  
#  # Comparison of predicted values from different optimization methods
#  #-------------------------------------------------------------------
#  
#  load("fit1_all.RData")
#  
#  a <- fit1_all[["fit"]][["zero"]][["BFGS"]][["pred"]][["yhat"]]
#  b <- fit1_all[["fit"]][["zero"]][["Nelder-Mead"]][["pred"]][["yhat"]]
#  c <- fit1_all[["fit"]][["zero"]][["hjn"]][["pred"]][["yhat"]]
#  d <- fit1_all[["fit"]][["zero"]][["nlminb"]][["pred"]][["yhat"]]
#  
#  a.d <- a - d
#  b.d <- b - d
#  c.d <- c - d
#  d.d <- d - d
#  
#  tmpdf <- as.data.frame(rbind(cbind(out = d.d, bl = d, alg = "nlminb", id = 1),
#                               cbind(out = a.d, bl = d, alg = "BFGS vs. nlminb", id = 2),
#                               cbind(out = b.d, bl = d, alg = "Nelder-Mead vs. nlminb", id = 3),
#                               cbind(out = c.d, bl = d, alg = "hjn vs. nlminb", id = 4)))
#  
#  tmpdf <- unique(tmpdf)
#  tmpdf <- tmpdf[order(tmpdf$id, tmpdf$bl), ]
#  
#  tmpdf[, "out"] <- as.numeric(as.character(tmpdf[, "out"]))
#  tmpdf[, "bl"] <- as.numeric(as.character(tmpdf[, "bl"]))
#  tmpdf[, "alg"] <- factor(tmpdf[, "alg"], levels = unique(tmpdf[order(tmpdf$id), "alg"]))
#  
#  plot_comp_pred <- ggplot2::ggplot(tmpdf, aes(x = bl, y = out, group = alg, color = alg)) +
#    geom_line(aes(linetype = alg)) +
#    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
#    scale_color_manual(values = c("black", "green", "blue", "red")) +
#    scale_x_continuous(breaks = seq(-0.2, 1, by = 0.1)) +
#    xlab("E[y|X] nlminb") +
#    ylab("Difference from E[y|X] nlminb") +
#    guides(linetype = guide_legend(nrow = 1)) +
#    ggplot_theme +
#    theme(panel.grid.major.x = element_blank(),
#          panel.grid.major.y = element_blank())
#  
#  ggsave("plot_comp_pred.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(fit1_all, a, b, c, tmpdf, plot_comp_pred)
#  
#  # Modified Hosmer-Lemeshow test by optimization method
#  #-----------------------------------------------------
#  
#  load("fit1_all.RData")
#  
#  set.seed(101010101)
#  plot_comp_mhl_bfgs <- est_mhl(fit1_all[["fit"]][["zero"]][["BFGS"]],
#                                ngroup = 10)
#  
#  ggsave("plot_comp_mhl_bfgs.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plot_comp_mhl_bfgs)
#  
#  set.seed(101010101)
#  plot_comp_mhl_nm <- est_mhl(fit1_all[["fit"]][["zero"]][["Nelder-Mead"]],
#                              ngroup = 10)
#  
#  ggsave("plot_comp_mhl_nm.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plot_comp_mhl_nm)
#  
#  set.seed(101010101)
#  plot_comp_mhl_nlminb <- est_mhl(fit1_all[["fit"]][["zero"]][["nlminb"]],
#                                  ngroup = 10)
#  
#  ggsave("plot_comp_mhl_nlminb.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plot_comp_mhl_nlminb)
#  
#  set.seed(101010101)
#  plot_comp_mhl_hjn <- est_mhl(fit1_all[["fit"]][["zero"]][["hjn"]],
#                               ngroup = 10)
#  
#  ggsave("plot_comp_mhl_hjn.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plot_comp_mhl_hjn)
#  
#  rm(fit1_all)
#  
#  # Box plot of fitted values in R and STATA
#  #-----------------------------------------
#  
#  # Load aldvmm objects
#  load("fit1_nlminb.RData")
#  load("fit1_cstr_stata.RData")
#  load("fit1_cons.RData")
#  load("fit2_stata.RData")
#  stata_yhat <- read.table("stata_yhat.csv",
#                           header = TRUE,
#                           sep = ";",
#                           dec = ".")
#  
#  stata_yhat_long <- stata_yhat
#  names(stata_yhat_long) <- c("Ref. case 1",
#                              "Ref. case 2",
#                              "Ref. case 3",
#                              "Ref. case 4")
#  suppressMessages(stata_yhat_long <- reshape2::melt(stata_yhat_long))
#  stata_yhat_long$variable <- as.character(stata_yhat_long$variable)
#  stata_yhat_long$software <- "STATA"
#  
#  r_yhat_long <- rbind(cbind(variable = "Ref. case 1",
#                             value = fit1_nlminb[["pred"]][["yhat"]],
#                             software = "R"),
#                       cbind(variable = "Ref. case 2",
#                             value = fit1_cstr_stata[["pred"]][["yhat"]],
#                             software = "R"),
#                       cbind(variable = "Ref. case 3",
#                             value = fit1_cons[["pred"]][["yhat"]],
#                             software = "R"),
#                       cbind(variable = "Ref. case 4",
#                             value = fit2_stata[["pred"]][["yhat"]],
#                             software = "R"))
#  
#  r_yhat_long <- as.data.frame(r_yhat_long)
#  
#  plotdf <- rbind(stata_yhat_long, r_yhat_long)
#  
#  suppressWarnings(
#    plot_box_yhat <- ggplot2::ggplot(plotdf,
#                                     aes(x = variable,
#                                         y = as.numeric(value),
#                                         fill = software)) +
#      geom_boxplot() +
#      ylab("Fitted values") +
#      ggplot_theme +
#      theme(axis.title.x = element_blank()) +
#      guides(fill = guide_legend(nrow = 1, byrow = TRUE))
#  )
#  
#  ggsave("plot_box_yhat.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plotdf, plot_box_yhat)
#  rm(fit1_cons, fit1_cstr_stata, fit1_nlminb)
#  rm(fit2_stata)
#  rm(stata_yhat, stata_yhat_long)
#  rm(r_yhat_long)
#  
#  # Box plot of standard errors of fitted values in R and STATA
#  #------------------------------------------------------------
#  
#  load("fit1_nlminb.RData")
#  load("fit1_cstr_stata.RData")
#  load("fit1_cons.RData")
#  load("fit2_stata.RData")
#  stata_se <- read.table("stata_se.csv",
#                         header = TRUE,
#                         sep = ";",
#                         dec = ".")
#  
#  stata_se_long <- stata_se
#  names(stata_se_long) <- c("Ref. case 1",
#                            "Ref. case 2",
#                            "Ref. case 3",
#                            "Ref. case 4")
#  suppressMessages(stata_se_long <- reshape2::melt(stata_se_long))
#  stata_se_long$variable <- as.character(stata_se_long$variable)
#  stata_se_long$software <- "STATA"
#  
#  r_se_long <- rbind(cbind(variable = "Ref. case 1",
#                           value = fit1_nlminb[["pred"]][["se.fit"]],
#                           software = "R"),
#                     # cbind(variable = "Ref. case 2",
#                     #       value = fit1_cstr_stata[["pred"]][["se.fit"]],
#                     #       software = "R"),
#                     cbind(variable = "Ref. case 3",
#                           value = fit1_cons[["pred"]][["se.fit"]],
#                           software = "R"),
#                     cbind(variable = "Ref. case 4",
#                           value = fit2_stata[["pred"]][["se.fit"]],
#                           software = "R"))
#  
#  r_se_long <- as.data.frame(r_se_long)
#  
#  plotdf <- rbind(stata_se_long, r_se_long)
#  
#  suppressWarnings(
#    plot_box_se <- ggplot2::ggplot(plotdf,
#                                   aes(x = variable,
#                                       y = as.numeric(value),
#                                       fill = software)) +
#      geom_boxplot() +
#      ylab("Standard errors of fitted values") +
#      ggplot_theme +
#      theme(axis.title.x = element_blank()) +
#      guides(fill = guide_legend(nrow = 1, byrow = TRUE))
#  )
#  
#  ggsave("plot_box_se.png",
#         width = 6,
#         height = 2.9,        dpi = 150)
#  
#  rm(plotdf, plot_box_se)
#  rm(fit1_cons, fit1_cstr_stata, fit1_nlminb)
#  rm(fit2_stata)
#  rm(stata_se, stata_se_long)
#  rm(r_se_long)
#  rm(df)
#  

## ----calc_tab, eval = FALSE, warning = FALSE, echo = FALSE, results = 'hide'----
#  
#  # Load data
#  #----------
#  
#  load("df.RData")
#  
#  # Model 1, comparison of optimization methods
#  #--------------------------------------------
#  
#  load("fit1_all.RData")
#  
#  tab_comp_ll <- matrix(NA,
#                        nrow     = length(fit1_all[["fit"]]),
#                        ncol     = length(fit1_all[["fit"]][[1]]),
#                        dimnames = list(names(fit1_all[["fit"]]),
#                                        names(fit1_all[["fit"]][[1]])))
#  tab_comp_time <- tab_comp_ll
#  tab_comp_cov <- tab_comp_ll
#  tab_comp_mse <- tab_comp_ll
#  tab_comp_mae <- tab_comp_ll
#  
#  for (i in names(fit1_all[["fit"]])) { # Initial value methods
#    for (j in names(fit1_all[["fit"]][[1]])) { # Optimization methods
#      if (length(fit1_all[["fit"]][[i]][[j]]) != 0) {
#        tab_comp_ll[i, j] <- -fit1_all[["fit"]][[i]][[j]][["gof"]][["ll"]]
#        tab_comp_time[i, j] <- fit1_all[["time"]][[i]][[j]]
#        cov <- fit1_all[["fit"]][[i]][[j]][["cov"]]
#        tab_comp_cov[i, j] <- sum(diag(cov) < 0) == 0 &
#          sum(is.na(diag(cov))) == 0
#        rm(cov)
#        tab_comp_mse[i, j] <- fit1_all[["fit"]][[i]][[j]][["gof"]][["mse"]]
#        tab_comp_mae[i, j] <- fit1_all[["fit"]][[i]][[j]][["gof"]][["mae"]]
#      }
#    }
#  }
#  
#  save(tab_comp_ll,
#       file = "tab_comp_ll.RData",
#       compress = TRUE)
#  
#  save(tab_comp_time,
#       file = "tab_comp_time.RData",
#       compress = TRUE)
#  
#  save(tab_comp_cov,
#       file = "tab_comp_cov.RData",
#       compress = TRUE)
#  
#  save(tab_comp_mse,
#       file = "tab_comp_mse.RData",
#       compress = TRUE)
#  
#  save(tab_comp_mae,
#       file = "tab_comp_mae.RData",
#       compress = TRUE)
#  
#  rm(tab_comp_ll, tab_comp_time, tab_comp_cov, tab_comp_mse, tab_comp_mae, i, j)
#  rm(fit1_all)
#  
#  # Model 1 comparison of coefficients by optimization method
#  #----------------------------------------------------------
#  
#  load("fit1_all.RData")
#  
#  tmplist1 <- rep_tab_fit(fit1_all[["fit"]][["zero"]][["BFGS"]])
#  tmplist2 <- rep_tab_fit(fit1_all[["fit"]][["zero"]][["Nelder-Mead"]])
#  tmplist3 <- rep_tab_fit(fit1_all[["fit"]][["zero"]][["nlminb"]])
#  tmplist4 <- rep_tab_fit(fit1_all[["fit"]][["zero"]][["hjn"]])
#  
#  tab_comp_coef <- cbind(tmplist1$table[, 1:3],
#                         tmplist2$table[, 3],
#                         tmplist3$table[, 3],
#                         tmplist4$table[, 3])
#  
#  tab_comp_coef <- tab_comp_coef[-nrow(tab_comp_coef),]
#  
#  tab_comp_coef <- rbind(tab_comp_coef,
#                         c("ll", "",
#                           round(fit1_all[["fit"]][["zero"]][["BFGS"]][["gof"]][["ll"]], 2),
#                           round(fit1_all[["fit"]][["zero"]][["Nelder-Mead"]][["gof"]][["ll"]], 2),
#                           round(fit1_all[["fit"]][["zero"]][["nlminb"]][["gof"]][["ll"]], 2),
#                           round(fit1_all[["fit"]][["zero"]][["hjn"]][["gof"]][["ll"]], 2)))
#  names(tab_comp_coef)  <- c("", "", "BFGS", "Nelder-Mead", "nlminb", "hjn")
#  
#  save(tab_comp_coef,
#       file = "tab_comp_coef.RData",
#       compress = TRUE)
#  
#  rm(tmplist1, tmplist2, tmplist3, tmplist4, tab_comp_coef)
#  rm(fit1_all)
#  
#  # Summary table model 1, zero, BFGS
#  #----------------------------------
#  
#  load("fit1_all.RData")
#  
#  tab_sum_mod1bfgs <- rep_tab_fit(fit1_all[["fit"]][["zero"]][["BFGS"]])
#  
#  save(tab_sum_mod1bfgs,
#       file = "tab_sum_mod1bfgs.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_mod1bfgs)
#  rm(fit1_all)
#  
#  # Summary table model 1, zero, nlminb
#  #------------------------------------
#  
#  load("fit1_all.RData")
#  
#  tab_sum_mod1nlminb <- rep_tab_fit(fit1_all[["fit"]][["zero"]][["nlminb"]])
#  
#  save(tab_sum_mod1nlminb,
#       file = "tab_sum_mod1nlminb.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_mod1nlminb)
#  rm(fit1_all)
#  
#  # Summary table model 1, constant-only model as starting values, nlminb
#  #----------------------------------------------------------------------
#  
#  load("fit1_all.RData")
#  
#  tab_sum_const <- rep_tab_fit(fit1_all[["fit"]][["constant"]][["nlminb"]])
#  
#  save(tab_sum_const,
#       file = "tab_sum_const.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_const)
#  rm(fit1_all)
#  
#  # Summary table model 1, zero, constrained
#  #-----------------------------------------
#  
#  load("fit1_cstr.RData")
#  
#  tab_sum_cstr <- rep_tab_fit(fit1_cstr)
#  
#  save(tab_sum_cstr,
#       file = "tab_sum_cstr.RData",
#       compress = TRUE)
#  
#  rm(fit1_cstr, tab_sum_cstr)
#  
#  # Summary table model 1, constrained with stata estimates as starting values
#  #---------------------------------------------------------------------------
#  
#  load("fit1_cstr_stata.RData")
#  
#  tab_sum_cstata <- rep_tab_fit(fit1_cstr_stata)
#  
#  save(tab_sum_cstata,
#       file = "tab_sum_cstata.RData",
#       compress = TRUE)
#  
#  rm(fit1_cstr_stata, tab_sum_cstata)
#  
#  # Summary table model 1, single-component, zero starting values, nlminb
#  #----------------------------------------------------------------------
#  
#  load("fit1_tobit.RData")
#  
#  tab_sum_tobit <- rep_tab_fit(fit1_tobit)
#  
#  save(tab_sum_tobit,
#       file = "tab_sum_tobit.RData",
#       compress = TRUE)
#  
#  rm(fit1_tobit, tab_sum_tobit)
#  
#  # Summary table model 2, nlminb
#  #------------------------------
#  
#  load("fit2.RData")
#  
#  tab_sum_mod2 <- rep_tab_fit(fit2)
#  
#  save(tab_sum_mod2,
#       file = "tab_sum_mod2.RData",
#       compress = TRUE)
#  
#  rm(fit2, tab_sum_mod2)
#  
#  # Summary table model 2, stata estimates as starting values, nlminb
#  #------------------------------------------------------------------
#  
#  load("fit2_stata.RData")
#  
#  tab_sum_mod2stata <- rep_tab_fit(fit2_stata)
#  
#  save(tab_sum_mod2stata,
#       file = "tab_sum_mod2stata.RData",
#       compress = TRUE)
#  
#  rm(fit2_stata, tab_sum_mod2stata)
#  
#  # Summary statistics of differences in fitted values between R and STATA
#  #-----------------------------------------------------------------------
#  
#  load("fit1_nlminb.RData")
#  load("fit1_cstr_stata.RData")
#  load("fit1_cons.RData")
#  load("fit2_stata.RData")
#  stata_yhat <- read.table("stata_yhat.csv",
#                           header = TRUE,
#                           sep = ";",
#                           dec = ".")
#  stata_se <- read.table("stata_se.csv",
#                         header = TRUE,
#                         sep = ";",
#                         dec = ".")
#  
#  
#  # Fitted values
#  sumhat <- list()
#  
#  sumhat[["yhat"]][["Ref. case 1"]] <- summary(stata_yhat[, 1] - fit1_nlminb[["pred"]][["yhat"]])
#  sumhat[["yhat"]][["Ref. case 2"]] <- summary(stata_yhat[, 2] - fit1_cstr_stata[["pred"]][["yhat"]])
#  sumhat[["yhat"]][["Ref. case 3"]] <- summary(stata_yhat[, 3] - fit1_cons[["pred"]][["yhat"]])
#  sumhat[["yhat"]][["Ref. case 4"]] <- summary(stata_yhat[, 4] - fit2_stata[["pred"]][["yhat"]])
#  
#  tab_diff_yhat <- suppressWarnings( round(do.call("cbind", sumhat[["yhat"]]), 6) )
#  tab_diff_yhat <- tab_diff_yhat[1:6, ]
#  
#  save(tab_diff_yhat,
#       file = "tab_diff_yhat.RData",
#       compress = TRUE)
#  
#  # Standard errors of fitted values
#  
#  sumhat[["se"]][["Ref. case 1"]] <- summary(stata_se[, 1] - fit1_nlminb[["pred"]][["se.fit"]])
#  sumhat[["se"]][["Ref. case 2"]] <- summary(stata_se[, 2] - fit1_cstr_stata[["pred"]][["se.fit"]])
#  sumhat[["se"]][["Ref. case 3"]] <- summary(stata_se[, 3] - fit1_cons[["pred"]][["se.fit"]])
#  sumhat[["se"]][["Ref. case 4"]] <- summary(stata_se[, 4] - fit2_stata[["pred"]][["se.fit"]])
#  
#  tab_diff_se <- suppressWarnings( round(do.call("cbind", sumhat[["se"]]), 6) )
#  tab_diff_se <- tab_diff_se[1:6, ]
#  
#  save(tab_diff_se,
#       file = "tab_diff_se.RData",
#       compress = TRUE)
#  
#  rm(fit1_cons, fit1_cstr_stata, fit1_nlminb)
#  rm(fit2_stata)
#  rm(tab_diff_se, tab_diff_yhat)
#  rm(stata_se, stata_yhat)
#  rm(sumhat)
#  
#  # Comparison of R and STATA results
#  #---------------------------------
#  
#  # Load summary tables
#  load("tab_sum_mod1nlminb.RData")
#  load("tab_sum_cstata.RData")
#  load("tab_sum_const.RData")
#  load("tab_sum_mod2stata.RData")
#  load("tab_sum_stata1.RData")
#  load("tab_sum_stata2.RData")
#  load("tab_sum_stata3.RData")
#  load("tab_sum_stata4.RData")
#  
#  # Model keys
#  tabvec <- c("tab_sum_mod1nlminb",  "tab_sum_stata1",
#              "tab_sum_cstata",     "tab_sum_stata2",
#              "tab_sum_const", "tab_sum_stata3",
#              "tab_sum_mod2stata", "tab_sum_stata4")
#  
#  # Matrix of coefficients
#  tab_rstata_coef <- matrix(NA,
#                 nrow = nrow(tab_sum_mod2stata$table),
#                 ncol = length(tabvec) + 2)
#  
#  tab_rstata_coef[, 1:2] <- as.matrix(tab_sum_mod2stata$table[, 1:2])
#  tab_rstata_coef[nrow(tab_rstata_coef), 2] <- ""
#  
#  # Matrix of standard errors
#  tab_rstata_se <- tab_rstata_coef
#  
#  # Populate tables
#  for (i in tabvec) {
#    nr <- nrow(get(i)$table)
#    for (j in 1:(nr - 1)) {
#  
#      lltmp <- format(
#        as.numeric(gsub("[^0-9.-]", "", as.matrix(get(i)$table)[nr, 2])),
#        big.mark = "'"
#      )
#  
#      tab_rstata_coef[j, 2 + match(i, tabvec)] <- as.matrix(get(i)$table)[j, 3]
#      tab_rstata_coef[nrow(tab_rstata_coef), 2 + match(i, tabvec)] <- lltmp
#      tab_rstata_coef[nrow(tab_rstata_coef), 2] <- "ll"
#  
#      tab_rstata_se[j, 2 + match(i, tabvec)] <- as.matrix(get(i)$table)[j, 4]
#      tab_rstata_se[nrow(tab_rstata_se), 2 + match(i, tabvec)] <- lltmp
#      tab_rstata_se[nrow(tab_rstata_se), 2] <- "ll"
#  
#    }
#    rm(nr)
#  }
#  rm(i, j, lltmp)
#  
#  tab_rstata_coef <- as.data.frame(rbind(c("", "", "R", "STATA", "R", "STATA", "R", "STATA", "R", "STATA"),
#                              tab_rstata_coef))
#  
#  tab_rstata_se <- as.data.frame(rbind(c("", "", "R", "STATA", "R", "STATA", "R", "STATA", "R", "STATA"),
#                              tab_rstata_se))
#  
#  names(tab_rstata_coef) <- names(tab_rstata_se) <- c("", "", "Ref. case 1", "", "Ref. case 2", "", "Ref. case 3", "", "Ref. case 4", "")
#  
#  save(tab_rstata_coef,
#       file = "tab_rstata_coef.RData",
#       compress = TRUE)
#  
#  save(tab_rstata_se,
#       file = "tab_rstata_se.RData",
#       compress = TRUE)
#  
#  rm(tab_rstata_coef, tab_rstata_se, tabvec, tab_sum_const, tab_sum_cstata, tab_sum_mod1nlminb, tab_sum_mod2stata,
#     tab_sum_stata1, tab_sum_stata2, tab_sum_stata3, tab_sum_stata4)
#  
#  # Remove fit objects
#  #-------------------
#  
#  # file.remove(dir(path = ".",  pattern="fi1_"))
#  # file.remove(dir(path = ".",  pattern="fi2_"))
#  rm(df)
#  rm(rep_tab_fit)
#  

## ----calc_tab_stata, eval = FALSE, warning = FALSE, echo = FALSE, results = 'hide'----
#  
#  # Model 1, default options
#  #-------------------------
#  
#  tab_sum_stata1 <- rep_tab_stata("stata_model1_default.csv")
#  
#  save(tab_sum_stata1,
#       file = "tab_sum_stata1.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_stata1)
#  
#  # Model 1, constraints
#  #----------------------
#  
#  tab_sum_stata2 <- rep_tab_stata("stata_model1_cstr.csv")
#  
#  save(tab_sum_stata2,
#       file = "tab_sum_stata2.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_stata2)
#  
#  # Model 1, constant-only initial values
#  #--------------------------------------
#  
#  tab_sum_stata3 <- rep_tab_stata("stata_model1_cons.csv")
#  
#  save(tab_sum_stata3,
#       file = "tab_sum_stata3.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_stata3)
#  
#  # Model 2, user-defined initial values
#  #-------------------------------------
#  
#  tab_sum_stata4 <- rep_tab_stata("stata_model2.csv")
#  
#  save(tab_sum_stata4,
#       file = "tab_sum_stata4.RData",
#       compress = TRUE)
#  
#  rm(tab_sum_stata4)
#  

## ----install, eval = FALSE, echo = TRUE---------------------------------------
#  install.packages("aldvmm")

## ----data, eval = FALSE, echo = TRUE------------------------------------------
#  temp <- tempfile()
#  
#  url <- paste0("https://files.digital.nhs.uk/publicationimport/pub11xxx/",
#                "pub11359/final-proms-eng-apr11-mar12-data-pack-csv.zip")
#  
#  download.file(url, temp)
#  rm(url)
#  
#  df <- read.table(unz(description = temp,
#                       filename = "Hip Replacement 1112.csv"),
#                   sep = ",",
#                   header = TRUE)
#  
#  unlink(temp)
#  rm(temp)
#  
#  df <- df[, c("AGEBAND", "SEX", "Q2_EQ5D_INDEX", "HR_Q2_SCORE")]
#  df <- df[df$AGEBAND != "*" & df$SEX != "*", ]
#  
#  df$eq5d <- df$Q2_EQ5D_INDEX
#  df$hr <- df$HR_Q2_SCORE/10
#  
#  df <- df[stats::complete.cases(df), ]
#  
#  set.seed(101010101)
#  df <- df[sample(1:nrow(df), size = nrow(df)*0.3), ]

## ----loadtextout, eval = TRUE, echo = FALSE-----------------------------------

load("textout.RData")


## ----plot-hist-obs, out.width = "90%", fig.cap = "Frequency distribution of observed EQ-5D-3L utilities", echo = FALSE----
knitr::include_graphics("plot_hist_obs.png")

## ----model1-fit, echo = TRUE, eval = FALSE------------------------------------
#  library("aldvmm")
#  
#  fit <- aldvmm::aldvmm(eq5d ~ hr | 1,
#                        data = df,
#                        psi = c(0.883, -0.594),
#                        ncmp = 2)
#  
#  summary(fit)
#  
#  pred <- predict(fit,
#                  se.fit = TRUE,
#                  type = "fit")

## ----tab-sum-mod1-load, echo = FALSE------------------------------------------

load("tab_sum_mod1bfgs.RData")

textout[["mod1bfgs"]][["aic"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_sum_mod1bfgs$table[nrow(tab_sum_mod1bfgs$table), 3])
  ), 
  big.mark = "'"
)

textout[["mod1bfgs"]][["ll"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_sum_mod1bfgs$table[nrow(tab_sum_mod1bfgs$table), 2])
  ), 
  big.mark = "'"
)

textout[["mod1bfgs"]][["intp1"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_sum_mod1bfgs$table[nrow(tab_sum_mod1bfgs$table) - 1, 3])
  ), 
  big.mark = "'"
)

rm(tab_sum_mod1bfgs)


## ----tab-sum-mod1-bfgs, echo = FALSE, results = "asis"------------------------

load("tab_sum_mod1bfgs.RData")

rep_tab_reg(tab_sum_mod1bfgs$table,
          caption = 'Regression results from model 1 with "BFGS" optimization method and "zero" starting values')

rm(tab_sum_mod1bfgs)


## ----plot-hist-pred, out.width = "90%", fig.cap = "Expected values from base case model", echo = FALSE----
knitr::include_graphics("plot_hist_pred.png")

## ----plot-comp-mhl-bfgs, out.width = "90%", fig.cap = "Mean residuals over deciles of expected values, BFGS with zero starting values", echo = FALSE----
knitr::include_graphics("plot_comp_mhl_bfgs.png")

## ----tab-comp-ll-load, echo = FALSE-------------------------------------------
load("tab_comp_ll.RData")

## ----fit-comp, echo = TRUE, eval = FALSE--------------------------------------
#  init.method <- c("zero", "random", "constant", "sann")
#  
#  optim.method <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlminb", "Rcgmin",
#                    "Rvmmin","hjn")
#  
#  fit1_all <- list()
#  
#  for (i in init.method) {
#    for (j in optim.method) {
#      set.seed(101010101) # Seed for random starting values
#      fit1_all[[i]][[j]] <- aldvmm::aldvmm(eq5d ~ hr | 1,
#                                           data         = df,
#                                           psi          = c(0.883, -0.594),
#                                           ncmp         = 2,
#                                           init.method  = i,
#                                           optim.method = j)
#    }
#  }

## ----tab-comp-ll, echo = FALSE, results = 'asis'------------------------------

knitr::kable(round(tab_comp_ll, 1),
             format = "simple",
             row.names = TRUE,
             caption = 'Log-likelihood by optimization method')

rm(tab_comp_ll)


## ----tab-comp-time-load, echo = FALSE, results = 'asis'-----------------------

load("tab_comp_time.RData")


## ----tab-comp-time, echo = FALSE, results = 'asis'----------------------------

knitr::kable(round(tab_comp_time, 2),
             format = "simple",
             row.names = TRUE,
             caption = 'Estimation time [minutes] by optimization method')

rm(tab_comp_time)


## ----tab-comp-coef, echo = FALSE, results = "asis"----------------------------

load("tab_comp_coef.RData")

rep_tab_reg(tab_comp_coef,
          caption = 'Regression results of model 1 with zero starting values in BFGS, Nelder-Mead, nlminb and hjn algorithms')

rm(tab_comp_coef)


## ----plot-comp-pred, out.width = "90%", fig.cap = "Deviation of expected values from BFGS, Nelder-Mead and hjn versus nlminb with zero starting values", echo = FALSE----
knitr::include_graphics("plot_comp_pred.png")

## ----tab-sum-cstr-load, echo = FALSE, results = "asis"------------------------

load("tab_sum_cstr.RData")

textout[["cstr"]][["ll"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_sum_cstr[[1]][nrow(tab_sum_cstr[[1]]), 2])
  ), 
  big.mark = "'"
)


## ----tab-cstr-show, echo = TRUE, eval = FALSE---------------------------------
#  init <- c(0,    0,   0,   0,    0,    0,    0.7283)
#  lo   <- c(-Inf, -Inf, -3,  -Inf, -Inf, -3, -Inf)
#  hi   <- c(Inf,  Inf,  Inf, Inf,  Inf,  Inf,  Inf)
#  
#  fit1_cstr <- aldvmm::aldvmm(eq5d ~ hr | 1,
#                              data = df,
#                              psi = c(0.883, -0.594),
#                              ncmp = 2,
#                              init.est = init,
#                              init.lo = lo,
#                              init.hi = hi)
#  
#  summary(fit1_cstr)

## ----tab-sum-cstr, echo = FALSE, results = "asis"-----------------------------

load("tab_sum_cstr.RData")

rep_tab_reg(tab_sum_cstr$table,
          caption = 'Regression results of model 1 with the L-BFGS-B method, parameter constraints and user-defined starting values')

rm(tab_sum_cstr)


## ----tobit-fit, echo = TRUE, eval = FALSE-------------------------------------
#  fit <- aldvmm::aldvmm(eq5d ~ hr,
#                        data = df,
#                        psi = c(0.883, -0.594),
#                        ncmp = 1,
#                        init.method = "zero",
#                        optim.method = "nlminb")
#  
#  summary(fit)

## ----tab-sum-tobit-load, echo = FALSE, results = "asis"-----------------------

load("tab_sum_tobit.RData")
load("tab_comp_coef.RData")

textout[["tobit"]][["aic"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_sum_tobit[[1]][nrow(tab_sum_tobit[[1]]), 3])
  ), 
  big.mark = "'"
)

textout[["comp"]][["nlminb"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_comp_coef[nrow(tab_comp_coef), "nlminb"])
  ), 
  big.mark = "'"
)

textout[["comp"]][["hjn"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_comp_coef[nrow(tab_comp_coef), "hjn"])
  ), 
  big.mark = "'"
)

rm(tab_comp_coef, tab_sum_tobit)


## ----tab-sum-tobit, echo = FALSE, results = "asis"----------------------------

load("tab_sum_tobit.RData")

rep_tab_reg(tab_sum_tobit$table,
          caption = 'Regression results of model 1 with 1 component, zero starting values in nlminb algorithm')

rm(tab_sum_tobit)


## ----model2-fit, echo = TRUE, eval = FALSE------------------------------------
#  init <- c(-.40293118, .30502755, .22614716, .14801581, -.70755741, 0,
#            -1.2632051, -2.4541401)
#  
#  fit2 <- aldvmm::aldvmm(eq5d ~ hr | hr,
#                         data = df,
#                         psi = c(0.883, -0.594),
#                         ncmp = 2,
#                         init.est = init,
#                         optim.method = "nlminb")
#  
#  summary(fit2)
#  

## ----tab-sum-mod2-load, echo = FALSE, results = "asis"------------------------

load("tab_sum_mod2.RData")

textout[["mod2"]][["aic"]] <- format(
  as.numeric(
    gsub("[^0-9.-]", 
         "",
         tab_sum_mod2[[1]][nrow(tab_sum_mod2[[1]]), 3])
  ), 
  big.mark = "'"
)

rm(tab_sum_mod2)


## ----tab-sum-mod2, echo = FALSE, results = "asis"-----------------------------

load("tab_sum_mod2.RData")

rep_tab_reg(tab_sum_mod2$table,
          caption = 'Regression results of model 2 with user-defined starting values in the nlminb algorithm')

rm(tab_sum_mod2)


## ----tab-calc-stata, echo = FALSE, results = "asis"---------------------------




## ----tab-comp-stata, echo = FALSE, results = "asis"---------------------------

load("tab_rstata_coef.RData")

rep_tab_reg(tab_rstata_coef,
          caption = 'Comparison of point estimates to the results of the STATA package')

rm(tab_rstata_coef)


## ----tab-compse-stata, echo = FALSE, results = "asis"-------------------------

load("tab_rstata_se.RData")

rep_tab_reg(tab_rstata_se,
          caption = 'Comparison of standard errors to the results of the STATA package')

rm(tab_rstata_se)


## ----box-pred-yhat-stata, out.width="90%", fig.cap = "Fitted values in R and STATA", echo = FALSE----
knitr::include_graphics("plot_box_yhat.png")

## ----box-pred-se-stata, out.width="90%", fig.cap = "Standard errors of fitted values in R and STATA", echo = FALSE----
knitr::include_graphics("plot_box_se.png")

## ----tab-pred-stata-yhat, echo = FALSE, results = "asis"----------------------

load("tab_diff_yhat.RData")

knitr::kable(tab_diff_yhat,
             format = "simple",
             row.names = TRUE,
             caption = 'Summary statistics of differences of fitted values in R and STATA (positive values suggest larger values in STATA)')

rm(tab_diff_yhat)


## ----tab-pred-stata-se, echo = FALSE, results = "asis"------------------------

load("tab_diff_se.RData")

knitr::kable(tab_diff_se,
             format = "simple",
             row.names = TRUE,
             caption = 'Summary statistics of differences in standard errors of fitted values in R and STATA (positive values suggest larger values in STATA)')

rm(tab_diff_se)


## ----atet-example, echo = TRUE, eval = FALSE, results = 'hide'----------------
#  
#  # Create treatment indicator
#  #---------------------------
#  
#  df$treated <- as.numeric(df$SEX == "2")
#  
#  # Fit model
#  #----------
#  
#  formula <- eq5d ~ treated + hr | 1
#  
#  fit <- aldvmm(formula,
#                data = df,
#                psi = c(-0.594, 0.883))
#  
#  # Predict treated
#  #----------------
#  
#  # Subsample of treated observations
#  tmpdf1 <- df[df$treated == 1, ]
#  
#  # Design matrix for treated observations
#  X1 <- aldvmm.mm(data = tmpdf1,
#                  formula = fit$formula,
#                  ncmp = fit$k,
#                  lcoef = fit$label$lcoef)
#  
#  # Average expected outcome of treated observations
#  mean1 <- mean(predict(fit,
#                        newdata = tmpdf1,
#                        type = "fit",
#                        se.fit = TRUE)[["yhat"]], na.rm = TRUE)
#  
#  # Predict counterfactual
#  #-----------------------
#  
#  # Subsample of counterfactual observations
#  tmpdf0 <- tmpdf1
#  rm(tmpdf1)
#  tmpdf0$treated <- 0
#  
#  # Design matrix for counterfactual observations
#  X0 <- aldvmm.mm(data = tmpdf0,
#                  formula = fit$formula,
#                  ncmp = fit$k,
#                  lcoef = fit$label$lcoef)
#  
#  # Average expected outcome of counterfactual osbervations
#  mean0 <- mean(predict(fit,
#                        newdata = tmpdf0,
#                        type = "fit",
#                        se.fit = TRUE)[["yhat"]], na.rm = TRUE)
#  
#  rm(tmpdf0)
#  
#  # Standard error of ATET
#  #-----------------------
#  
#  atet.grad <- numDeriv::jacobian(func = function(z) {
#  
#    yhat1 <- aldvmm.pred(par   = z,
#                         X     = X1,
#                         y     = rep(0, nrow(X1[[1]])),
#                         psi   = fit$psi,
#                         ncmp  = fit$k,
#                         dist  = fit$dist,
#                         lcoef = fit$label$lcoef,
#                         lcmp  = fit$label$lcmp,
#                         lcpar = fit$label$lcpar)[["yhat"]]
#  
#    yhat0 <- aldvmm.pred(par   = z,
#                         X     = X0,
#                         y     = rep(0, nrow(X0[[1]])),
#                         psi   = fit$psi,
#                         ncmp  = fit$k,
#                         dist  = fit$dist,
#                         lcoef = fit$label$lcoef,
#                         lcmp  = fit$label$lcmp,
#                         lcpar = fit$label$lcpar)[["yhat"]]
#  
#    mean(yhat1 - yhat0, na.rm = TRUE)
#  
#  },
#  x = fit$coef)
#  
#  se.atet <- sqrt(atet.grad %*% fit$cov %*% t(atet.grad))
#  
#  # Summarize
#  #----------
#  
#  out <- data.frame(atet = mean1 - mean0,
#                    se = se.atet,
#                    z = (mean1 - mean0) / se.atet)
#  out$p <- 2*stats::pnorm(abs(out$z), lower.tail = FALSE)
#  out$ul <- out$atet + stats::qnorm((1 + 0.95)/2) * out$se
#  out$ll <- out$atet - stats::qnorm((1 + 0.95)/2) * out$se
#  
#  print(out)
#  

## ----sandwich-example, echo = TRUE, eval = FALSE, results = 'hide'------------
#  
#  # Create cluster indicator
#  #-------------------------
#  
#  df$grp <- as.factor(round(0.5 + runif(nrow(df)) * 5, 0))
#  
#  # Fit model
#  #----------
#  
#  formula <- eq5d ~ hr | 1
#  
#  fit <- aldvmm(formula,
#                data = df,
#                psi = c(-0.594, 0.883))
#  
#  # Calculate robust and clustered standard errors
#  #-----------------------------------------------
#  
#  vc1 <- sandwich::sandwich(fit)
#  vc2 <- sandwich::vcovCL(fit, cluster = ~ grp)
#  vc3 <- sandwich::vcovPL(fit, cluster = ~ grp)
#  vc4 <- sandwich::vcovHAC(fit, cluster = ~ grp)
#  vc5 <- sandwich::vcovBS(fit)
#  vc6 <- sandwich::vcovBS(fit, cluster = ~ grp)
#  
#  # Calculate test statistics
#  #--------------------------
#  
#  lmtest::coeftest(fit)
#  lmtest::coeftest(fit, vcov = vc1)
#  lmtest::coeftest(fit, vcov = vc2)
#  lmtest::coeftest(fit, vcov = vc3)
#  lmtest::coeftest(fit, vcov = vc4)
#  lmtest::coeftest(fit, vcov = vc5)
#  lmtest::coeftest(fit, vcov = vc6)
#  

## ----mhl-show, echo = TRUE, eval = FALSE--------------------------------------
#  # Number of percentiles
#  ngroup <- 10
#  
#  # Extract expected values and residuals
#  yhat <- fit1_all[["zero"]][["Nelder-Mead"]][["pred"]][["yhat"]]
#  res <- fit1_all[["zero"]][["Nelder-Mead"]][["pred"]][["res"]]
#  
#  # Make groups
#  group <- as.numeric(cut(yhat, breaks = ngroup), na.rm = TRUE)
#  
#  # Auxiliary regression
#  aux <- stats::lm(res ~ factor(group))
#  
#  # Data set of predictions from auxiliary regressions
#  newdf <- data.frame(group = unique(group)[order(unique(group))])
#  predict <- predict(aux,
#                     newdata = newdf,
#                     se.fit = TRUE,
#                     interval = 'confidence',
#                     level = 0.95)
#  
#  plotdat <- as.data.frame(rbind(
#    cbind(group = newdf$group,
#          outcome = "mean",
#          value = predict$fit[ , 'fit']),
#    cbind(group = newdf$group,
#          outcome = "ll",
#          value = predict$fit[ , 'lwr']),
#    cbind(group = newdf$group,
#          outcome = "ul",
#          value = predict$fit[ , 'upr'])
#  ))
#  
#  # Make plot
#  plot <- ggplot2::ggplot(plotdat, aes(x = factor(as.numeric(group)),
#                                       y = as.numeric(value),
#                                       group = factor(outcome))) +
#    geom_line(aes(linetype = factor(outcome)))

## ----tab-comp-cov, echo = FALSE, results = 'asis'-----------------------------

load("tab_comp_cov.RData")

knitr::kable(tab_comp_cov,
             format = "simple",
             row.names = FALSE,
             caption = 'Covariance matrix by optimization method')

rm(tab_comp_cov)


## ----plot-comp-mhl-nm, out.width="90%", fig.cap = "Mean residuals over deciles of expected values, Nelder-Mead with zero starting values", echo = FALSE----
knitr::include_graphics("plot_comp_mhl_nm.png")

## ----plot-comp-mhl-nlminb, out.width="90%", fig.cap = "Mean residuals over deciles of expected values, nlminb with zero starting values", echo = FALSE----
knitr::include_graphics("plot_comp_mhl_nlminb.png")

## ----plot-comp-mhl-hjn, out.width="90%", fig.cap = "Mean residuals over deciles of expected values, hjn with zero starting values", echo = FALSE----
knitr::include_graphics("plot_comp_mhl_hjn.png")

## ----tab-comp-stata-show, echo = TRUE, eval = FALSE---------------------------
#  # (1) Reference case 1: Model 1 with default optimization settings
#  fit1_default <- aldvmm::aldvmm(eq5d ~ hr | 1,
#                                 data = df,
#                                 psi = c(0.883, -0.594),
#                                 ncmp = 2,
#                                 init.method = "zero",
#                                 optim.method = "nlminb")
#  
#  # (2) Reference case 2: Model 1 with user-defined initial values and constraints on parameters
#  init <- c(0,    0,   0,   0,    0,    0,    0.7283)
#  lo   <- c(-Inf, -Inf, -3,  -Inf, -Inf, -3, -Inf)
#  hi   <- c(Inf,  Inf,  Inf, Inf,  Inf,  Inf,  Inf)
#  
#  fit1_cstr <- aldvmm::aldvmm(eq5d ~ hr | 1,
#                              data = df,
#                              psi = c(0.883, -0.594),
#                              ncmp = 2,
#                              init.est = init,
#                              init.lo = lo,
#                              init.hi = hi)
#  
#  # (3) Reference case 3: Model 1 with initial values from constant-only model
#  fit1_const <- aldvmm::aldvmm(eq5d ~ hr | 1,
#                               data = df,
#                               psi = c(0.883, -0.594),
#                               ncmp = 2,
#                               init.method = "constant",
#                               optim.method = "nlminb")
#  
#  # (4) Reference case 4: Model 2 with user-defined initial values
#  init <- c(-.40293118, .30502755, .22614716, .14801581, -.70755741, 0,
#            -1.2632051, -2.4541401)
#  
#  fit2 <- aldvmm::aldvmm(eq5d ~ hr | hr,
#                         data = df,
#                         psi = c(0.883, -0.594),
#                         ncmp = 2,
#                         init.est = init,
#                         optim.method = "nlminb")

## ----stata_code, echo = TRUE, eval = FALSE------------------------------------
#  * (1) Reference case 1: Model 1 with default optimization settings
#  aldvmm eq5d hr, ncomponents(2)
#  
#  * (2) Reference case 2: Model 1 with user-defined initial values and constraints on parameters
#  matrix  input a = (0, 0, 0, 0, 0, 0, 0.7283)
#  constraint 1 [Comp_2]:hr10 = 0
#  constraint 2 [Comp_2]:_cons = 100
#  constraint 3 [lns_2]:_cons = 1e-30
#  aldvmm eq5d hr, ncomp(2) from(a) c(1 2 3)
#  
#  * (3) Reference case 3: Model 1 with initial values from constant-only model
#  aldvmm eq5d hr, ncomp(2) inim(cons)
#  
#  * (4) Reference case 4: Model 2 with user-defined initial values
#  matrix  input start = (.14801581, .22614716, .30502755, -.40293118, 0,
#                         -.70755741, -2.4541401, -1.2632051)
#  aldvmm eq5d hr, ncomp(2) prob(hr) from(start)

## ----end, echo = FALSE, results = 'hide'--------------------------------------

rm(ggplot_theme, est_mhl, rep_tab_fit, rep_tab_stata)


