test_that("Check sjt.lm", { 
  skip_on_cran()
  library(sjmisc)
  # sample data
  data(efc)
  # set variable labels
  efc <- set_var_labels(efc, get_var_labels(efc))
  # fit first model
  fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data=efc)
  # fit second model
  fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data=efc)
  # Note that both models share the same predictors and only differ 
  # in their dependent variable. See examples of stepwise models 
  # later...
  
  CSS = list(css.table = '+border-collapse:separate;',
             css.tdata = 'border: 1px solid red;',
             css.tgrpdata = 'border: 1px solid blue;',
             css.grouprow = 'border: 1px solid blue;',
             css.separatorcol = 'border: 1px solid green;')
  
  sjt.lm(fit1, fit2, CSS = CSS, useViewer = F)
  sjt.lm(fit1, fit2, showHeaderStrings = T, CSS = CSS, useViewer = F)
  sjt.lm(fit1, fit2, pvaluesAsNumbers = F, CSS = CSS, useViewer = F)
  
  sjt.lm(fit1, fit2, 
         showEst = F,
         showR2 = T,
         showAIC = T,
         showFStat = T,
         showStdBeta = T,
         separateConfColumn = F,
         showHeaderStrings = T, 
         useViewer = F,
         CSS = CSS)
  
  sjt.lm(fit1, fit2, 
         showEst = F,
         showR2 = T,
         showAIC = T,
         showFStat = T,
         showStdBeta = T,
         separateConfColumn = F,
         showHeaderStrings = T, 
         pvaluesAsNumbers = F,
         useViewer = F,
         CSS = CSS)
  
  sjt.lm(fit1, fit2, 
         showEst = F,
         showR2 = T,
         showAIC = T,
         showFStat = T,
         showStdBeta = T,
         separateConfColumn = T,
         showHeaderStrings = T, 
         pvaluesAsNumbers = F,
         useViewer = F,
         CSS = CSS)

  sjt.lm(fit1, fit2, 
         showEst = F,
         showR2 = T,
         showAIC = T,
         showFStat = T,
         showStdBeta = T,
         showConfInt = F,
         showHeaderStrings = T, 
         pvaluesAsNumbers = F,
         useViewer = F,
         CSS = CSS)

  sjt.lm(fit1, fit2, 
         showEst = F,
         showR2 = T,
         showAIC = T,
         showFStat = T,
         showStdBeta = T,
         showConfInt = F,
         showHeaderStrings = T, 
         pvaluesAsNumbers = F,
         useViewer = F,
         CSS = CSS)
  
  sjt.lm(fit1,
         fit2,
         useViewer = F,
         CSS = CSS,
         labelDependentVariables = c("Barthel-Index",
                                     "Negative Impact"))  
  
  sjt.lm(fit1,
         fit2,
         showHeaderStrings = TRUE,
         useViewer = F,
         CSS = CSS,
         stringB = "Estimate",
         stringCI = "Conf. Int.",
         stringP = "p-value",
         stringDependentVariables = "Response",
         stringPredictors = "Coefficients",
         stringIntercept = "Konstante",
         labelDependentVariables = c("Barthel-Index",
                                     "Negative Impact"))  
  
  sjt.lm(fit1, fit2,
         useViewer = F,
         CSS = CSS,
         separateConfColumn = FALSE, # ci in same cell as estimates
         showStdBeta = TRUE,         # also show standardized beta values
         pvaluesAsNumbers = FALSE)   # "*" instead of numeric values  
  
  sjt.lm(fit1, fit2,
         useViewer = F,
         CSS = CSS,
         labelPredictors = c("Carer's Age",
                             "Hours of Care",
                             "Carer's Sex",
                             "Educational Status"))  
  
  # fit first model
  fit1 <- lm(neg_c_7 ~ c160age + c172code + c161sex, data=efc)
  # fit second model
  fit2 <- lm(neg_c_7 ~ c160age + c172code + c161sex + c12hour, data=efc)
  # fit second model
  fit3 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e, data=efc)

  sjt.lm(fit1, fit2, fit3, 
         useViewer = F,
         CSS = CSS,
         separateConfColumn = FALSE,
         showAIC = TRUE,
         showFStat = TRUE) 
  
  # make education categorical
  efc$c172code <- to_fac(efc$c172code)
  # make dependency categorical
  efc$e42dep <- to_fac(efc$e42dep)
  # fit first model again (with c172code as factor)
  fit1 <- lm(barthtot ~ c160age + c12hour + c172code + c161sex + e42dep, data=efc)
  # fit second model again (with c172code as factor)
  fit2 <- lm(neg_c_7 ~ c160age + c12hour + c172code + c161sex + e42dep, data=efc)  
  
  sjt.lm(fit1, fit2, 
         useViewer = F,
         CSS = CSS)
         
  data(efc)
  # attach variable labels to each variable of the data
  # frame - useful for automatic label detection
  efc <- set_var_labels(efc, get_var_labels(efc))
  # make education categorical
  efc$c172code <- to_fac(efc$c172code)
  # make education categorical
  efc$e42dep <- to_fac(efc$e42dep)
  # make prettier variable labels
  efc$c172code <- set_var_labels(efc$c172code, "Education")
  efc$e42dep <- set_var_labels(efc$e42dep, "Dependency")
  # fit first model
  fit1 <- lm(neg_c_7 ~ c160age + c172code + c161sex, data=efc)
  # fit second model
  fit2 <- lm(neg_c_7 ~ c160age + c172code + c161sex + c12hour, data=efc)
  # fit third model
  fit3 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e, data=efc)

  sjt.lm(fit1, fit2, fit3,
         useViewer = F,
         CSS = CSS)
  
  sjt.lm(fit1, fit2, fit3,
         useViewer = F,
         CSS = CSS,
         labelPredictors = c("mid education", 
                             "hi education", 
                             "Hours of Care", 
                             "slight dependency", 
                             "moderate dependency", 
                             "severe dependency", 
                             "Service Usage"),         
         remove.estimates = c("c160age", "c161sex"))  
  
  sjt.lm(fit1, fit2, fit3,
         showHeaderStrings = T,
         useViewer = F,
         showStdError = T,
         CSS = CSS,
         remove.estimates = c(2,5,6,10))  
})