# SAS-like summary of regression
# Author: Mattias Villani

library(knitr)
library(xtable)
library(erify)
kable(xtable(tabanova),format='markdown')

lmsummary <- function(lmobject, vif_factors = F, anova = T, conf_intervals = F){
  
  if ("(Intercept)" %in% names(lmfit$coefficients)) intercept = 1 else intercept = 0
  print(intercept)
  
  lmsummary = summary(lmobject)
  df_regr = lmsummary$df[1] - intercept
  df_error = lmsummary$df[2]
  df_total = df_error + df_regr 
  sse = (lmsummary$sigma^2)*df_error
  sst = var(lmobject$model[,1])*df_total
  ssr = sst - sse
  
  # Anova table
  anova_table = NA
  if (anova){
    anova_table = matrix(rep(NA,5*3),3,5)
    anova_table[,1] = c(df_regr,df_error,df_total)
    anova_table[,2] = c(ssr,sse,sst)
    anova_table[1:2,3] = c(ssr/df_regr, sse/df_error)
    anova_table[1,4] = lmsummary$fstatistic[1]
    anova_table[1,5] = pf(lmsummary$fstatistic[1], lmsummary$fstatistic[2], lmsummary$fstatistic[3], lower.tail = F)
    rownames(anova_table) <- c("Regr","Error","Total")
    colnames(anova_table) <- c("df","SS","MS","F","Pr(>F)")
  }
  
  rsqr_row = c(sqrt(sse/df_error), lmsummary$r.squared, lmsummary$adj.r.squared)
  names(rsqr_row) <- c("Root MSE","R2","R2-adj")
  
  # Confidence intervals on parameters
  if (conf_intervals){
    param_table = cbind(lmsummary$coefficients, confint(lmobject))
  }else
  {
    param_table = lmsummary$coefficients
  }
  
  # Variance inflation factors
  if (vif_factors & df_regr>1){
    X = lmobject$model[,2:(df_regr+1)]
    vif = rep(NA,df_regr)
    for (j in 1:df_regr){
      vif[j] = 1/summary(lm(X[,j] ~ data.matrix(X[,-j])))$r.squared
    }
    if (intercept) vif = c(NA,vif)
    param_table = cbind(param_table,vif)
    colnames(param_table)[ncol(param_table)] = "VIF"
  }
  
  message("\nAnalysis of variance - ANOVA\n------------------------------------------------")
  print(anova_table, digits = 5, na.print = "")
  message("\n")
  print(rsqr_row, digits = 5)
  message("\n")
  message("Parameter estimates\n------------------------------------------------")
  print(param_table, digits = 5, na.print = "")
  
  return(NULL)
}

# Load bike share data to replicate SAS-output on Slide nr 8 here: https://github.com/mattiasvillani/Regression/raw/master/Slides/Regression_L4.pdf
bike = read.csv(file = "https://raw.githubusercontent.com/mattiasvillani/Regression/master/Data/cykeluthyr.csv")
lmfit = lm(nRides ~ temp + hum + windspeed, data = bike)
summary(lmfit)
lmsummary(lmfit, vif_factors = T, anova = T, conf_intervals = T)


