print_results = function(lm.Object, HAC.type = 0, digits = 6, Prewhite = FALSE, mLag = ceiling(0.75*N^(1/3))){
  # HAC.type =-2 -->  homoskedastic OLS not recommended
  # HAC.type =-1 -->  White (1982) no DF adjustment 
  # HAC.type = 0 -->  White (1982) with DF adjutment, MacKinnon and White (1985).
  # HAC.type = 1 -->  Newey-West (1994) m-lag-selection, no prewhitening, 
  #                   if mLag is supplied, the supplied values is used, otherwise mLag = ceiling(0.75*N^(1/3)
  #                   if Prewhite = FALSE, no prewhitening is performed. 
  #---------------------------------------------------------------------------------------------------------------------
  
  summary_ols = summary(lm.Object);
  uhat    = as.matrix(lm.Object$residuals)
  bhat    = as.matrix(lm.Object$coefficients)
  yhat    = as.matrix(lm.Object$fitted.values)
  y       = as.matrix(yhat + uhat)
  N       = length(y)
  std_y   = sqrt(var(y))
  mean_y  = mean(y)
  SSE     = sum(uhat^2)
  MSE     = SSE/N
  LL      = -N/2*( log(2*pi) + log(SSE/N) + 1 );
  # check if intercept included in model
  I = as.numeric("(Intercept)" %in% names(lm.Object$coefficients)) 
  K = length(lm.Object$coefficients) # includes intercept term if it exists
  # information criteria
  SSY     = sum( (y-mean_y)^2 )
  R2      = 1 - SSE/SSY
  Rbar2   = 1 - (N-1)/(N-K)*(1-R2)
  IC_AIC  = (-2*LL + 2*K)/N    
  IC_AICc = (N*IC_AIC+ 2*K*(K+1)/(N-K-1))/N
  IC_BIC  = (-2*LL + K*log(N)) /N
  IC_HQ   = (-2*LL + 2*K*log(log(N)))/N;
  u.1     = uhat[2:N] - uhat[1:N-1]
  DW      = sum(u.1^2)/SSE;
  varNames= variable.names(lm.Object)
  X = as.matrix(lm.Object$model[-1])
  if (I == 1) {
    varNames[1] = "Constant"
    X = cbind(1,as.matrix(lm.Object$model[-1]))
  }
  
  # F-statistic, DFs adn pvalues
  Fstat = summary_ols$fstatistic
  Fstat_pval = pf(Fstat[1],Fstat[2],Fstat[3],lower.tail = FALSE)
  # standard error of regression
  sigma_u     = summary_ols$sigma
  sigma_u_MLE = sqrt(SSE/N)
  
  # COMPUTE THE VARIANCE COVARIANC MATRIX OF THE POINT ESTIMATES
  # plain vanilla, Homskedasticity OLSsame as inv(X'X)*Var(U)
  vcv0 = vcov(lm.Object) 
  # White (1982)
  vcv1 = vcovHC(lm.Object, type = "HC0") 
  # White (1982) with DF adjutment, MacKinnon and White (1985). This is what Eviews Reports
  vcv2 = vcovHC(lm.Object, type = "HC1") # set to default if not Time Series/Dynamic data. 
  # Newey-West 
  # mNW    = ceiling(0.75*N^(1/3))
  # vcvNW0 = NeweyWest(lm.Object, lag = mNW, prewhite = FALSE)
  # print(sqrt(diag(vcvNW0)))
    vcvNW = NeweyWest(lm.Object, lag = mLag, prewhite = Prewhite)
  
  
  # choose which VCV to print out
  if (HAC.type ==-2) VCV = vcv0  # HAC.type =-2 --> homoskedastic OLS
  if (HAC.type ==-1) VCV = vcv1  # HAC.type =-1 --> # White (1982) no DF adjustment 
  if (HAC.type == 0) VCV = vcv2  # HAC.type = 0 --> White (1982) with DF adjutment, MacKinnon and White (1985).
  if (HAC.type == 1) VCV = vcvNW # HAC.type = 1 --> Newey-West (1994) m-lag-selection, no prewhitening, if 
  
  HAC_lag = mLag
  if (HAC.type <= 0) HAC_lag = 0
  pre_white_I = as.numeric(Prewhite)

  stderr = sqrt(diag(VCV))
  tstat  = bhat/stderr
  pvalue = 2*pnorm(abs(tstat), lower.tail = FALSE) # it is a two-sided pvalue
    
  # PRINT THE RESULTS TO SCREEN NOW
  # make output table for main coefficients
  baseoutput = matrix(rep(NA,K*9),K,9)
  a0 = "   "
  a1 = "       "
  colnames(baseoutput) = c("Variable",
                           a1,
                           "Estimate",
                           a0,
                           "std.err",
                           a0,
                           "t-stat",
                           a0,
                           "p-value")
  # rownames(baseoutput) = c(varNames,":")
  bdf = as.data.frame(baseoutput)
  bdf[,1] = varNames
  bdf[,2] = a0
  bdf[,3] = bhat
  bdf[,4] = a0
  bdf[,5] = stderr
  bdf[,6] = a0
  bdf[,7] = tstat
  bdf[,8] = a0
  bdf[,9] = pvalue
  
  
  # cat("\014"); 
  cat(strrep("-", 110)); cat("\n")
  print( round_df(bdf, digits = digits), row.names = FALSE)
  # print( round_df(bdf, digits = 6), col.names = FALSE)
  cat(strrep("-", 110))
  
  # as.data.frame(baseoutput)
  
  # storage for output for left side
  xtraout1 = matrix(rep(NA,10*6),10,6)
  # colnames(xtraout1) = c("","")
  rownames(xtraout1) = c(" R-squared             ",  # 1   
                         " Rbar-squared          ",  # 2 
                         " SE of regression      ",  # 3 
                         " Sum Squared Errors    ",  # 4 
                         " Log-likelihood        ",  # 5 
                         " F-statistic           ",  # 6 
                         " Pr(F-statistic)       ",  # 7 
                         " No. of observations   ",  # 8 
                         " Std.err.MLE (div by T)",  # 9 
                         " Include Pre-whitening ")  # 10 
  # make data.frame and now fill the respective columns
  xdf = as.data.frame(xtraout1)
  colnames(xdf) = character(6)
  # storage for output for left side
  ColI    = c("|     ")
  Col2 = c( R2,
            Rbar2,
            sigma_u,
            SSE,
            LL,
            Fstat[1],
            Fstat_pval,
            N,
            sigma_u_MLE,
            pre_white_I)
  Col3 = c( "No. of Regressors    ",  # 1           
            "Plus Const.(if exist)",  # 2           
            "Mean(y)              ",  # 3           
            "Stdev(y)             ",  # 4
            "AIC                  ",  # 5
            "AICc                 ",  # 6
            "BIC                  ",  # 7
            "HQIC                 ",  # 8
            "DW-stat.             ",  # 9
            "HAC Trunct.Lag       ")  # 10      
  Col4 = c(K - I,
           K,
           mean_y,
           std_y,
           IC_AIC,  
           IC_AICc, 
           IC_BIC,   
           IC_HQ,  
           DW,
           HAC_lag)
  
  xdf[,1] = c(":    ")
  xdf[,2] = Col2
  xdf[,3] = ColI
  xdf[,4] = Col3
  xdf[,5] = c(":    ")
  xdf[,6] = Col4
  
  print(round_df(xdf, digits = digits))
  cat(strrep("-", 110))
  
  
  # Save variables to return
  ols_out <- list()
  ols_out$uhat     = uhat
  ols_out$bhat     = bhat
  ols_out$yhat     = yhat
  ols_out$y        = y
  ols_out$X        = X
  ols_out$K        = K
  ols_out$N        = N     
  ols_out$std_y    = std_y 
  ols_out$mean_y   = mean_y
  ols_out$SSE      = SSE   
  ols_out$LogLike  = LL    
  ols_out$MSE      = MSE     
  ols_out$SSY      = SSY     
  ols_out$R2       = R2      
  ols_out$Rbar2    = Rbar2   
  ols_out$IC_AIC   = IC_AIC  
  ols_out$IC_BIC   = IC_BIC  
  ols_out$IC_AICc  = IC_AICc 
  ols_out$IC_HQ    = IC_HQ   
  ols_out$DW_stat  = DW      
  ols_out$varNames = varNames
  
  
  return(ols_out)
  
}






# rounding of data frame function for nice looking table output with columns of characters
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}