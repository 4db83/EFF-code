## PREAMBLE: clear screen/workspace -----
cat("\014"); rm(list = ls()); gc();
# SET DEFAULTS: display options, font and y axis label rotation
options(digits = 8); options(scipen = 999);  options(max.print=10000)
# INSTALL PACMAN: if not installed (note: may need to disable windows firewall for packages to install)
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD/INSTALL: other required packages
pacman::p_load(dplyr,readxl,wooldridge,AER,sandwich)
# source("./R_help_functions.R") # link to local functions, if needed

## SCRIPT: starts here ----
# read the data
data(CASchools)
# compute the student-to-teacher ratio
CASchools$STR = CASchools$students / CASchools$teachers
# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2
# RUN OLS REGRESSION
fit1    = lm(score ~ STR + teachers + students, data = CASchools)
tmp_sum = summary(fit1)
sqrt(diag(sandwich(fit1)))
# vcovHC(fit1, type = "HC")


## print(tmp_sum) ----
uhat  = fit1$residuals
N     = length(uhat)
SSE   = sum(uhat^2)
LL		= -N/2*( log(2*pi) + log(SSE/N) + 1 );
Y     = fit1$fitted.values + fit1$residuals
# check if intercept included in model
I = as.numeric("(Intercept)" %in% names(fit1$coefficients)) 
K = length(fit1$coefficients) # includes intercept term if it exists
# information criteria
# MSE_	  = SSE/N;
SSY     = sum( (Y-mean(Y))^2 )
R2      = 1 - SSE/SSY
Rbar2   = 1 - (N-1)/(N-K)*(1-R2);
IC_AIC 	= -2*LL + 2*K;		
IC_BIC	= -2*LL + K*log(N);
IC_AICc = IC_AIC+ 2*K*(K+1)/(N-K-1);
IC_HQ		= -2*LL + 2*K*log(log(N));
u.1     = uhat[2:N] - uhat[1:N-1]
DW		  = sum(u.1^2)/SSE;
varNames = variable.names(fit1)
if (I == 1) varNames[1] = "Constant"
stdErr  = as.vector(sqrt(diag(kernHAC(fit1))))
bhat    = tmp_sum$coefficients[1:K]
tstat   = bhat/stdErr
pvalue  = 1-pt(abs(tstat), N-K)
pvalue

####---------#---------#------------

# % make the hat matrix X*inv(X'X)*X'(y) = yhat = X*bhat
#                             hatmatrix = x*invxpx*x';
# % BETA  = X\Y; IS MUCH MUCH SLOWER!!!
# u_		= u(2:end)-u(1:end-1);
# DW		= u_'*u_/SSE;
baseoutput = matrix(rep(NA,K*5),K,5)
colnames(baseoutput) = c("","Estimate ",
                            "Std.err ", 
                            "t-stat ",
                            "p-value ")
rownames(baseoutput) = varNames
# baseoutput[1:K,1] = NA
baseoutput[1:K,2] = bhat
baseoutput[1:K,3] = stdErr
baseoutput[1:K,4] = tstat
baseoutput[1:K,5] = pvalue
cat("\014")
print(baseoutput, digits = 6, na.print = ":  ")
# as.data.frame(baseoutput)

###----------
# storage for output for left side
xtraout1 = matrix(rep(NA,10*2),10,2)
colnames(xtraout1) = c("","")
rownames(xtraout1) = c("R-squared             ",  # 1   
                       "Rbar-squared          ",  # 2 
                       "SE of regression      ",  # 3 
                       "Sum Squared Errors    ",  # 4 
                       "Log-likelihood        ",  # 5 
                       "F-statistic           ",  # 6 
                       "Pr(F-statistic)       ",  # 7 
                       "No. of observations   ",  # 8 
                       "Std.err.MLE (div by T)",  # 9 
                       "Include Pre-whitening ")  # 10 

xtraout1[1]  = R2
xtraout1[2]  = Rbar2
xtraout1[3]  = tmp_sum$sigma
xtraout1[4]  = SSE
xtraout1[5]  = LL
xtraout1[6]  = tmp_sum$fstatistic[1]
xtraout1[7]  = pf(tmp_sum$fstatistic[1],tmp_sum$fstatistic[2],tmp_sum$fstatistic[3],lower.tail = FALSE)
xtraout1[8]  = N
xtraout1[9]  = sqrt(SSE/N)
xtraout1[10] = 0


print(xtraout1,digits = 5, na.print = "")

# storage for output for left side
xtraout2 = matrix(rep(NA,10*2),10,2)
colnames(xtraout2) = c("","")
rownames(xtraout2) = c( "No. of Regressors    ",	# 1						
                        "Plus Const.(if exist)",	# 2						
                        "Mean(y)              ",	# 3						
                        "Stdev(y)             ",	# 4
                        "AIC                  ",	# 5
                        "AICc                 ",	# 6
                        "BIC                  ",	# 7
                        "HQIC                 ",	# 8
                        "DW-stat.             ",	# 9
                        "HAC Trunct.Lag       ")	# 10			

xtraout2[1]  = K - I
xtraout2[2]  = K
xtraout2[3]  = mean(Y)
xtraout2[4]  = sqrt(var(Y))
xtraout2[5]  = IC_AIC  / N 
xtraout2[6]  = IC_BIC	 / N 
xtraout2[7]  = IC_AICc / N  
xtraout2[8]  = IC_HQ	 / N	  
xtraout2[9]  = DW
xtraout2[10] = 0


print((xtraout2),digits = 5, na.print = "")
# aa[,2] = rownames(xtraout2)
# aa = as.data.frame(xtraout1)
# as.data.frame(xtraout1)
