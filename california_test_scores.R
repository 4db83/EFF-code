## PREAMBLE: clear screen/workspace -----
cat("\014"); rm(list = ls()); gc();
# SET DEFAULTS: display options, font and y axis label rotation
options(digits = 8); options(scipen = 999);  options(max.print=10000)
# INSTALL PACMAN: if not installed (note: may need to disable windows firewall for packages to install)
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD/INSTALL: other required packages
pacman::p_load(dplyr,readxl,wooldridge,AER)
# source("./R_help_functions.R") # link to local functions, if needed

## SCRIPT: starts here ----
# read the data
data(CASchools)
# compute the student-to-teacher ratio
CASchools$STR = CASchools$students / CASchools$teachers
# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2
# run the regression
fit1 <- lm(score ~ STR, data = CASchools)
tmp_sum = summary(fit1)
print(tmp_sum)

xtraout1 = matrix(rep(NA,10*4),10,4)
# rownames(xtraout) = c("R-squred:",
#                       "Rbar-squared:",
#                       "SE of regression:",
#                       )
rownames(xtraout1) = c("R-squared             ", 
                      "Rbar-squared          ",
                      "SE of regression      ",
                      "Sum Squared Errors    ",
                      "Log-likelihood        ",
                      "F-statistic           ",
                      "Pr(F-statistic)       ",
                      "No. of observations   ",
                      "Std.err.MLE (div by T)",
                      "Include Pre-whitening ")
colnames(xtraout1) = c("","","","")

xtraout1[1] = tmp_sum$r.squared
xtraout1[2] = tmp_sum$adj.r.squared
xtraout1[3] = tmp_sum$sigma
xtraout1[4] = sum(fit1$residuals^2)
xtraout1[5] = sum(fit1$residuals^2)
xtraout1[6] = sum(fit1$residuals^2)
xtraout1[7] = sum(fit1$residuals^2)
xtraout1[8] = sum(fit1$residuals^2)
xtraout1[9] = sqrt(sum(fit1$residuals^2)/length(fit1$residuals))
xtraout1[10] = 0


print((xtraout1),digits = 5, na.print = "")




