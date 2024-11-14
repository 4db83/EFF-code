## PREAMBLE: clear screen/workspace -----
cat("\014"); rm(list = ls()); gc();
# SET DEFAULTS: display options, font and y axis label rotation
options(digits = 12); options(scipen = 999);  options(max.print=10000)
# INSTALL PACMAN: if not installed (note: may neeD:\_teaching\_current.teaching\_SU.EFF\code-EFF\helper_functions\print_results.Rd to disable windows firewall for packages to install)
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD/INSTALL: other required packages
pacman::p_load(dplyr,readxl,sandwich)
# LOAD HELPER FUNCTIONS STORED IN functions_path
functions_path  = c("./helper_functions/")
# source("D:/matlab.tools/db.toolbox/R_utility_functions.R")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))

## SCRIPT: starts here ----
# read the data from .XLSX file in directory ./data
CASchools = read_xlsx("./data/caschool.xlsx")
CASchools$Hi_EL = as.numeric(CASchools$el_pct >= 10)
# RUN OLS REGRESSION
ols1a = lm(testscr ~ str, data = CASchools)
ols1b = print.results(ols1a)

# plot the results for visual clarity----
plot(CASchools$str,CASchools$testscr,xlim = c(10,30), las = 1, ylim = c(600,750),
     xlab="Student-Teacher Ratio", ylab="Test Score", pch = 16, col = rgb(.1,.45,.75))
abline(h = mean(CASchools$testscr))
abline(v = mean(CASchools$str))
abline(ols1a, col = rgb(.85,.22,.1), lw = 2)

# adding interaction dummies
ols2a = lm(testscr ~ str + str*Hi_EL+ Hi_EL, data = CASchools)
ols2b = print.results(ols2a)

# testscore vs income ---- 
cat("\014")
ols5a  = lm( testscr ~ avginc                             , data = CASchools)
ols6a  = lm( testscr ~ avginc + I(avginc^2)               , data = CASchools)
ols7a  = lm( testscr ~ avginc + I(avginc^2) + I(avginc^3) , data = CASchools)
ols8a  = lm( testscr ~ log(avginc)                        , data = CASchools)
ols5b  = print.results(ols5a)
ols6b  = print.results(ols6a)
ols7b  = print.results(ols7a)
ols8b  = print.results(ols8a)
# plot((CASchools$avginc),(CASchools$testscr),
# par(mar = c(6, 5, 1, 1))
plot( (CASchools$avginc),(CASchools$testscr), family = "serif",
      xlim = c(5,60), las = 1, cex.lab = 1.3, cex.axis = 1.25,
      ylim = c(600,740),
      # ylim = c(6.40,6.60),
      ylab="Income", 
      xlab="District Income (thousands of dollars)", 
      pch = 16, col = 1, cex = 1.5,
      # xaxt = "n", yaxt = "n",
      )
# axis ticks inside
# axis(1, tick = TRUE, las = 1, tcl = .5, family = "serif",cex.lab = 1.3, cex.axis = 1.25)
# axis(2, tick = TRUE, las = 1, tcl = .5, family = "serif",cex.lab = 1.3, cex.axis = 1.25)
# abline(h = mean(CASchools$testscr))
abline(v = 10); abline(h = 650)
xgrd <- seq(min(CASchools$avginc), max(CASchools$avginc), length.out = 500)
lines(xgrd, predict(ols5a, newdata = data.frame(avginc = xgrd)), col = 3, lwd = 3)
lines(xgrd, predict(ols6a, newdata = data.frame(avginc = xgrd)), col = 2, lwd = 3)
lines(xgrd, predict(ols7a, newdata = data.frame(avginc = xgrd)), col = 4, lwd = 3)
lines(xgrd, predict(ols8a, newdata = data.frame(avginc = xgrd)), col = 5, lwd = 3)
text(50, 730, "Linear Regression",      cex = 1.3, col = 3, family = "serif")
text(55, 696, "Cubic Regression",       cex = 1.3, col = 4, family = "serif")
text(50, 687, "Quadratic Regression",   cex = 1.3, col = 2, family = "serif")
text(50, 705, "Linear-Log Regression",  cex = 1.3, col = 5, family = "serif")

# testscore vs income ----
# ols3a  = lm( testscr ~ avginc + I(avginc^2) ,
cat("\014")
ols3a  = lm( log(testscr) ~ log(avginc) , data = CASchools)
ols4a  = lm( log(testscr) ~ avginc      , data = CASchools)
ols3b  = print.results(ols3a)
ols4b  = print.results(ols4a)
# plot((CASchools$avginc),(CASchools$testscr),
plot((CASchools$avginc),log(CASchools$testscr), family = "serif",
     xlim = c(5,60), las = 1, cex.lab = 1.3, cex.axis = 1.25,
     # ylim = c(600,750),
     ylim = c(6.40,6.60),
     ylab="log(Income)", 
     xlab="District Income (thousands of dollars)", 
     # pch = 18, col = rgb(.1,.45,.75),
     pch = 16, col = 1, cex = 1.5,
     xaxt = "n", yaxt = "n",
)
# axis ticks inside
axis(1, tick = TRUE, las = 1, tcl = .5);axis(2, tick = TRUE, las = 1, tcl = .5)
# abline(h = mean(CASchools$testscr))
abline(v = 10)
xgrd <- seq(min(CASchools$avginc), max(CASchools$avginc), length.out = 500)
lines(xgrd, predict(ols3a, newdata = data.frame(avginc = xgrd)), col = "blue",  lwd = 3)
lines(xgrd, predict(ols4a, newdata = data.frame(avginc = xgrd)), col = "red",   lwd = 3)
text(44, 6.581, "Log-Log Regression",    cex = 1.3, col = "red",  family = "serif")
text(45, 6.535, "Log-Linear Regression", cex = 1.3, col = "blue", family = "serif")
# curve(coef(ols3a)[1] + coef(ols3a)[2] * CASchools$avginc + coef(ols3a)[3] * CASchools$avginc^2, add = TRUE, col = "blue", lwd = 2)
# abline(ols3a, col = rgb(.85,.22,.1), lw = 2)

exp(unname(coef(ols3a)[1] + coef(ols3a)[2] * log(10)))*0.00554
