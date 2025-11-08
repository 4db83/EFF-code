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
lm.testscr_str    = lm(testscr ~ str, data = CASchools)
prnt.testscr_str  = print_lm_results(lm.testscr_str)

# plot the results for visual clarity----
plot(CASchools$str,CASchools$testscr,xlim = c(10,30), las = 1, ylim = c(600,750),
     xlab="Student-Teacher Ratio", ylab="Test Score", pch = 16, col = rgb(.1,.45,.75))
abline(h = mean(CASchools$testscr))
abline(v = mean(CASchools$str))
abline(lm.testscr_str, col = rgb(.85,.22,.1), lw = 2)

# adding interaction dummies
lm.testscr_str_HiEL = lm(testscr ~ str + str*Hi_EL+ Hi_EL, data = CASchools)
prnt.testscr_str_HiEL = print_lm_results(lm.testscr_str_HiEL)

plot(CASchools$str,CASchools$testscr,xlim = c(10,30), las = 1, ylim = c(600,710),
     xlab="Student-Teacher Ratio", ylab="Test Score", pch = 16, col = rgb(.1,.45,.75))

xgrd <- seq(min(CASchools$str), max(CASchools$str), length.out = 100)
# Plot fitted lines for low and high Hi_EL values
lines(xgrd, predict(lm.testscr_str_HiEL,
                    newdata = data.frame(str = xgrd, Hi_EL = 0)),
      col = "blue", lwd = 3)
lines(xgrd, predict(lm.testscr_str_HiEL,
                    newdata = data.frame(str = xgrd, Hi_EL = 1)),
      col = "red", lwd = 3)

legend("topright", legend = c("Low Hi_EL", "High Hi_EL"),
       col = c("blue", "red"), lwd = 3, bty = "o", bg = "white")


# testscore vs income ---- 
cat("\014")
ols5a  = lm( testscr ~ avginc                             , data = CASchools)
ols6a  = lm( testscr ~ avginc + I(avginc^2)               , data = CASchools)
ols7a  = lm( testscr ~ avginc + I(avginc^2) + I(avginc^3) , data = CASchools)
ols8a  = lm( testscr ~ log(avginc)                        , data = CASchools)
ols5b  = print_lm_results(ols5a)
ols6b  = print_lm_results(ols6a)
ols7b  = print_lm_results(ols7a)
ols8b  = print_lm_results(ols8a)
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
lm.loglog  = lm( log(testscr) ~ log(avginc) , data = CASchools)
lm.loglin  = lm( log(testscr) ~ avginc      , data = CASchools)
prnt.loglog = print_lm_results(lm.loglog)
prnt.loglin = print_lm_results(lm.loglin)
# plot((CASchools$avginc),(CASchools$testscr),
plot((CASchools$avginc),log(CASchools$testscr), family = "serif",
     xlim = c(5,60), las = 1, cex.lab = 1.3, cex.axis = 1.25,
     # ylim = c(600,750),
     ylim = c(6.40,6.60),
     ylab="log(Test Score)", 
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
lines(xgrd, predict(lm.loglog, newdata = data.frame(avginc = xgrd)), col = "blue",  lwd = 3)
lines(xgrd, predict(lm.loglin, newdata = data.frame(avginc = xgrd)), col = "red",   lwd = 3)
text(44, 6.581, "Log-Log Regression",    cex = 1.3, col = "red",  family = "serif")
text(45, 6.535, "Log-Linear Regression", cex = 1.3, col = "blue", family = "serif")
# curve(coef(ols3a)[1] + coef(ols3a)[2] * CASchools$avginc + coef(ols3a)[3] * CASchools$avginc^2, add = TRUE, col = "blue", lwd = 2)
# abline(ols3a, col = rgb(.85,.22,.1), lw = 2)

exp(unname(coef(lm.loglog)[1] + coef(lm.loglog)[2] * log(10)))*0.00554


# plot ---- lin lin with log-linear ----
# par(family = "serif")
par(family = "serif", mgp = c(2.2, 0.5, 0))
plot((CASchools$avginc),log(CASchools$testscr), family = "serif", 
     xlim = c(2,58), las = 1, cex.lab = 1.3, cex.axis = 1.25,
     # ylim = c(600,750),
     ylim = c(6.40,6.60),
     ylab="Test Score", 
     xlab="District Income (thousands of dollars)", 
     # pch = 18, col = rgb(.1,.45,.75),
     pch = 16, col = 1, cex = 1.5,
     xaxt = "n", yaxt = "n",
)
TL = .25
axis(1, at = seq(0, 60, by = 5),              tcl = -TL )  # x-axis: ticks every 5
axis(1, at = seq(0, 60, by = 5),              tcl =  TL, labels = FALSE )  # x-axis: ticks every 5
# axis(2, at = seq(600, 750, by = 10), las = 1, tcl = -TL )  # y-axis: ticks every 10
# axis(2, at = seq(600, 750, by = 10), las = 1, tcl =  TL, labels = FALSE )  # y-axis: ticks every 10
axis(2, at = seq(6.4, 6.6, by = .02), las = 1, tcl = -TL )  # y-axis: ticks every 10
axis(2, at = seq(6.4, 6.6, by = .02), las = 1, tcl =  TL, labels = FALSE )  # y-axis: ticks every 10
box()  # redraw box around plot
# abline(h = mean(CASchools$testscr))
abline(v = 10); abline(h = 640)
xgrd <- seq(min(CASchools$avginc), max(CASchools$avginc), length.out = 500)
lines(xgrd, (predict(lm.loglog, newdata = data.frame(avginc = xgrd))), col = "blue",  lwd = 3)
lines(xgrd, (predict(lm.loglin, newdata = data.frame(avginc = xgrd))), col = "red",   lwd = 3)
text(44, 6.531, "Log-Log Regression",    cex = 1.3, col = "blue",  family = "serif")
text(45, 6.585, "Log-Linear Regression", cex = 1.3, col = "red", family = "serif")

### Check dummy regressions from Lecture, Slide 6. ---- 


