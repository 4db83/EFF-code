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
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))

## SCRIPT: starts here ----
# read the data from .XLSX file in directory ./data
CASchools = read_xlsx("./data/caschool.xlsx")
# RUN OLS REGRESSION
ols0 = lm(testscr ~ str, data = CASchools)
cat("             Dependent Variable: Student-Teacher-Ratio (str) \n")
ols1 = print_results(ols0)

# plot the results for visual clarity-
plot(CASchools$str,CASchools$testscr,xlim = c(10,30), las = 1, ylim = c(600,750),
     xlab="Student-Teacher Ratio", ylab="Test Score", pch = 16, col = rgb(.1,.45,.75))
abline(h = mean(CASchools$testscr))
abline(v = mean(CASchools$str))
abline(ols0, col = rgb(.85,.22,.1), lw = 2)