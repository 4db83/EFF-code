# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc(); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = ""   # this script makes no plots
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# Calculation Exercises 2: percentage change and log change of sales

# %% QUESTION 1) PERCENTAGE CHANGE VERSUS LOG CHANGE (parts a. to c.)
Sales_2013 = 196
Sales_2014 = 500

change      = 100*(Sales_2014 - Sales_2013)/Sales_2013
log_change  = 100*(log(Sales_2014) - log(Sales_2013))
# cat("     Change is:", round(change     , digits = 4), '%\n')
# cat(" Log-Change is:", round(log_change , digits = 4), '%')
cat(sprintf('     Change is:  %.4f%%\n',     change))
cat(sprintf(' Log-Change is:  %.4f%%\n', log_change))


