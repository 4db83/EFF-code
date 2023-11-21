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

## Question 1) a to c. -----
Sales_2013 = 196
Sales_2014 = 500

change      = 100*(Sales_2014 - Sales_2013)/Sales_2013
log_change  = 100*(log(Sales_2014) - log(Sales_2013))
# cat("     Change is:", round(change     , digits = 4), '%\n')
# cat(" Log-Change is:", round(log_change , digits = 4), '%')
cat(sprintf('     Change is:  %.4f%%\n',     change))
cat(sprintf(' Log-Change is:  %.4f%%\n', log_change))


