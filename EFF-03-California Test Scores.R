# CLEAR SCREEN AND WORKSPACE ----
rm(list = ls()); cat("\014"); gc(); 
# SET DEFAULTS: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
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
CASchools$Hi_EL = as.numeric(CASchools$el_pct >= 10)
# RUN OLS REGRESSION
lm.testscr_str    = lm(testscr ~ str, data = CASchools)
prnt.testscr_str  = print_lm_results(lm.testscr_str)

# PLOT THE RESULTS FOR VISUAL CLARITY----
par( family = "serif", las = 1, 
mgp = c(3.5, .5, 0),   # axis title, labels, line, position of y,x labels to axis
mar = c(5, 5, 5, 3),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
oma = c(1, 1, 1, 1),   # outer margins for overall spacing
tcl = -0.25            # shorter tick marks
)
# font size
fnt = 1.35
plot( CASchools$str, CASchools$testscr, 
      xlim = c(10.5,29.5), 
      ylim = c(602,718),
      las = 1, 
      xlab="Student-Teacher Ratio", 
      ylab="Test Score", 
      pch = 16, 
      col = rgb(.1,.45,.75),
      # Font sizes
      cex.lab  = fnt,
      cex.axis = fnt-.1,
      xaxt = "n", yaxt = "n",  # Remove default y,x-axis labels
     )

# ADJUST AXIS TICKS ETC
set_axis_ticks(tcl = .15, cex = fnt)
abline(lm.testscr_str, col = rgb(.85,.22,.1), lw = 2)
# axis(1, at = seq(10.0, 30.00, by = 5), cex.axis = fnt-.1, tcl = tickLength )
# axis(1, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), cex.axis = fnt-.1, tcl = tickLength )

# ADD EQUATION TO PLOT
coefs <- coef(lm.testscr_str)
eq_text <- bquote(widehat(italic(TestScore[i])) == .(round(coefs[1], 2)) -
                     .(abs(round(coefs[2], 4))) %*% italic(STR[i]))
text( x = max(CASchools$str)*.77,
      y = max(CASchools$testscr)*.98,
      labels = eq_text, pos = 4,
      col = rgb(.95,.02,.1),
      cex = fnt+.2)

arrows(24, 687,            # Start near bottom of text
       26, 642,            # End on the line
       col = rgb(.95,.02,.1),
       lwd = 2.0,
       length = 0.1)

