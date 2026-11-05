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
pdf_name  = "EFF-03-California Test Scores.pdf"
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# READ THE DATA: from .XLSX file in directory ./data
CASchools = read_xlsx("./data/caschool.xlsx")
CASchools$Hi_EL = as.numeric(CASchools$el_pct >= 10)
# RUN OLS REGRESSION
lm.testscr_str    = lm(testscr ~ str, data = CASchools)
prnt.testscr_str  = print_lm_results(lm.testscr_str)

# %% PLOT THE SCATTER WITH THE FITTED REGRESSION LINE
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name), width = 10, height = 7 ) }
op = par( family = "serif", las = 1,
mgp = c(3.5, .5, 0),   # axis title, labels, line, position of y,x labels to axis
mar = c(5, 5, 5, 3),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
oma = c(1, 1, 1, 1),   # outer margins for overall spacing
tcl = -0.25            # shorter tick marks
)
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

# adjust axis ticks etc
set_axis_ticks(tcl = .15, cex = fnt)
abline(lm.testscr_str, col = rgb(.85,.22,.1), lw = 2)
# axis(1, at = seq(10.0, 30.00, by = 5), cex.axis = fnt-.1, tcl = tickLength )
# axis(1, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), cex.axis = fnt-.1, tcl = tickLength )

# add equation to plot
coefs <- coef(lm.testscr_str)
eq_text <- bquote(widehat(italic(TestScore[i])) == .(round(coefs[1], 2)) -
            .(abs(round(coefs[2], 4))) %*% italic(STR[i]))
text( x = max(CASchools$str)*.77,
      y = max(CASchools$testscr)*.98,
      labels = eq_text, pos = 4,
      col = rgb(.95,.02,.1),
      cex = fnt+.2)

arrows(	24, 687,            # Start near bottom of text
        26, 642,            # End on the line
        col = rgb(.95,.02,.1),
        lwd = 2.0,
        length = 0.1)
if (print2pdf == 1){ par(op); dev.off() }

