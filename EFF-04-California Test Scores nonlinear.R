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
pdf_name  = c("EFF-04-California Test Scores Linear Fit.pdf",
              "EFF-04-California Test Scores Polynomial Fits.pdf")
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
CASchools = read_xlsx("./data/caschool.xlsx")           # read the data from .XLSX file in directory ./data
CASchools$Hi_EL = as.numeric(CASchools$el_pct >= 10)    # create dummy variable that is equal to 1 when el_pct >= 10% 

# %% LINEAR REGRESSION OF TEST SCORES ON DISTRICT INCOME
# SW Figure 8.2, p.237
cat("\014")
lm.testscr_avginc   = lm( testscr ~ avginc, data = CASchools)
prnt.testscr_avginc  = print_lm_results(lm.testscr_avginc)

# %% PLOT THE SCATTER WITH THE LINEAR FIT
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 10, height = 7 ) }
op = par(family = "serif", las = 1,
    mgp = c(3.5, .5, 0),   # axis title, labels, line, position of y,x labels to axis
    mar = c(5, 5, 5, 3),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
    oma = c(1, 1, 1, 1),   # outer margins for overall spacing
    tcl = -0.25            # shorter tick marks
)
use_dark_plots()
if (print2pdf == 1) {use_dark_plots("white") }
plot( CASchools$avginc, CASchools$testscr, 
      xlim = c(2,58), 
      ylim = c(602,737),
      las = 1, 
      xlab="District Income", 
      ylab="Test Score", 
      pch = 16, 
      col = "steelblue",
      # Font sizes
      cex.lab  = fnt,
      cex.axis = fnt-.1,
      xaxt = "n", yaxt = "n",  # Remove default y,x-axis labels
)

# ADJUST AXIS TICKS ETC
set_axis_ticks(tcl = .15, cex = fnt)
xgrd <- seq(min(CASchools$avginc), max(CASchools$avginc), length.out = 100)
lines(xgrd, ( predict(lm.testscr_avginc, newdata = data.frame(avginc = xgrd)) ), 
      col = "red3",  lwd = 2 )
# abline(lm.testscr_avginc, col = rgb(.85,.22,.1), lw = 2) # this goes over the entire plot range

# ADD EQUATION TO PLOT
coefs <- coef(lm.testscr_avginc)
eq_text <- bquote(widehat(italic(TestScore[i])) == .(round(coefs[1], 2)) -
                    .(abs(round(coefs[2], 4))) %*% italic(AvgInc[i]))
text( x = max(CASchools$avginc)*.07,
      y = max(CASchools$testscr)*1.02,
      labels = eq_text, pos = 4,
      col = "red3",
      cex = fnt+.2)
# ADD ARROWS
arrows(	14, 715,            # Start near bottom of text
        27, 680,            # End on the line
        col = "red3",
        lwd = 2.0,
        length = 0.1)
if (print2pdf == 1){ par(op); dev.off() }

# %% QUADRATIC AND CUBIC REGRESSIONS OF TEST SCORES ON DISTRICT INCOME
# SW Figure 8.3, p.239
lm.testscr_avginc2  = lm( testscr ~ avginc + I(avginc^2)               , data = CASchools)
lm.testscr_avginc3  = lm( testscr ~ avginc + I(avginc^2) + I(avginc^3) , data = CASchools)
prnt.testscr_avginc2  = print_lm_results(lm.testscr_avginc2)
prnt.testscr_avginc3  = print_lm_results(lm.testscr_avginc3)

# %% PLOT THE SCATTER WITH THE LINEAR, QUADRATIC AND CUBIC FITS
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 10, height = 7 ) }
op = par(family = "serif", las = 1,
    mgp = c(3.5, .5, 0),   # axis title, labels, line, position of y,x labels to axis
    mar = c(5, 5, 5, 3),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
    oma = c(1, 1, 1, 1),   # outer margins for overall spacing
    tcl = -0.25            # shorter tick marks
)
# use_dark_plots("white")
# use_dark_plots("black")
plot( CASchools$avginc, CASchools$testscr, 
      xlim = c(2,58), 
      ylim = c(602,737),
      las = 1, 
      xlab="District Income", 
      ylab="Test Score", 
      pch = 16, 
      col = "steelblue",
      # Font sizes
      cex.lab  = fnt,
      cex.axis = fnt-.1,
      xaxt = "n", yaxt = "n",  # Remove default y,x-axis labels
)

# ADJUST AXIS TICKS ETC
set_axis_ticks(tcl = .15, cex = fnt)
lines(xgrd, ( predict(lm.testscr_avginc, newdata = data.frame(avginc = xgrd)) ), 
      col = "red1",   lwd = 2 )
lines(xgrd, ( predict(lm.testscr_avginc2, newdata = data.frame(avginc = xgrd)) ), 
      col = "blue2",  lwd = 2 )
lines(xgrd, ( predict(lm.testscr_avginc3, newdata = data.frame(avginc = xgrd)) ), 
      col = "green4", lwd = 2 )

# ADD EQUATION TO PLOT
coefs1 <- coef(lm.testscr_avginc)
coefs2 <- coef(lm.testscr_avginc2)
coefs3 <- coef(lm.testscr_avginc3)
eq_text1 <- bquote( widehat(italic(TestScore[i])) == .(round(coefs1[1], 2)) 
                    - .(abs(round(coefs1[2], 2))) %*% italic(AvgInc[i]) )
eq_text2 <- bquote( widehat(italic(TestScore[i])) == .(round(coefs2[1], 2)) 
                    + .(abs(round(coefs2[2], 2))) %*% italic(AvgInc[i]) 
                    - .(abs(round(coefs2[3], 2))) %*% italic(AvgInc[i]^2) ) 
eq_text3 <- bquote( widehat(italic(TestScore[i])) == .(round(coefs3[1], 2)) 
                    + .(abs(round(coefs3[2], 2))) %*% italic(AvgInc[i]) 
                    - .(abs(round(coefs3[3], 2))) %*% italic(AvgInc[i]^2)  
                    + .(abs(round(coefs3[4], 4))) %*% italic(AvgInc[i]^3) ) 
# linear
text( x = max(CASchools$avginc)*.07,
      y = max(CASchools$testscr)*1.02,
      labels = eq_text1, pos = 4,
      col = "red1",
      cex = fnt+.2)
# ADD ARROWS
arrows(	29, 720,            # Start near bottom of text
        45, 715,            # End on the line
        col = "red1",
        lwd = 2.0,
        length = 0.1)
# Quadratic
text( x = max(CASchools$avginc)*.4,
      y = max(CASchools$testscr)*.91,
      labels = eq_text2, pos = 4,
      col = "blue2",
      cex = fnt+.2)
# ADD ARROWS
arrows(	44, 650,            # Start near bottom of text
        50, 690,            # End on the line
        col = "blue2",
        lwd = 2.0,
        length = 0.1)
# Cubic
text( x = max(CASchools$avginc)*.07,
      y = max(CASchools$testscr)*1,
      labels = eq_text3, pos = 4,
      col = "green4",
      cex = fnt+.2)
# ADD ARROWS
arrows(	49, 706,            # Start near bottom of text
        54, 702,            # End on the line
        col = "green4",
        lwd = 2.0,
        length = 0.1)
if (print2pdf == 1){ par(op); dev.off() }

# %% LINEAR-LOG, LOG-LINEAR AND LOG-LOG REGRESSIONS
# semi-elasticities and elasticities, output only, no plotting
cat("\014")
lm.linlog  = lm( testscr ~ log(avginc)       , data = CASchools)
lm.loglog  = lm( log(testscr) ~ log(avginc)  , data = CASchools)
lm.loglin  = lm( log(testscr) ~ avginc       , data = CASchools)
cat("Linear-Log: TestScore vs. log(Income)\n")
prnt.linlog = print_lm_results(lm.linlog)
cat("Log-Linear: log(TestScore) vs. Income\n")
prnt.loglin = print_lm_results(lm.loglin)
cat("Log-Log: log(TestScore) vs. log(Income)\n")
prnt.loglog = print_lm_results(lm.loglog)





