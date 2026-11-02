# %% CLEAR SCREEN AND WORKSPACE ----
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

# MAIN SCRIPT: starts here 
CASchools = read_xlsx("./data/caschool.xlsx")           # read the data from .XLSX file in directory ./data
CASchools$Hi_EL = as.numeric(CASchools$el_pct >= 10)    # create dummy variable that is equal to 1 when el_pct >= 10% 

# WITH INTERACTION DUMMIES
lm.testscr_str_HiEL = lm(testscr ~ str + str*Hi_EL+ Hi_EL, data = CASchools)
prnt.testscr_str_HiEL = print_lm_results(lm.testscr_str_HiEL)
print2pdf = 0
path2graphics = "../source.tex.files/graphics/"
path2graphics = ""

if (print2pdf == 1){ pdf( paste0(path2graphics, "California Test Scores Dummy Example.pdf"),
                          width = 11.1, height = 7.6, onefile = TRUE ) }

# %% PLOT FIT AND SCATTER
fnt = 1.45 # font size
op = par( mfrow = c(1, 1), family = "serif", las = 1, 
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 4, 5, 3),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks

# use_dark_plots()
if (print2pdf == 1) {use_dark_plots("white") }

plot( subset(CASchools$str,    CASchools$Hi_EL == 1), 
      subset(CASchools$testscr,CASchools$Hi_EL == 1), 
      xlim = c(10.5,29.5), 
      ylim = c(602,718),
      las = 1, 
      xlab="Student-Teacher Ratio", 
      ylab="Test Score", 
      pch = 16, 
      col = "steelblue",
      # Font sizes
      cex.lab  = fnt,
      cex.axis = fnt-.1,
      xaxt = "n", yaxt = "n",  # Remove default y,x-axis labels
)

# add scatter for Hi_EL == 0
points( subset(CASchools$str,     CASchools$Hi_EL == 0),
        subset(CASchools$testscr, CASchools$Hi_EL == 0),
        pch = 16,
        col = "red3"
)

# ADJUST AXIS TICKS ETC
set_axis_ticks(tcl = .15, cex = fnt)
xgrd <- seq(min(CASchools$str), max(CASchools$str), length.out = 100)
# xgrd <- seq(0, max(CASchools$str), length.out = 100)
# Plot fitted lines for low and high Hi_EL values
lines(xgrd, predict(lm.testscr_str_HiEL,
                    newdata = data.frame(str = xgrd, Hi_EL = 1)),
      col = "steelblue", lwd = 3)

lines(xgrd, predict(lm.testscr_str_HiEL,
                    newdata = data.frame(str = xgrd, Hi_EL = 0)),
      col = "red3", lwd = 3)

legend("topright", 
        legend  = c("High Hi-EL", "Low Hi-EL"),
        col     = c("steelblue", "red3"), 
        lwd = 3, bty = "o"  )

# ADD EQUATION TO PLOT 
coefs <- coef(lm.testscr_str_HiEL)
eq_text_D0 <- bquote(widehat(italic(TestScore[i])) == .(round(coefs[1], 2)) -
              .(abs(round(coefs[2], 2))) %*% italic(STR[i]))

eq_text_D1 <- bquote(widehat(italic(TestScore[i])) == .(round( coefs[1]+coefs[3], 2)) -
              .(abs(round( coefs[2]+coefs[4], 2))) %*% italic(STR[i]))

# add equations for D=1
text( x = max(CASchools$str)*.82,
      y = max(CASchools$testscr)*.855,
      labels = eq_text_D1, pos = 4,
      col = "steelblue",
      cex = fnt+.1)
# add equations for D=0
text( x = max(CASchools$str)*.4,
      y = max(CASchools$testscr)*1.01,
      labels = eq_text_D0, pos = 4,
      col = "red3",
      cex = fnt+.1)

if (print2pdf == 1){ par(op);dev.off() }



