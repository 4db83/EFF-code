# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc(); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = "Calc-2-Q5.pdf"
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# Calculation Exercises 2, Question 5 (SW2020 Exercise 8.x)

# X is continuous on [5,100], Z is binary. Sketch the fitted regression functions.
x = seq(5, 100, length.out = 1000)
y.a    = 2 + 3*log(x)
y.b    = 2 - 3*log(x)
y.c.z1 = 2 + 3*log(x) + 4*1
y.c.z0 = 2 + 3*log(x) + 4*0
y.d.z1 = 2 + 3*log(x) + 4*1 - 1*1*log(x)
y.d.z0 = 2 + 3*log(x) + 4*0 - 1*0*log(x)
y.e    = 1 + 125*x - 0.01*x^2

# %% PLOT THE 4 PANELS
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name), width = 11.1, height = 7.6 ) }
op = par( mfrow = c(2, 2), family = "serif", las = 1,
          mgp = c(2.6, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(4, 4.5, 1, 1), # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
if (print2pdf == 1) {use_dark_plots("white") }

# panel a) and b)
plot( x, y.a, type = "l", lwd = 2, col = "steelblue", ylim = c(-13, 24),
      xlab = expression(italic(X)), ylab = expression(hat(italic(Y))),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(x, y.b, lwd = 2, col = "red3")
abline(h = 0, col = "grey60")
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend( "topleft", bty = "o", bg = "white", cex = fnt-.35, lwd = 2, col = c("steelblue", "red3"),
        legend = c( expression( "a) " ~ hat(Y) == 2 + 3 %*% ln(X) ),
                    expression( "b) " ~ hat(Y) == 2 - 3 %*% ln(X) ) ) )

# panel c) i. and ii.
plot( x, y.c.z1, type = "l", lwd = 2, col = "steelblue", ylim = c(5, 20),
      xlab = expression(italic(X)), ylab = expression(hat(italic(Y))),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(x, y.c.z0, lwd = 2, col = "red3", lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend( "bottomright", bty = "o", bg = "white", cex = fnt-.35, lwd = 2, lty = c(1,2), col = c("steelblue", "red3"),
        legend = c( expression( "c) i.  " ~ hat(Y) == 2 + 3 %*% ln(X) + 4 * Z ~ "," ~ Z == 1),
                    expression( "c) ii. " ~ hat(Y) == 2 + 3 %*% ln(X) + 4 * Z ~ "," ~ Z == 0) ) )

# panel d) i. and ii.
plot( x, y.d.z1, type = "l", lwd = 2, col = "steelblue", ylim = c(5, 17),
      xlab = expression(italic(X)), ylab = expression(hat(italic(Y))),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(x, y.d.z0, lwd = 2, col = "red3", lty = 2)
abline(v = exp(4), col = "grey60", lty = 3)  # the two lines cross at X = exp(4)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend( "bottomright", bty = "o", bg = "white", cex = fnt-.35, lwd = 2, lty = c(1,2), col = c("steelblue", "red3"),
        legend = c( expression( "d) i.  " ~ hat(Y) == 2 + 3 %*% ln(X) + 4 * Z - Z %*% ln(X) ~ "," ~ Z == 1),
                    expression( "d) ii. " ~ hat(Y) == 2 + 3 %*% ln(X) + 4 * Z - Z %*% ln(X) ~ "," ~ Z == 0) ) )

# panel e)
plot( x, y.e, type = "l", lwd = 2, col = "steelblue",
      xlab = expression(italic(X)), ylab = expression(hat(italic(Y))),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend( "bottomright", bty = "o", bg = "white", cex = fnt-.35, lwd = 2, col = "steelblue",
        legend = expression( "e) " ~ hat(Y) == 1 + 125 %*% X - 0.01 %*% X^2 ) )

if (print2pdf == 1){ par(op);dev.off() }

# %% WHERE DO THE TWO LINES IN d) CROSS?
# 2 + 3ln(X) = 6 + 2ln(X) <=> ln(X) = 4
cat(sprintf(' Lines in d) cross at X = exp(4) = %.4f\n', exp(4)))
# turning point of the quadratic in e): 125 - 0.02X = 0
cat(sprintf(' Maximum of e) is at X = 125/0.02 = %.0f (far outside [5,100])\n', 125/0.02))

#EOF
