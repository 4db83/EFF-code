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
pdf_name  = "EFF-02-Consistency Visually.pdf"
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# consistency of the sample mean (lecture 2): ybar_n = (1/n) sum_{i=1}^n y_i  -->p  mu_y as n -> infinity
rng_seed = sample.int(1e3, 1)
# cat(rng_seed,'\n') set.seed(rng_seed)   	
set.seed(851)
# gives a path like the one on the slide; change the seed to see a different sample path
# draw one long i.i.d. sample and compute the sample mean for every sample size n = 1, ..., N
N     = 5000      # largest sample sizeh
mu.Y  = 1         # population mean
sig.Y = 1         # population standard deviation
Y     = rnorm(N, mean = mu.Y, sd = sig.Y)
n     = 1:N
Y.bar = cumsum(Y)/n

# the variance of Ybar_n is sig.Y^2/n, so Ybar_n settles down around mu.Y as n grows
for (k in c(10, 100, 1000, 5000)) {
  cat(sprintf(' n = %4d: Ybar_n = %.4f, SD(Ybar_n) = sig/sqrt(n) = %.4f\n', k, Y.bar[k], sig.Y/sqrt(k)))
}

# %% PLOT THE SAMPLE MEAN AGAINST THE SAMPLE SIZE
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.25; pdf( paste0(path2graphics, pdf_name), width = 10, height = 5 ) }
yos = .005	# y-axis limit off-set
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(3, 4, 1, 4),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.3)            # outward tick marks
x.ticks  = seq(0, N, by = N/10)
y.ticks  = seq(.9, 1.5, by = 0.1)
add_bands = 0   # set to 1 to add the band mu.Y +/- 1.96 sig.Y/sqrt(n)
plot( n, Y.bar, type = "n", xlim = c(0, N), ylim = c(min(y.ticks)-yos, max(y.ticks)+yos), xaxs = "i", yaxs = "i",
      xlab = "", ylab = "", axes = FALSE )
abline(v = x.ticks, h = y.ticks, col = "grey80", lty = 3)
if (add_bands == 1) {
  lines(n, mu.Y + 1.96*sig.Y/sqrt(n), col = "steelblue", lty = 2, lwd = 1.5)
  lines(n, mu.Y - 1.96*sig.Y/sqrt(n), col = "steelblue", lty = 2, lwd = 1.5)
}
abline(h = mu.Y, lwd = 1.5)
lines(n, Y.bar, col = "steelblue", lwd = 2.5)
axis(1, at = x.ticks, cex.axis = fnt-.1)
axis(2, at = y.ticks, labels = sprintf("%.1f", y.ticks), cex.axis = fnt-.1)
axis(4, at = y.ticks, labels = sprintf("%.1f", y.ticks), cex.axis = fnt-.1)
axis(3, at = x.ticks, labels = FALSE, tcl = 0.3)
box()
if (print2pdf == 1){ par(op);dev.off() }
