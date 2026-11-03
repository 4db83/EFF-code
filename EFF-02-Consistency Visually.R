# %% CLEAR SCREEN AND WORKSPACE
rm(list = ls()); cat("\014"); gc();
# SET DEFAULTS: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# INSTALL PACMAN: if not installed (note: may need to disable windows firewall for packages to install)
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD HELPER FUNCTIONS STORED IN functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# %% CONSISTENCY OF THE SAMPLE MEAN (Lecture 2): Ybar_n = (1/n) sum_{i=1}^n Y_i  -->p  mu_Y as n -> infinity
# draw one long i.i.d. sample and compute the sample mean for every sample size n = 1, ..., N
N     = 5000      # largest sample size
mu.Y  = 1         # population mean
sig.Y = 1         # population standard deviation
set.seed(1234)   # gives a path like the one on the slide; change the seed to see a different sample path
Y     = rnorm(N, mean = mu.Y, sd = sig.Y)
n     = 1:N
Y.bar = cumsum(Y)/n

# the variance of Ybar_n is sig.Y^2/n, so Ybar_n settles down around mu.Y as n grows
for (k in c(10, 100, 1000, 5000)) {
  cat(sprintf(' n = %4d: Ybar_n = %.4f, SD(Ybar_n) = sig/sqrt(n) = %.4f\n', k, Y.bar[k], sig.Y/sqrt(k)))
}

# %% PLOT THE SAMPLE MEAN AGAINST THE SAMPLE SIZE
add_bands = 0                 # set to 1 to add the band mu.Y +/- 1.96 sig.Y/sqrt(n)
if (print2pdf == 1){ pdf( paste0(path2graphics, "EFF-02-Consistency Visually.pdf"), width = 10, height = 6 ) }
fnt = 1.8 # font size
yos = .005
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(3, 4, 1, 4),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.3)            # outward tick marks
x.ticks  = seq(0, N, by = 500)
y.ticks  = seq(0.6, 1.2, by = 0.1)
plot( n, Y.bar, type = "n", xlim = c(0, N), ylim = c(min(y.ticks)-yos, max(y.ticks)+yos), xaxs = "i", yaxs = "i",
      xlab = "", ylab = "", axes = FALSE )
abline(v = x.ticks, h = y.ticks, col = "grey80", lty = 3)
if (add_bands == 1) {
  lines(n, mu.Y + 1.96*sig.Y/sqrt(n), col = "steelblue", lty = 2, lwd = 1.5)
  lines(n, mu.Y - 1.96*sig.Y/sqrt(n), col = "steelblue", lty = 2, lwd = 1.5)
}
abline(h = mu.Y, lwd = 1.5)
lines(n, Y.bar, col = "#E07B39", lwd = 2.5)
axis(1, at = x.ticks, cex.axis = fnt-.1)
axis(2, at = y.ticks, labels = sprintf("%.1f", y.ticks), cex.axis = fnt-.1)
axis(4, at = y.ticks, labels = sprintf("%.1f", y.ticks), cex.axis = fnt-.1)
axis(3, at = x.ticks, labels = FALSE, tcl = 0.3)
box()
if (print2pdf == 1){ par(op);dev.off() }
