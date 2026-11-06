# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); gc(); cat("\014"); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(tictoc, matrixStats, ggplot2)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 1
pdf_name  = "Calc-01-CLT Visually.pdf"
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# Draw from Bernoulli RV with success probablity p
rbernoulli = function(n_rows, n_cols, p) { matrix( as.integer( runif(n_rows*n_cols) < p ), nrow = n_rows, ncol = n_cols) }

# %% SCRIPT STARTS HERE
# set the number of repetitions and the sample sizes
N.reps = 1e5
sample.sizes = c(2, 5, 25, 100)
N.size = max(sample.sizes)
N = length(sample.sizes)

# set seed for reproducibility 
set.seed(1)
p = 0.78  # success probability

# draw from Bernoulli RV of 
# Bernoulli.Y  = matrix( as.integer( runif(N.reps*N.size) < p ), nrow = N.reps, ncol = N.size)
Bernoulli.Y = rbernoulli(N.reps, N.size, p)

# make storage
p.hat = matrix( data = NA, nrow = N.reps, ncol = N )
p.std = matrix( data = NA, nrow = N.reps, ncol = N )
tic();  
for (n in 1:N){
  p.hat[,n] = rowMeans(Bernoulli.Y [,1:sample.sizes[n]]);
  p.std[,n] = rowSds(Bernoulli.Y [,1:sample.sizes[n]]);
}
toc()

# % PLOT THE HISTOGRAMS OF THE SAMPLE PROPORTION
fnt = 1.85 		# font size
if (print2pdf == 1){fnt = 1.4; pdf( paste0(path2graphics, pdf_name), width = 10, height = 7 ) }
# subdivide the plot panel into a 2-by-2 array
op = par(	mfrow = c(2, 2), family = "serif", las = 1, 
          mgp 	= c(2.9, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar 	= c(3, 5, 0, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma 	= c(0, 0, 0, 0),   # outer margins for overall spacing
          tcl 	= -0.3             # shorter tick marks
          )

# do this manually as R's base function returns the density/probability, 
# not the "relative frequency of each bin"  
Up.y.lim = c(.7,.5,.25,.125)

for (n in 1:N) {
  z <- (p.hat[, n] - p) / sqrt(p * (1 - p) / sample.sizes[n])

  # Use fixed-width bins; include the full range of simulated values
  breaks = seq(-5.6, 5.6, by = .24)
  
  h <- hist(z, breaks = breaks, plot = FALSE)

  if (n > 2) par(mgp = c(3.2, .5, 0))

  plot( h,
        freq = FALSE,             # density scale, so the normal curve is comparable
        col = "steelblue",
        border = "black",
        main = "",
        xlim = c(-4, 4),
        ylim = c(0, 1.21*max(h$density, dnorm(0))),
        xlab = "",
        ylab = "Density",
        cex.lab = fnt,
        cex.axis = fnt - .1,
        xaxt = "n"
  )

  axis(1, at = -5:5, cex.axis = fnt - .1)

  curve(dnorm(x), from = -5, to = 5,
        add = TRUE, col = "red", lwd = 2)

  mtext(bquote(italic(n) == .(sample.sizes[n])),
        side = 1, line = 2.2, cex = fnt - .25)
}

if (print2pdf == 1){ par(op); dev.off() }

























  