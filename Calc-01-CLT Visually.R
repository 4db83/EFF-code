# CLEAR SCREEN AND WORKSPACE ----
rm(list = ls()); cat("\014"); gc(); 
# SET DEFAULTS: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# INSTALL PACMAN: if not installed (note: may neeD:\_teaching\_current.teaching\_SU.EFF\code-EFF\helper_functions\print_results.Rd to disable windows firewall for packages to install)
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD REQUIRED PACKAGES 
pacman::p_load(tictoc, matrixStats, ggplot2)
# LOAD HELPER FUNCTIONS STORED IN functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))

# set the number of repetitions and the sample sizes
N.reps = 1e5
sample.sizes = c(2, 5, 25, 100)
N.size = max(sample.sizes)
N = length(sample.sizes)

# set seed for reproducibility ----
set.seed(123)
p = 0.78  # success probability

Brn.rvs = matrix( as.integer( runif(N.reps*N.size) < p ), nrow = N.reps, ncol = N.size)
# Brn.rvs = as.integer(U.rvs < p)
# tic();Brn.rvs = ifelse(U.rvs < p, 1, 0);toc()

# make storage
p.hat = matrix( data = NA, nrow = N.reps, ncol = N )
p.std = matrix( data = NA, nrow = N.reps, ncol = N )
tic();  
for (n in 1:N){
  p.hat[,n] = rowMeans(Brn.rvs[,1:sample.sizes[n]]);
  p.std[,n] = rowSds(Brn.rvs[,1:sample.sizes[n]]);
}
toc()

# now do the plotting of the histogram ----
use_dark_plots()            # dark background,  change use_dark_plots("white)
# subdivide the plot panel into a 2-by-2 array
par( mfrow = c(2, 2), family = "serif", las = 1, 
     mgp = c(2.9, .5, 0),   # axis title, labels, line, position of y,x labels to axis
     mar = c(5, 5, 0, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
     oma = c(1, 1, 1, 1),   # outer margins for overall spacing
     tcl = -0.3             # shorter tick marks
    )
# font size
fnt = 1.35

# do this manually as R's base function returns the density/probability, 
# not the "relative frequency of each bin"  
Up.y.lim = c(.7,.5,.25,.125)
for (n in 1:N){
  h <- hist(p.hat[,n], breaks = 32, plot = FALSE)
  # Normalize manually to probabilities
  h$density <- h$counts / sum(h$counts)
  # Plot with probabilities
  if (n > 2){par( mgp = c(3.2, .5, 0) )}
  plot( h,
        freq = FALSE,          # Plot densities, not counts
        col = "steelblue",
        main = "",
        border = "black",
        # lwd = .05,
        xlim = c(-0, 1),
        ylim = c(0, Up.y.lim[n]),
        xlab = bquote(italic(n) == .(sample.sizes[n])),  # Correct syntax
        ylab = "Relative Frequency",
        # Font sizes
        cex.lab  = fnt,
        cex.axis = fnt-.1,
        xaxt = "n",  # Remove default x-axis
      )
  # axis(1, at = seq(0.0, 1.00, by = 0.05), cex.axis = 1.1)
  axis(1, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), cex.axis = fnt-.1)
  
  
  
  # hist(p.hat[,n],
  #      col = "steelblue",
  #      freq = FALSE,
  #      breaks = 23,
  #      xlim = c(-0, 1),
  #      ylim = y.lims,
  #      xlab = bquote(italic(n) == .(sample.sizes[n])),  # Correct syntax
  #      main = "",
  #      ylab = "Probability",
  #      # Font sizes
  #      cex.main = 1.8,
  #      cex.lab  = fnt,
  #      cex.axis = fnt-.2,
  # )
}













