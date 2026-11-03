# %% CLEAR SCREEN AND WORKSPACE
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
# set to 1 to print to pdf instead of screen
print2pdf = 1
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# set the number of repetitions and the sample sizes
N.reps = 1e5
sample.sizes = c(2, 5, 25, 100)
N.size = max(sample.sizes)
N = length(sample.sizes)

# set seed for reproducibility 
set.seed(1)
p = 0.78  # success probability

Bernoulli.Y  = matrix( as.integer( runif(N.reps*N.size) < p ), nrow = N.reps, ncol = N.size)
# Bernoulli.Y  = as.integer(U.rvs < p)

# make storage
p.hat = matrix( data = NA, nrow = N.reps, ncol = N )
p.std = matrix( data = NA, nrow = N.reps, ncol = N )
tic();  
for (n in 1:N){
  p.hat[,n] = rowMeans(Bernoulli.Y [,1:sample.sizes[n]]);
  p.std[,n] = rowSds(Bernoulli.Y [,1:sample.sizes[n]]);
}
toc()

# %% now do the plotting of the histogram 
fnt = 1.85 		# font size
if (print2pdf == 1){fnt = 1.4; pdf( paste0(path2graphics, "EFF-02-CLT Visually.pdf"), width = 10, height = 7 ) }
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
for (n in 1:N){
  # if (n > 2) { par(mgp = c(3.2, .5, 0)) }
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
        # xlab = bquote(italic(n) == .(sample.sizes[n])),  # Correct syntax
        xlab = "",
        ylab = "Relative Frequency",
        # Font sizes
        cex.lab  = fnt,
        cex.axis = fnt-.1,
        xaxt = "n"
      )
  # axis(1, at = seq(0.0, 1.00, by = 0.05), cex.axis = 1.1)
  axis(1, at = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), cex.axis = fnt-.1)
  # adjust text manually
  mtext(bquote(italic(n) == .(sample.sizes[n])), side = 1, line = 2.2, cex = fnt-.25)
}
if (print2pdf == 1){ par(op); dev.off() }

























