# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); gc(); cat("\014"); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,matrixStats)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = "EFF-06-Poisson MoM Example.pdf"
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# two MoM estimators of the Poisson parameter lambda: from E(X) = lambda and from E(X^2) = lambda(1 + lambda)

# %% SIMULATE THE TWO MOM ESTIMATORS
set.seed(1)
yLims = c(0,.15)
xLims = c(-0,20)
NSim = 1e4;
n = 4;
lambda = 10;
xmat_lambda = matrix( rpois( n*NSim, lambda = lambda ), nrow = NSim, ncol = n )
mu_x = rowMeans(xmat_lambda)
mu_z = rowMeans(xmat_lambda^2)           # element wise squaring to make Z = X^2
lambda.hat1 = mu_x
lambda.hat2 = .5*sqrt(4*mu_z+1) - 1/2

# Simulation variance of estimator
var(lambda.hat1)
var(lambda.hat2)
# Theory variance
var.mom1 = 1/n*(lambda)
var.mom2 = 1/n*(lambda + 2*lambda^2/(1+2*lambda)^2)
# cat(sprintf("%.4f", var.mom1), "\n")
# cat(sprintf("%.4f", var.mom2), "\n")

results_df <- data.frame(
  Estimator   = c(  "E(X)=λ     :",
                    "E(Z)=λ(1+λ):"),
  Theory      =   c(  var.mom1, 
                      var.mom2),
  Simulation  =  c( var(lambda.hat1), 
                    var(lambda.hat2))
)
cat("Variance of the two MoM Estimators \n")
print( format(results_df, digits = 4, nsmall = 4), row.names = FALSE )

# %% PLOT THE SAMPLING DISTRIBUTIONS OF THE TWO MOM ESTIMATORS
fnt = 1.9 # font size
if (print2pdf == 1){fnt = 1.45; pdf( paste0(path2graphics, pdf_name), width = 11.1, height = 6 ) }

op = par( mfrow = c(1, 2), family = "serif", las = 1, 
          mgp = c(3.0, .5, 0),    # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 4, 1, 0),    # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),    # outer margins for overall spacing
          tcl = -0.25)            # shorter tick marks

lambda.hat = cbind( lambda.hat1,lambda.hat2 )
colnames(lambda.hat) = c( paste0("E(X)|Lambda=",lambda,",n=",n), 
                          paste0("E(Z)|Lambda=",lambda,",n=",n) )

for (n in 1:2){
  h <- hist(lambda.hat[,n], breaks = 32, plot = FALSE)
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
        xlim = xLims,
        ylim = yLims,
        xlab = colnames(lambda.hat)[n],  # Correct syntax
        ylab = "Relative Frequency",
        # Font sizes
        cex.lab  = fnt,
        cex.axis = fnt-.1,
        # xaxt = "n",  # Remove default x-axis
    )
  abline(v = mean(lambda.hat[,n]), col = "red")
  text( y = max(yLims)*.95,
        x = mean(lambda.hat[,n])*.2,
        labels = paste0("Mean = " ,round(mean(lambda.hat[,n]),2)), pos = 4,
        col = "red",
        cex = fnt-.1)
  
  text( y = max(yLims)*.85,
        x = mean(lambda.hat[,n])*.2,
        labels = paste0("Var = " ,round(var(lambda.hat[,n]),2)), pos = 4,
        col = "red",
        cex = fnt-.1)
}



if (print2pdf == 1){ par(op);dev.off() }



