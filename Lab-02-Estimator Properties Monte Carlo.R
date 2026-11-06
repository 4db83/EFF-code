# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc(); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,matrixStats,tictoc)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = "Lab-02-CLT Exponential.pdf"
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
mom3 = function(x) mean((x-mean(x))^3)/mean((x-mean(x))^2)^(3/2)

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 4) UNBIASEDNESS AND CONSISTENCY OF TWO VARIANCE ESTIMATORS
# Calc-1, Question 7
set.seed(1)
N.reps = 1e4                       # number of Monte Carlo replications
sample.sizes = c(5, 10, 25, 100, 1000)
mu = 1; sigma2 = 4                 # Y ~ N(1,4)
tic()
var_df = data.frame()
for (n in sample.sizes){
  Y = matrix( rnorm(N.reps*n, mean = mu, sd = sqrt(sigma2)), nrow = N.reps, ncol = n )
  s2.unb = rowVars(Y)              # divides by (n-1)
  s2.n   = s2.unb*(n-1)/n          # divides by n
  var_df = rbind( var_df, data.frame( n = n,
                  E.s2n   = mean(s2.n),   Theory.s2n = (n-1)/n*sigma2,
                  E.s2unb = mean(s2.unb),
                  Pr.far.s2n   = mean(abs(s2.n   - sigma2) > 0.5),   # Pr(|estimator - sigma2| > 0.5)
                  Pr.far.s2unb = mean(abs(s2.unb - sigma2) > 0.5) ))
}
toc()
cat(" Mean of the two variance estimators over", N.reps, "replications (true sigma^2 = 4)\n")
print(format(var_df, digits = 4, nsmall = 4), row.names = FALSE)

# %% QUESTION 5) EFFICIENCY: SAMPLE MEAN VERSUS SAMPLE MEDIAN
# normal and fat-tailed (t with 3 DF) data
set.seed(2)
N.reps = 1e4
eff_df = data.frame()
for (dist in c("Normal", "t(3)")){
  for (n in c(25, 100)){
    if (dist == "Normal") { Y = matrix( rnorm(N.reps*n),       nrow = N.reps ) }
    if (dist == "t(3)")   { Y = matrix( rt(N.reps*n, df = 3),  nrow = N.reps ) }
    Y.mean = rowMeans(Y)
    Y.med  = rowMedians(Y)
    eff_df = rbind( eff_df, data.frame( Dist = dist, n = n,
                    Mean.mean = mean(Y.mean), Mean.med = mean(Y.med),
                    Var.mean = var(Y.mean), Var.med = var(Y.med),
                    Ratio = var(Y.med)/var(Y.mean) ))
  }
}
cat(" Sampling distribution of the mean and median (true centre = 0)\n")
print(format(eff_df, digits = 4, nsmall = 4), row.names = FALSE)
cat(sprintf(' Large-n theory for normal data: Var(median)/Var(mean) = pi/2 = %.4f\n', pi/2))

# %% QUESTION 6) THE CLT WITH SKEWED DATA
# Y ~ Exponential with mean theta = 1
set.seed(3)
N.reps = 1e5
theta  = 1
sample.sizes = c(2, 5, 25, 100)
Z = matrix(NA, nrow = N.reps, ncol = length(sample.sizes))
for (j in 1:length(sample.sizes)){
  n = sample.sizes[j]
  Y.bar  = rowMeans( matrix( rexp(N.reps*n, rate = 1/theta), nrow = N.reps ) )
  Z[,j]  = sqrt(n)*(Y.bar - theta)/theta        # standardised sample mean (sd of Y equals theta)
}
clt_df = data.frame(n = sample.sizes,
                    Pr.Z.below.m1645 = colMeans(Z <= -1.645),
                    Pr.Z.above.p1645 = colMeans(Z >  1.645),
                    Skew.Z = apply(Z, 2, mom3),
                    Skew.Theory = 2/sqrt(sample.sizes) )
cat(" Tail probabilities of Z_n (both are 0.05 under N(0,1)) and skewness of Z_n\n")
print(format(clt_df, digits = 4, nsmall = 4), row.names = FALSE)

# %% PLOT THE HISTOGRAMS OF Z_n WITH THE N(0,1) DENSITY
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name), width = 9, height = 6 ) }
op = par( mfrow = c(2, 2), family = "serif", las = 1,
          mgp = c(2.6, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(4, 4.5, 1, 1), # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
if (print2pdf == 1) {use_dark_plots("white") }
zgrd = seq(-4, 4, length.out = 400)
for (j in 1:length(sample.sizes)){
  hist( Z[Z[,j] < 4 & Z[,j] > -4, j], breaks = 60, freq = FALSE, col = "steelblue", border = "white", main = "",
        xlim = c(-4, 4), ylim = c(0, .6), xlab = bquote(italic(Z[n]) ~ "with" ~ italic(n) == .(sample.sizes[j])),
        ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
  lines(zgrd, dnorm(zgrd), col = "red3", lwd = 2)
  set_axis_ticks(tcl = .15, cex = fnt-.1)
}
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 7) PLIM VERSUS EXPECTATION
# estimating 1/mu by 1/Ybar (Slutsky's theorem)
set.seed(4)
N.reps = 1e5
theta  = 2                      # Y ~ Exponential with mean 2, so 1/mu = 0.5
slut_df = data.frame()
for (n in c(5, 10, 50, 500)){
  Y.bar = rowMeans( matrix( rexp(N.reps*n, rate = 1/theta), nrow = N.reps ) )
  slut_df = rbind(slut_df, data.frame( n = n,
                  E.inv.Ybar  = mean(1/Y.bar),
                  Exact       = n/((n-1)*theta),       # E(1/Ybar) for exponential data (Ybar is Gamma distributed)
                  Inv.mu      = 1/theta,
                  Pr.far      = mean(abs(1/Y.bar - 1/theta) > 0.05) ))
}
cat(" E(1/Ybar) vs 1/E(Ybar) = 0.5, and Pr(|1/Ybar - 0.5| > 0.05)\n")
print(format(slut_df, digits = 4, nsmall = 4), row.names = FALSE)

#EOF
