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
pdf_name  = c("Lab-03-MoM-Fat Tails.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# print a data frame with its non-integer numbers rounded to 4 decimals, as in the tables of the answer sheet
print_table = function(df, dp = 4){
  is.real = sapply(df, is.double)
  df[is.real] = lapply(df[is.real], function(x) sprintf(paste0("%.", dp, "f"), x))
  print(df, row.names = FALSE)
}

# E|e|/sd(e) for a Student t random variable e with nu > 2 degrees of freedom
# E|e| = 2 sqrt(nu) Gamma((nu+1)/2)/( sqrt(pi) (nu-1) Gamma(nu/2) ) and sd(e) = sqrt(nu/(nu-2))
abs_moment_ratio = function(nu){
  E.abs.e = 2*sqrt(nu)*exp( lgamma((nu + 1)/2) - lgamma(nu/2) )/( sqrt(pi)*(nu - 1) )
  E.abs.e/sqrt(nu/(nu - 2))
}
# MoM estimates for one return series: four moments, the Jarque-Bera test and two estimators of nu
fat_tails_mom = function(r){
  n          = length(r)
  mu.hat     = mean(r)
  e          = r - mu.hat
  sigma2.hat = mean(e^2)
  S.hat      = mean(e^3)/sigma2.hat^1.5               # skewness
  K.hat      = mean(e^4)/sigma2.hat^2                 # kurtosis
  JB         = n*( S.hat^2/6 + (K.hat - 3)^2/24 )
  # b. kurtosis of a t(nu) is 3 + 6/(nu - 4): nu = 4 + 6/(K - 3), valid only if nu > 4
  nu.K       = 4 + 6/(K.hat - 3)
  # c. E|r - mu|/sigma = abs_moment_ratio(nu): solve for nu, valid if nu > 2
  ratio.hat  = mean(abs(e))/sqrt(sigma2.hat)
  nu.A       = uniroot(function(nu) abs_moment_ratio(nu) - ratio.hat, c(2.0001, 1e4), tol = 1e-12)$root
  # scale of the t: Var(r) = s^2 nu/(nu - 2), so s = sigma sqrt((nu - 2)/nu)
  s.A        = sqrt(sigma2.hat*(nu.A - 2)/nu.A)
  VaR.t      = mu.hat + s.A*qt(0.01, nu.A)
  VaR.normal = mu.hat + sqrt(sigma2.hat)*qnorm(0.01)
  c(n = n, mean = mu.hat, sd = sqrt(sigma2.hat), skewness = S.hat, kurtosis = K.hat, JB = JB,
    ratio = ratio.hat, nu.K = nu.K, nu.A = nu.A, s.A = s.A,
    VaR.data = unname(quantile(r, 0.01)), VaR.normal = VaR.normal, VaR.t = VaR.t,
    hit.normal = mean(r < VaR.normal), hit.t = mean(r < VaR.t))
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 8) FAT TAILS BY THE METHOD OF MOMENTS
# daily closing prices of six stock indices, 4 January 1999 - 2 April 2014 ([HMPY2021] Exercise 10.2)
idx = read.csv("./data/hurn_stockindices.csv")
index.names = c(spx = "S&P 500", djx = "Dow Jones", hkx = "Hang Seng", nkx = "Nikkei", dax = "DAX", ukx = "FTSE 100")
tails.table = matrix(NA, length(index.names), 15)
rownames(tails.table) = index.names
for (k in 1:length(index.names)){
  r = 100*diff(log(idx[[names(index.names)[k]]]))      # percentage log returns
  tails.table[k, ] = fat_tails_mom(r)
}
colnames(tails.table) = names(fat_tails_mom(r))
print(round(tails.table[, c("n", "mean", "sd", "skewness", "kurtosis", "JB")], 4))
cat(sprintf(" 5%% critical value of the chi2(2) distribution = %.2f; E|e|/sd(e) for the normal = %.4f\n",
            qchisq(0.95, 2), sqrt(2/pi)))
print(round(tails.table[, c("ratio", "nu.K", "nu.A", "s.A")], 4))
print(round(tails.table[, c("VaR.data", "VaR.normal", "VaR.t", "hit.normal", "hit.t")], 4))

# d. S&P 500: share of the fourth moment due to the 1% largest absolute returns, and recursive estimates
r.spx  = 100*diff(log(idx$spx))
e.spx  = r.spx - mean(r.spx)
top.1  = abs(e.spx) >= quantile(abs(e.spx), 0.99)
cat(sprintf(" S&P 500: the 1%% largest |returns| account for %.1f%% of sum(e^4) and %.1f%% of sum(e^2)\n",
            100*sum(e.spx[top.1]^4)/sum(e.spx^4), 100*sum(e.spx[top.1]^2)/sum(e.spx^2)))
cat(sprintf(" kurtosis without the 1%% largest |returns| = %.2f\n", mean(e.spx[!top.1]^4)/mean(e.spx[!top.1]^2)^2))
n.grid = seq(250, length(r.spx), by = 25)
recursive = t(sapply(n.grid, function(nn) fat_tails_mom(r.spx[1:nn])[c("kurtosis", "nu.K", "nu.A")]))
print(round(recursive[c(1, 20, 60, 100, nrow(recursive)), ], 3))
# Monte Carlo check: t(3) data, where the kurtosis does not exist
set.seed(8)
mc = t(replicate(1000, fat_tails_mom(rt(4000, df = 3))[c("kurtosis", "nu.K", "nu.A")]))
cat(" t(3) data, n = 4000, 1000 samples: median and 90% range\n")
print(round(apply(mc, 2, quantile, c(0.05, 0.5, 0.95)), 4))

# %% PLOT THE RECURSIVE KURTOSIS AND THE TWO MoM ESTIMATES OF nu FOR THE S&P 500
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(4, 4.5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
plot( n.grid, recursive[, "kurtosis"], type = "l", lwd = 2, col = "steelblue",
      xlab = "Number of daily returns used", ylab = "Sample kurtosis", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = 3, col = "gray40", lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
matplot( n.grid, recursive[, c("nu.K", "nu.A")], type = "l", lty = c(2, 1), lwd = 2, col = c("red3", "green4"), ylim = c(2, 8),
         xlab = "Number of daily returns used", ylab = expression(hat(nu)), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = 4, col = "gray40", lty = 3)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("From the kurtosis", "From the mean absolute deviation"), col = c("red3", "green4"), lty = c(2, 1), lwd = 2,
       bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
