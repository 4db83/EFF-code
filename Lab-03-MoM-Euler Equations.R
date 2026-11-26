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
pdf_name  = c("Lab-03-MoM-CCAPM Moment Functions.pdf",
              "Lab-03-MoM-Equity Premium Moment Functions.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# print a data frame with its non-integer numbers rounded to 4 decimals, as in the tables of the answer sheet
print_table = function(df, dp = 4){
  is.real = sapply(df, is.double)
  df[is.real] = lapply(df[is.real], function(x) sprintf(paste0("%.", dp, "f"), x))
  print(df, row.names = FALSE)
}

# MoM covariance matrix (Lecture 6): Var(theta.hat) = (1/n) d^{-1} S (d')^{-1},
# with d = (1/n) sum dm_t/dtheta' and S = Var(m_t), estimated by (1/n) sum m_t m_t' (L = 0)
# or with Newey-West weights up to lag L
mom_vcov = function(d, m, L = 0){
  n = nrow(m)
  S = crossprod(m)/n
  if (L > 0) {
    for (l in 1:L){
      Gamma.l = crossprod(m[(l+1):n, , drop = FALSE], m[1:(n-l), , drop = FALSE])/n
      S = S + (1 - l/(L+1))*(Gamma.l + t(Gamma.l))
    }
  }
  d.inv = solve(d)
  d.inv %*% S %*% t(d.inv)/n
}
# all roots of a function of one variable: sign changes on a grid, each one refined with uniroot()
find_roots = function(f, grid, ...){
  f.grid = sapply(grid, f, ...)
  k = which(diff(sign(f.grid)) != 0)
  sapply(k, function(j) uniroot(f, c(grid[j], grid[j+1]), ..., tol = 1e-12)$root)
}
# QUESTION 1: h(gamma) = (1/n) sum (z_t - zbar) x_{t+1}^(-gamma) R_{t+1}, a sample covariance
h_ccapm = function(gamma, z, x.1, R.1) mean( (z - mean(z))*x.1^(-gamma)*R.1 )
# QUESTION 3: difference of the two Euler equations, h(gamma) = (1/n) sum (1 + R_c)^(-gamma) (R_s - R_b)
h_premium = function(gamma, R.c, R.s, R.b) mean( (1 + R.c)^(-gamma)*(R.s - R.b) )

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 1) THE CONSUMPTION CAPM
# [MHH2013] Exercise 10.4, Hansen and Singleton (1982)
# Euler equation: E_t[ beta x_{t+1}^(-gamma) R_{t+1} - 1 ] = 0, instruments [1, z_t]'
cc  = read.csv("./data/mhh_ccapm.csv")
T   = nrow(cc)
x.1 = cc$cratio[-1]                          # consumption growth x_{t+1} = c_{t+1}/c_t
R.1 = cc$R[-1]                               # gross real return R_{t+1}
n   = length(x.1)
# the three instruments, all dated t
z.list = list( "c(t)/c(t-1)" = cc$cratio[-T],
               "r(t)"        = cc$R[-T] - 1,
               "e(t)"        = cc$e[-T] )
ccapm.df = data.frame()
for (k in 1:length(z.list)){
  z = z.list[[k]]
  # a.-b. solve h(gamma) = 0 for gamma, then beta = 1/mean( x_{t+1}^(-gamma) R_{t+1} )
  gamma.hat = find_roots(h_ccapm, seq(-60, 60, by = 0.5), z = z, x.1 = x.1, R.1 = R.1)
  w.hat     = x.1^(-gamma.hat)*R.1           # marginal utility weighted return
  beta.hat  = 1/mean(w.hat)
  # c. standard errors: m_t = [1, z_t]' eps_{t+1}; d = mean of dm_t/d[beta, gamma]
  eps.hat   = beta.hat*w.hat - 1             # Euler equation errors
  Z         = cbind(1, z)
  m         = Z*eps.hat
  d.beta    = colMeans(Z*w.hat)
  d.gamma   = colMeans(-Z*beta.hat*log(x.1)*w.hat)
  d         = cbind(d.beta, d.gamma)
  V         = mom_vcov(d, m)                 # Euler errors are not autocorrelated: L = 0
  ccapm.df  = rbind(ccapm.df, data.frame( Instrument = names(z.list)[k],
                    beta     = beta.hat,
                    se.beta  = sqrt(V[1,1]),
                    gamma    = gamma.hat,
                    se.gamma = sqrt(V[2,2]),
                    t.gamma  = gamma.hat/sqrt(V[2,2]),
                    Corr.z.x = cor(z, x.1),
                    max.abs.m.bar = sprintf("%.1e", max(abs(colMeans(m)))) ))
}
cat(sprintf(" n = %d months; mean of c(t+1)/c(t) = %.5f, mean of R(t+1) = %.5f, sd = %.5f\n",
            n, mean(x.1), mean(R.1), sd(R.1)))
print_table(ccapm.df)
cat(sprintf(" annual discount factor beta^12 = %.4f; 95%% CI for gamma: [%.2f, %.2f]\n",
            ccapm.df$beta[1]^12,
            ccapm.df$gamma[1] - 1.96*ccapm.df$se.gamma[1],
            ccapm.df$gamma[1] + 1.96*ccapm.df$se.gamma[1]))
cat(sprintf(" first order autocorrelation of consumption growth = %.3f\n", cor(x.1, cc$cratio[-T])))

# %% PLOT THE MOMENT FUNCTIONS h(gamma)/sd(z) FOR THE THREE INSTRUMENTS
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 9, height = 5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(2.6, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(4, 4.5, 1, 1), # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
gamma.grid = seq(-30, 10, length.out = 401)
H.grid = sapply(z.list, function(z) sapply(gamma.grid, h_ccapm, z = z, x.1 = x.1, R.1 = R.1)/sd(z))
clrs = c("steelblue", "red3", "green4")
matplot( gamma.grid, H.grid, type = "l", lty = c(1, 2, 4), lwd = 2, col = clrs, xlab = expression(gamma),
         ylab = expression(h(gamma)/sd(z[t])), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = 0, col = "gray40")
abline(v = ccapm.df$gamma, lty = 3, lwd = 1.5, col = clrs)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("bottomright", legend = paste("z(t) =", names(z.list)), col = clrs, lty = c(1, 2, 4), lwd = 2,
       bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 3) RISK AVERSION AND THE EQUITY PREMIUM PUZZLE
# [MHH2013] Exercise 10.10, Mehra and Prescott (1985): annual US data, returns from 1889/90 to 1978/79
mp  = read.csv("./data/mhh_equity_mp.csv")
n   = nrow(mp) - 1
t.0 = 1:n                                    # year t
t.1 = 2:(n+1)                                # year t+1
# a. real returns on stocks and bonds and the growth of real per capita consumption
R.s = (mp$s[t.1] + mp$d[t.0] - mp$s[t.0])/mp$s[t.0]
R.b = (1 + mp$r[t.0]/100)*(mp$p[t.0]/mp$p[t.1]) - 1
R.c = (mp$c[t.1] - mp$c[t.0])/mp$c[t.0]
ret.df = data.frame( Series = c("Stocks R.s", "Bonds R.b", "Consumption R.c", "Premium R.s - R.b"),
                     Mean   = c(mean(R.s), mean(R.b), mean(R.c), mean(R.s - R.b)),
                     SD     = c(sd(R.s), sd(R.b), sd(R.c), sd(R.s - R.b)),
                     Corr.with.R.c = c(cor(R.s, R.c), cor(R.b, R.c), 1, cor(R.s - R.b, R.c)) )
print_table(ret.df)

# b. both Euler equations with a constant instrument (2 moments, 2 parameters)
#    their difference does not involve beta: h(gamma) = 0, then beta = 1/mean( (1 + R.c)^(-gamma) (1 + R.b) )
gamma.hat = find_roots(h_premium, seq(0, 60, by = 0.5), R.c = R.c, R.s = R.s, R.b = R.b)
w.hat     = (1 + R.c)^(-gamma.hat)
beta.hat  = 1/mean(w.hat*(1 + R.b))
m         = cbind( beta.hat*w.hat*(1 + R.b) - 1,
                   beta.hat*w.hat*(1 + R.s) - 1 )
d.bonds   = c( mean(w.hat*(1 + R.b)), -beta.hat*mean(log(1 + R.c)*w.hat*(1 + R.b)) )
d.stocks  = c( mean(w.hat*(1 + R.s)), -beta.hat*mean(log(1 + R.c)*w.hat*(1 + R.s)) )
d         = rbind(d.bonds, d.stocks)
V         = mom_vcov(d, m)
se.beta   = sqrt(V[1,1])
se.gamma  = sqrt(V[2,2])
cat(sprintf(' Two Euler equations, constant instrument: beta = %.4f (%.4f), gamma = %.4f (%.4f)\n',
            beta.hat, se.beta, gamma.hat, se.gamma))
cat(sprintf(' 95%% CI for gamma: [%.2f, %.2f]; sample moments at the estimates: %s\n',
            gamma.hat - 1.96*se.gamma, gamma.hat + 1.96*se.gamma,
            paste(sprintf("%.1e", colMeans(m)), collapse = ", ")))
gamma.values = c(0, 1, 2, 5, 10, gamma.hat)
beta.df = data.frame( gamma       = gamma.values,
                      h.gamma     = sapply(gamma.values, h_premium, R.c = R.c, R.s = R.s, R.b = R.b),
                      beta.bonds  = sapply(gamma.values, function(g) 1/mean((1 + R.c)^(-g)*(1 + R.b))),
                      beta.stocks = sapply(gamma.values, function(g) 1/mean((1 + R.c)^(-g)*(1 + R.s))) )
print_table(beta.df)
cat(sprintf(' mean of (1 + R.c)^(-gamma.hat) = %.4f\n', mean(w.hat)))

# c. lognormal approximation: E(r.s - r.b) + Var(r.s)/2 = gamma Cov(dc, r.s)
r.s  = log(1 + R.s)
r.b  = log(1 + R.b)
dc   = log(1 + R.c)
gamma.lognormal = (mean(r.s - r.b) + var(r.s)/2)/cov(dc, r.s)
cat(sprintf(' Lognormal: mean(r.s - r.b) = %.4f, var(r.s)/2 = %.4f, cov(dc, r.s) = %.5f = %.3f x %.4f x %.4f, gamma = %.2f\n',
            mean(r.s - r.b), var(r.s)/2, cov(dc, r.s), cor(dc, r.s), sd(dc), sd(r.s), gamma.lognormal))
SR.premium = mean(R.s - R.b)/sd(R.s - R.b)
cat(sprintf(' Sharpe ratio of R.s - R.b = %.4f; Hansen-Jagannathan bound: gamma >= SR/sd(dc) = %.2f\n',
            SR.premium, SR.premium/sd(dc)))

# d. robustness: one Euler equation at a time with instruments [1, R.c(t)]'
#    Question 1 a. applies with z_t = R.c(t) and x_{t+1} = 1 + R.c(t+1)
x.1 = 1 + R.c[-1]
z.c = R.c[-n]
one.df = data.frame()
for (asset in c("Stocks", "Bonds")){
  if (asset == "Stocks") R.1 = 1 + R.s[-1]
  if (asset == "Bonds")  R.1 = 1 + R.b[-1]
  gamma.roots = find_roots(h_ccapm, seq(-20, 60, by = 0.5), z = z.c, x.1 = x.1, R.1 = R.1)
  for (g in gamma.roots){
    one.df = rbind(one.df, data.frame(Asset = asset, gamma = g, beta = 1/mean(x.1^(-g)*R.1)))
  }
}
print_table(one.df)

# %% PLOT THE MOMENT FUNCTIONS OF (b) AND (d)
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(4, 4.5, 2, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
gamma.grid = seq(0, 30, length.out = 301)
h.grid = sapply(gamma.grid, h_premium, R.c = R.c, R.s = R.s, R.b = R.b)
plot( gamma.grid, h.grid, type = "l", lwd = 2, col = "steelblue", xlab = expression(gamma), ylab = expression(h(gamma)),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
title(main = "(b) Stocks and bonds, constant", cex.main = fnt-.2, font.main = 1)
abline(h = 0, col = "gray40")
abline(v = gamma.hat, lty = 3, lwd = 1.5, col = "steelblue")
set_axis_ticks(tcl = .15, cex = fnt-.1)
h.stocks = sapply(gamma.grid, h_ccapm, z = z.c, x.1 = x.1, R.1 = 1 + R.s[-1])
h.bonds  = sapply(gamma.grid, h_ccapm, z = z.c, x.1 = x.1, R.1 = 1 + R.b[-1])
matplot( gamma.grid, cbind(h.stocks, h.bonds)/sd(z.c), type = "l", lty = c(1, 2), lwd = 2, col = c("steelblue", "red3"),
         xlab = expression(gamma), ylab = expression(h(gamma)/sd(z[t])), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
title(main = expression("(d) One asset, instruments [1," ~ R[c*","*t] * "]'"), cex.main = fnt-.2, font.main = 1)
abline(h = 0, col = "gray40")
abline(v = one.df$gamma, lty = 3, lwd = 1.5, col = ifelse(one.df$Asset == "Stocks", "steelblue", "red3"))
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("bottomleft", legend = c("Stocks", "Bonds"), col = c("steelblue", "red3"), lty = c(1, 2), lwd = 2,
       bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
