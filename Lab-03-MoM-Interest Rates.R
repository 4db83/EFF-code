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
pdf_name  = c("Lab-03-MoM-Levels Effect.pdf",
              "Lab-03-MoM-Interest Rate Distribution.pdf")
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
# QUESTION 2: the CKLS model r_{t+1} - r_t = alpha + beta r_t + sigma r_t^gamma z_{t+1}, instruments [1, r_t]'
ckls_mom = function(r){
  dr  = diff(r)                              # r_{t+1} - r_t
  r.0 = head(r, -1)                          # r_t
  X   = cbind(1, r.0)
  # the first two moments are the OLS normal equations: alpha and beta are the OLS estimates
  ab.hat = as.vector(solve(crossprod(X), crossprod(X, dr)))
  u.hat  = as.vector(dr - X %*% ab.hat)
  # the third gives sigma^2(gamma) = mean(u^2)/mean(r^(2 gamma)); the fourth then leaves one equation in gamma
  h_gamma   = function(g) mean(u.hat^2*r.0)/mean(u.hat^2) - mean(r.0^(2*g + 1))/mean(r.0^(2*g))
  gamma.hat = find_roots(h_gamma, seq(0, 4, by = 0.05))
  sigma.hat = sqrt( mean(u.hat^2)/mean(r.0^(2*gamma.hat)) )
  # standard errors: m_t = [u, u r, v, v r]' with v = u^2 - sigma^2 r^(2 gamma)
  r.2g  = r.0^(2*gamma.hat)
  log.r = ifelse(r.0 > 0, log(r.0), 0)       # r^(2 gamma) log(r) -> 0 as r -> 0
  v.hat = u.hat^2 - sigma.hat^2*r.2g
  m     = cbind(u.hat, u.hat*r.0, v.hat, v.hat*r.0)
  # d = mean of dm_t/d[alpha, beta, sigma, gamma]
  d = rbind( c(-1,                -mean(r.0),            0,                             0),
             c(-mean(r.0),        -mean(r.0^2),          0,                             0),
             c(-2*mean(u.hat),     -2*mean(u.hat*r.0),     -2*sigma.hat*mean(r.2g),       -2*sigma.hat^2*mean(r.2g*log.r)),
             c(-2*mean(u.hat*r.0), -2*mean(u.hat*r.0^2),   -2*sigma.hat*mean(r.2g*r.0),   -2*sigma.hat^2*mean(r.2g*log.r*r.0)) )
  theta.hat = c(alpha = ab.hat[1], beta = ab.hat[2], sigma = sigma.hat, gamma = gamma.hat)
  list( theta = theta.hat, V = mom_vcov(d, m), m.bar = colMeans(m),
        lhs = mean(u.hat^2*r.0)/mean(u.hat^2), r.bar = mean(r.0) )
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 2) LEVELS EFFECT IN US INTEREST RATES
# [MHH2013] Section 10.5.2 and Exercise 10.9
lv = read.csv("./data/mhh_level.csv")
maturities = c("r0m", "r1m", "r3m", "r6m", "r9m", "r10y")

# a. the equation in gamma for the 3 month rate: left side (squared residual weighted mean) and the mean rate
fit.3m = ckls_mom(lv$r3m)
cat(sprintf(' 3 month rate: sum(u^2 r)/sum(u^2) = %.2f, mean rate = %.2f\n', fit.3m$lhs, fit.3m$r.bar))

# b. Vasicek (gamma = 0) and CIR (gamma = 1/2): with gamma fixed, [u, u r, v]' identify [alpha, beta, sigma]'
r     = lv$r3m
dr    = diff(r)
r.0   = head(r, -1)
ols   = lm(dr ~ r.0)
u.hat = resid(ols)
alpha.hat = unname(coef(ols)[1])
beta.hat  = unname(coef(ols)[2])
vasicek.cir.df = data.frame( Model = c("Vasicek (gamma = 0)", "CIR (gamma = 1/2)"),
                             gamma = c(0, 0.5),
                             alpha = alpha.hat,
                             beta  = beta.hat,
                             sigma = c(sqrt(mean(u.hat^2)), sqrt(mean(u.hat^2)/mean(r.0))),
                             Long.run.mean    = -alpha.hat/beta.hat,
                             Half.life.months = log(0.5)/log(1 + beta.hat) )
print_table(vasicek.cir.df)
ols.table = summary(ols)$coefficients
print(ols.table)                                              # t-statistic of beta

# c. CKLS model for all six maturities, with standard errors as in [MHH2013] Table 10.6
levels.df = data.frame()
for (k in maturities){
  fit = ckls_mom(lv[[k]])
  se  = sqrt(diag(fit$V))
  levels.df = rbind(levels.df, data.frame( Maturity = k,
                    alpha = fit$theta[1], se.alpha = se[1],
                    beta  = fit$theta[2], se.beta  = se[2],
                    sigma = fit$theta[3], se.sigma = se[3],
                    gamma = fit$theta[4], se.gamma = se[4],
                    max.abs.m.bar = sprintf("%.1e", max(abs(fit$m.bar))) ))
}
print_table(levels.df)

# d. tests of gamma = 0, 0.5, 1.0 and 1.5: t-statistics (their squares are the Wald statistics of the book)
test.df = data.frame( Maturity = levels.df$Maturity,
                      gamma    = levels.df$gamma,
                      t.0      = (levels.df$gamma - 0.0)/levels.df$se.gamma,
                      t.05     = (levels.df$gamma - 0.5)/levels.df$se.gamma,
                      t.1      = (levels.df$gamma - 1.0)/levels.df$se.gamma,
                      t.15     = (levels.df$gamma - 1.5)/levels.df$se.gamma )
print_table(test.df)

# %% e. PLOT (r_{t+1} - r_t)/r_t^gamma FOR gamma = 0, 0.5, 1.0, 1.5 (3 MONTH RATE)
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 9, height = 6 ) }
op = par( mfrow = c(2, 2), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(3, 4.5, 2, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
year.t1 = 1947 + (0:(length(dr)-1))/12        # dates of r_{t+1}, January 1947 to February 1991
for (g in c(0, 0.5, 1, 1.5)){
  plot( year.t1, dr/r.0^g, type = "l", col = "steelblue", xlab = "", ylab = "",
        cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
  title(main = bquote(gamma == .(g)), cex.main = fnt, font.main = 1)
  set_axis_ticks(tcl = .15, cex = fnt-.1)
}
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 9) THE DISTRIBUTION OF THE 3 MONTH RATE: GAMMA, LOGNORMAL AND THE CIR MODEL
r = lv$r3m
T = length(r)
# a. two moment conditions for theta = [mu, sigma^2]' (mean and variance with divisor T)
mu.hat     = mean(r)
sigma2.hat = mean((r - mu.hat)^2)
#    gamma distribution: E(r) = p/alpha and Var(r) = p/alpha^2, so alpha = mu/sigma^2 and p = mu^2/sigma^2
alpha.gamma = mu.hat/sigma2.hat
p.gamma     = mu.hat^2/sigma2.hat
#    standard errors: d = -I, so Var(theta.hat) = S/T; then the delta method for [alpha, p]'
m = cbind(r - mu.hat, (r - mu.hat)^2 - sigma2.hat)
G = rbind( c(1/sigma2.hat,       -mu.hat/sigma2.hat^2),        # d alpha/d[mu, sigma^2]
           c(2*mu.hat/sigma2.hat, -mu.hat^2/sigma2.hat^2) )     # d p/d[mu, sigma^2]
se.df = data.frame()
for (L in c(0, 12, 60)){
  V.theta = mom_vcov(-diag(2), m, L)
  V.gamma = G %*% V.theta %*% t(G)
  se.df = rbind(se.df, data.frame( NW.lags = L,
                se.mu = sqrt(V.theta[1,1]), se.sigma2 = sqrt(V.theta[2,2]),
                se.alpha = sqrt(V.gamma[1,1]), se.p = sqrt(V.gamma[2,2]) ))
}
cat(sprintf(' T = %d months: mean = %.4f, variance = %.4f (sd %.4f), min = %.3f, max = %.3f\n',
            T, mu.hat, sigma2.hat, sqrt(sigma2.hat), min(r), max(r)))
cat(sprintf(' Gamma by MoM: alpha = %.4f, p = %.4f; autocorrelations of r (lags 1, 12, 60): %s\n',
            alpha.gamma, p.gamma, paste(round(acf(r, lag.max = 60, plot = FALSE)$acf[c(2, 13, 61)], 3), collapse = ", ")))
print_table(se.df)

# b. lognormal: E(r) = exp(mu.l + s2.l/2) and E(r^2) = exp(2 mu.l + 2 s2.l)
#    so s2.l = log(E(r^2)/E(r)^2) = log(1 + sigma^2/mu^2) and mu.l = log(E(r)) - s2.l/2
s2.lognormal = log(1 + sigma2.hat/mu.hat^2)
mu.lognormal = log(mu.hat) - s2.lognormal/2
#    for comparison: the mean and variance of log(r), which are the MLEs of the lognormal (Lab 5)
mu.log.r = mean(log(r))
s2.log.r = mean((log(r) - mu.log.r)^2)
cat(sprintf(' Lognormal by MoM: mu = %.4f, sigma^2 = %.4f; mean and variance of log(r): %.4f, %.4f\n',
            mu.lognormal, s2.lognormal, mu.log.r, s2.log.r))
cat(sprintf(' Implied means: MoM %.4f (= mean(r)), from log(r) %.4f; implied medians: MoM %.4f, from log(r) %.4f, sample %.4f\n',
            exp(mu.lognormal + s2.lognormal/2), exp(mu.log.r + s2.log.r/2), exp(mu.lognormal), exp(mu.log.r), median(r)))
# third moment as a check of the fit: skewness of the data, the gamma (2/sqrt(p)) and the lognormal
skew.data      = mean((r - mu.hat)^3)/sigma2.hat^1.5
skew.gamma     = 2/sqrt(p.gamma)
skew.lognormal = (exp(s2.lognormal) + 2)*sqrt(exp(s2.lognormal) - 1)
cat(sprintf(' Skewness: data %.4f, gamma %.4f, lognormal %.4f\n', skew.data, skew.gamma, skew.lognormal))
# share of months with r below 2% under each fitted distribution
cat(sprintf(' Pr(r < 2): data %.4f, gamma %.4f, lognormal %.4f\n', mean(r < 2),
            pgamma(2, shape = p.gamma, rate = alpha.gamma), plnorm(2, mu.lognormal, sqrt(s2.lognormal))))

# d. the stationary distribution of the CIR model dr = kappa (mu.r - r) dt + sigma sqrt(r) dW is a gamma
#    distribution with shape p = 2 kappa mu.r/sigma^2 and rate alpha = 2 kappa/sigma^2.
#    The discrete CIR model of Question 2 b. has kappa = -beta, mu.r = -alpha/beta and sigma = sigma.CIR (monthly)
kappa.cir = -beta.hat
mu.r.cir  = -alpha.hat/beta.hat
sigma.cir = vasicek.cir.df$sigma[2]
p.cir     = 2*kappa.cir*mu.r.cir/sigma.cir^2
alpha.cir = 2*kappa.cir/sigma.cir^2
cat(sprintf(' CIR stationary gamma: p = %.4f, alpha = %.4f, mean = %.4f, sd = %.4f (data: mean %.4f, sd %.4f)\n',
            p.cir, alpha.cir, p.cir/alpha.cir, sqrt(p.cir)/alpha.cir, mu.hat, sqrt(sigma2.hat)))

# %% c. PLOT THE HISTOGRAM WITH THE FITTED GAMMA, LOGNORMAL AND CIR DENSITIES
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(4, 4.5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
r.grid = seq(0.01, 17, length.out = 600)
hist( r, breaks = seq(0, 17, by = 0.5), freq = FALSE, col = "steelblue", border = "white", main = "", ylim = c(0, 0.25),
      xlab = "3 month interest rate (% p.a.)", ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(r.grid, dgamma(r.grid, shape = p.gamma, rate = alpha.gamma), col = "red3", lwd = 2)
lines(r.grid, dlnorm(r.grid, mu.lognormal, sqrt(s2.lognormal)), col = "green4", lwd = 2, lty = 2)
lines(r.grid, dgamma(r.grid, shape = p.cir, rate = alpha.cir), col = "orange", lwd = 2, lty = 4)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("Gamma (MoM)", "Lognormal (MoM)", "CIR stationary density"),
       col = c("red3", "green4", "orange"), lty = c(1, 2, 4), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
