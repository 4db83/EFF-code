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
pdf_name  = c("Lab-05-MLE-Skewed t Densities.pdf",
              "Lab-05-MLE-Conditional Moments.pdf",
              "Lab-05-MLE-Value at Risk.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# Hansen's (1994) skewed t distribution, standardised to mean 0 and variance 1 (nu > 2, -1 < lambda < 1)
# constants a, b and c (all functions work elementwise, so nu and lambda can vary over time)
sst_abc = function(nu, lambda){
  c = exp(lgamma((nu + 1)/2) - lgamma(nu/2))/sqrt(pi*(nu - 2))
  a = 4*lambda*c*(nu - 2)/(nu - 1)
  b = sqrt(1 + 3*lambda^2 - a^2)
  list(a = a, b = b, c = c)
}
# log density of z
log_sst = function(z, nu, lambda){
  k = sst_abc(nu, lambda)
  w = (k$b*z + k$a)/ifelse(z < -k$a/k$b, 1 - lambda, 1 + lambda)
  log(k$b) + log(k$c) - (nu + 1)/2*log(1 + w^2/(nu - 2))
}
# quantile function: F(z) = (1 - lambda) F_t(u) left of the mode -a/b, with u = sqrt(nu/(nu - 2)) (b z + a)/(1 - lambda)
q_sst = function(p, nu, lambda){
  k = sst_abc(nu, lambda)
  s = sqrt((nu - 2)/nu)
  # both branches are computed for all p (ifelse() picks one), so qt() can warn about arguments outside (0,1)
  q.left  = suppressWarnings( ((1 - lambda)*s*qt(p/(1 - lambda), nu) - k$a)/k$b )
  q.right = suppressWarnings( ((1 + lambda)*s*qt(0.5 + (p - (1 - lambda)/2)/(1 + lambda), nu) - k$a)/k$b )
  ifelse(p < (1 - lambda)/2, q.left, q.right)
}
# skewness (nu > 3) and kurtosis (nu > 4), Jondeau and Rockinger (2003)
sst_moments = function(nu, lambda){
  k  = sst_abc(nu, lambda)
  m2 = 1 + 3*lambda^2
  m3 = 16*k$c*lambda*(1 + lambda^2)*(nu - 2)^2/((nu - 1)*(nu - 3))
  m4 = 3*(nu - 2)*(1 + 10*lambda^2 + 5*lambda^4)/(nu - 4)
  skewness = (m3 - 3*k$a*m2 + 2*k$a^3)/k$b^3
  kurtosis = (m4 - 4*k$a*m3 + 6*k$a^2*m2 - 3*k$a^4)/k$b^4
  cbind(skewness = skewness, kurtosis = kurtosis)
}
# QUESTION 13: sample skewness and kurtosis, and the negative log-likelihood of y ~ SkewT(mu, sigma, nu, lambda)
skewness_sample = function(v) mean((v - mean(v))^3)/mean((v - mean(v))^2)^1.5
kurtosis_sample = function(v) mean((v - mean(v))^4)/mean((v - mean(v))^2)^2
nll_sst = function(mu, sigma, nu, lambda, y){
  # keep the optimiser inside the parameter space (for nu > 1e6 lgamma() loses precision; the t is then normal anyway)
  if (any(!is.finite(nu)) || any(nu > 1e6) || any(abs(lambda) >= 1)) return(1e10)
  value = -sum( log_sst((y - mu)/sigma, nu, lambda) - log(sigma) )
  if (is.finite(value)) value else 1e10
}
# QUESTION 14: mu_t = b0 + b1 x, log sigma_t = g0 + g1 x, nu_t = 2 + exp(d0 + d1 x), lambda_t = tanh(k0 + k1 x)
conditional_parameters = function(theta, x){
  list( mu     = theta[1] + theta[2]*x,
        sigma  = exp(theta[3] + theta[4]*x),
        nu     = 2 + exp(theta[5] + theta[6]*x),
        lambda = tanh(theta[7] + theta[8]*x) )
}
nll_conditional = function(theta, y, x){
  p = conditional_parameters(theta, x)
  nll_sst(p$mu, p$sigma, p$nu, p$lambda, y)
}
# restricted model: the parameters in 'fix' are set to zero, the others are estimated (BFGS, then Nelder-Mead)
restricted_lnL = function(fix, theta.start, y, x){
  free = setdiff(1:8, fix)
  nll_free = function(theta.free){
    theta = rep(0, 8)
    theta[free] = theta.free
    nll_conditional(theta, y, x)
  }
  opt = optim(theta.start[free], nll_free, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12))
  opt = optim(opt$par, nll_free, method = "Nelder-Mead", control = list(maxit = 20000, reltol = 1e-14))
  -opt$value
}
lr_test = function(lnL.unrestricted, lnL.restricted, q){
  LR = 2*(lnL.unrestricted - lnL.restricted)
  sprintf('LR = %7.3f, df = %d, p = %.6f', LR, q, 1 - pchisq(LR, q))
}
# conditional normal model with the same equations for the mean and the volatility
nll_conditional_normal = function(theta, y, x) -sum( dnorm(y, theta[1] + theta[2]*x, exp(theta[3] + theta[4]*x), log = TRUE) )

# %% SCRIPT STARTS HERE: one cell for each question
# the hurn_*.csv files are created from the [HMPY2021] book resources by 'Data-Hurn et al workfiles to csv.R'
# QUESTION 12) THE DENSITY: CHECKS BY NUMERICAL INTEGRATION
for (p in list(c(5, -0.3), c(5, 0.3), c(10, -0.5))) {
  nu.p     = p[1]
  lambda.p = p[2]
  g = function(z) exp(log_sst(z, nu.p, lambda.p))
  moments.p = sapply(1:4, function(j) integrate(function(z) z^j*g(z), -Inf, Inf)$value)
  formulas  = sst_moments(nu.p, lambda.p)
  cat(sprintf(' nu = %4.1f, lambda = %5.2f: integral = %.6f, mean = %.6f, variance = %.6f, skewness = %.4f (formula %.4f), kurtosis = %.4f (formula %.4f)\n',
              nu.p, lambda.p, integrate(g, -Inf, Inf)$value, moments.p[1], moments.p[2], moments.p[3], formulas[1], moments.p[4], formulas[2]))
  k.p  = sst_abc(nu.p, lambda.p)
  mode.p = -k.p$a/k.p$b
  cat(sprintf('   Pr(z < -a/b) = %.4f = (1 - lambda)/2 = %.4f; F(q_sst(0.01)) = %.6f\n',
              integrate(g, -Inf, mode.p)$value, (1 - lambda.p)/2, integrate(g, -Inf, q_sst(0.01, nu.p, lambda.p))$value))
}

# %% PLOT THE DENSITY FOR DIFFERENT VALUES OF lambda AND nu
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 1, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
z.grid = seq(-4, 4, length.out = 801)
plot( z.grid, dnorm(z.grid), type = "l", lwd = 2, col = "grey50", lty = 2, ylim = c(0, 0.62), xlab = "z", ylab = "Density",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(z.grid, exp(log_sst(z.grid, 5, -0.4)), col = "red3", lwd = 2)
lines(z.grid, exp(log_sst(z.grid, 5,  0.0)), col = "steelblue", lwd = 2)
lines(z.grid, exp(log_sst(z.grid, 5,  0.4)), col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c(expression(lambda == -0.4), expression(lambda == 0), expression(lambda == 0.4), "N(0,1)"),
       col = c("red3", "steelblue", "green4", "grey50"), lty = c(1, 1, 1, 2), lwd = 2, bty = "o", bg = "white",
       cex = fnt-.2, title = expression(nu == 5))
plot( z.grid, dnorm(z.grid, log = TRUE), type = "l", lwd = 2, col = "grey50", lty = 2, ylim = c(-12, 0), xlab = "z",
      ylab = "Log density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(z.grid, log_sst(z.grid, 3.5, -0.3), col = "red3", lwd = 2)
lines(z.grid, log_sst(z.grid, 8,   -0.3), col = "steelblue", lwd = 2)
lines(z.grid, log_sst(z.grid, 30,  -0.3), col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("bottom", legend = c(expression(nu == 3.5), expression(nu == 8), expression(nu == 30), "N(0,1)"),
       col = c("red3", "steelblue", "green4", "grey50"), lty = c(1, 1, 1, 2), lwd = 2, bty = "o", bg = "white",
       cex = fnt-.2, title = expression(lambda == -0.3))
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 13) S&P 500 RETURNS: UNCONDITIONAL FITS
vix = read.csv("./data/hurn_vix.csv")
r   = 100*vix$ret                            # daily log return in percent
VIX = 100*sqrt(252*vix$h)                    # VIX in percent per year
n   = length(r)
y   = r[-1]                                  # r_t, t = 2, ..., n
x   = log(VIX[-n]/sqrt(252))                 # x_{t-1} = log of the VIX-implied daily volatility (percent) at the close of day t-1
T   = length(y)
cat(sprintf(' T = %d: returns skewness = %.4f, kurtosis = %.4f; returns/exp(x): skewness = %.4f, kurtosis = %.4f\n',
            T, skewness_sample(y), kurtosis_sample(y), skewness_sample(y/exp(x)), kurtosis_sample(y/exp(x))))
# normal, symmetric t (lambda = 0) and skewed t with constant parameters
# theta = (mu, log sigma, log(nu - 2), atanh(lambda)) keeps sigma > 0, nu > 2 and -1 < lambda < 1
lnL.normal = sum(dnorm(y, mean(y), sqrt(mean((y - mean(y))^2)), log = TRUE))
opt.t    = optim(c(0, 0, log(3)), function(th) nll_sst(th[1], exp(th[2]), 2 + exp(th[3]), 0, y), method = "BFGS")
opt.skew = optim(c(opt.t$par, 0), function(th) nll_sst(th[1], exp(th[2]), 2 + exp(th[3]), tanh(th[4]), y), method = "BFGS")
par.skew = c(mu = opt.skew$par[1], sigma = exp(opt.skew$par[2]), nu = 2 + exp(opt.skew$par[3]), lambda = tanh(opt.skew$par[4]))
H.skew   = optimHess(par.skew, function(p) nll_sst(p[1], p[2], p[3], p[4], y))
print(round(cbind(estimate = par.skew, se = sqrt(diag(solve(H.skew)))), 4))
LR.lambda = 2*(opt.t$value - opt.skew$value)
cat(sprintf(' lnL: normal = %.2f, symmetric t = %.2f (nu = %.3f), skewed t = %.2f; LR(lambda = 0) = %.3f (p = %.6f)\n',
            lnL.normal, -opt.t$value, 2 + exp(opt.t$par[3]), -opt.skew$value, LR.lambda, 1 - pchisq(LR.lambda, 1)))

# %% QUESTION 14) CONDITIONAL SKEWED t: ALL FOUR PARAMETERS ARE FUNCTIONS OF x_{t-1}
theta.start = c(opt.skew$par[1], 0, 0, 1, opt.skew$par[3], 0, opt.skew$par[4], 0)
opt.c = optim(theta.start, nll_conditional, y = y, x = x, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12))
opt.c = optim(opt.c$par, nll_conditional, y = y, x = x, method = "Nelder-Mead", control = list(maxit = 20000, reltol = 1e-14))
opt.c = optim(opt.c$par, nll_conditional, y = y, x = x, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12))
theta.c = opt.c$par
V.c     = solve(optimHess(theta.c, nll_conditional, y = y, x = x))
names(theta.c) = c("b0", "b1", "g0", "g1", "d0", "d1", "k0", "k1")
lnL.c   = -opt.c$value
print(round(cbind(estimate = theta.c, se = sqrt(diag(V.c)), t = theta.c/sqrt(diag(V.c))), 4))
cat(sprintf(' lnL conditional skewed t = %.3f (convergence code %d)\n', lnL.c, opt.c$convergence))
# restricted models, each estimated by setting parameters to zero
cat(' (i)   constant mean, b1 = 0:              ', lr_test(lnL.c, restricted_lnL(2, theta.c, y, x), 1), '\n')
cat(' (ii)  constant shape, d1 = k1 = 0:        ', lr_test(lnL.c, restricted_lnL(c(6, 8), theta.c, y, x), 2), '\n')
cat(' (iii) constant skewness parameter, k1 = 0:', lr_test(lnL.c, restricted_lnL(8, theta.c, y, x), 1), '\n')
cat(' (iv)  symmetric, k0 = k1 = 0:             ', lr_test(lnL.c, restricted_lnL(c(7, 8), theta.c, y, x), 2), '\n')
# conditional normal model with the same mean and volatility equations
opt.cn = optim(c(0, 0, 0, 1), nll_conditional_normal, y = y, x = x, method = "BFGS", control = list(reltol = 1e-12))
cat(sprintf(' lnL conditional normal = %.3f: b0 = %.4f, b1 = %.4f, g0 = %.4f, g1 = %.4f\n',
            -opt.cn$value, opt.cn$par[1], opt.cn$par[2], opt.cn$par[3], opt.cn$par[4]))
# (v) Wald test that the VIX is an unbiased forecast of volatility: sigma_t = VIX_{t-1}/sqrt(252), i.e. g0 = 0 and g1 = 1
R.wald = rbind(c(0, 0, 1, 0, 0, 0, 0, 0),
               c(0, 0, 0, 1, 0, 0, 0, 0))
q.wald = c(0, 1)
d.wald = R.wald %*% theta.c - q.wald
W = as.numeric( t(d.wald) %*% solve(R.wald %*% V.c %*% t(R.wald)) %*% d.wald )
cat(sprintf(' (v)   Wald test of g0 = 0, g1 = 1: W = %.2f (p = %.6f)\n', W, 1 - pchisq(W, 2)))

# %% QUESTION 15) WHAT THE MODEL IMPLIES: CONDITIONAL MOMENTS AND VALUE AT RISK
# as functions of the VIX
VIX.values = c(10, 15, 20, 30, 45, 70)
x.values   = log(VIX.values/sqrt(252))
p.values   = conditional_parameters(theta.c, x.values)
m.values   = sst_moments(p.values$nu, p.values$lambda)
q01.skew   = p.values$mu + p.values$sigma*q_sst(0.01, p.values$nu, p.values$lambda)
q01.normal = opt.cn$par[1] + opt.cn$par[2]*x.values + exp(opt.cn$par[3] + opt.cn$par[4]*x.values)*qnorm(0.01)
moments.table = cbind(VIX = VIX.values, implied.vol = VIX.values/sqrt(252), mean = p.values$mu, sd = p.values$sigma,
                      nu = p.values$nu, lambda = p.values$lambda, m.values, q01.sst = q01.skew, q01.normal = q01.normal)
print(round(moments.table, 4))
cat(sprintf(' VIX percentiles (5, 50, 95, max): %s\n', paste(round(quantile(VIX, c(.05, .5, .95, 1)), 1), collapse = ", ")))
# in-sample VaR hit rates
p.c      = conditional_parameters(theta.c, x)
mu.n     = opt.cn$par[1] + opt.cn$par[2]*x
sd.n     = exp(opt.cn$par[3] + opt.cn$par[4]*x)
for (a in c(0.01, 0.05)) {
  VaR.skew   = p.c$mu + p.c$sigma*q_sst(a, p.c$nu, p.c$lambda)
  VaR.normal = mu.n + sd.n*qnorm(a)
  upper.skew   = p.c$mu + p.c$sigma*q_sst(1 - a, p.c$nu, p.c$lambda)
  upper.normal = mu.n + sd.n*qnorm(1 - a)
  cat(sprintf(' %2.0f%% VaR hit rates: skewed t = %.4f, normal = %.4f; upper %2.0f%% quantile hit rates: skewed t = %.4f, normal = %.4f\n',
              100*a, mean(y < VaR.skew), mean(y < VaR.normal), 100*a, mean(y > upper.skew), mean(y > upper.normal)))
}

# %% PLOT THE CONDITIONAL VOLATILITY, DEGREES OF FREEDOM, SKEWNESS AND KURTOSIS AGAINST THE VIX
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 11, height = 7.5 ) }
op = par( mfrow = c(2, 2), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
VIX.grid = seq(9, 75, length.out = 300)
p.grid   = conditional_parameters(theta.c, log(VIX.grid/sqrt(252)))
m.grid   = sst_moments(p.grid$nu, p.grid$lambda)
plot( VIX.grid, p.grid$sigma, type = "l", lwd = 2, col = "steelblue", xlab = "VIX on the previous day", ylab = "Conditional SD (% per day)",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(VIX.grid, VIX.grid/sqrt(252), col = "grey50", lty = 2, lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topleft", legend = c(expression(sigma[t]), "VIX/sqrt(252)"), col = c("steelblue", "grey50"), lty = c(1, 2), lwd = 2,
       bty = "o", bg = "white", cex = fnt-.2)
plot( VIX.grid, p.grid$nu, type = "l", lwd = 2, col = "steelblue", xlab = "VIX on the previous day", ylab = expression(nu[t]),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
set_axis_ticks(tcl = .15, cex = fnt-.1)
plot( VIX.grid, m.grid[, "skewness"], type = "l", lwd = 2, col = "steelblue", xlab = "VIX on the previous day", ylab = "Conditional skewness",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
set_axis_ticks(tcl = .15, cex = fnt-.1)
plot( VIX.grid, m.grid[, "kurtosis"], type = "l", lwd = 2, col = "steelblue", xlab = "VIX on the previous day", ylab = "Conditional kurtosis",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = 3, col = "grey50", lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
if (print2pdf == 1){ par(op);dev.off() }

# %% PLOT THE RETURNS WITH THE 1% VaR OF THE CONDITIONAL SKEWED t AND NORMAL MODELS
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[3]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(3, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
year.y = as.numeric(substr(vix$date[-1], 1, 4)) + (as.numeric(format(as.Date(vix$date[-1]), "%j")) - 1)/365
VaR.skew   = p.c$mu + p.c$sigma*q_sst(0.01, p.c$nu, p.c$lambda)
VaR.normal = mu.n + sd.n*qnorm(0.01)
plot( year.y, y, type = "h", col = "grey70", ylim = c(-12, 12), xlab = "", ylab = "Daily return (%)",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(year.y, VaR.normal, col = "red3", lwd = 1.2)
lines(year.y, VaR.skew, col = "steelblue", lwd = 1.2)
violations = y < VaR.skew
points(year.y[violations], y[violations], pch = 16, cex = .6, col = "steelblue")
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topleft", legend = c("1% VaR, conditional skewed t", "1% VaR, conditional normal"), col = c("steelblue", "red3"), lwd = 2,
       bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
