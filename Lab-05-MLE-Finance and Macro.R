# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = c("Lab-05-MLE-Fat Tails.pdf",
              "Lab-05-MLE-Interest Rates.pdf",
              "Lab-05-MLE-Durations.pdf",
              "Lab-05-MLE-Recession Probit.pdf",
              "Lab-05-MLE-GARCH Volatility.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# QUESTION 6: log-likelihood of the location-scale t distribution, r = mu + s e with e ~ t(nu); par = (mu, s, nu)
lnL_t = function(par, r){
  mu = par[1]
  s  = par[2]
  nu = par[3]
  if (s < 1e-8 || nu < 1e-8 || !is.finite(nu)) return(-1e10)   # keep the optimiser away from s = 0 and nu = 0
  sum( dt((r - mu)/s, df = nu, log = TRUE) - log(s) )
}
# optimise over (mu, log s, log nu) so that s and nu stay positive, then get the Hessian in (mu, s, nu)
fit_t = function(r){
  start = c(mean(r), log(sd(r)), log(5))
  opt   = optim(start, function(p) -lnL_t(c(p[1], exp(p[2:3])), r), method = "BFGS")
  par.hat = c(opt$par[1], exp(opt$par[2:3]))
  H = optimHess(par.hat, function(p) -lnL_t(p, r))
  list(par = par.hat, se = sqrt(diag(solve(H))), lnL = -opt$value)
}
# normal and t fits, Value-at-Risk and hit rates for one return series
fat_tails_mle = function(r){
  T.r       = length(r)
  mu.hat    = mean(r)
  sigma.hat = sqrt(mean((r - mu.hat)^2))                         # normal MLEs (note the divisor T)
  lnL.normal = sum(dnorm(r, mu.hat, sigma.hat, log = TRUE))
  ft = fit_t(r)
  VaR.normal = mu.hat + sigma.hat*qnorm(0.01)
  VaR.t      = ft$par[1] + ft$par[2]*qt(0.01, ft$par[3])
  c(T = T.r, mean = mu.hat, sigma = sigma.hat, kurtosis = mean((r - mu.hat)^4)/sigma.hat^4,
    mu.t = ft$par[1], s.t = ft$par[2], nu = ft$par[3], se.nu = ft$se[3],
    lnL.normal = lnL.normal, lnL.t = ft$lnL, VaR.emp = unname(quantile(r, 0.01)), VaR.normal = VaR.normal, VaR.t = VaR.t,
    hit.normal = mean(r < VaR.normal), hit.t = mean(r < VaR.t))
}
# QUESTION 7: gamma log-likelihood with rate alpha and shape p; par = (alpha, p)
lnL_gamma = function(par, y) sum( dgamma(y, shape = par[2], rate = par[1], log = TRUE) )
# QUESTION 8: exponential and Weibull log-likelihoods of durations u; par = (alpha, beta)
lnL_exponential = function(alpha, u) length(u)*log(alpha) - alpha*sum(u)
lnL_weibull     = function(par, u) sum( log(par[1]) + log(par[2]) + (par[2] - 1)*log(u) - par[1]*u^par[2] )
# QUESTION 9: geometric log-likelihood
lnL_geometric = function(theta, y) length(y)*log(theta) + sum(y)*log(1 - theta)
# QUESTION 10: probit log-likelihood, Pr(y = 1|x) = Phi(b0 + b1 x)
lnL_probit = function(b, y, x){
  p = pnorm(b[1] + b[2]*x)
  sum( y*log(p) + (1 - y)*log(1 - p) )
}
# QUESTION 11: GARCH(1,1) conditional variances h_t = omega + alpha e_{t-1}^2 + beta h_{t-1}, started at the sample variance
garch_h = function(par, e){
  h = numeric(length(e))
  h[1] = var(e)
  for (t in 2:length(e)) h[t] = par[1] + par[2]*e[t - 1]^2 + par[3]*h[t - 1]
  h
}
lnL_garch = function(par, e){
  if (par[1] <= 0 || par[2] < 0 || par[3] < 0 || par[2] + par[3] >= 1) return(-1e10)
  h = garch_h(par, e)
  -0.5*sum( log(2*pi) + log(h) + e^2/h )
}

# %% SCRIPT STARTS HERE: one cell for each question
# the hurn_*.csv files are created from the [HMPY2021] book resources by 'Data-Hurn et al workfiles to csv.R'
# QUESTION 6) FAT TAILS AND VALUE AT RISK: NORMAL VERSUS STUDENT t
# [HMPY2021] Exercise 10.2
idx = read.csv("./data/hurn_stockindices.csv")
index.names = c(spx = "S&P 500", djx = "Dow Jones", hkx = "Hang Seng", nkx = "Nikkei", dax = "DAX", ukx = "FTSE 100")
tails.table = NULL
for (ix in names(index.names)){
  r = 100*diff(log(idx[[ix]]))                                   # percentage log returns
  tails.table = rbind(tails.table, fat_tails_mle(r))
}
rownames(tails.table) = names(index.names)
print(round(tails.table, 4))

# %% PLOT S&P 500 RETURNS WITH THE FITTED NORMAL AND t DENSITIES
# levels and log scale
r.spx  = 100*diff(log(idx$spx))
ft.spx = fit_t(r.spx)
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 1, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
if (print2pdf == 1) {use_dark_plots("white") }
r.grid  = seq(-10, 12, length.out = 1000)
dens.normal = dnorm(r.grid, tails.table["spx", "mean"], tails.table["spx", "sigma"])
dens.t      = dt((r.grid - ft.spx$par[1])/ft.spx$par[2], df = ft.spx$par[3])/ft.spx$par[2]
hist( r.spx, breaks = seq(-10, 12, by = .25), freq = FALSE, col = "steelblue", border = "white", main = "", xlim = c(-6, 6),
      xlab = "S&P 500 daily return (%)", ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(r.grid, dens.normal, col = "red3", lwd = 2)
lines(r.grid, dens.t, col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("Normal", "Student t"), col = c("red3", "green4"), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
h.spx = hist(r.spx, breaks = seq(-10, 12, by = .5), plot = FALSE)
positive = h.spx$density > 0
plot( h.spx$mids[positive], log(h.spx$density[positive]), pch = 16, col = "steelblue", ylim = c(-12, 0),
      xlab = "S&P 500 daily return (%)", ylab = "Log density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(r.grid, log(dens.normal), col = "red3", lwd = 2)
lines(r.grid, log(dens.t), col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 7) INTEREST RATES: LOGNORMAL OR GAMMA?
# [HMPY2021] Exercise 10.3
euro = read.csv("./data/hurn_eurodollar.csv")
y = na.omit(euro$rate)
T = length(y)
cat(sprintf(' T = %d daily Eurodollar rates (%s to %s), mean = %.4f, sd = %.4f\n', T, euro$date[1], tail(euro$date, 1), mean(y), sd(y)))
# a. lognormal: closed form MLEs from the logs
mu.lognormal = mean(log(y))
s2.lognormal = mean((log(y) - mu.lognormal)^2)
lnL.lognormal = sum(dlnorm(y, mu.lognormal, sqrt(s2.lognormal), log = TRUE))
cat(sprintf(' Lognormal: mu = %.4f, sigma2 = %.4f, lnL = %.2f, implied mean exp(mu + sigma2/2) = %.4f, median exp(mu) = %.4f\n',
            mu.lognormal, s2.lognormal, lnL.lognormal, exp(mu.lognormal + s2.lognormal/2), exp(mu.lognormal)))
# b. gamma: f(y) = alpha^p/Gamma(p) y^(p-1) exp(-alpha y), numerical MLE over (log alpha, log p)
start.gamma = log(c(mean(y)/var(y), mean(y)^2/var(y)))           # MoM starting values
opt.gamma   = optim(start.gamma, function(p) -lnL_gamma(exp(p), y), method = "BFGS")
par.gamma   = exp(opt.gamma$par)
se.gamma    = sqrt(diag(solve(optimHess(par.gamma, function(p) -lnL_gamma(p, y)))))
cat(sprintf(' Gamma: alpha = %.4f (%.4f), p = %.4f (%.4f), lnL = %.2f, implied mean p/alpha = %.4f\n',
            par.gamma[1], se.gamma[1], par.gamma[2], se.gamma[2], -opt.gamma$value, par.gamma[2]/par.gamma[1]))
cat(sprintf(' First order autocorrelation of the rates = %.4f\n', cor(y[-1], y[-T])))

# %% PLOT THE HISTOGRAM WITH THE FITTED LOGNORMAL AND GAMMA DENSITIES
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
hist( y, breaks = seq(0, 24.5, by = .5), freq = FALSE, col = "steelblue", border = "white", main = "",
      xlab = "Eurodollar rate (%)", ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
y.grid = seq(0.01, 24, length.out = 600)
lines(y.grid, dlnorm(y.grid, mu.lognormal, sqrt(s2.lognormal)), col = "red3", lwd = 2)
lines(y.grid, dgamma(y.grid, shape = par.gamma[2], rate = par.gamma[1]), col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("Lognormal", "Gamma"), col = c("red3", "green4"), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 8) DURATIONS BETWEEN TRADES: EXPONENTIAL VERSUS WEIBULL
# [HMPY2021] Sections 10.3.1 and 10.7
amr = read.csv("./data/hurn_amr.csv")
u = amr$duration[amr$trade == 1]
T = length(u)
cat(sprintf(' T = %d trades, mean duration = %.4f seconds, sd = %.4f\n', T, mean(u), sd(u)))
# a. exponential f(u) = alpha exp(-alpha u): MLE alpha = 1/mean(u), Hessian = -T/alpha^2
alpha.hat = 1/mean(u)
lnL.exp.hat = lnL_exponential(alpha.hat, u)
cat(sprintf(' Exponential: alpha = %.4f, Hessian = %.2f, SE = %.6f, lnL = %.4f (average %.4f)\n',
            alpha.hat, -T/alpha.hat^2, alpha.hat/sqrt(T), lnL.exp.hat, lnL.exp.hat/T))
# b. Weibull f(u) = alpha beta u^(beta-1) exp(-alpha u^beta), numerical MLE over (log alpha, log beta)
opt.weibull = optim(log(c(alpha.hat, 1)), function(p) -lnL_weibull(exp(p), u), method = "BFGS")
par.weibull = exp(opt.weibull$par)
V.weibull   = solve(optimHess(par.weibull, function(p) -lnL_weibull(p, u)))
lnL.weibull.hat = -opt.weibull$value
cat(sprintf(' Weibull: alpha = %.4f (%.4f), beta = %.4f (%.4f), lnL = %.4f (average %.4f)\n',
            par.weibull[1], sqrt(V.weibull[1, 1]), par.weibull[2], sqrt(V.weibull[2, 2]), lnL.weibull.hat, lnL.weibull.hat/T))
# c. tests of H0: beta = 1 (exponential)
LR   = 2*(lnL.weibull.hat - lnL.exp.hat)
Wald = (par.weibull[2] - 1)^2/V.weibull[2, 2]
cat(sprintf(' LR = %.4f (p = %.6f), Wald = %.4f (p = %.6f), 5%% critical value = %.4f\n',
            LR, 1 - pchisq(LR, 1), Wald, 1 - pchisq(Wald, 1), qchisq(.95, 1)))

# %% PLOT THE EXPONENTIAL LOG-LIKELIHOOD AND THE FITTED DENSITIES
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[3]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
alpha.grid = seq(0.01, 1, length.out = 500)
plot( alpha.grid, lnL_exponential(alpha.grid, u)/1e3, type = "l", lwd = 2, col = "steelblue", xlab = expression(alpha),
      ylab = expression(ln*L(alpha) * " (thousands)"), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(v = alpha.hat, col = "red3", lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
hist( u, breaks = seq(0, 100, by = 2), freq = FALSE, col = "steelblue", border = "white", main = "", xlim = c(0, 40),
      xlab = "Duration between trades (seconds)", ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
u.grid = seq(0.2, 40, length.out = 400)
dens.weibull = par.weibull[1]*par.weibull[2]*u.grid^(par.weibull[2] - 1)*exp(-par.weibull[1]*u.grid^par.weibull[2])
lines(u.grid, dexp(u.grid, rate = alpha.hat), col = "red3", lwd = 2)
lines(u.grid, dens.weibull, col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("Exponential", "Weibull"), col = c("red3", "green4"), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 9) GLOBAL FINANCIAL CRISES: GEOMETRIC VERSUS POISSON
# [HMPY2021] Exercise 10.7
crisis = read.csv("./data/hurn_crisis.csv")
for (v in c("banking", "currency")) {
  y = crisis[[v]]
  T = length(y)
  # a. geometric MLE theta = 1/(1 + mean(y)) and its standard error from the Fisher information
  theta.hat = 1/(1 + mean(y))
  se.theta  = sqrt(theta.hat^2*(1 - theta.hat)/T)
  cat(sprintf('\n %s crises, %d years: mean = %.4f, variance = %.4f\n', v, T, mean(y), var(y)))
  cat(sprintf(' Geometric: theta = %.4f (SE %.4f), implied variance (1-theta)/theta^2 = %.4f, lnL = %.4f\n',
              theta.hat, se.theta, (1 - theta.hat)/theta.hat^2, lnL_geometric(theta.hat, y)))
  # b. Poisson MLE lambda = mean(y)
  cat(sprintf(' Poisson:   lambda = %.4f, implied variance = %.4f, lnL = %.4f\n',
              mean(y), mean(y), sum(dpois(y, mean(y), log = TRUE))))
  # observed and fitted frequencies of 0, 1, 2, 3, 4 and 5 or more crises
  observed   = c(sapply(0:4, function(j) sum(y == j)), sum(y >= 5))
  fitted.geo = T*c(dgeom(0:4, theta.hat), 1 - pgeom(4, theta.hat))
  fitted.poi = T*c(dpois(0:4, mean(y)), 1 - ppois(4, mean(y)))
  print(round(rbind(observed = observed, geometric = fitted.geo, poisson = fitted.poi), 4))
  # c. LR test of a constant theta before and after 1945
  y.pre  = y[crisis$year < 1945]
  y.post = y[crisis$year >= 1945]
  theta.pre  = 1/(1 + mean(y.pre))
  theta.post = 1/(1 + mean(y.post))
  lnL.unrestricted = lnL_geometric(theta.pre, y.pre) + lnL_geometric(theta.post, y.post)
  LR.crisis = 2*(lnL.unrestricted - lnL_geometric(theta.hat, y))
  cat(sprintf(' theta before 1945 = %.4f, from 1945 = %.4f, LR = %.4f (p = %.6f)\n',
              theta.pre, theta.post, LR.crisis, 1 - pchisq(LR.crisis, 1)))
}

# %% QUESTION 10) PREDICTING RECESSIONS WITH THE YIELD CURVE: A PROBIT MODEL BY MLE
macro.m = as.data.frame(read_xlsx("./data/us_macro_monthly.xlsx"))
macro.m$qtr = rep(seq_len(nrow(macro.m)/3), each = 3)
rates.q = aggregate(cbind(GS10, TB3MS) ~ qtr, data = macro.m, FUN = mean)
rates.q$year.q = 1955 + (rates.q$qtr - 1)/4
rates.q$spread = rates.q$GS10 - rates.q$TB3MS
# NBER business cycle peaks and troughs (quarterly); a recession runs from the quarter after the peak to the trough
peak   = c(1957.50, 1960.25, 1969.75, 1973.75, 1980.00, 1981.50, 1990.50, 2001.00, 2007.75)
trough = c(1958.25, 1961.00, 1970.75, 1975.00, 1980.50, 1982.75, 1991.00, 2001.75, 2009.25)
rates.q$recession = 0
for (j in seq_along(peak)) rates.q$recession[rates.q$year.q > peak[j] & rates.q$year.q <= trough[j]] = 1
# recession in quarter t explained by the spread four quarters earlier
h.ahead = 4
n.q = nrow(rates.q)
d.rec = data.frame( year.q    = rates.q$year.q[(h.ahead + 1):n.q],
                    recession = rates.q$recession[(h.ahead + 1):n.q],
                    spread    = rates.q$spread[1:(n.q - h.ahead)] )
cat(sprintf(' T = %d quarters (%.2f to %.2f), %d recession quarters, mean = %.4f\n',
            nrow(d.rec), d.rec$year.q[1], tail(d.rec$year.q, 1), sum(d.rec$recession), mean(d.rec$recession)))
# a. probit log-likelihood: Bernoulli with probability Phi(b0 + b1 spread)
opt.probit = optim(c(0, 0), function(b) -lnL_probit(b, d.rec$recession, d.rec$spread), method = "BFGS", hessian = TRUE)
b.probit   = opt.probit$par
se.probit  = sqrt(diag(solve(opt.probit$hessian)))
lnL.probit = -opt.probit$value
cat(sprintf(' Probit MLE: b0 = %.4f (%.4f), b1 = %.4f (%.4f), lnL = %.4f\n', b.probit[1], se.probit[1], b.probit[2], se.probit[2], lnL.probit))
print(summary(glm(recession ~ spread, family = binomial(link = "probit"), data = d.rec))$coefficients)
# b. LR test of H0: b1 = 0; the restricted model is the Bernoulli model with theta = mean(recession)
theta.0 = mean(d.rec$recession)
lnL.0   = sum( d.rec$recession*log(theta.0) + (1 - d.rec$recession)*log(1 - theta.0) )
LR.probit = 2*(lnL.probit - lnL.0)
cat(sprintf(' Restricted lnL = %.4f, LR = %.4f (p = %.8f), McFadden R2 = %.4f\n', lnL.0, LR.probit,
            1 - pchisq(LR.probit, 1), 1 - lnL.probit/lnL.0))
# c. predicted probabilities and the marginal effect at the mean spread
for (s in c(-1, 0, 1, 2, 3)) {
  cat(sprintf(' spread = %4.1f: Pr(recession in 4 quarters) = %.4f\n', s, pnorm(b.probit[1] + b.probit[2]*s)))
}
spread.bar = mean(d.rec$spread)
marginal.effect = dnorm(b.probit[1] + b.probit[2]*spread.bar)*b.probit[2]
cat(sprintf(' Marginal effect at the mean spread (%.4f) = %.4f\n', spread.bar, marginal.effect))
spread.last = tail(rates.q$spread, 1)
cat(sprintf(' Spread in 2017:Q4 = %.4f, predicted probability of a recession in 2018:Q4 = %.4f\n',
            spread.last, pnorm(b.probit[1] + b.probit[2]*spread.last)))

# %% PLOT THE PREDICTED RECESSION PROBABILITY WITH THE NBER RECESSIONS SHADED
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[4]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(3, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
prob.recession = pnorm(b.probit[1] + b.probit[2]*d.rec$spread)
plot( d.rec$year.q, prob.recession, type = "n", ylim = c(0, 1), xlab = "",
      ylab = "Pr(recession)", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
for (j in seq_along(peak)) rect(peak[j], 0, trough[j], 1, col = "grey85", border = NA)
lines(d.rec$year.q, prob.recession, col = "steelblue", lwd = 2)
box()
set_axis_ticks(tcl = .15, cex = fnt-.1)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 11) EXTENSION: VOLATILITY CLUSTERING AND THE GARCH(1,1) MODEL
# [HMPY2021] Chapter 13
vix = read.csv("./data/hurn_vix.csv")
r = 100*vix$ret
e = r - mean(r)
T = length(e)
opt.garch = optim(c(0.05, 0.08, 0.90), function(p) -lnL_garch(p, e), method = "Nelder-Mead",
                  control = list(maxit = 5000, reltol = 1e-12))
par.garch = opt.garch$par
se.garch  = sqrt(diag(solve(optimHess(par.garch, function(p) -lnL_garch(p, e)))))
lnL.constant = sum(dnorm(e, 0, sqrt(mean(e^2)), log = TRUE))
persistence  = sum(par.garch[2:3])
cat(sprintf(' GARCH(1,1): omega = %.5f (%.5f), alpha = %.4f (%.4f), beta = %.4f (%.4f)\n',
            par.garch[1], se.garch[1], par.garch[2], se.garch[2], par.garch[3], se.garch[3]))
cat(sprintf(' alpha + beta = %.4f, unconditional variance = %.4f (sample %.4f), half-life = %.1f days\n',
            persistence, par.garch[1]/(1 - persistence), var(e), log(0.5)/log(persistence)))
cat(sprintf(' lnL GARCH = %.2f, lnL constant variance = %.2f\n', -opt.garch$value, lnL.constant))
h.garch = garch_h(par.garch, e)
cat(sprintf(' Correlation of the GARCH volatility with the VIX = %.4f\n', cor(sqrt(252*h.garch), 100*sqrt(252*vix$h))))

# %% PLOT THE GARCH VOLATILITY AND THE VIX
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[5]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(3, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
if (print2pdf == 1) {use_dark_plots("white") }
year.vix = as.numeric(substr(vix$date, 1, 4)) + (as.numeric(format(as.Date(vix$date), "%j")) - 1)/365
vix.pa   = 100*sqrt(252*vix$h)
plot( year.vix, vix.pa, type = "l", col = "grey60", xlab = "", ylab = "Volatility (% p.a.)",
      ylim = c(0, max(vix.pa, sqrt(252*h.garch))), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(year.vix, sqrt(252*h.garch), col = "red3", lwd = 1.5)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topleft", legend = c("VIX", "GARCH(1,1) volatility"), col = c("grey60", "red3"), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
