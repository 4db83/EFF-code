# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,AER)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = c("Lab-05-MLE-Bernoulli Likelihood.pdf",
              "Lab-05-MLE-Exponential Fit.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# QUESTION 1: Bernoulli likelihood and log-likelihood
L_bern     = function(theta, x) theta^sum(x)*(1 - theta)^(length(x) - sum(x))
lnL_bern   = function(theta, x) sum(x)*log(theta) + (length(x) - sum(x))*log(1 - theta)
negLL_bern = function(theta, x) -lnL_bern(theta, x)
# QUESTION 2: exponential log-likelihood with mean theta
lnL_exp = function(theta, y) -length(y)*log(theta) - sum(y)/theta
# QUESTION 3: Poisson log-likelihood, and the MLE and the MoM estimator from E(X^2) = lambda (1 + lambda)
lnL_pois = function(lambda, x) sum(x)*log(lambda) - length(x)*lambda - sum(lgamma(x + 1))
poisson_estimators = function(x){
  lambda.mle = mean(x)
  lambda.mom = 0.5*sqrt(4*mean(x^2) + 1) - 0.5
  c(lambda.mle, lambda.mom)
}
# QUESTION 4: negative log-likelihood of an i.i.d. N(mu, sigma^2) sample, par = (mu, sigma^2)
negLL_norm = function(par, y){
  mu = par[1]
  s2 = par[2]
  0.5*length(y)*log(2*pi*s2) + sum((y - mu)^2)/(2*s2)
}
# QUESTION 5: negative log-likelihood of the normal regression model
# par = (beta, log sigma^2) for the optimiser, and par = (beta, sigma^2) for the Hessian
negLL_reg_log = function(par, y, X){
  K  = ncol(X)
  b  = par[1:K]
  s2 = exp(par[K+1])
  0.5*length(y)*log(2*pi*s2) + sum((y - X %*% b)^2)/(2*s2)
}
negLL_reg = function(par, y, X){
  K  = ncol(X)
  b  = par[1:K]
  s2 = par[K+1]
  0.5*length(y)*log(2*pi*s2) + sum((y - X %*% b)^2)/(2*s2)
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 1) BERNOULLI: THE WHEEL OF FORTUNE AND UP-DAYS ON THE NYSE
# wheel of fortune example from Lecture 9
# a. likelihood and log-likelihood of the sequence {W, W, L} = {1, 1, 0}
x.wwl = c(1, 1, 0)
cat(sprintf(' L(1/4) = %.6f (= 3/64), L(3/4) = %.6f (= 9/64)\n', L_bern(1/4, x.wwl), L_bern(3/4, x.wwl)))
opt.L   = optimize(L_bern,   c(0, 1), x = x.wwl, maximum = TRUE, tol = 1e-10)
opt.lnL = optimize(lnL_bern, c(0.001, 0.999), x = x.wwl, maximum = TRUE, tol = 1e-10)
cat(sprintf(' argmax L = %.6f, argmax lnL = %.6f, sample mean = %.6f\n', opt.L$maximum, opt.lnL$maximum, mean(x.wwl)))

# %% PLOT THE LIKELIHOOD AND THE LOG-LIKELIHOOD
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 9, height = 4 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 1, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
theta.grid = seq(0.01, 0.99, length.out = 500)
plot( theta.grid, L_bern(theta.grid, x.wwl), type = "l", lwd = 2, col = "steelblue", xlab = expression(theta),
      ylab = expression(L(theta*"|"*x)), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(v = 2/3, col = "red3", lty = 2)
points(c(1/4, 3/4), L_bern(c(1/4, 3/4), x.wwl), pch = 16, col = "green4", cex = 1.3)
set_axis_ticks(tcl = .15, cex = fnt-.1)
plot( theta.grid, lnL_bern(theta.grid, x.wwl), type = "l", lwd = 2, col = "steelblue", xlab = expression(theta),
      ylab = expression(ln*L(theta*"|"*x)), cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(v = 2/3, col = "red3", lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 1) CONTINUED: UP-DAYS ON THE NYSE
# c. up-days of the NYSE (Lab 1): numerical MLE and standard errors
data("NYSESW", package = "AER")
D = as.numeric(diff(log(as.numeric(NYSESW))) > 0)
n = length(D)
opt = optim(0.5, negLL_bern, x = D, method = "L-BFGS-B", lower = 1e-6, upper = 1 - 1e-6, hessian = TRUE)
theta.hat   = opt$par
se.hessian  = sqrt(1/opt$hessian)
se.formula  = sqrt(mean(D)*(1 - mean(D))/n)
cat(sprintf(' NYSE up-days: theta.MLE = %.6f (mean = %.6f), SE from Hessian = %.6f, SE formula sqrt(theta(1-theta)/n) = %.6f\n',
            theta.hat, mean(D), se.hessian, se.formula))
t.half = (mean(D) - 0.5)/se.formula
cat(sprintf(' t-statistic for H0: theta = 0.5: %.4f\n', t.half))

# %% QUESTION 2) EXPONENTIAL: STRIKE DURATIONS
# AER data StrikeDuration
data("StrikeDuration", package = "AER")
y = StrikeDuration$duration
n = length(y)
opt.exp = optim(mean(y)/2, function(theta) -lnL_exp(theta, y), method = "L-BFGS-B", lower = 1e-6, upper = 1e4, hessian = TRUE)
cat(sprintf(' theta.MLE: numerical = %.4f, analytical = %.4f\n', opt.exp$par, mean(y)))
cat(sprintf(' SE: Hessian = %.4f, I(theta)^-1/n = theta^2/n -> %.4f\n', sqrt(1/opt.exp$hessian), mean(y)/sqrt(n)))
cat(sprintf(' mean = %.4f, sd = %.4f (an exponential RV has mean = sd)\n', mean(y), sd(y)))
cat(sprintf(' Maximised log-likelihood = %.4f\n', -opt.exp$value))
# c. the gamma distribution nests the exponential (shape = 1): MoM and ML estimates of the shape
shape.mom = mean(y)^2/mean((y - mean(y))^2)
opt.gamma = optim(c(0, log(1/mean(y))), function(p) -sum(dgamma(y, shape = exp(p[1]), rate = exp(p[2]), log = TRUE)), method = "BFGS")
cat(sprintf(' Gamma shape: MoM = %.4f, MLE = %.4f\n', shape.mom, exp(opt.gamma$par[1])))
# d. Monte Carlo check of the asymptotic distribution with theta = 42.68 and n = 62
set.seed(9)
theta.mc = replicate(1e4, mean(rexp(n, rate = 1/mean(y))))
skew.mc  = mean((theta.mc - mean(theta.mc))^3)/sd(theta.mc)^3
cat(sprintf(' Monte Carlo: mean = %.4f, sd = %.4f vs theta/sqrt(n) = %.4f, skewness = %.4f\n',
            mean(theta.mc), sd(theta.mc), mean(y)/sqrt(n), skew.mc))

# %% PLOT THE HISTOGRAM WITH THE FITTED EXPONENTIAL DENSITY
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
hist( y, breaks = seq(0, 220, by = 10), freq = FALSE, col = "steelblue", border = "white", main = "",
      xlab = "Strike duration (days)", ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
y.grid = seq(0, 220, length.out = 400)
lines(y.grid, dexp(y.grid, rate = 1/opt.exp$par), col = "red3", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("Strike durations", expression("Exponential density with " * hat(theta)[MLE] == 42.68)),
       col = c("steelblue", "red3"), lwd = c(8, 2), bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 3) POISSON: MLE VERSUS MoM
# Lectures 6 and 9
lambda = 2
n      = 100
N.reps = 1e4
set.seed(10)
estimates = t(replicate(N.reps, poisson_estimators(rpois(n, lambda))))
poisson.df = data.frame( Estimator = c("MLE = mean", "MoM from E(X^2)"),
                         Mean      = colMeans(estimates),
                         Var.x.n   = n*apply(estimates, 2, var),
                         Theory    = c(lambda, lambda + 2*lambda^2/(1 + 2*lambda)^2) )
print(format(poisson.df, digits = 4, nsmall = 4), row.names = FALSE)
cat(sprintf(' Cramer-Rao bound for n Var: 1/I(lambda) = lambda = %.1f\n', lambda))
# b. real data: DoctorVisits
data("DoctorVisits", package = "AER")
x = DoctorVisits$visits
opt.pois = optimize(lnL_pois, c(0.01, 5), x = x, maximum = TRUE, tol = 1e-10)
se.information = sqrt(mean(x)/length(x))                  # inverse Fisher information: lambda/n
se.sandwich    = sqrt(mean((x - mean(x))^2)/length(x))    # MoM (sandwich) standard error of the mean: sigma^2/n
cat(sprintf(' DoctorVisits: lambda.MLE = %.6f, mean = %.6f, variance = %.4f\n', opt.pois$maximum, mean(x), var(x)))
cat(sprintf(' SE from the information = %.6f, sandwich SE = %.6f\n', se.information, se.sandwich))

# %% QUESTION 4) NORMAL: LOG HOURLY EARNINGS
# CPS2015 data
CPS = read_xlsx("./data/CPS2015.xlsx")
y = log(CPS$ahe)
n = length(y)
opt.norm = optim(c(mean(y) + 0.1, var(y)*1.2), negLL_norm, y = y, method = "L-BFGS-B", lower = c(-Inf, 1e-8), hessian = TRUE)
mu.hat = mean(y)
s2.hat = mean((y - mu.hat)^2)
norm.df = data.frame( Parameter  = c("mu", "sigma^2"),
                      Numerical  = opt.norm$par,
                      Analytical = c(mu.hat, s2.hat),
                      SE.Hessian = sqrt(diag(solve(opt.norm$hessian))),
                      SE.Formula = c(sqrt(s2.hat/n), sqrt(2*s2.hat^2/n)) )
print(format(norm.df, digits = 6, nsmall = 6), row.names = FALSE)
cat(sprintf(' Invariance: sigma.MLE = sqrt(sigma2.MLE) = %.6f;  unbiased s^2 = %.6f\n', sqrt(s2.hat), var(y)))

# %% QUESTION 5) THE NORMAL REGRESSION MODEL BY MAXIMUM LIKELIHOOD
# California test scores
CASchools = read_xlsx("./data/caschool.xlsx")
y = CASchools$testscr
X = cbind(1, CASchools$str, CASchools$el_pct)
n = length(y)
K = ncol(X)
opt.reg = optim(c(mean(y), 0, 0, log(var(y))), negLL_reg_log, y = y, X = X, method = "BFGS",
                control = list(maxit = 5000, reltol = 1e-14))
theta.hat = c(opt.reg$par[1:K], exp(opt.reg$par[K+1]))
H = optimHess(theta.hat, negLL_reg, y = y, X = X)        # Hessian of -lnL at the MLE, in terms of (beta, sigma^2)
lm.reg = lm(testscr ~ str + el_pct, data = CASchools)
prnt.reg = print_lm_results(lm.reg, Hide = 1)
s2.mle = sum(resid(lm.reg)^2)/n
reg.df = data.frame( Parameter       = c("Constant", "str", "el_pct", "sigma^2"),
                     MLE             = theta.hat,
                     OLS             = c(coef(lm.reg), sum(resid(lm.reg)^2)/(n - K)),
                     SE.MLE.Hessian  = sqrt(diag(solve(H))),
                     SE.sigma2MLE.XX = c(sqrt(diag(s2.mle*solve(crossprod(X)))), sqrt(2*theta.hat[K+1]^2/n)),
                     SE.lm.homo      = c(sqrt(diag(vcov(lm.reg))), NA) )
print(format(reg.df, digits = 6, nsmall = 6), row.names = FALSE)
cat(sprintf(' Maximised log-likelihood: optim = %.6f, print_lm_results = %.6f, logLik(lm) = %.6f\n',
            -opt.reg$value, prnt.reg$LogLike, as.numeric(logLik(lm.reg))))

#EOF
