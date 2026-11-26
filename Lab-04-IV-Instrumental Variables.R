# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,sandwich,lmtest,AER,wooldridge)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = "Lab-04-IV-Weak Instruments.pdf"
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# simple IV estimator for one regressor and one instrument (Lecture 7): Cov(Z,Y)/Cov(Z,X)
iv_simple = function(y, x, z) sum( (z - mean(z))*(y - mean(y)) )/sum( (z - mean(z))*(x - mean(x)) )
# simple OLS slope: Cov(X,Y)/Var(X)
ols_simple = function(y, x) sum( (x - mean(x))*(y - mean(y)) )/sum( (x - mean(x))^2 )
# QUESTION 2: simulate n equilibrium prices and quantities
# supply: Q = b.0 + b.1 P + u.s ;  demand: Q = g.0 + g.1 P + g.2 Z + u.d
sim_market = function(n, s.s = 1, s.d = 1, g.2 = 1){
  Z   = rnorm(n)
  u.s = rnorm(n, 0, s.s)
  u.d = rnorm(n, 0, s.d)
  P   = (g.0 - b.0 + g.2*Z + u.d - u.s)/(b.1 - g.1)       # equilibrium price
  Q   = b.0 + b.1*P + u.s                                 # equilibrium quantity
  data.frame(P = P, Q = Q, Z = Z)
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 1) MEASUREMENT ERROR: ATTENUATION BIAS AND AN IV FIX
# Y = 2 X.star + u, observed X = X.star + e; a second noisy measure W = X.star + e.2 is the instrument
set.seed(71)
N.reps   = 2000
n        = 1000
beta.1   = 2
s2.Xstar = 1
me.df = data.frame()
for (s2.e in c(0, 0.25, 0.5, 1)){
  b.ols = numeric(N.reps)
  b.iv  = numeric(N.reps)
  for (r in 1:N.reps){
    X.star = rnorm(n, 0, sqrt(s2.Xstar))
    y = beta.1*X.star + rnorm(n)
    x = X.star + rnorm(n, 0, sqrt(s2.e))                  # mismeasured regressor
    w = X.star + rnorm(n, 0, sqrt(0.5))                   # second, independent measurement (instrument)
    b.ols[r] = ols_simple(y, x)
    b.iv[r]  = iv_simple(y, x, w)
  }
  me.df = rbind(me.df, data.frame( sigma2.e   = s2.e,
                                   Mean.OLS   = mean(b.ols),
                                   Theory.OLS = beta.1*(1 - s2.e/(s2.Xstar + s2.e)),   # beta_1 (1 - sigma_e^2/mu'_2X), Lecture 7
                                   Mean.IV    = mean(b.iv),
                                   SD.OLS     = sd(b.ols),
                                   SD.IV      = sd(b.iv) ))
}
print(format(me.df, digits = 4, nsmall = 4), row.names = FALSE)

# %% QUESTION 2) SIMULTANEITY: SUPPLY AND DEMAND
# Z shifts demand only
set.seed(72)
b.0 = 1
b.1 = 1
g.0 = 10
g.1 = -1
g.2 = 1
n      = 1000
N.reps = 2000
b.ols = numeric(N.reps)
b.iv  = numeric(N.reps)
for (r in 1:N.reps){
  d = sim_market(n)
  b.ols[r] = ols_simple(d$Q, d$P)
  b.iv[r]  = iv_simple(d$Q, d$P, d$Z)
}
# plim of OLS = b.1 + Cov(P, u.s)/Var(P), with Cov(P, u.s) = -s.s^2/(b.1 - g.1) and Var(P) = (g.2^2 + s.d^2 + s.s^2)/(b.1 - g.1)^2
Cov.P.us = -1/(b.1 - g.1)
Var.P    = (g.2^2 + 1 + 1)/(b.1 - g.1)^2
plim.ols = b.1 + Cov.P.us/Var.P
cat(sprintf(' Supply slope b.1 = %.1f: mean OLS = %.4f (plim %.4f), mean IV = %.4f\n', b.1, mean(b.ols), plim.ols, mean(b.iv)))
# Calculation Exercises 3, Question 3: demand depends on neither P nor Z (g.1 = 0, g.2 = 0)
# plim OLS = b.1 s.d^2/(s.d^2 + s.s^2)
g.1 = 0
d = sim_market(1e6, s.s = 1, s.d = 2, g.2 = 0)
cat(sprintf(' Calc-3 Q3 case (s.d = 2, s.s = 1): OLS = %.4f, formula b.1 s.d^2/(s.d^2 + s.s^2) = %.4f\n',
            ols_simple(d$Q, d$P), b.1*4/(4 + 1)))
g.1 = -1

# %% QUESTION 3) RETURN TO EDUCATION FOR MARRIED WOMEN
# [W2020] Example 15.1 (data mroz)
data("mroz", package = "wooldridge")
mroz = subset(mroz, inlf == 1)                            # women in the labour force (wage observed), n = 428
y = mroz$lwage
x = mroz$educ
z = mroz$fatheduc
n = nrow(mroz)
# a. OLS
lm.ols = lm(lwage ~ educ, data = mroz)
print(coeftest(lm.ols, vcov = vcovHC(lm.ols, type = "HC1")))
# b. first stage
lm.first = lm(educ ~ fatheduc, data = mroz)
print(coeftest(lm.first, vcov = vcovHC(lm.first, type = "HC1")))
cat(sprintf(' First stage R2 = %.4f\n', summary(lm.first)$r.squared))
# c. IV by hand
b.1.iv  = iv_simple(y, x, z)
b.0.iv  = mean(y) - b.1.iv*mean(x)
u.iv    = y - b.0.iv - b.1.iv*x                            # IV residuals use X, not the fitted X
s2.u    = sum(u.iv^2)/(n - 2)
rho2.xz = cor(x, z)^2
se.iv.hand = sqrt( s2.u/sum((x - mean(x))^2)/rho2.xz )     # Lecture 7: pseudo OLS variance/Corr(X,Z)^2
iv.fit = ivreg(lwage ~ educ | fatheduc, data = mroz)
cat(sprintf(' IV by hand: b.0 = %.6f, b.1 = %.6f, SE(b.1) = %.6f;  Corr(X,Z)^2 = %.4f\n', b.0.iv, b.1.iv, se.iv.hand, rho2.xz))
print(coeftest(iv.fit))                                    # homoskedasticity-only standard errors
print(coeftest(iv.fit, vcov = vcovHC(iv.fit, type = "HC1")))
# d. ratio of the standard errors
se.ratio = sqrt(diag(vcov(iv.fit)))[2]/sqrt(diag(vcov(lm.ols)))[2]
cat(sprintf(' Ratio of IV to OLS standard error = %.4f, 1/|Corr(X,Z)| = %.4f\n', se.ratio, 1/sqrt(rho2.xz)))

# %% QUESTION 4) FERTILITY AND LABOUR SUPPLY
# [SW2020] Empirical Exercise E12.1 (AER data Fertility)
data("Fertility", package = "AER")
Fert = transform(Fertility,
                 morekids = as.numeric(morekids == "yes"),
                 samesex  = as.numeric(gender1 == gender2),
                 black    = as.numeric(afam == "yes"),
                 hispan   = as.numeric(hispanic == "yes"),
                 othrace  = as.numeric(other == "yes"))
cat(sprintf(' n = %d women, mean weeks worked = %.4f, share with more than 2 kids = %.4f\n',
            nrow(Fert), mean(Fert$work), mean(Fert$morekids)))
lm.a  = lm(work ~ morekids, data = Fert)
lm.c  = lm(morekids ~ samesex, data = Fert)
iv.f  = ivreg(work ~ morekids | samesex, data = Fert)
iv.g  = ivreg(work ~ morekids + age + black + hispan + othrace | samesex + age + black + hispan + othrace, data = Fert)
lm.cg = lm(morekids ~ samesex + age + black + hispan + othrace, data = Fert)
print(coeftest(lm.a, vcov = vcovHC(lm.a, type = "HC1")))
print(coeftest(lm.c, vcov = vcovHC(lm.c, type = "HC1")))
t.samesex = coef(lm.c)[2]/sqrt(vcovHC(lm.c, type = "HC1")[2,2])
cat(sprintf(' First stage robust F-statistic (= t^2) = %.2f\n', t.samesex^2))
print(coeftest(iv.f, vcov = vcovHC(iv.f, type = "HC1")))
print(coeftest(iv.g, vcov = vcovHC(iv.g, type = "HC1")))
t.samesex.g = coef(lm.cg)["samesex"]/sqrt(vcovHC(lm.cg, type = "HC1")["samesex","samesex"])
cat(sprintf(' First stage F with controls = %.2f\n', t.samesex.g^2))

# %% QUESTION 5) WEAK INSTRUMENTS: THE IV ESTIMATOR IS FAR FROM NORMAL
# cf. Pagan and Robertson (1998)
# Y = 1 X + u, X = pi Z + v, Corr(u, v) = 0.8; instrument strength is measured by the concentration parameter mu2 = n pi^2
set.seed(75)
N.reps   = 1e4
n        = 100
beta.1   = 1
rho.uv   = 0.8
mu2.values = c(1, 10, 100)
b.iv    = matrix(NA, N.reps, 3)
reject  = matrix(NA, N.reps, 3)
F.first = matrix(NA, N.reps, 3)
for (j in 1:3){
  pi.1 = sqrt(mu2.values[j]/n)
  for (r in 1:N.reps){
    z = rnorm(n)
    v = rnorm(n)
    u = rho.uv*v + sqrt(1 - rho.uv^2)*rnorm(n)
    x = pi.1*z + v
    y = beta.1*x + u
    b     = iv_simple(y, x, z)
    u.hat = (y - mean(y)) - b*(x - mean(x))
    se    = sqrt( sum(u.hat^2)/(n - 2)/sum((x - mean(x))^2)/cor(x, z)^2 )
    b.iv[r,j]    = b
    reject[r,j]  = abs((b - beta.1)/se) > 1.96             # t-test of the true null H0: beta_1 = 1
    F.first[r,j] = (n - 2)*cor(x, z)^2/(1 - cor(x, z)^2)
  }
}
weak.df = data.frame( mu2 = mu2.values,
                      Mean.F.first    = colMeans(F.first),
                      Median.b.IV     = apply(b.iv, 2, median),
                      IQR.b.IV        = apply(b.iv, 2, IQR),
                      Pr.abs.dev.gt.1 = colMeans(abs(b.iv - beta.1) > 1),
                      Size.t.test     = colMeans(reject) )
print(format(weak.df, digits = 4, nsmall = 4), row.names = FALSE)
cat(sprintf(' plim of OLS = beta.1 + Cov(X,u)/Var(X) = 1 + 0.8/(1 + pi^2): about %.2f\n', 1 + rho.uv))

# %% PLOT THE SAMPLING DISTRIBUTIONS OF THE IV ESTIMATOR
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name), width = 9, height = 3.8 ) }
op = par( mfrow = c(1, 3), family = "serif", las = 1,
          mgp = c(2.6, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(4, 4, 1, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
for (j in 1:3){
  b.j = b.iv[,j]
  b.j = b.j[b.j > -1 & b.j < 3]
  hist( b.j, breaks = seq(-1, 3, by = 0.05), freq = FALSE, col = "steelblue", border = "white", main = "",
        xlim = c(-1, 3), ylim = c(0, 4.2), xlab = bquote(hat(beta)[1]^IV ~ "," ~ mu^2 == .(mu2.values[j])), ylab = "Density",
        cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
  abline(v = beta.1, col = "red3", lwd = 2)
  abline(v = 1 + rho.uv, col = "green4", lwd = 2, lty = 2)
  set_axis_ticks(tcl = .15, cex = fnt-.1)
}
if (print2pdf == 1){ par(op);dev.off() }

#EOF
