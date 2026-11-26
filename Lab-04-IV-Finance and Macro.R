# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,lmtest,AER,car)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = c("Lab-04-IV-Risk Return.pdf",
              "Lab-04-IV-Taylor Rule Data.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# heteroskedasticity robust covariance matrix (HC1)
vcov_robust = function(fit) vcovHC(fit, type = "HC1")
# lag and lead of a vector by k periods (NA at the start or the end)
lag_k  = function(x, k = 1) c(rep(NA, k), head(x, -k))
lead_k = function(x, k = 1) c(tail(x, -k), rep(NA, k))
# QUESTION 21: Hodrick-Prescott trend, the solution of (I + lambda D'D) trend = y with D the second difference matrix
hp_trend = function(y, lambda = 1600){
  I.T = diag(length(y))
  D   = diff(I.T, differences = 2)
  solve(I.T + lambda*crossprod(D), y)
}

# %% SCRIPT STARTS HERE: one cell for each question
# the hurn_*.csv files are created from the [HMPY2021] book resources by 'Data-Hurn et al workfiles to csv.R'
# QUESTION 19) THE RISK-RETURN TRADE-OFF AND THE VIX
# [HMPY2021] Section 8.2
vix = read.csv("./data/hurn_vix.csv")
vix$date = as.Date(vix$date)
n = nrow(vix)
cat(sprintf(' T = %d days (%s to %s), mean return = %.6f, mean h = %.7f (VIX = %.1f%% p.a.)\n', n, min(vix$date), max(vix$date),
            mean(vix$ret), mean(vix$h), 100*sqrt(252*mean(vix$h))))
# a. OLS of the daily return on the VIX-implied daily variance
ols.rr = lm(ret ~ h, data = vix)
print(coeftest(ols.rr))
print(coeftest(ols.rr, vcov = vcov_robust(ols.rr)))
# b. timing: the VIX at the close of day t already reacts to the return of day t
vix$h.lag = lag_k(vix$h)
print(round(cor(vix[, c("ret", "h", "h.lag")], use = "complete.obs"), 4))
# c. IV with h_{t-1} as the instrument for h_t
d.rr     = na.omit(vix)
first.rr = lm(h ~ h.lag, data = d.rr)
print(coeftest(first.rr, vcov = vcov_robust(first.rr)))
t.first.rr = coeftest(first.rr, vcov = vcov_robust(first.rr))[2, 3]
cat(sprintf(' First stage F (robust) = %.1f, R2 = %.4f\n', t.first.rr^2, summary(first.rr)$r.squared))
iv.rr = ivreg(ret ~ h | h.lag, data = d.rr)
print(coeftest(iv.rr))
print(coeftest(iv.rr, vcov = vcov_robust(iv.rr)))
gamma.iv = coef(iv.rr)[2]
cat(sprintf(' Implied annual risk premium at the mean VIX: gamma x 252 x mean(h) = %.4f\n', gamma.iv*252*mean(d.rr$h)))
# d. regression based Hausman test: add the first stage residual
d.rr$v.hat = resid(first.rr)
cf.rr = lm(ret ~ h + v.hat, data = d.rr)
print(coeftest(cf.rr, vcov = vcov_robust(cf.rr)))

# %% PLOT RETURNS AGAINST THE VIX-IMPLIED VARIANCE, AND THE VIX OVER TIME
# with the OLS and IV regression lines
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 2), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 1, 1),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks
plot( 1e4*vix$h, 100*vix$ret, pch = 16, cex = .4, col = adjustcolor("steelblue", .4),
      xlab = expression("VIX-implied daily variance " * h[t] * " (x " * 10^4 * ")"),
      ylab = "Daily return (%)", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
h.grid = seq(0, max(vix$h), length.out = 100)
lines(1e4*h.grid, 100*(coef(ols.rr)[1] + coef(ols.rr)[2]*h.grid), col = "red3", lwd = 2)
lines(1e4*h.grid, 100*(coef(iv.rr)[1] + coef(iv.rr)[2]*h.grid), col = "green4", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c(sprintf("OLS: slope %.2f", coef(ols.rr)[2]), sprintf("IV:   slope %.2f", coef(iv.rr)[2])),
       col = c("red3", "green4"), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
year.vix = as.numeric(format(vix$date, "%Y")) + (as.numeric(format(vix$date, "%j")) - 1)/365
plot( year.vix, 100*sqrt(252*vix$h), type = "l", col = "steelblue", xlab = "", ylab = "VIX (% p.a.)",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
set_axis_ticks(tcl = .15, cex = fnt-.1)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 20) CONSUMPTION CAPM: RISK AVERSION AND WEAK INSTRUMENTS
# [HMPY2021] Section 8.6, Table 8.1
cc = read.csv("./data/hurn_ccapm.csv")
cc$dc.lag = lag_k(cc$dc)
cc$tb.lag = lag_k(cc$tb)
cat(sprintf(' T = %d quarters (%s to %s), mean consumption growth = %.4f, sd = %.4f\n',
            nrow(cc), cc$date[1], tail(cc$date, 1), mean(cc$dc), sd(cc$dc)))
assets = c("gb", "cb", "d1", "d10")
d.cc = na.omit(cc)
# a.-b. OLS (all 163 quarters, as in Table 8.1) and IV with one or two instruments
# (vcovHC() warns that some leverages are close to 1: the fitted values of dc are almost constant except
#  in a few quarters, another symptom of irrelevant instruments (see part c), so the warning is suppressed)
ccapm.table = matrix(NA, length(assets), 8)
rownames(ccapm.table) = assets
colnames(ccapm.table) = c("ols", "p.ols", "iv1", "se.iv1", "p.iv1", "iv2", "se.iv2", "p.iv2")
for (a in assets){
  ols.a  = lm(as.formula(paste(a, "~ dc")), data = cc)
  iv1.a  = ivreg(as.formula(paste(a, "~ dc | dc.lag")), data = d.cc)
  iv2.a  = ivreg(as.formula(paste(a, "~ dc | dc.lag + tb.lag")), data = d.cc)
  V.iv1  = suppressWarnings(vcov_robust(iv1.a))
  V.iv2  = suppressWarnings(vcov_robust(iv2.a))
  ccapm.table[a, "ols"]    = coef(ols.a)[2]
  ccapm.table[a, "p.ols"]  = coeftest(ols.a, vcov = vcov_robust(ols.a))[2, 4]
  ccapm.table[a, "iv1"]    = coef(iv1.a)[2]
  ccapm.table[a, "se.iv1"] = sqrt(V.iv1[2, 2])
  ccapm.table[a, "p.iv1"]  = coeftest(iv1.a, vcov = V.iv1)[2, 4]
  ccapm.table[a, "iv2"]    = coef(iv2.a)[2]
  ccapm.table[a, "se.iv2"] = sqrt(V.iv2[2, 2])
  ccapm.table[a, "p.iv2"]  = coeftest(iv2.a, vcov = V.iv2)[2, 4]
}
print(round(ccapm.table, 4))
# c. strength of the instruments: first stage regressions of dc_{t+1} on the instruments
first1.cc = lm(dc ~ dc.lag, data = d.cc)
first2.cc = lm(dc ~ dc.lag + tb.lag, data = d.cc)
F1.cc = linearHypothesis(first1.cc, "dc.lag = 0", vcov. = vcov_robust(first1.cc))$F[2]
F2.cc = linearHypothesis(first2.cc, c("dc.lag = 0", "tb.lag = 0"), vcov. = vcov_robust(first2.cc))$F[2]
cat(sprintf(' First stage: R2 = %.6f, robust F = %.4f (one instrument); R2 = %.6f, robust F = %.4f (two instruments)\n',
            summary(first1.cc)$r.squared, F1.cc, summary(first2.cc)$r.squared, F2.cc))
print(coeftest(first2.cc, vcov = vcov_robust(first2.cc)))
# the fitted values of the first stage barely vary, and one quarter dominates them
iv2.gb = ivreg(gb ~ dc | dc.lag + tb.lag, data = d.cc)
cat(sprintf(' sd(fitted dc)/sd(dc) = %.4f; largest leverage in the IV regression: %s\n',
            sd(fitted(first2.cc))/sd(d.cc$dc), d.cc$date[which.max(hatvalues(iv2.gb))]))
# re-estimate without 1950:Q4, the quarter after the Korean War buying spree
d.cc2 = subset(d.cc, date != "1950-10-01")
gamma.without.1950 = sapply(assets, function(a) coef(ivreg(as.formula(paste(a, "~ dc | dc.lag + tb.lag")), data = d.cc2))[2])
print(round(gamma.without.1950, 3))
# d. reverse regression: dc_{t+1} on the return, instrumented by dc_t and tb_t (Yogo, 2004)
reverse.table = matrix(NA, length(assets), 4)
rownames(reverse.table) = assets
colnames(reverse.table) = c("F.first", "psi", "se.psi", "gamma.implied")
for (a in assets){
  first.a   = lm(as.formula(paste(a, "~ dc.lag + tb.lag")), data = d.cc)
  reverse.a = ivreg(as.formula(paste("dc ~", a, "| dc.lag + tb.lag")), data = d.cc)
  reverse.table[a, "F.first"]       = linearHypothesis(first.a, c("dc.lag = 0", "tb.lag = 0"), vcov. = vcov_robust(first.a))$F[2]
  reverse.table[a, "psi"]           = coef(reverse.a)[2]
  reverse.table[a, "se.psi"]        = sqrt(vcov_robust(reverse.a)[2, 2])
  reverse.table[a, "gamma.implied"] = 1/coef(reverse.a)[2]
}
print(round(reverse.table, 4))

# %% QUESTION 21) A FORWARD-LOOKING TAYLOR RULE
# Clarida, Gali and Gertler (2000)
macro.m = as.data.frame(read_xlsx("./data/us_macro_monthly.xlsx"))
macro.q = as.data.frame(read_xlsx("./data/us_macro_quarterly.xlsx"))
# quarterly averages of the monthly federal funds rate
macro.m$qtr = rep(seq_len(nrow(macro.m)/3), each = 3)
tr = aggregate(FEDFUNDS ~ qtr, data = macro.m, FUN = mean)
tr$date = as.Date(macro.q[[1]])
# inflation over the past four quarters (PCE price index) and the output gap from an HP filter (lambda = 1600)
tr$pi    = 100*(log(macro.q$PCECTPI) - lag_k(log(macro.q$PCECTPI), 4))
tr$gap   = 100*(log(macro.q$GDPC1) - hp_trend(log(macro.q$GDPC1)))
tr$i     = tr$FEDFUNDS
tr$pi.f4 = lead_k(tr$pi, 4)              # inflation over the next four quarters
for (k in 1:4) tr[[paste0("pi.l", k)]] = lag_k(tr$pi, k)
tr$i.l1  = lag_k(tr$i)
year.tr  = as.numeric(format(tr$date, "%Y")) + (as.numeric(format(tr$date, "%m")) - 1)/12
periods  = list("1960:Q1-1979:Q2" = c(1960, 1979.3), "1979:Q3-2007:Q4" = c(1979.5, 2007.9))
instruments.tr = "pi.l1 + pi.l2 + pi.l3 + pi.l4"

for (p in names(periods)) {
  d.tr = na.omit(tr[year.tr >= periods[[p]][1] & year.tr <= periods[[p]][2], ])
  cat(sprintf('\n ==== %s: %d quarters\n', p, nrow(d.tr)))
  # a. OLS with current and with future (realised) inflation
  ols.current = lm(i ~ pi + gap, data = d.tr)
  ols.future  = lm(i ~ pi.f4 + gap, data = d.tr)
  # b. IV with lagged inflation as instruments for future inflation
  first.tr = lm(as.formula(paste("pi.f4 ~ gap +", instruments.tr)), data = d.tr)
  F.tr     = linearHypothesis(first.tr, paste0("pi.l", 1:4, " = 0"), vcov. = vcov_robust(first.tr))$F[2]
  iv.tr    = ivreg(as.formula(paste("i ~ pi.f4 + gap | gap +", instruments.tr)), data = d.tr)
  print(round(cbind(OLS.current = coef(ols.current), OLS.future = coef(ols.future), IV = coef(iv.tr),
                    IV.robust.se = sqrt(diag(vcov_robust(iv.tr)))), 4))
  cat(sprintf(' First stage robust F = %.4f\n', F.tr))
  print(summary(iv.tr, diagnostics = TRUE)$diagnostics)
  # c. serial correlation of the residuals, then interest rate smoothing: i_{t-1} as an exogenous regressor
  acf.static = acf(resid(iv.tr), plot = FALSE)$acf[2:5]
  cat(sprintf(' Residual autocorrelations (lags 1-4), static rule: %s\n', paste(round(acf.static, 4), collapse = ", ")))
  iv.smooth = ivreg(as.formula(paste("i ~ pi.f4 + gap + i.l1 | gap + i.l1 +", instruments.tr)), data = d.tr)
  b = coef(iv.smooth)
  print(coeftest(iv.smooth, vcov = vcov_robust(iv.smooth)))
  beta.long  = b["pi.f4"]/(1 - b["i.l1"])
  gamma.long = b["gap"]/(1 - b["i.l1"])
  cat(sprintf(' Long-run responses: beta = %.4f, gamma = %.4f (rho = %.4f)\n', beta.long, gamma.long, b["i.l1"]))
  acf.smooth = acf(resid(iv.smooth), plot = FALSE)$acf[2:5]
  cat(sprintf(' Residual autocorrelations (lags 1-4), smoothing rule: %s\n', paste(round(acf.smooth, 4), collapse = ", ")))
  print(summary(iv.smooth, diagnostics = TRUE)$diagnostics)
  # d. Taylor principle: beta = 1 is the linear restriction b.pi + rho = 1
  lh = linearHypothesis(iv.smooth, "pi.f4 + i.l1 = 1", vcov. = vcov_robust(iv.smooth))
  a.sum  = c(1, 1)
  V.sum  = vcov_robust(iv.smooth)[c("pi.f4", "i.l1"), c("pi.f4", "i.l1")]
  t.stat = (b["pi.f4"] + b["i.l1"] - 1)/sqrt(as.numeric(t(a.sum) %*% V.sum %*% a.sum))
  cat(sprintf(' H0: beta = 1: robust Wald = %.4f (p = %.4f), t = %.4f, one-sided p (H1: beta > 1) = %.4f\n',
              lh$Chisq[2], lh$`Pr(>Chisq)`[2], t.stat, 1 - pnorm(t.stat)))
}

# %% PLOT THE FEDERAL FUNDS RATE, INFLATION AND THE OUTPUT GAP
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 11, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(3, 5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
plot( year.tr, tr$i, type = "l", lwd = 2, col = "steelblue", ylim = c(-6, 20), xlab = "", ylab = "Percent",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(year.tr, tr$pi, col = "red3", lwd = 2)
lines(year.tr, tr$gap, col = "green4", lwd = 1.5)
abline(h = 0, col = "grey50")
abline(v = 1979.5, lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = c("Federal funds rate", "Inflation (PCE, 4 quarters)", "Output gap (HP filter)"),
       col = c("steelblue", "red3", "green4"), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
