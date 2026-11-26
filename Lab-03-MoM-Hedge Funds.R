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
pdf_name  = c("Lab-03-MoM-Sharpe Ratios.pdf",
              "Lab-03-MoM-Variance Ratios.pdf")
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
# lag of a vector by k periods (NA at the start)
lag_k = function(x, k = 1) c(rep(NA, k), head(x, -k))
# QUESTION 6: exactly identified linear model y = Z theta + u with instruments Z (moments E[Z_t u_t] = 0)
linear_mom = function(y, Z, L = 10){
  theta.hat = as.vector(solve(crossprod(Z), crossprod(Z, y)))   # sample moments Z'(y - Z theta)/n = 0
  u.hat = as.vector(y - Z %*% theta.hat)
  m = Z*u.hat
  d = -crossprod(Z)/nrow(Z)                                     # d = mean of dm_t/dtheta' = -(1/n) Z'Z
  list( theta = theta.hat, V.white = mom_vcov(d, m, 0), V.nw = mom_vcov(d, m, L) )
}
# QUESTION 7: variance ratio VR(q) = sigma_q^2/sigma_1^2 by MoM, with iid and Newey-West standard errors
variance_ratio = function(x, q, L = 2*q){
  n.all = length(x)
  y.q   = stats::filter(x, rep(1, q), sides = 1)                # y_t = x_t + x_{t-1} + ... + x_{t-q+1}
  keep  = q:n.all
  x.t   = x[keep]
  y.t   = as.vector(y.q[keep])
  n     = length(keep)
  # three moment conditions for theta = [mu, sigma_1^2, sigma_q^2]'
  mu.hat  = mean(x.t)
  s2.1    = mean((x.t - mu.hat)^2)
  s2.q    = mean((y.t - q*mu.hat)^2)/q
  VR.hat  = s2.q/s2.1
  m = cbind( x.t - mu.hat,
             (x.t - mu.hat)^2 - s2.1,
             (y.t - q*mu.hat)^2/q - s2.q )
  d = rbind( c(-1,                                0,  0),
             c(-2*mean(x.t - mu.hat),            -1,  0),
             c(-2*mean(y.t - q*mu.hat),           0, -1) )
  G = c(0, -s2.q/s2.1^2, 1/s2.1)                               # gradient of VR = sigma_q^2/sigma_1^2
  se.nw  = sqrt( as.numeric(t(G) %*% mom_vcov(d, m, L) %*% G) )
  se.iid = sqrt( 2*(2*q - 1)*(q - 1)/(3*q*n) )                 # Lo and MacKinlay (1988), i.i.d. returns
  c(VR = VR.hat, z.iid = (VR.hat - 1)/se.iid, se.nw = se.nw, z.nw = (VR.hat - 1)/se.nw)
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 5) SHARPE RATIOS OF HEDGE FUNDS AND THEIR STANDARD ERRORS
# Lo (2002); daily hedge fund index returns, 1 April 2003 - 28 May 2010 ([MHH2013] Section 7.9.1)
hf = read.csv("./data/mhh_hedge.csv")
fund.names = c("conv.arb", "distressed", "equity.hedge", "event.driven", "macro", "merger.arb", "eq.mkt.neutral")
X  = cbind(hf[, fund.names] - hf$rf, market = hf$mkt.excess)   # daily excess returns in percent
q  = 252                                                       # trading days per year
sharpe.df = data.frame()
for (k in 1:ncol(X)){
  x  = X[,k]
  n  = length(x)
  # a. MoM: m_t = [x_t - mu, (x_t - mu)^2 - sigma^2]' gives mu = mean(x) and sigma^2 = mean((x - mu)^2)
  mu.hat     = mean(x)
  sigma2.hat = mean((x - mu.hat)^2)
  SR.hat     = mu.hat/sqrt(sigma2.hat)
  # b. Var(theta.hat) from Lecture 6 (here d = -I), then the delta method with the gradient of SR = mu/sigma
  m  = cbind(x - mu.hat, (x - mu.hat)^2 - sigma2.hat)
  G  = c(1/sqrt(sigma2.hat), -mu.hat/(2*sigma2.hat^(3/2)))
  se.iid   = sqrt( (1 + SR.hat^2/2)/n )                        # i.i.d. normal returns (Lo, 2002)
  se.white = sqrt( as.numeric(t(G) %*% mom_vcov(-diag(2), m, 0) %*% G) )
  # c. first order autocorrelation and Newey-West standard errors with 10 lags
  rho.1 = sum( (x[-1] - mu.hat)*(x[-n] - mu.hat) )/sum( (x - mu.hat)^2 )
  se.nw = sqrt( as.numeric(t(G) %*% mom_vcov(-diag(2), m, 10) %*% G) )
  # d. Lo (2002) annualisation: SR(q) = eta(q) SR, eta(q) = q/sqrt(q + 2 sum_{k=1}^{q-1} (q-k) rho_k), rho_k = rho_1^k
  k.lags = 1:(q-1)
  eta.q  = q/sqrt( q + 2*sum( (q - k.lags)*rho.1^k.lags ) )
  sharpe.df = rbind(sharpe.df, data.frame( Fund = colnames(X)[k],
                    Mean.pa = mu.hat*q, SD.pa = sqrt(sigma2.hat*q), SR.pa = SR.hat*sqrt(q),
                    SE.iid = se.iid*sqrt(q), SE.white = se.white*sqrt(q), t.white = SR.hat/se.white,
                    rho.1 = rho.1, SE.nw = se.nw*sqrt(q), t.nw = SR.hat/se.nw, SR.pa.Lo = SR.hat*eta.q ))
}
cat(" n =", nrow(X), "daily observations; annualised Sharpe ratios with standard errors\n")
print_table(sharpe.df)

# %% PLOT THE ANNUALISED SHARPE RATIOS WITH 95% CONFIDENCE INTERVALS (WHITE AND NEWEY-WEST)
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 9, height = 5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(2.6, .5, 0),    # axis title, labels, line, position of y,x labels to axis
          mar = c(6.5, 4.5, 1, 1),# bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),    # outer margins for overall spacing
          tcl = -0.25)            # shorter tick marks
K  = nrow(sharpe.df)
xx = 1:K
ci.lower = cbind(sharpe.df$SR.pa - 1.96*sharpe.df$SE.white, sharpe.df$SR.pa - 1.96*sharpe.df$SE.nw)
ci.upper = cbind(sharpe.df$SR.pa + 1.96*sharpe.df$SE.white, sharpe.df$SR.pa + 1.96*sharpe.df$SE.nw)
plot( xx, sharpe.df$SR.pa, pch = 19, col = "steelblue", xlim = c(.5, K + .5), ylim = range(ci.lower, ci.upper), xlab = "",
      ylab = "Annualised Sharpe ratio", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = 0, col = "gray40")
segments(xx - .1, ci.lower[,1], xx - .1, ci.upper[,1], col = "steelblue", lwd = 3)
segments(xx + .1, ci.lower[,2], xx + .1, ci.upper[,2], col = "red3", lwd = 3)
set_yaxis_ticks(side = 2, tcl = .15, cex = fnt-.1)
fund.labels = c("Conv. arb.", "Distressed", "Equity hedge", "Event driven", "Macro", "Merger arb.", "Mkt. neutral", "Market")
axis(1, at = xx, labels = fund.labels, las = 2, cex.axis = fnt-.3, tcl = .15)
legend("bottomright", legend = c("95% CI, White", "95% CI, Newey-West"), col = c("steelblue", "red3"), lwd = 3,
       bty = "o", bg = "white", cex = fnt-.3)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 6) MARKET EXPOSURE OF HEDGE FUNDS: ALPHA, BETA AND STALE PRICES
# x_t = alpha + beta m_t + u_t with moments E(u_t) = 0 and E(u_t m_t) = 0 (exactly identified: OLS)
m.t  = hf$mkt.excess
K.lags = 4                                                     # lags of the market return in (b)
m.lags = sapply(1:K.lags, function(k) lag_k(m.t, k))
keep = (K.lags + 1):length(m.t)                                # same sample for both models
beta.df = data.frame()
for (k in 1:length(fund.names)){
  x = X[keep, k]
  # a. instruments [1, m_t]: alpha and beta
  Z.0  = cbind(1, m.t[keep])
  fit.0 = linear_mom(x, Z.0)
  se.0  = sqrt(diag(fit.0$V.nw))
  # b. instruments [1, m_t, m_{t-1}, ..., m_{t-K}]: the summed (Dimson) beta
  Z.K   = cbind(1, m.t[keep], m.lags[keep, ])
  fit.K = linear_mom(x, Z.K)
  a.sum = c(0, rep(1, K.lags + 1))                             # beta.sum = a' theta
  beta.sum    = sum(a.sum*fit.K$theta)
  se.beta.sum = sqrt( as.numeric(t(a.sum) %*% fit.K$V.nw %*% a.sum) )
  # c. Wald test of H0: beta_1 = ... = beta_K = 0 (Newey-West covariance matrix)
  R.lags = cbind(matrix(0, K.lags, 2), diag(K.lags))
  b.lags = R.lags %*% fit.K$theta
  Wald   = as.numeric( t(b.lags) %*% solve(R.lags %*% fit.K$V.nw %*% t(R.lags)) %*% b.lags )
  beta.df = rbind(beta.df, data.frame( Fund = fund.names[k],
                  alpha.pa = 252*fit.0$theta[1], se.alpha.pa = 252*se.0[1], t.alpha = fit.0$theta[1]/se.0[1],
                  beta.0 = fit.0$theta[2], se.beta.0 = se.0[2], R2 = cor(x, m.t[keep])^2,
                  beta.lag1 = fit.K$theta[3], beta.sum = beta.sum, se.beta.sum = se.beta.sum,
                  alpha.pa.K = 252*fit.K$theta[1], se.alpha.pa.K = 252*sqrt(fit.K$V.nw[1,1]),
                  t.alpha.K = fit.K$theta[1]/sqrt(fit.K$V.nw[1,1]),
                  Wald.lags = Wald, p.Wald = 1 - pchisq(Wald, K.lags) ))
}
cat(sprintf(" n = %d days; market: mean excess return p.a. = %.2f%%\n", length(keep), 252*mean(m.t[keep])))
print_table(beta.df)
cat(" Ratio beta.sum/beta.0:", round(beta.df$beta.sum/beta.df$beta.0, 2), "\n")
cat(" White vs Newey-West SE of beta.0 for conv.arb:",
    round(sqrt(diag(linear_mom(X[keep,1], cbind(1, m.t[keep]))$V.white))[2], 4),
    round(sqrt(diag(linear_mom(X[keep,1], cbind(1, m.t[keep]))$V.nw))[2], 4), "\n")

# %% QUESTION 7) VARIANCE RATIOS: ARE HEDGE FUND RETURNS SMOOTHED?
q.values = c(2, 5, 10, 20)
vr.list = list()
for (k in 1:ncol(X)){
  vr.k = t(sapply(q.values, function(qq) variance_ratio(X[,k], qq)))
  vr.list[[colnames(X)[k]]] = vr.k
}
vr.table = matrix(NA, ncol(X), 8)
rownames(vr.table) = colnames(X)
colnames(vr.table) = c("VR.2", "VR.5", "VR.10", "VR.20", "z.iid.5", "z.nw.5", "z.iid.20", "z.nw.20")
for (k in 1:ncol(X)){
  v = vr.list[[k]]
  vr.table[k, 1:4] = v[, "VR"]
  vr.table[k, 5:6] = v[2, c("z.iid", "z.nw")]
  vr.table[k, 7:8] = v[4, c("z.iid", "z.nw")]
}
print(round(vr.table, 4))
# VR(2) = 1 + rho_1 (approximately): compare with the first order autocorrelations of Question 5
print(round(cbind(VR.2 = vr.table[, "VR.2"], one.plus.rho1 = 1 + sharpe.df$rho.1), 4))
# d. monthly Sharpe ratios: naive sqrt(20) SR and the Lo adjustment sqrt(20) SR/sqrt(VR(20))
SR.daily   = sharpe.df$SR.pa/sqrt(252)
SR.month.naive = sqrt(20)*SR.daily
SR.month.lo    = sqrt(20)*SR.daily/sqrt(vr.table[, "VR.20"])
print(round(cbind(SR.month.naive, SR.month.lo, ratio = SR.month.lo/SR.month.naive), 4))
# check of the iid standard error: simulated iid returns
set.seed(7)
x.sim = rnorm(nrow(X))
print(round(t(sapply(q.values, function(qq) variance_ratio(x.sim, qq))), 3))

# %% PLOT THE VARIANCE RATIOS AGAINST THE HORIZON q
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[2]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(4, 4.5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
q.grid  = 1:30
vr.grid = sapply(1:ncol(X), function(k) c(1, sapply(q.grid[-1], function(qq) variance_ratio(X[,k], qq)["VR"])))
clrs = c("red3", "orange", "green4", "purple", "steelblue", "cyan3", "grey50", "black")
matplot( q.grid, vr.grid, type = "l", lty = c(rep(1, 7), 2), lwd = c(rep(1.8, 7), 2.5), col = clrs,
         xlab = "Horizon q (days)", ylab = "Variance ratio VR(q)", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = 1, col = "gray40")
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topleft", legend = fund.labels, col = clrs, lty = c(rep(1, 7), 2), lwd = 2, bty = "o", bg = "white", cex = fnt-.5, ncol = 2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
