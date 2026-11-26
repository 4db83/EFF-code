# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr, numDeriv)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = c("Lab-03-MoM-Minimum Variance Portfolio.pdf")
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
# the six distinct elements of a 3 x 3 covariance matrix, in the order 11, 21, 31, 22, 32, 33
ij.index = rbind(c(1,1), c(2,1), c(3,1), c(2,2), c(3,2), c(3,3))
vech_to_matrix = function(s.vech){
  Sigma = matrix(0, 3, 3)
  for (k in 1:6){
    i = ij.index[k,1]
    j = ij.index[k,2]
    Sigma[i,j] = s.vech[k]
    Sigma[j,i] = s.vech[k]
  }
  Sigma
}
# QUESTION 4: one-factor model e_it = lambda_i s_t + sigma_i z_it for three returns, solved in closed form
one_factor = function(P, L = 10){
  e     = scale(100*diff(log(P)), scale = FALSE)                 # centred daily percentage returns
  n     = nrow(e)
  S     = crossprod(e)/n                                         # sample covariances s_ij (divisor n)
  lambda.sq  = c( S[1,2]*S[1,3]/S[2,3], S[1,2]*S[2,3]/S[1,3], S[1,3]*S[2,3]/S[1,2] )
  lambda.hat = sqrt(lambda.sq)*sign(c(1, S[1,2], S[1,3]))        # normalisation lambda_1 > 0
  sigma.hat  = sqrt( diag(S) - lambda.hat^2 )
  # moments m_t = vech(e_t e_t') - vech(Sigma(theta)), in the order 11, 21, 31, 22, 32, 33
  m = matrix(NA, n, 6)
  for (k in 1:6){
    i = ij.index[k,1]
    j = ij.index[k,2]
    m[,k] = e[,i]*e[,j] - ( lambda.hat[i]*lambda.hat[j] + (i == j)*sigma.hat[i]^2 )
  }
  # d = dm_t/d[lambda_1, lambda_2, lambda_3, sigma_1, sigma_2, sigma_3]
  d = matrix(0, 6, 6)
  for (k in 1:6){
    i = ij.index[k,1]
    j = ij.index[k,2]
    d[k, i] = d[k, i] - lambda.hat[j]
    d[k, j] = d[k, j] - lambda.hat[i]                            # gives -2 lambda_i when i = j
    if (i == j) d[k, 3 + i] = -2*sigma.hat[i]
  }
  V = mom_vcov(d, m, L)                                          # squared returns are autocorrelated: Newey-West
  list( n = n, S = S, R = cov2cor(S), lambda = lambda.hat, sigma = sigma.hat, se = sqrt(diag(V)),
        share = lambda.hat^2/diag(S), m.bar = colMeans(m) )
}
# QUESTION 10: weights of the global minimum variance portfolio, w = Sigma^{-1} iota/(iota' Sigma^{-1} iota)
gmv_weights = function(s.vech){
  Sigma.inv = solve(vech_to_matrix(s.vech))
  iota = rep(1, 3)
  as.vector( Sigma.inv %*% iota/sum(Sigma.inv %*% iota) )
}
gmv_variance = function(s.vech){
  Sigma.inv = solve(vech_to_matrix(s.vech))
  1/sum(Sigma.inv)                                                # 1/(iota' Sigma^{-1} iota)
}
# QUESTION 10: MoM for the means and the covariance matrix, with the standard errors of the GMV weights
gmv_mom = function(P, L = 10){
  r     = 100*diff(log(P))                                       # daily percentage returns
  n     = nrow(r)
  mu.hat = colMeans(r)
  e     = sweep(r, 2, mu.hat)
  S     = crossprod(e)/n
  s.vech = S[ij.index]
  # moments m_t = [r_t - mu ; vech(e_t e_t') - vech(Sigma)]: d = -I at the estimates (sum of e_t is zero)
  m = cbind(e, sapply(1:6, function(k) e[, ij.index[k,1]]*e[, ij.index[k,2]] - s.vech[k]))
  V = mom_vcov(-diag(9), m, L)
  V.sigma = V[4:9, 4:9]
  # delta method: Var(w.hat) = J Var(vech(Sigma.hat)) J', J = dw/dvech(Sigma)' computed numerically
  w.hat = gmv_weights(s.vech)
  J     = jacobian(gmv_weights, s.vech)
  V.w   = J %*% V.sigma %*% t(J)
  list( n = n, S = S, w = w.hat, se.w = sqrt(diag(V.w)), var.gmv = gmv_variance(s.vech),
        var.equal = sum(S)/9, var.single = diag(S) )
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 4) HOW MUCH OF THE RISK IN INTERNATIONAL EQUITY MARKETS IS COMMON?
# [MHH2013] Exercise 10.5: daily S&P 500, FTSE 100 and EURO STOXX 50, 29 July 2004 - 3 March 2009
eq = read.csv("./data/mhh_equity_decomposition.csv")
P  = as.matrix(eq[, c("sp500", "ftse100", "euro50")])
index.names = c("S&P 500", "FTSE 100", "EURO 50")
# a. covariance and correlation matrix of the centred returns
fit = one_factor(P)
cat(" Covariance matrix of the centred returns (n =", fit$n, ")\n")
print(round(fit$S, 4))
cat(" Correlation matrix\n")
print(round(fit$R, 3))
# b. closed form MoM estimates: lambda_1^2 = s12 s13/s23 etc., sigma_i^2 = s_ii - lambda_i^2
factor.df = data.frame( Index = index.names,
                        lambda = fit$lambda, se.lambda = fit$se[1:3],
                        sigma  = fit$sigma,  se.sigma  = fit$se[4:6],
                        Var = diag(fit$S), Common.share = fit$share, Idio.share = 1 - fit$share )
print_table(factor.df)
Sigma.model = fit$lambda %o% fit$lambda + diag(fit$sigma^2)
cat(sprintf(" Fit: max |S - (lambda lambda' + diag(sigma^2))| = %.1e\n", max(abs(fit$S - Sigma.model))))
# d. before and after the peak of the S&P 500 (October 2007)
k.peak = which.max(P[,1])
fit.pre  = one_factor(P[1:k.peak, ])
fit.post = one_factor(P[k.peak:nrow(P), ])
sub.df = data.frame( Index = index.names,
                     lambda.pre  = fit.pre$lambda,  sigma.pre  = fit.pre$sigma,  share.pre  = fit.pre$share,
                     lambda.post = fit.post$lambda, sigma.post = fit.post$sigma, share.post = fit.post$share )
cat(sprintf(" S&P 500 peaks at observation %d (%.2f); n = %d before and %d after\n",
            k.peak, P[k.peak,1], fit.pre$n, fit.post$n))
print_table(sub.df)
cat(" Correlations before / after the peak (S&P-FTSE, S&P-EURO, FTSE-EURO):",
    round(fit.pre$R[lower.tri(fit.pre$R)], 3), "/", round(fit.post$R[lower.tri(fit.post$R)], 3), "\n")

# %% QUESTION 10) DIVERSIFICATION: THE GLOBAL MINIMUM VARIANCE PORTFOLIO
# a.-c. full sample: weights, standard errors and portfolio variances
gmv = gmv_mom(P)
# b. two assets in closed form: w_1 = (s22 - s12)/(s11 + s22 - 2 s12) for the S&P 500 and the FTSE 100
S.12 = gmv$S[1:2, 1:2]
w.1.two = (S.12[2,2] - S.12[1,2])/(S.12[1,1] + S.12[2,2] - 2*S.12[1,2])
var.two = (S.12[1,1]*S.12[2,2] - S.12[1,2]^2)/(S.12[1,1] + S.12[2,2] - 2*S.12[1,2])
cat(sprintf(" Two assets (S&P 500, FTSE 100): w1 = %.4f, w2 = %.4f, GMV variance = %.4f (s11 = %.4f, s22 = %.4f)\n",
            w.1.two, 1 - w.1.two, var.two, S.12[1,1], S.12[2,2]))
gmv.df = data.frame( Index = index.names, w = gmv$w, se.w.NW = gmv$se.w, t.w = gmv$w/gmv$se.w,
                     Var = gmv$var.single, SD.pa = sqrt(252*gmv$var.single) )
print_table(gmv.df)
cat(sprintf(" Variances: GMV = %.4f (sd p.a. %.4f%%), equally weighted = %.4f (sd p.a. %.4f%%)\n",
            gmv$var.gmv, sqrt(252*gmv$var.gmv), gmv$var.equal, sqrt(252*gmv$var.equal)))
gmv.iid = gmv_mom(P, L = 0)
cat(" Standard errors of w without Newey-West (L = 0):", round(gmv.iid$se.w, 4), "\n")
# d. before and after the peak of the S&P 500
gmv.pre  = gmv_mom(P[1:k.peak, ])
gmv.post = gmv_mom(P[k.peak:nrow(P), ])
period.df = data.frame( Period = c("Before peak", "After peak"),
                        w.SP500 = c(gmv.pre$w[1], gmv.post$w[1]),
                        w.FTSE  = c(gmv.pre$w[2], gmv.post$w[2]),
                        w.EURO  = c(gmv.pre$w[3], gmv.post$w[3]),
                        sd.GMV.pa      = sqrt(252*c(gmv.pre$var.gmv, gmv.post$var.gmv)),
                        sd.min.single  = sqrt(252*c(min(gmv.pre$var.single), min(gmv.post$var.single))),
                        Var.ratio      = c(gmv.pre$var.gmv/min(gmv.pre$var.single), gmv.post$var.gmv/min(gmv.post$var.single)) )
print_table(period.df)
cat(" Standard errors of the weights before / after:", round(gmv.pre$se.w, 4), "/", round(gmv.post$se.w, 4), "\n")

# %% PLOT THE VOLATILITY OF S&P 500 - FTSE 100 PORTFOLIOS AGAINST THE WEIGHT ON THE S&P 500
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(4, 4.5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
w.grid  = seq(-0.5, 1.5, length.out = 401)
sd_portfolio = function(w, S2) sqrt( 252*(w^2*S2[1,1] + (1 - w)^2*S2[2,2] + 2*w*(1 - w)*S2[1,2]) )
S.pre  = gmv.pre$S[1:2, 1:2]
S.post = gmv.post$S[1:2, 1:2]
sd.pre  = sd_portfolio(w.grid, S.pre)
sd.post = sd_portfolio(w.grid, S.post)
plot( w.grid, sd.post, type = "l", lwd = 2, col = "red3", ylim = c(0, max(sd.post)),
      xlab = "Weight on the S&P 500 (rest in the FTSE 100)", ylab = "Volatility (% p.a.)",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(w.grid, sd.pre, lwd = 2, col = "steelblue", lty = 2)
w.min.pre  = (S.pre[2,2] - S.pre[1,2])/(S.pre[1,1] + S.pre[2,2] - 2*S.pre[1,2])
w.min.post = (S.post[2,2] - S.post[1,2])/(S.post[1,1] + S.post[2,2] - 2*S.post[1,2])
points(c(w.min.pre, w.min.post), c(sd_portfolio(w.min.pre, S.pre), sd_portfolio(w.min.post, S.post)),
       pch = 19, col = c("steelblue", "red3"), cex = 1.3)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topleft", legend = c("Before the peak", "After the peak"), col = c("steelblue", "red3"), lty = c(2, 1), lwd = 2,
       bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
