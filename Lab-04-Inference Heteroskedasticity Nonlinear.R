# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,lmtest,AER)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# predicted change when Age goes from a0 to a0+1 (holding Female and Bachelor fixed), used in Question 9
pred_change = function(fit, a0, log.y){
  nd = data.frame(age = c(a0, a0+1), female = 0, bachelor = 0)
  p  = predict(fit, nd)
  if (log.y) 100*diff(p) else diff(p)        # % change for ln(AHE) models, $ change for the level model
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 5) TESTS AND CONFIDENCE INTERVALS
# SW2020 Empirical Exercise E5.2 (GrowthSW, excluding Malta)
data("GrowthSW", package = "AER")
GrowthSW.noM = GrowthSW[rownames(GrowthSW) != "Malta",]
lm.growth = lm(growth ~ tradeshare, data = GrowthSW.noM)
prnt.growth = print_lm_results(lm.growth)                    # White (1982) robust standard errors (HC1)
b1   = coef(lm.growth)[2]
se1  = sqrt(diag(vcovHC(lm.growth, type = "HC1")))[2]
t1   = b1/se1
cat(sprintf(' t = %.4f, two-sided p-value = %.4f\n', t1, 2*pnorm(-abs(t1))))
cat(sprintf(' Reject H0: b1 = 0 at 10%%: %s, at 5%%: %s, at 1%%: %s\n', abs(t1) > 1.645, abs(t1) > 1.96, abs(t1) > 2.576))
cat(sprintf(' 90%% CI for b1: [%.4f, %.4f]\n', b1 - 1.645*se1, b1 + 1.645*se1))
cat(sprintf(' Homoskedasticity-only SE = %.4f vs robust SE = %.4f\n', sqrt(diag(vcov(lm.growth)))[2], se1))

# %% QUESTION 6) ROBUST STANDARD ERRORS BY HAND
# SW2020 eq. 5.4: California test scores
CASchools = read_xlsx("./data/caschool.xlsx")
lm.testscr_str = lm(testscr ~ str, data = CASchools)
X = CASchools$str
u.hat = resid(lm.testscr_str)
n = length(X)
SSX = sum( (X - mean(X))^2 )
# robust (heteroskedasticity consistent) variance of b1
var.b1.robust = (1/n) * ( (1/(n-2))*sum( (X - mean(X))^2 * u.hat^2 ) ) / ( (1/n)*SSX )^2
# homoskedasticity-only variance of b1
var.b1.homo   = ( sum(u.hat^2)/(n-2) ) / SSX
se_df = data.frame( Method = c("Robust by hand", "vcovHC(type='HC1')", "Homoskedastic by hand", "vcov() from lm()"),
                    SE.b1  = c(sqrt(var.b1.robust), sqrt(vcovHC(lm.testscr_str, type = "HC1")[2,2]),
                               sqrt(var.b1.homo),   sqrt(vcov(lm.testscr_str)[2,2])) )
print(format(se_df, digits = 6, nsmall = 6), row.names = FALSE)
print( coeftest(lm.testscr_str, vcov = vcovHC(lm.testscr_str, type = "HC1")) )

# %% QUESTION 7) COVERAGE OF 95% CONFIDENCE INTERVALS IN REPEATED SAMPLES
# Y = 1 + 2X + u, X ~ U[0,10]; homoskedastic errors: sd(u|X) = 5, heteroskedastic errors: sd(u|X) = X
set.seed(42)
N.reps = 1e4
n      = 100
beta1  = 2
cover  = array(NA, dim = c(N.reps, 2, 2), dimnames = list(NULL, c("Homo errors", "Hetero errors"), c("Homo SE", "Robust SE")))
CI.store = matrix(NA, N.reps, 2)       # store the robust CIs for the heteroskedastic case to plot
for (r in 1:N.reps){
  x = runif(n, 0, 10)
  for (j in 1:2){
    sd.u = if (j == 1) 5 else x
    y  = 1 + beta1*x + rnorm(n, 0, sd.u)
    xd = x - mean(x)
    b1.hat = sum( xd*(y - mean(y)) )/sum(xd^2)
    u.hat  = (y - mean(y)) - b1.hat*xd
    se.homo   = sqrt( sum(u.hat^2)/(n-2)/sum(xd^2) )
    se.robust = sqrt( n/(n-2)*sum(xd^2*u.hat^2)/sum(xd^2)^2 )      # same as HC1
    t.crit = qt(0.975, n-2)
    cover[r, j, 1] = abs(b1.hat - beta1) <= t.crit*se.homo
    cover[r, j, 2] = abs(b1.hat - beta1) <= t.crit*se.robust
    if (j == 1) CI.store[r,] = b1.hat + c(-1,1)*t.crit*se.homo
  }
}
cat(" Coverage rates of nominal 95% confidence intervals for b1\n")
print(round(apply(cover, c(2,3), mean), 4))

# %% PLOT THE FIRST 100 CONFIDENCE INTERVALS (homoskedastic case, usual SE)
if (print2pdf == 1){ pdf( paste0(path2graphics, "Lab-04-CI Coverage.pdf"), width = 9, height = 4.5 ) }
fnt = 1.35 # font size
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 2, 2),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks

K = 100
miss = (CI.store[1:K,1] > beta1) | (CI.store[1:K,2] < beta1)
plot( NA, xlim = c(1, K), ylim = range(CI.store[1:K,]), xlab = "Sample number", ylab = expression("95% CI for " * beta[1]),
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
segments(1:K, CI.store[1:K,1], 1:K, CI.store[1:K,2], col = ifelse(miss, "red3", "steelblue"), lwd = 2)
abline(h = beta1, lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
if (print2pdf == 1){ par(op);dev.off() }
cat(sprintf(' Number of the first %d intervals that miss beta1: %d\n', K, sum(miss)))

# %% QUESTION 8) PREDICTED CHANGES IN A QUADRATIC REGRESSION
# Lecture 4, testscr on avginc
lm.testscr_avginc2 = lm(testscr ~ avginc + I(avginc^2), data = CASchools)
prnt.testscr_avginc2 = print_lm_results(lm.testscr_avginc2)
b = coef(lm.testscr_avginc2)
V = vcovHC(lm.testscr_avginc2, type = "HC1")
chg_df = data.frame()
for (x0 in c(5, 25, 45)){
  a  = c(0, 1, 2*x0 + 1)                  # Delta Y = b1 + b2*(2*x0 + 1) for Delta X = 1
  dY = sum(a*b)
  se = sqrt( t(a) %*% V %*% a )
  chg_df = rbind(chg_df, data.frame( From = x0, To = x0+1, Delta.Y = dY, SE = se,
                 CI.low = dY - 1.96*se, CI.up = dY + 1.96*se,
                 Derivative = b[2] + 2*b[3]*x0 ))   # calculus approximation f'(x0)*1
}
print(format(chg_df, digits = 4, nsmall = 4), row.names = FALSE)

# %% QUESTION 9) AGE AND EARNINGS
# SW2020 Empirical Exercise E8.2 (a) to (h) with CPS2015
CPS = read_xlsx("./data/CPS2015.xlsx")
lm.a = lm(ahe ~ age + female + bachelor, data = CPS)
lm.b = lm(log(ahe) ~ age + female + bachelor, data = CPS)
lm.c = lm(log(ahe) ~ log(age) + female + bachelor, data = CPS)
lm.d = lm(log(ahe) ~ age + I(age^2) + female + bachelor, data = CPS)
cat(" (a) AHE on Age, Female, Bachelor\n");                 prnt.a = print_lm_results(lm.a)
cat(" (b) ln(AHE) on Age, Female, Bachelor\n");             prnt.b = print_lm_results(lm.b)
cat(" (c) ln(AHE) on ln(Age), Female, Bachelor\n");         prnt.c = print_lm_results(lm.c)
cat(" (d) ln(AHE) on Age, Age^2, Female, Bachelor\n");      prnt.d = print_lm_results(lm.d)
# predicted changes when Age goes from 25 to 26 and from 33 to 34 (holding Female and Bachelor fixed)
age_df = data.frame( Model = c("(a) level-level ($)", "(b) log-linear (%)", "(c) log-log (%)", "(d) quadratic (%)"),
                     Age25to26 = c(pred_change(lm.a,25,FALSE), pred_change(lm.b,25,TRUE), pred_change(lm.c,25,TRUE), pred_change(lm.d,25,TRUE)),
                     Age33to34 = c(pred_change(lm.a,33,FALSE), pred_change(lm.b,33,TRUE), pred_change(lm.c,33,TRUE), pred_change(lm.d,33,TRUE)),
                     adj.R2    = c(prnt.a$Rbar2, prnt.b$Rbar2, prnt.c$Rbar2, prnt.d$Rbar2) )
print(format(age_df, digits = 4, nsmall = 4), row.names = FALSE)
t.age2 = coef(lm.d)[3]/sqrt(vcovHC(lm.d, type = "HC1")[3,3])
cat(sprintf(' Robust t-statistic on Age^2 in (d): %.4f\n', t.age2))

# %% (h) PLOT THE THREE ESTIMATED ln(AHE)-AGE PROFILES FOR MALES WITH A HIGH SCHOOL DIPLOMA
if (print2pdf == 1){ pdf( paste0(path2graphics, "Lab-04-Age Profiles.pdf"), width = 9, height = 5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 2, 2), oma = c(1, 1, 1, 1), tcl = -0.25)

agrd = seq(25, 34, length.out = 100)
nd   = data.frame(age = agrd, female = 0, bachelor = 0)
plot( agrd, predict(lm.b, nd), type = "l", lwd = 2, col = "steelblue", ylim = c(2.6, 2.95),
      xlab = "Age", ylab = "Predicted ln(AHE)", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(agrd, predict(lm.c, nd), lwd = 2, col = "red3", lty = 2)
lines(agrd, predict(lm.d, nd), lwd = 2, col = "green4", lty = 4)
set_axis_ticks(at1 = 25:34, tcl = .15, cex = fnt-.1)
legend("topleft", legend = c("(b) log-linear", "(c) log-log", "(d) quadratic"), col = c("steelblue", "red3", "green4"),
       lwd = 2, lty = c(1, 2, 4), bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
