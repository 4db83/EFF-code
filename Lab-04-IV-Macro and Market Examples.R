# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,haven,sandwich,lmtest,car,AER,wooldridge)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = c("Lab-04-IV-Anderson Rubin.pdf")
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# heteroskedasticity robust covariance matrix (HC1)
vcov_robust = function(fit) vcovHC(fit, type = "HC1")
# coefficient table with robust standard errors
coef_robust = function(fit) coeftest(fit, vcov = vcov_robust(fit))
# robust first stage F-statistic for the excluded instruments
first_stage_F = function(fit.first, instruments){
  linearHypothesis(fit.first, paste(instruments, "= 0"), vcov. = vcov_robust(fit.first))$F[2]
}
# QUESTION 18: Anderson-Rubin statistic for H0: beta = b, regress y - b x on the exogenous regressors and the
# instruments and test that the instruments have zero coefficients (robust F); valid however weak the instruments
anderson_rubin = function(b, y, x, data, exogenous, instruments){
  data$y.tilde = y - b*x
  fit = lm(reformulate(c(exogenous, instruments), "y.tilde"), data = data)
  linearHypothesis(fit, paste(instruments, "= 0"), vcov. = vcov_robust(fit))$F[2]
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 14) MONEY GROWTH, OUTPUT GROWTH AND INFLATION: THE QUANTITY THEORY OF MONEY
# [HGL2011] Exercise 10.3, Brumm (2005): 1995 data for 76 countries
br = as.data.frame(read_dta("./data/brumm.dta"))
br = transform(br, inflat = y, money = x, output = z)
cat(sprintf(" n = %d countries: mean inflation = %.2f%%, mean money growth = %.2f%%, mean output growth = %.2f%%\n",
            nrow(br), mean(br$inflat), mean(br$money), mean(br$output)))
# a. OLS, and tests of the strong (b1 = 0, b2 = 1, b3 = -1) and weak (b2 = 1, b3 = -1) hypotheses
ols.br = lm(inflat ~ money + output, data = br)
print(coef_robust(ols.br))
strong.H0 = c("(Intercept) = 0", "money = 1", "output = -1")
weak.H0   = c("money = 1", "output = -1")
test_hypotheses = function(fit){
  strong.homo   = linearHypothesis(fit, strong.H0)
  strong.robust = linearHypothesis(fit, strong.H0, vcov. = vcov_robust(fit))
  weak.homo     = linearHypothesis(fit, weak.H0)
  weak.robust   = linearHypothesis(fit, weak.H0, vcov. = vcov_robust(fit))
  last = function(tab) tab[2, ncol(tab)]
  stat = function(tab) tab[2, ncol(tab) - 1]
  round(c(strong.homo = stat(strong.homo), p = last(strong.homo), strong.robust = stat(strong.robust), p = last(strong.robust),
          weak.homo = stat(weak.homo), p = last(weak.homo), weak.robust = stat(weak.robust), p = last(weak.robust)), 4)
}
print(test_hypotheses(ols.br))
# b. heteroskedasticity related to money growth: regress the squared residuals on money (Breusch-Pagan type)
br$u2 = resid(ols.br)^2
bp.br = lm(u2 ~ money, data = br)
cat(sprintf(" Squared residuals on money: nR2 = %.3f (p = %.5f)\n", nrow(br)*summary(bp.br)$r.squared,
            1 - pchisq(nrow(br)*summary(bp.br)$r.squared, 1)))
# c. homoskedastic versus robust standard errors
print(round(cbind(se.homo = sqrt(diag(vcov(ols.br))), se.robust = sqrt(diag(vcov_robust(ols.br)))), 4))
# d. IV with initial, school, inv and poprate as instruments for output
instruments.br = c("initial", "school", "inv", "poprate")
iv.br = ivreg(inflat ~ money + output | money + initial + school + inv + poprate, data = br)
print(coef_robust(iv.br))
# e. the two hypotheses with the IV estimates
print(test_hypotheses(iv.br))
# f. regression based Hausman test with robust standard errors
first.br = lm(output ~ money + initial + school + inv + poprate, data = br)
br$v.hat = resid(first.br)
haus.br  = lm(inflat ~ money + output + v.hat, data = br)
print(round(coef_robust(haus.br)["v.hat", ], 4))
# g. test of the three overidentifying restrictions (nR2 of the IV residuals on all instruments)
br$u.iv = resid(iv.br)
overid.br = lm(u.iv ~ money + initial + school + inv + poprate, data = br)
nR2.br = nrow(br)*summary(overid.br)$r.squared
cat(sprintf(" Overidentification: nR2 = %.3f, df = 3, p = %.4f\n", nR2.br, 1 - pchisq(nR2.br, 3)))
# h. relevance: joint F-test of the instruments in the first stage
print(coef_robust(first.br))
cat(sprintf(" First stage F: homoskedastic = %.2f, robust = %.2f\n",
            linearHypothesis(first.br, paste(instruments.br, "= 0"))$F[2], first_stage_F(first.br, instruments.br)))
print(summary(iv.br, diagnostics = TRUE)$diagnostics)
# influence of the high inflation countries
cat(" Countries with inflation above 100%:", sum(br$inflat > 100), "; max inflation =", max(br$inflat), "\n")
iv.br.low = ivreg(inflat ~ money + output | money + initial + school + inv + poprate, data = subset(br, inflat < 100))
print(round(coef_robust(iv.br.low), 4))

# %% QUESTION 15) THE FISHER EFFECT: INTEREST RATES AND INFLATION
# [W2020] Computer Exercise C15.4 (data intdef, annual, 1948 - 2003)
data("intdef", package = "wooldridge")
fisher = subset(intdef, !is.na(inf_1))                       # omit the first year, as in the exercise
cat(sprintf(" n = %d years (%d - %d); corr(inf, inf_1) = %.4f\n", nrow(fisher), min(fisher$year), max(fisher$year),
            cor(fisher$inf, fisher$inf_1)))
# a. OLS of the 3 month T-bill rate on inflation
ols.fisher = lm(i3 ~ inf, data = fisher)
print(coef_robust(ols.fisher))
cat(" Newey-West standard errors (4 lags):", round(sqrt(diag(NeweyWest(ols.fisher, lag = 4, prewhite = FALSE))), 4), "\n")
# b. IV with lagged inflation
first.fisher = lm(inf ~ inf_1, data = fisher)
print(coef_robust(first.fisher))
iv.fisher = ivreg(i3 ~ inf | inf_1, data = fisher)
print(coef_robust(iv.fisher))
cat(" IV Newey-West standard errors (4 lags):", round(sqrt(diag(NeweyWest(iv.fisher, lag = 4, prewhite = FALSE))), 4), "\n")
cat(sprintf(" First stage robust F = %.2f\n", first_stage_F(first.fisher, "inf_1")))
# c. first differences
fisher$d.i3    = c(NA, diff(fisher$i3))
fisher$d.inf   = c(NA, diff(fisher$inf))
fisher$d.inf.1 = c(NA, head(fisher$d.inf, -1))
ols.d.fisher = lm(d.i3 ~ d.inf, data = fisher)
print(coef_robust(ols.d.fisher))
# d. is the lagged change in inflation a good instrument for the change in inflation?
first.d.fisher = lm(d.inf ~ d.inf.1, data = fisher)
print(coef_robust(first.d.fisher))
cat(sprintf(" First stage robust F (differences) = %.3f, R2 = %.4f\n", first_stage_F(first.d.fisher, "d.inf.1"),
            summary(first.d.fisher)$r.squared))
iv.d.fisher = ivreg(d.i3 ~ d.inf | d.inf.1, data = subset(fisher, !is.na(d.inf.1)))
print(coef_robust(iv.d.fisher))
cat(" Autocorrelations of i3 and inf (lags 1, 2):", round(acf(fisher$i3, plot = FALSE)$acf[2:3], 3), "/",
    round(acf(fisher$inf, plot = FALSE)$acf[2:3], 3), "\n")

# %% QUESTION 16) CONSUMPTION, INCOME AND INTEREST RATES: CAMPBELL AND MANKIW (1990)
# [W2020] Example 16.7 and Computer Exercise C16.4 (data consump, annual, 1959 - 1995)
data("consump", package = "wooldridge")
cm = subset(consump, !is.na(gc_1))
cat(sprintf(" n = %d years (%d - %d); mean gc = %.4f, mean gy = %.4f, mean r3 = %.3f\n", nrow(cm), min(cm$year), max(cm$year),
            mean(cm$gc), mean(cm$gy), mean(cm$r3)))
# a. OLS
ols.cm = lm(gc ~ gy + r3, data = cm)
print(coef_robust(ols.cm))
# b. IV with the first lags of all three variables (Example 16.7)
iv.cm = ivreg(gc ~ gy + r3 | gc_1 + gy_1 + r3_1, data = cm)
print(coeftest(iv.cm))
print(coef_robust(iv.cm))
first.cm.gy = lm(gy ~ gc_1 + gy_1 + r3_1, data = cm)
first.cm.r3 = lm(r3 ~ gc_1 + gy_1 + r3_1, data = cm)
cat(sprintf(" First stage robust F: gy = %.2f (R2 %.3f), r3 = %.2f (R2 %.3f)\n",
            first_stage_F(first.cm.gy, c("gc_1", "gy_1", "r3_1")), summary(first.cm.gy)$r.squared,
            first_stage_F(first.cm.r3, c("gc_1", "gy_1", "r3_1")), summary(first.cm.r3)$r.squared))
# c. test of the single overidentifying restriction (C16.4 i)
cm$u.iv = resid(iv.cm)
overid.cm = lm(u.iv ~ gc_1 + gy_1 + r3_1, data = cm)
nR2.cm = nrow(cm)*summary(overid.cm)$r.squared
cat(sprintf(" Overidentification: nR2 = %.3f, p = %.4f\n", nR2.cm, 1 - pchisq(nR2.cm, 1)))
print(summary(iv.cm, diagnostics = TRUE)$diagnostics)
# d. second lags as instruments (C16.4 ii-iii)
cm2 = subset(consump, !is.na(gc_2))
iv.cm2 = ivreg(gc ~ gy + r3 | gc_2 + gy_2 + r3_2, data = cm2)
print(coef_robust(iv.cm2))
first.cm2.gy = lm(gy ~ gc_2 + gy_2 + r3_2, data = cm2)
first.cm2.r3 = lm(r3 ~ gc_2 + gy_2 + r3_2, data = cm2)
cat(sprintf(" Second lags, first stage robust F: gy = %.2f (R2 %.3f), r3 = %.2f (R2 %.3f)\n",
            first_stage_F(first.cm2.gy, c("gc_2", "gy_2", "r3_2")), summary(first.cm2.gy)$r.squared,
            first_stage_F(first.cm2.r3, c("gc_2", "gy_2", "r3_2")), summary(first.cm2.r3)$r.squared))
# the share of rule-of-thumb consumers is the coefficient on gy; can gc be predicted from lagged data?
predict.gc = lm(gc ~ gc_1 + gy_1 + r3_1, data = cm)
cat(sprintf(" gc on the lagged variables: robust F = %.2f, R2 = %.3f\n",
            first_stage_F(predict.gc, c("gc_1", "gy_1", "r3_1")), summary(predict.gc)$r.squared))

# %% QUESTION 17) SUPPLY AND DEMAND FOR TRUFFLES
# [HGL2011] Sections 11.1 - 11.6 (data truffles: 30 market periods)
tr = as.data.frame(read_dta("./data/truffles.dta"))
cat(sprintf(" n = %d markets: mean price = %.2f, mean quantity = %.2f\n", nrow(tr), mean(tr$p), mean(tr$q)))
# a. reduced forms for Q and P on all exogenous variables
rf.q = lm(q ~ ps + di + pf, data = tr)
rf.p = lm(p ~ ps + di + pf, data = tr)
print(coeftest(rf.q))
print(coeftest(rf.p))
# b. demand: Q = a1 + a2 P + a3 PS + a4 DI + e_d, with PF (a supply shifter) as the instrument for P
iv.demand = ivreg(q ~ p + ps + di | ps + di + pf, data = tr)
print(coeftest(iv.demand))
#    supply: Q = b1 + b2 P + b3 PF + e_s, with PS and DI (demand shifters) as instruments for P
iv.supply = ivreg(q ~ p + pf | pf + ps + di, data = tr)
print(coeftest(iv.supply))
# c. OLS for comparison
print(round(cbind(OLS.demand = coef(lm(q ~ p + ps + di, data = tr)), IV.demand = coef(iv.demand)), 4))
print(round(cbind(OLS.supply = coef(lm(q ~ p + pf, data = tr)), IV.supply = coef(iv.supply)), 4))
# price elasticities at the means
cat(sprintf(" Elasticities at the means: demand %.3f, supply %.3f\n",
            coef(iv.demand)["p"]*mean(tr$p)/mean(tr$q), coef(iv.supply)["p"]*mean(tr$p)/mean(tr$q)))
# first stage F-statistics and the overidentification test of the supply equation
cat(sprintf(" First stage F: demand (pf) = %.2f; supply (ps, di) = %.2f\n",
            linearHypothesis(rf.p, "pf = 0")$F[2], linearHypothesis(rf.p, c("ps = 0", "di = 0"))$F[2]))
print(summary(iv.supply, diagnostics = TRUE)$diagnostics)
# d. indirect least squares for the demand equation: the effect of pf on q divided by its effect on p
cat(sprintf(" Indirect least squares: a2 = pi_q,pf/pi_p,pf = %.4f\n", coef(rf.q)["pf"]/coef(rf.p)["pf"]))

# %% QUESTION 18) THE FULTON FISH MARKET
# [HGL2011] Section 11.7 and Graddy (1995, 2006) (data fultonfish: 111 days, December 1991 - May 1992)
ff = as.data.frame(read_dta("./data/fultonfish.dta"))
days = c("mon", "tue", "wed", "thu")
cat(sprintf(" n = %d days; mean log quantity = %.3f, mean log price = %.3f; stormy days = %d, mixed = %d\n",
            nrow(ff), mean(ff$lquan), mean(ff$lprice), sum(ff$stormy), sum(ff$mixed)))
# a. OLS demand
ols.fish = lm(lquan ~ lprice + mon + tue + wed + thu, data = ff)
print(coef_robust(ols.fish))
# b. reduced forms: weather at sea (stormy) shifts supply, the day of the week dummies shift demand
rf.fish.q = lm(lquan ~ stormy + mon + tue + wed + thu, data = ff)
rf.fish.p = lm(lprice ~ stormy + mon + tue + wed + thu, data = ff)
print(coeftest(rf.fish.q))
print(coeftest(rf.fish.p))
F.stormy.homo = linearHypothesis(rf.fish.p, "stormy = 0")$F[2]
F.days.homo   = linearHypothesis(rf.fish.p, paste(days, "= 0"))
cat(sprintf(" Price equation: F for stormy = %.2f (robust %.2f); F for the day dummies = %.3f (p = %.4f, robust p = %.4f)\n",
            F.stormy.homo, first_stage_F(rf.fish.p, "stormy"), F.days.homo$F[2], F.days.homo$`Pr(>F)`[2],
            linearHypothesis(rf.fish.p, paste(days, "= 0"), vcov. = vcov_robust(rf.fish.p))$`Pr(>F)`[2]))
# c. 2SLS demand with stormy as the instrument ([HGL2011] Table 11.5)
iv.fish = ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy, data = ff)
print(coeftest(iv.fish))
print(coef_robust(iv.fish))
b.fish  = coef(iv.fish)["lprice"]
se.fish = sqrt(vcov_robust(iv.fish)["lprice", "lprice"])
cat(sprintf(" Demand elasticity %.3f, robust 95%% Wald CI [%.3f, %.3f]\n", b.fish, b.fish - 1.96*se.fish, b.fish + 1.96*se.fish))
cat(sprintf(" Indirect least squares: pi_q,stormy/pi_p,stormy = %.4f\n", coef(rf.fish.q)["stormy"]/coef(rf.fish.p)["stormy"]))
# d. mixed weather as a second instrument, with the overidentification test
iv.fish2 = ivreg(lquan ~ lprice + mon + tue + wed + thu | mon + tue + wed + thu + stormy + mixed, data = ff)
print(coef_robust(iv.fish2))
first.fish2 = lm(lprice ~ mon + tue + wed + thu + stormy + mixed, data = ff)
cat(sprintf(" First stage F (stormy, mixed): homoskedastic = %.2f, robust = %.2f\n",
            linearHypothesis(first.fish2, c("stormy = 0", "mixed = 0"))$F[2], first_stage_F(first.fish2, c("stormy", "mixed"))))
print(summary(iv.fish2, diagnostics = TRUE)$diagnostics)
# e. Anderson-Rubin confidence set for the specification in c.: all b with AR(b) below the 5% critical value of F(1, n - 6)
b.grid  = seq(-6, 3, by = 0.005)
AR.grid = sapply(b.grid, anderson_rubin, y = ff$lquan, x = ff$lprice, data = ff, exogenous = days, instruments = "stormy")
F.crit  = qf(0.95, 1, nrow(ff) - 6)
b.accepted = b.grid[AR.grid < F.crit]
cat(sprintf(" Anderson-Rubin 95%% confidence set: [%.3f, %.3f] (grid %.1f to %.1f; F critical value %.3f); AR(0) = %.2f\n",
            min(b.accepted), max(b.accepted), min(b.grid), max(b.grid), F.crit,
            anderson_rubin(0, ff$lquan, ff$lprice, ff, days, "stormy")))
cat(" Is the AR set one interval?", all(diff(b.accepted) < 0.0051), "\n")
cat(sprintf(" AR(0) equals the robust F of stormy in the quantity reduced form: %.2f\n", first_stage_F(rf.fish.q, "stormy")))

# %% PLOT THE ANDERSON-RUBIN STATISTIC AGAINST THE HYPOTHESISED ELASTICITY
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 9, height = 4.5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(2.6, .5, 0), mar = c(4, 4.5, 1, 1), oma = c(1, 1, 1, 1), tcl = -0.25)
plot( b.grid, AR.grid, type = "l", lwd = 2, col = "steelblue", xlab = "Hypothesised price elasticity b",
      ylab = "Anderson-Rubin F-statistic", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
abline(h = F.crit, col = "red3", lty = 2, lwd = 1.5)
abline(v = c(b.fish - 1.96*se.fish, b.fish + 1.96*se.fish), col = "green4", lty = 3, lwd = 1.5)
abline(v = b.fish, col = "green4", lwd = 1.5)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topleft", legend = c("AR statistic", "5% critical value", "2SLS estimate and Wald 95% CI"),
       col = c("steelblue", "red3", "green4"), lty = c(1, 2, 3), lwd = 2, bty = "o", bg = "white", cex = fnt-.3)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
