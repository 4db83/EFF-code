# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,lmtest,car,AER,wooldridge)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = ""   # this script makes no plots
path2graphics = "./graphics/"

# IN-FILE FUNCTION DEFINITIONS
# heteroskedasticity robust covariance matrix (HC1)
vcov_robust = function(fit) vcovHC(fit, type = "HC1")
# coefficient table with robust standard errors
coef_robust = function(fit) coeftest(fit, vcov = vcov_robust(fit))
# robust first stage F-statistic for the excluded instruments
first_stage_F = function(fit.first, instruments){
  hypotheses = paste(instruments, "= 0")
  linearHypothesis(fit.first, hypotheses, vcov. = vcov_robust(fit.first))$F[2]
}
# J-statistic (Lecture 8): regress the TSLS residuals on all exogenous variables and instruments, J = m F (homoskedastic F)
j_statistic = function(fit.iv, data, exogenous, instruments){
  data$u.tsls = resid(fit.iv)
  fit.u   = lm(reformulate(c(exogenous, instruments), "u.tsls"), data = data)
  F.homo  = linearHypothesis(fit.u, paste(instruments, "= 0"))$F[2]
  m       = length(instruments)
  J       = m*F.homo
  c(J = J, nR2 = nrow(data)*summary(fit.u)$r.squared)
}

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 10) THE DEMAND FOR CIGARETTES: TEN YEAR CHANGES
# [SW2020] Section 12.4, Table 12.1 and Exercise 12.1 (AER data CigarettesSW, 48 U.S. states, 1985 and 1995)
data("CigarettesSW", package = "AER")
Cig = transform(CigarettesSW,
                rprice   = price/cpi,                 # real price per pack (incl. taxes)
                rincome  = income/population/cpi,     # real per capita income
                salestax = (taxs - tax)/cpi,          # real general sales tax per pack
                cigtax   = tax/cpi)                   # real cigarette specific tax per pack
Cig.85 = subset(Cig, year == "1985")
Cig.95 = subset(Cig, year == "1995")
# a. ten year changes: the state specific (fixed) effects cancel
Dif = data.frame( d.log.packs   = log(Cig.95$packs)   - log(Cig.85$packs),
                  d.log.rprice  = log(Cig.95$rprice)  - log(Cig.85$rprice),
                  d.log.rincome = log(Cig.95$rincome) - log(Cig.85$rincome),
                  d.salestax    = Cig.95$salestax - Cig.85$salestax,
                  d.cigtax      = Cig.95$cigtax   - Cig.85$cigtax )
cat(sprintf(" n = %d states: mean change in log(packs) = %.4f, in log(rprice) = %.4f, in log(rincome) = %.4f\n",
            nrow(Dif), mean(Dif$d.log.packs), mean(Dif$d.log.rprice), mean(Dif$d.log.rincome)))
# b. TSLS with the sales tax, the cigarette tax, and both taxes as instruments (Table 12.1)
iv.cig.1 = ivreg(d.log.packs ~ d.log.rprice + d.log.rincome | d.log.rincome + d.salestax, data = Dif)
iv.cig.2 = ivreg(d.log.packs ~ d.log.rprice + d.log.rincome | d.log.rincome + d.cigtax, data = Dif)
iv.cig.3 = ivreg(d.log.packs ~ d.log.rprice + d.log.rincome | d.log.rincome + d.salestax + d.cigtax, data = Dif)
print(coef_robust(iv.cig.1))
print(coef_robust(iv.cig.2))
print(coef_robust(iv.cig.3))
first.cig.1 = lm(d.log.rprice ~ d.log.rincome + d.salestax, data = Dif)
first.cig.2 = lm(d.log.rprice ~ d.log.rincome + d.cigtax, data = Dif)
first.cig.3 = lm(d.log.rprice ~ d.log.rincome + d.salestax + d.cigtax, data = Dif)
print(coef_robust(first.cig.1))
cat(sprintf(" First stage robust F: (1) %.4f, (2) %.4f, (3) %.4f\n",
            first_stage_F(first.cig.1, "d.salestax"),
            first_stage_F(first.cig.2, "d.cigtax"),
            first_stage_F(first.cig.3, c("d.salestax", "d.cigtax"))))
J.cig = j_statistic(iv.cig.3, Dif, "d.log.rincome", c("d.salestax", "d.cigtax"))
cat(sprintf(" J-statistic (m F) = %.4f (p = %.4f); Sargan nR2 = %.4f\n", J.cig["J"], 1 - pchisq(J.cig["J"], 1), J.cig["nR2"]))
ols.cig = lm(d.log.packs ~ d.log.rprice + d.log.rincome, data = Dif)
print(coef_robust(ols.cig))
# 95% confidence intervals for the price elasticity
for (fit in list(iv.cig.1, iv.cig.2, iv.cig.3)){
  b  = coef(fit)["d.log.rprice"]
  se = sqrt(vcov_robust(fit)["d.log.rprice", "d.log.rprice"])
  cat(sprintf(" elasticity %.4f (%.4f), 95%% CI [%.4f, %.4f]\n", b, se, b - 1.96*se, b + 1.96*se))
}
# c. Exercise 12.1 a: a tax that raises the price from 7.50 to 8.00 dollars, column (1)
d.log.P   = log(8.00/7.50)
b.price   = coef(iv.cig.1)["d.log.rprice"]
se.price  = sqrt(vcov_robust(iv.cig.1)["d.log.rprice", "d.log.rprice"])
d.log.Q   = b.price*d.log.P
se.d.log.Q = se.price*d.log.P
cat(sprintf(" Price change of %.4f log points: predicted change in log(packs) = %.4f (%.2f%%), 95%% CI [%.2f%%, %.2f%%]\n",
            d.log.P, d.log.Q, 100*d.log.Q, 100*(d.log.Q - 1.96*se.d.log.Q), 100*(d.log.Q + 1.96*se.d.log.Q)))
#    Exercise 12.1 b: income falls by 2%
b.income  = coef(iv.cig.1)["d.log.rincome"]
se.income = sqrt(vcov_robust(iv.cig.1)["d.log.rincome", "d.log.rincome"])
d.log.income = log(0.98)
d.income.lower = 100*(b.income + 1.96*se.income)*d.log.income      # d.log.income < 0: the upper b gives the lower bound
d.income.upper = 100*(b.income - 1.96*se.income)*d.log.income
cat(sprintf(" Income falls by 2%%: predicted change in demand = %.2f%% (95%% CI [%.2f%%, %.2f%%])\n",
            100*b.income*d.log.income, d.income.lower, d.income.upper))
#    the 1995 cross section of Question 9 for comparison (short run variation)
iv.cs = ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax, data = Cig.95)
cat(sprintf(" 1995 cross section with the sales tax: elasticity = %.3f\n", coef(iv.cs)[2]))

# %% QUESTION 11) PROXIMITY TO COLLEGE AS AN INSTRUMENT FOR EDUCATION
# [W2020] Example 15.4 and Computer Exercises C15.3 and C15.5, Card (1995) (data card)
data("card", package = "wooldridge")
regions   = paste0("reg66", 2:9)
exogenous = c("exper", "expersq", "black", "smsa", "south", "smsa66", regions)
cat(sprintf(" n = %d men; share that grew up near a 4-year college = %.4f\n", nrow(card), mean(card$nearc4)))
# a. OLS and the first stage
ols.card   = lm(reformulate(c("educ", exogenous), "lwage"), data = card)
first.card = lm(reformulate(c("nearc4", exogenous), "educ"), data = card)
print(round(coef_robust(ols.card)["educ", ], 5))
print(round(coef_robust(first.card)["nearc4", ], 5))
cat(sprintf(" First stage robust F for nearc4 = %.2f\n", first_stage_F(first.card, "nearc4")))
# b. IV with nearc4 as the instrument for educ
formula.iv = as.formula(paste("lwage ~", paste(c("educ", exogenous), collapse = " + "),
                              "|", paste(c("nearc4", exogenous), collapse = " + ")))
iv.card = ivreg(formula.iv, data = card)
print(round(coef(summary(iv.card))["educ", ], 5))
print(round(coef_robust(iv.card)["educ", ], 5))
b.iv  = coef(iv.card)["educ"]
se.iv = sqrt(vcov_robust(iv.card)["educ", "educ"])
cat(sprintf(" IV return to education %.4f, robust 95%% CI [%.3f, %.3f]; OLS %.4f\n",
            b.iv, b.iv - 1.96*se.iv, b.iv + 1.96*se.iv, coef(ols.card)["educ"]))
# c. is nearc4 related to ability? IQ on nearc4, without and with the geographic controls (C15.3)
card.iq = subset(card, !is.na(IQ))
iq.simple   = lm(IQ ~ nearc4, data = card.iq)
iq.controls = lm(reformulate(c("nearc4", "smsa66", regions), "IQ"), data = card.iq)
cat(sprintf(" IQ available for %d men\n", nrow(card.iq)))
print(round(coef_robust(iq.simple)["nearc4", ], 5))
print(round(coef_robust(iq.controls)["nearc4", ], 5))
# d. regression based Hausman test (C15.5 i), and nearc2 as a second instrument with the J test (C15.5 ii-iii)
card$v.hat = resid(first.card)
haus.card  = lm(reformulate(c("educ", exogenous, "v.hat"), "lwage"), data = card)
print(round(coef_robust(haus.card)["v.hat", ], 5))
formula.iv2 = as.formula(paste("lwage ~", paste(c("educ", exogenous), collapse = " + "),
                               "|", paste(c("nearc4", "nearc2", exogenous), collapse = " + ")))
iv.card2 = ivreg(formula.iv2, data = card)
print(round(coef_robust(iv.card2)["educ", ], 5))
first.card2 = lm(reformulate(c("nearc4", "nearc2", exogenous), "educ"), data = card)
cat(sprintf(" First stage robust F (nearc4, nearc2) = %.2f\n", first_stage_F(first.card2, c("nearc4", "nearc2"))))
J.card = j_statistic(iv.card2, card, exogenous, c("nearc4", "nearc2"))
cat(sprintf(" J-statistic = %.3f (p = %.4f), Sargan nR2 = %.3f\n", J.card["J"], 1 - pchisq(J.card["J"], 1), J.card["nR2"]))
print(summary(iv.card2, diagnostics = TRUE)$diagnostics)

# %% QUESTION 12) 401(k) PLANS AND INDIVIDUAL RETIREMENT ACCOUNTS
# [W2020] Computer Exercise C15.8 (data k401ksubs): is there a trade-off between the two ways of saving?
data("k401ksubs", package = "wooldridge")
cat(sprintf(" n = %d households: share with an IRA = %.4f, eligible for 401(k) = %.4f, participating = %.4f\n",
            nrow(k401ksubs), mean(k401ksubs$pira), mean(k401ksubs$e401k), mean(k401ksubs$p401k)))
cat(sprintf(" share of eligible households that participate = %.4f; participants that are not eligible = %d\n",
            mean(k401ksubs$p401k[k401ksubs$e401k == 1]), sum(k401ksubs$p401k == 1 & k401ksubs$e401k == 0)))
# a. linear probability model by OLS
ols.ira = lm(pira ~ p401k + inc + incsq + age + agesq, data = k401ksubs)
print(coef_robust(ols.ira))
# b. reduced form (first stage) for p401k
first.ira = lm(p401k ~ e401k + inc + incsq + age + agesq, data = k401ksubs)
print(round(coef_robust(first.ira)["e401k", ], 5))
cat(sprintf(" First stage robust F = %.1f\n", first_stage_F(first.ira, "e401k")))
# c. IV with e401k as the instrument for p401k
iv.ira = ivreg(pira ~ p401k + inc + incsq + age + agesq | e401k + inc + incsq + age + agesq, data = k401ksubs)
print(coef_robust(iv.ira))
# d. robust regression based test of the exogeneity of p401k
k401ksubs$v.hat = resid(first.ira)
haus.ira = lm(pira ~ p401k + inc + incsq + age + agesq + v.hat, data = k401ksubs)
print(round(coef_robust(haus.ira)["v.hat", ], 5))
# predicted probability of an IRA at the means, and the IV effect relative to it
cat(sprintf(" Mean of pira among participants = %.4f, among non-participants = %.4f\n",
            mean(k401ksubs$pira[k401ksubs$p401k == 1]), mean(k401ksubs$pira[k401ksubs$p401k == 0])))

# %% QUESTION 13) DO VIOLENT MOVIES LEAD TO VIOLENT BEHAVIOUR?
# [SW2020] Empirical Exercise E12.2, Dahl and DellaVigna (2009): 516 weekends, 1995 - 2004
mv = as.data.frame(read_xlsx("./data/Movies.xlsx"))
mv$ln.assaults = log(mv$assaults)
mv$attend      = mv$attend_v + mv$attend_m + mv$attend_n
year.dummies   = paste0("year", 2:10)
month.dummies  = paste0("month", 2:12)
holidays       = c("h_chris", "h_newyr", "h_easter", "h_july4", "h_mem", "h_labor")
weather        = c("w_maxa", "w_maxb", "w_maxc", "w_mina", "w_minb", "w_minc", "w_rain", "w_snow")
controls       = c(year.dummies, month.dummies, holidays, weather)
endogenous     = c("attend_v", "attend_m", "attend_n")
cat(sprintf(" n = %d weekends; mean assaults = %.1f; mean attendance (millions): violent %.2f, mild %.2f, nonviolent %.2f\n",
            nrow(mv), mean(mv$assaults), mean(mv$attend_v), mean(mv$attend_m), mean(mv$attend_n)))
# a. seasonality: joint robust F-tests of the month dummies
season.assaults = lm(reformulate(c(year.dummies, month.dummies), "ln.assaults"), data = mv)
season.attend   = lm(reformulate(c(year.dummies, month.dummies), "attend"), data = mv)
F.month.assaults = linearHypothesis(season.assaults, paste(month.dummies, "= 0"), vcov. = vcov_robust(season.assaults))
F.month.attend   = linearHypothesis(season.attend,   paste(month.dummies, "= 0"), vcov. = vcov_robust(season.attend))
cat(sprintf(" Month dummies: assaults F = %.2f (p = %.6f); attendance F = %.2f (p = %.6f)\n",
            F.month.assaults$F[2], F.month.assaults$`Pr(>F)`[2], F.month.attend$F[2], F.month.attend$`Pr(>F)`[2]))
print(round(coef(season.assaults)[month.dummies], 3))
print(round(coef(season.attend)[month.dummies], 3))
# b. OLS with all controls
ols.mv = lm(reformulate(c(endogenous, controls), "ln.assaults"), data = mv)
print(coef_robust(ols.mv)[endogenous, ])
# effect of the blockbuster: +6 million violent, -2 million mild, -1 million nonviolent (a'beta)
a.blockbuster = c(6, -2, -1)
blockbuster_effect = function(fit){
  b  = coef(fit)[endogenous]
  V  = vcov_robust(fit)[endogenous, endogenous]
  effect = sum(a.blockbuster*b)
  se     = sqrt( as.numeric(t(a.blockbuster) %*% V %*% a.blockbuster) )
  c(effect = effect, se = se, lower = effect - 1.96*se, upper = effect + 1.96*se)
}
equal_effects = function(fit){
  test.vm = linearHypothesis(fit, "attend_v = attend_m", vcov. = vcov_robust(fit))
  test.vn = linearHypothesis(fit, "attend_v = attend_n", vcov. = vcov_robust(fit))
  test.all = linearHypothesis(fit, c("attend_v = attend_m", "attend_v = attend_n"), vcov. = vcov_robust(fit))
  c(p.v.eq.m = test.vm[2, ncol(test.vm)], p.v.eq.n = test.vn[2, ncol(test.vn)], p.all.equal = test.all[2, ncol(test.all)])
}
print(round(blockbuster_effect(ols.mv), 4))
print(round(equal_effects(ols.mv), 4))
# c. IV with the predicted attendance from historical patterns
instruments.pr = c("pr_attend_v", "pr_attend_m", "pr_attend_n")
iv_movies = function(instruments){
  f = as.formula(paste("ln.assaults ~", paste(c(endogenous, controls), collapse = " + "),
                       "|", paste(c(instruments, controls), collapse = " + ")))
  ivreg(f, data = mv)
}
iv.mv.pr = iv_movies(instruments.pr)
print(coef_robust(iv.mv.pr)[endogenous, ])
print(round(blockbuster_effect(iv.mv.pr), 4))
print(round(equal_effects(iv.mv.pr), 4))
# first stage F-statistics for each endogenous regressor
first_stages = function(instruments){
  sapply(endogenous, function(v) first_stage_F(lm(reformulate(c(instruments, controls), v), data = mv), instruments))
}
print(round(first_stages(instruments.pr), 1))
# d. IV with attendance in the following and preceding weekends
instruments.fb = c("attend_v_f", "attend_m_f", "attend_n_f", "attend_v_b", "attend_m_b", "attend_n_b")
iv.mv.fb = iv_movies(instruments.fb)
print(coef_robust(iv.mv.fb)[endogenous, ])
print(round(blockbuster_effect(iv.mv.fb), 4))
print(round(equal_effects(iv.mv.fb), 4))
print(round(first_stages(instruments.fb), 1))
# e. all nine instruments and the J test of the six overidentifying restrictions
instruments.all = c(instruments.pr, instruments.fb)
iv.mv.all = iv_movies(instruments.all)
print(coef_robust(iv.mv.all)[endogenous, ])
J.mv = j_statistic(iv.mv.all, mv, controls, instruments.all)
cat(sprintf(" J-statistic = %.3f, df = 6, p = %.4f; Sargan nR2 = %.3f\n", J.mv["J"], 1 - pchisq(J.mv["J"], 6), J.mv["nR2"]))
print(round(blockbuster_effect(iv.mv.all), 4))

#EOF
