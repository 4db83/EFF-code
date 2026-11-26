# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,sandwich,lmtest,car,AER,wooldridge)
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

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 6) TSLS WITH TWO INSTRUMENTS
# [W2020] Example 15.5 (mroz, working women)
data("mroz", package = "wooldridge")
mroz = subset(mroz, inlf == 1) 
n = nrow(mroz)
# a. first stage: educ on the exogenous regressors and both instruments, and the F-test of instrument strength
lm.first = lm(educ ~ exper + expersq + motheduc + fatheduc, data = mroz)
print(coeftest(lm.first))
F.homo    = linearHypothesis(lm.first, c("motheduc = 0", "fatheduc = 0"))
F.robust  = linearHypothesis(lm.first, c("motheduc = 0", "fatheduc = 0"), vcov. = vcov_robust(lm.first))
F.overall = summary(lm.first)$fstatistic[1]
cat(sprintf(' First stage F (instruments only): homoskedastic = %.4f, robust = %.4f;  overall regression F = %.4f\n',
            F.homo$F[2], F.robust$F[2], F.overall))
# b. second stage by hand: replace educ by its fitted value
mroz$educ.hat = fitted(lm.first)
lm.second = lm(lwage ~ educ.hat + exper + expersq, data = mroz)
# c. TSLS with ivreg, and OLS for comparison
iv.tsls = ivreg(lwage ~ educ + exper + expersq | exper + expersq + motheduc + fatheduc, data = mroz)
lm.ols  = lm(lwage ~ educ + exper + expersq, data = mroz)
# d. the correct residual variance uses educ (not educ.hat) with the TSLS coefficients
X       = cbind(1, mroz$educ, mroz$exper, mroz$expersq)
u.iv    = mroz$lwage - X %*% coef(iv.tsls)
u.2nd   = resid(lm.second)                              # = lwage - [1 educ.hat exper expersq] b
s.iv    = sqrt( sum(u.iv^2)/(n - 4) )
s.2nd   = sqrt( sum(u.2nd^2)/(n - 4) )
tsls.df = data.frame( b.second.stage  = coef(lm.second),
                      b.ivreg         = coef(iv.tsls),
                      b.OLS           = coef(lm.ols),
                      SE.second.stage = sqrt(diag(vcov(lm.second))),
                      SE.corrected    = sqrt(diag(vcov(lm.second)))*s.iv/s.2nd,
                      SE.ivreg        = sqrt(diag(vcov(iv.tsls))),
                      SE.ivreg.robust = sqrt(diag(vcov_robust(iv.tsls))),
                      SE.OLS          = sqrt(diag(vcov(lm.ols))) )
print(format(tsls.df, digits = 4, nsmall = 4))
cat(sprintf(' sigma_u from IV residuals (with educ) = %.6f, from second stage residuals (with educ.hat) = %.6f, ratio = %.4f\n',
            s.iv, s.2nd, s.iv/s.2nd))
# e. the GIVE formula of Lecture 8 (no exogenous regressors): b.1 = sum((x.hat - x.bar)(y - y.bar))/sum((x.hat - x.bar)(x - x.bar))
x     = mroz$educ
y     = mroz$lwage
x.hat = fitted(lm(educ ~ motheduc + fatheduc, data = mroz))
b.1.give  = sum( (x.hat - mean(x))*(y - mean(y)) )/sum( (x.hat - mean(x))*(x - mean(x)) )
iv.simple = ivreg(lwage ~ educ | motheduc + fatheduc, data = mroz)
cat(sprintf(' GIVE formula: b.1 = %.6f;  ivreg: b.1 = %.6f;  mean(x.hat) - mean(x) = %.2e\n',
            b.1.give, coef(iv.simple)[2], mean(x.hat) - mean(x)))

# %% QUESTION 7) TESTING FOR ENDOGENEITY
# [W2020] Example 15.7
# a. regression based (control function) version of the Hausman test
mroz$v.hat = resid(lm.first)                            # first stage residuals
lm.hausman = lm(lwage ~ educ + exper + expersq + v.hat, data = mroz)
print(coeftest(lm.hausman))
print(coeftest(lm.hausman, vcov = vcov_robust(lm.hausman)))
# c. the original Hausman (1978) statistic for the coefficient on educ (homoskedastic variances)
b.diff = coef(iv.tsls)["educ"] - coef(lm.ols)["educ"]
v.diff = vcov(iv.tsls)["educ", "educ"] - vcov(lm.ols)["educ", "educ"]
h.stat = b.diff^2/v.diff
t.v.hat = coeftest(lm.hausman)["v.hat", 3]
cat(sprintf(' Hausman h-statistic = %.4f, p-value (chi2 with 1 DF) = %.4f;  t^2 from the regression = %.4f\n',
            h.stat, 1 - pchisq(h.stat, 1), t.v.hat^2))

# %% QUESTION 8) TESTING THE OVERIDENTIFYING RESTRICTION
# [W2020] Example 15.8
mroz$u.tsls = resid(iv.tsls)                            # TSLS residuals (computed with educ)
lm.overid = lm(u.tsls ~ exper + expersq + motheduc + fatheduc, data = mroz)
nR2 = n*summary(lm.overid)$r.squared
cat(sprintf(' Overidentification test: nR2 = %.4f, p-value (chi2 with 2 - 1 = 1 DF) = %.4f\n', nR2, 1 - pchisq(nR2, 1)))
# all three diagnostics in one go
print(summary(iv.tsls, diagnostics = TRUE)$diagnostics)

# %% QUESTION 9) THE DEMAND FOR CIGARETTES
# [SW2020] Chapter 12 (AER data CigarettesSW, 48 U.S. states in 1995)
data("CigarettesSW", package = "AER")
Cig = subset(CigarettesSW, year == "1995")
Cig = transform(Cig,
                rprice   = price/cpi,                   # real price per pack (incl. taxes)
                rincome  = income/population/cpi,       # real per capita income
                salestax = (taxs - tax)/cpi,            # real general sales tax per pack
                cigtax   = tax/cpi)                     # real cigarette specific tax per pack
# b. exactly identified: log(rprice) instrumented by the sales tax ([SW2020] eq. 12.9)
iv.cig.1 = ivreg(log(packs) ~ log(rprice) | salestax, data = Cig)
print(coeftest(iv.cig.1, vcov = vcov_robust(iv.cig.1)))
# c. income as an exogenous regressor, one and two instruments
iv.cig.2 = ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax, data = Cig)
iv.cig.3 = ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax + cigtax, data = Cig)
print(coeftest(iv.cig.2, vcov = vcov_robust(iv.cig.2)))
print(coeftest(iv.cig.3, vcov = vcov_robust(iv.cig.3)))
lm.cig.first = lm(log(rprice) ~ log(rincome) + salestax + cigtax, data = Cig)
F.cig = linearHypothesis(lm.cig.first, c("salestax = 0", "cigtax = 0"), vcov. = vcov_robust(lm.cig.first))
cat(sprintf(' First stage robust F (salestax, cigtax) = %.4f\n', F.cig$F[2]))
print(summary(iv.cig.3, diagnostics = TRUE)$diagnostics)
lm.cig.ols = lm(log(packs) ~ log(rprice) + log(rincome), data = Cig)
cat(sprintf(' OLS price elasticity = %.4f\n', coef(lm.cig.ols)[2]))

#EOF
