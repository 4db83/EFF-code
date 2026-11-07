# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,lmtest,car)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 10) INTERACTIONS
# SW2020 Empirical Exercise E8.2 (i) to (l) with CPS2015
CPS = read_xlsx("./data/CPS2015.xlsx")
# i. female x bachelor interaction
lm.i = lm(log(ahe) ~ age + I(age^2) + female + bachelor + female:bachelor, data = CPS)
prnt.i = print_lm_results(lm.i)
people = data.frame(Name = c("Alexis", "Jane", "Bob", "Jim"), age = 30,
                    female = c(1, 1, 0, 0), bachelor = c(1, 0, 1, 0) )
people$ln.AHE.hat = predict(lm.i, people)
people$AHE.hat    = exp(people$ln.AHE.hat)
print(format(people, digits = 4, nsmall = 4), row.names = FALSE)
cat(sprintf(' Alexis - Jane = %.4f (log points), Bob - Jim = %.4f, difference = %.4f = interaction coefficient\n',
            people$ln.AHE.hat[1] - people$ln.AHE.hat[2], people$ln.AHE.hat[3] - people$ln.AHE.hat[4],
            (people$ln.AHE.hat[1] - people$ln.AHE.hat[2]) - (people$ln.AHE.hat[3] - people$ln.AHE.hat[4])))

# j. does the effect of Age differ by gender? interact Age and Age^2 with Female, F-test (robust)
lm.j = lm(log(ahe) ~ age + I(age^2) + female + bachelor + female:age + female:I(age^2), data = CPS)
F.j  = linearHypothesis(lm.j, c("age:female = 0", "I(age^2):female = 0"), vcov. = vcovHC(lm.j, type = "HC1"))
print(F.j)
# k. does the effect of Age differ by education?
lm.k = lm(log(ahe) ~ age + I(age^2) + female + bachelor + bachelor:age + bachelor:I(age^2), data = CPS)
F.k  = linearHypothesis(lm.k, c("age:bachelor = 0", "I(age^2):bachelor = 0"), vcov. = vcovHC(lm.k, type = "HC1"))
print(F.k)
# a simpler linear version of (j) and (k): one interaction each
lm.jk = lm(log(ahe) ~ age + female + bachelor + female:age + bachelor:age, data = CPS)
print( coeftest(lm.jk, vcov = vcovHC(lm.jk, type = "HC1")) )

# %% QUESTION 11) OMITTED VARIABLE BIAS
# California test scores (Lectures 5 and 7)
CASchools = read_xlsx("./data/caschool.xlsx")
lm.short = lm(testscr ~ str,          data = CASchools)
lm.long  = lm(testscr ~ str + el_pct, data = CASchools)
lm.aux   = lm(el_pct  ~ str,          data = CASchools)    # auxiliary regression of omitted on included regressor
prnt.long = print_lm_results(lm.long)
b.short = coef(lm.short)[2];  b.long = coef(lm.long)[2:3];  d1 = coef(lm.aux)[2]
cat(sprintf(' Short regression slope       = %.6f\n', b.short))
cat(sprintf(' Long slope + b2*delta1       = %.6f + (%.6f)*(%.6f) = %.6f\n', b.long[1], b.long[2], d1, b.long[1] + b.long[2]*d1))
cat(sprintf(' Corr(str, el_pct) = %.4f;  Cov/Var = %.6f\n', cor(CASchools$str, CASchools$el_pct),
            cov(CASchools$str, CASchools$el_pct)/var(CASchools$str)))

# %% QUESTION 12) JOINT HYPOTHESES AND THE F-STATISTIC
# SW2020 Section 7.2
CASchools$expn = CASchools$expn_stu/1000                    # expenditure per pupil in $1000s
lm.U = lm(testscr ~ str + expn + el_pct, data = CASchools)  # unrestricted
lm.R = lm(testscr ~ el_pct,              data = CASchools)  # restricted: beta_str = beta_expn = 0
prnt.U = print_lm_results(lm.U)
# a. robust F-statistic with linearHypothesis()
F.robust = linearHypothesis(lm.U, c("str = 0", "expn = 0"), vcov. = vcovHC(lm.U, type = "HC1"))
print(F.robust)
# b. robust F by hand from the two t-statistics and their correlation (Lecture 5, q = 2)
V   = vcovHC(lm.U, type = "HC1")
t1  = coef(lm.U)["str"] /sqrt(V["str","str"])
t2  = coef(lm.U)["expn"]/sqrt(V["expn","expn"])
rho = V["str","expn"]/sqrt(V["str","str"]*V["expn","expn"])   # correlation of the two estimators (= of t1, t2)
F.hand = 0.5*(t1^2 + t2^2 - 2*rho*t1*t2)/(1 - rho^2)
cat(sprintf(' t1 = %.4f, t2 = %.4f, rho = %.4f, F by hand = %.4f\n', t1, t2, rho, F.hand))
# c. homoskedasticity-only F from SSRs and from R2s
q  = 2;  n = nrow(CASchools);  k.U = 3
SSR.U = sum(resid(lm.U)^2);  SSR.R = sum(resid(lm.R)^2)
R2.U  = summary(lm.U)$r.squared;  R2.R = summary(lm.R)$r.squared
F.SSR = ((SSR.R - SSR.U)/q)/(SSR.U/(n - k.U - 1))
F.R2  = ((R2.U - R2.R)/q)/((1 - R2.U)/(n - k.U - 1))
cat(sprintf(' Homoskedastic F: from SSRs = %.4f, from R2s = %.4f, linearHypothesis = %.4f\n', F.SSR, F.R2,
            linearHypothesis(lm.U, c("str = 0", "expn = 0"))$F[2]))
cat(sprintf(' 5%% critical value of F(2,Inf) = %.4f, of F(2,%d) = %.4f\n', qchisq(.95, 2)/2, n-k.U-1, qf(.95, 2, n-k.U-1)))

# %% QUESTION 13) THE SIZE OF THE "ONE AT A TIME" TEST
# simulation under H0: beta1 = beta2 = 0
set.seed(5)
N.reps = 1e4
n      = 200
size_df = data.frame()
for (rho.x in c(0, 0.8, -0.8)){
  rej = matrix(NA, N.reps, 2)
  for (r in 1:N.reps){
    x1 = rnorm(n)
    x2 = rho.x*x1 + sqrt(1 - rho.x^2)*rnorm(n)
    y  = 1 + rnorm(n)                                  # H0 is true: y does not depend on x1, x2
    X  = cbind(1, x1, x2)
    XX.inv = solve(crossprod(X))
    b  = XX.inv %*% crossprod(X, y)
    u  = y - X %*% b
    Vb = sum(u^2)/(n - 3)*XX.inv
    t  = b[2:3]/sqrt(diag(Vb)[2:3])
    F  = 0.5*t(b[2:3]) %*% solve(Vb[2:3,2:3]) %*% b[2:3]
    rej[r,1] = any(abs(t) > 1.96)                      # one at a time
    rej[r,2] = F > qf(0.95, 2, n - 3)                  # F-test
  }
  size_df = rbind(size_df, data.frame( Corr.X1.X2 = rho.x, One.at.a.time = mean(rej[,1]), F.test = mean(rej[,2]) ))
}
print(format(size_df, digits = 4, nsmall = 4), row.names = FALSE)

# %% QUESTION 14) MULTIPLE REGRESSION IN MATRIX FORM
# the regression of Question 12
y = CASchools$testscr
X = cbind(1, CASchools$str, CASchools$expn, CASchools$el_pct)
colnames(X) = c("Constant", "str", "expn", "el_pct")
n = nrow(X); K = ncol(X)
XX     = t(X) %*% X
b.hat  = solve(XX) %*% t(X) %*% y               # (X'X)^-1 X'y
u.hat  = as.vector(y - X %*% b.hat)
s2     = sum(u.hat^2)/(n - K)
V.homo = s2*solve(XX)                           # homoskedastic: s^2 (X'X)^-1
meat   = t(X) %*% diag(u.hat^2) %*% X           # sum of u_i^2 x_i x_i'
V.HC1  = n/(n - K)*solve(XX) %*% meat %*% solve(XX)
mat_df = data.frame(b.matrix = as.vector(b.hat), b.lm = coef(lm.U),
                    SE.homo = sqrt(diag(V.homo)), SE.homo.lm = sqrt(diag(vcov(lm.U))),
                    SE.HC1 = sqrt(diag(V.HC1)),  SE.HC1.sandwich = sqrt(diag(vcovHC(lm.U, type = "HC1"))) )
print(format(mat_df,digits = 6, nsmall = 6))

# %% QUESTION 15) MULTICOLLINEARITY
# a. perfect multicollinearity: percentage of English speakers = 100 - el_pct
CASchools$pct_es = 100 - CASchools$el_pct
lm.perfect = lm(testscr ~ str + el_pct + pct_es, data = CASchools)
print(coef(lm.perfect))
X.p = cbind(1, CASchools$str, CASchools$el_pct, CASchools$pct_es)
cat(sprintf(' rank of X\'X = %d (K = %d), reciprocal condition number = %.2e\n', qr(crossprod(X.p))$rank, ncol(X.p), rcond(crossprod(X.p))))
# b. imperfect multicollinearity: sd of b1 as Corr(X1,X2) -> 1
set.seed(6)
N.reps = 5000; n = 100
mc_df = data.frame()
for (rho.x in c(0, 0.5, 0.9, 0.99)){
  b1 = numeric(N.reps)
  for (r in 1:N.reps){
    x1 = rnorm(n); x2 = rho.x*x1 + sqrt(1 - rho.x^2)*rnorm(n)
    y  = 1 + x1 + x2 + rnorm(n)
    b1[r] = coef(lm.fit(cbind(1, x1, x2), y))[2]
  }
  mc_df = rbind(mc_df, data.frame( Corr.X1.X2 = rho.x, Mean.b1 = mean(b1), SD.b1 = sd(b1),
                SD.ratio = sd(b1)/ifelse(rho.x == 0, sd(b1), mc_df$SD.b1[1]),
                Theory.ratio = 1/sqrt(1 - rho.x^2) ))     # square root of the variance inflation factor
}
print(format(mc_df, digits = 4, nsmall = 4), row.names = FALSE)

#EOF
