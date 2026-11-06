# %% CLEAR SCREEN AND WORKSPACE
rm(list = ls()); gc(); cat("\014"); 
# SET DEFAULTS: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# INSTALL PACMAN: if not installed (note: may need to disable windows firewall for packages to install)
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD/INSTALL: other required packages
pacman::p_load(dplyr,readxl,AER)
# LOAD HELPER FUNCTIONS STORED IN functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 1
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# %% Question 1) JOINT, MARGINAL AND CONDITIONAL DISTRIBUTIONS (table from Calc-1, Question 6)
# a. enter the joint PMF: rows are X = 1,5,8, columns are Y = 14,22,30,40,65
x.vals = c(1, 5, 8)
y.vals = c(14, 22, 30, 40, 65)
P.xy   = matrix(c(0.02, 0.05, 0.10, 0.03, 0.01,
                  0.17, 0.15, 0.05, 0.02, 0.01,
                  0.02, 0.03, 0.15, 0.10, 0.09), nrow = 3, byrow = TRUE,
                  dimnames = list(paste0("X=",x.vals), paste0("Y=",y.vals)) )
cat(sprintf(' Sum of joint probabilities: %.4f\n', sum(P.xy)))
p.x = rowSums(P.xy)    # marginal of X
p.y = colSums(P.xy)    # marginal of Y
print(rbind(P.xy, "Pr(Y=y)" = p.y))

# b. moments of Y, and conditional moments of Y given each value of X
E.y   = sum(y.vals*p.y)
Var.y = sum(y.vals^2*p.y) - E.y^2
P.y_x = P.xy/p.x                      # each row divided by Pr(X=x): conditional PMFs of Y given X
E.y_x   = as.vector(P.y_x %*% y.vals)
Var.y_x = as.vector(P.y_x %*% y.vals^2) - E.y_x^2
cond_df = data.frame(X = x.vals, PrX = p.x, E_Y_given_X = E.y_x, Var_Y_given_X = Var.y_x)
cat(sprintf(' E(Y) = %.4f   Var(Y) = %.4f\n', E.y, Var.y))
print(format(cond_df, digits = 6, nsmall = 4), row.names = FALSE)

# c. covariance and correlation
E.x   = sum(x.vals*p.x)
Var.x = sum(x.vals^2*p.x) - E.x^2
E.xy  = sum( outer(x.vals, y.vals)*P.xy )
Cov.xy  = E.xy - E.x*E.y
Corr.xy = Cov.xy/sqrt(Var.x*Var.y)
cat(sprintf(' E(X) = %.4f  Var(X) = %.4f  Cov(X,Y) = %.4f  Corr(X,Y) = %.4f\n', E.x, Var.x, Cov.xy, Corr.xy))

# d. law of iterated expectations and law of total variance
cat(sprintf(' LIE: E[E(Y|X)] = %.4f  vs  E(Y) = %.4f\n', sum(E.y_x*p.x), E.y))
EVar = sum(Var.y_x*p.x)                      # E[Var(Y|X)]
VarE = sum(E.y_x^2*p.x) - sum(E.y_x*p.x)^2   # Var[E(Y|X)]
cat(sprintf(' E[Var(Y|X)] + Var[E(Y|X)] = %.4f + %.4f = %.4f  vs  Var(Y) = %.4f\n', EVar, VarE, EVar + VarE, Var.y))

# e. simulate from the joint distribution and compare sample with population moments
set.seed(1)
n.sim = 1e5
cell  = sample(1:15, n.sim, replace = TRUE, prob = as.vector(t(P.xy)))   # cells ordered row by row
X.sim = rep(x.vals, each = 5)[cell]
Y.sim = rep(y.vals, times = 3)[cell]
sim_df = data.frame(Moment     = c("E(Y)", "Var(Y)", "E(Y|X=8)", "Var(Y|X=8)", "Corr(X,Y)"),
                    Population = c(E.y, Var.y, E.y_x[3], Var.y_x[3], Corr.xy),
                    Simulation = c(mean(Y.sim), mean((Y.sim-mean(Y.sim))^2), mean(Y.sim[X.sim==8]),
                                    mean((Y.sim[X.sim==8]-mean(Y.sim[X.sim==8]))^2), cor(X.sim, Y.sim)) )
print(format(sim_df, digits = 6, nsmall = 4), row.names = FALSE)

# %% Question 2) AGE AND HOURLY EARNINGS: CPS2015 (adapted from SW2020 Empirical Exercise E2.1)
CPS = read_xlsx("./data/CPS2015.xlsx")
# a. marginal distribution of Age
p.age = prop.table(table(CPS$age))
print(round(p.age, 4))
# b. conditional mean of AHE given Age
E.ahe_age = tapply(CPS$ahe, CPS$age, mean)
print(round(E.ahe_age, 4))
lm.ahe_age = lm(E.ahe_age ~ as.numeric(names(E.ahe_age)))   # slope of the conditional means
cat(sprintf(' Conditional means rise by about $%.3f per year of age\n', coef(lm.ahe_age)[2]))

# c. law of iterated expectations
cat(sprintf(' LIE: sum Pr(Age=a)E(AHE|Age=a) = %.4f  vs  mean(AHE) = %.4f\n', sum(p.age*E.ahe_age), mean(CPS$ahe)))

# d. variance, covariance and correlation (population formulas, i.e., divide by n)
pvar = function(x) mean((x - mean(x))^2)
Var.ahe = pvar(CPS$ahe)
Cov.ahe = mean( (CPS$ahe - mean(CPS$ahe))*(CPS$age - mean(CPS$age)) )
Cor.ahe = Cov.ahe/sqrt(Var.ahe*pvar(CPS$age))
cat(sprintf(' Var(AHE) = %.4f   Cov(AHE,Age) = %.4f   Corr(AHE,Age) = %.4f\n', Var.ahe, Cov.ahe, Cor.ahe))

# e. law of total variance: how much of Var(AHE) is due to differences in E(AHE|Age)?
Var.ahe_age = tapply(CPS$ahe, CPS$age, pvar)
EVar.ahe = sum(p.age*Var.ahe_age)
VarE.ahe = sum(p.age*(E.ahe_age - mean(CPS$ahe))^2)
cat(sprintf(' E[Var(AHE|Age)] = %.4f  Var[E(AHE|Age)] = %.4f  sum = %.4f  Var(AHE) = %.4f\n', EVar.ahe, VarE.ahe, EVar.ahe+VarE.ahe, Var.ahe))
cat(sprintf(' Share of Var(AHE) explained by Age: %.4f%%\n', 100*VarE.ahe/Var.ahe))

# f. conditional means by Age and gender
E.ahe_age_fem = tapply(CPS$ahe, list(CPS$age, CPS$female), mean)
colnames(E.ahe_age_fem) = c("Male", "Female")
print(round(E.ahe_age_fem, 4))

# PLOT CONDITIONAL MEANS
if (print2pdf == 1){ pdf( paste0(path2graphics, "Lab-01-AHE by Age.pdf"), width = 9, height = 5 ) }
fnt = 1.35 # font size
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 2, 2),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks

ages = as.numeric(names(E.ahe_age))
plot( ages, E.ahe_age, type = "b", pch = 16, lwd = 2, col = "steelblue", ylim = c(14, 28),
      xlab = "Age", ylab = "E(AHE | Age)", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
lines(ages, E.ahe_age_fem[,"Male"],   type = "b", pch = 15, lwd = 2, col = "red3", lty = 2)
lines(ages, E.ahe_age_fem[,"Female"], type = "b", pch = 17, lwd = 2, col = "green4", lty = 2)
set_axis_ticks(at1 = ages, tcl = .15, cex = fnt-.1)
legend("topleft", legend = c("All", "Male", "Female"), col = c("steelblue", "red3", "green4"),
        pch = c(16, 15, 17), lty = c(1, 2, 2), lwd = 2, bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

# %% Question 3) MOMENTS OF DAILY STOCK RETURNS: NYSE Composite Index 1990-2005 (AER data NYSESW)
data("NYSESW", package = "AER")
r = 100*diff(log(as.numeric(NYSESW)))    # daily log returns in percent
n = length(r)
# a./b. sample moments, using the population-moment definitions from Lecture 1 (divide by n)
mom = function(x, k) mean( (x - mean(x))^k )
skew = function(x) mom(x,3)/mom(x,2)^(3/2)
kurt = function(x) mom(x,4)/mom(x,2)^2
ret_df = data.frame( Statistic = c( "Mean (% per day)", "Std.Dev (% per day)", "Skewness", "Kurtosis",
                                    "Excess Kurtosis", "Mean (% p.a., x252)", "Volatility (% p.a., x sqrt(252))","Min","Max"),
                      Value    = c( mean(r), sqrt(mom(r,2)), skew(r), kurt(r), kurt(r)-3, 252*mean(r), sqrt(252)*sqrt(mom(r,2)),min(r),max(r)) )
cat(sprintf(' Number of daily returns: %d\n', n))
print(format(ret_df, digits = 4, nsmall = 4), row.names = FALSE)

# c. tail probabilities: data vs. a normal distribution with the same mean and variance
z = (r - mean(r))/sqrt(mom(r,2))
tail_df = data.frame( Threshold = c("|z|>2", "|z|>3", "|z|>4"),
                      Data      = c(mean(abs(z)>2), mean(abs(z)>3), mean(abs(z)>4)),
                      Normal    = 2*pnorm(-c(2,3,4)) )
print(format(tail_df, digits = 4, nsmall = 6), row.names = FALSE)
cat(sprintf(' Largest daily loss: %.4f%% on %s\n', min(r), format(time(NYSESW)[-1][which.min(r)])))

# d. a Bernoulli variable: D = 1 if the return is positive (Calc-1, Question 3)
D = as.numeric(r > 0)
p.hat = mean(D)
bern_df = data.frame( Moment  = c("Mean", "Variance", "Skewness", "Kurtosis"),
                      Formula = c(p.hat, p.hat*(1-p.hat), (1-2*p.hat)/sqrt(p.hat*(1-p.hat)), 1/(p.hat*(1-p.hat))-3),
                      Sample  = c(mean(D), mom(D,2), skew(D), kurt(D)) )
print(format(bern_df, digits = 6, nsmall = 6), row.names = FALSE)

# PLOT HISTOGRAM OF RETURNS WITH NORMAL DENSITY
if (print2pdf == 1){ pdf( paste0(path2graphics, "Lab-01-NYSE Returns.pdf"), width = 10, height = 6 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 2, 2), oma = c(1, 1, 1, 1), tcl = -0.25)

hist( r, breaks = 80, freq = FALSE, col = "steelblue", border = "white", main = "", xlim = c(-5.0, 5.0), ylim = c(0,.7),
      xlab = "Daily log return (%)", ylab = "Density", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
xgrd = seq(-5, 5, length.out = 500)
# axis(side = 1, at = seq(-5, 5, by = 1), cex.axis = fnt-.1)
lines(xgrd, dnorm(xgrd, mean(r), sqrt(mom(r,2))), col = "red3", lwd = 1.5)
set_axis_ticks(tcl = .15, cex = fnt-.1, at1 = seq(-5, 5, by = 1))
legend("topright", legend = c("NYSE daily returns", "Normal with same mean and variance"),
        col = c("steelblue", "red3"), lwd = c(3, 2), bty = "o", bg = "white", cex = fnt-.21)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
