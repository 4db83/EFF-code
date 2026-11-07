# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc();
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,AER,wooldridge)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# %% SCRIPT STARTS HERE: one cell for each question
# QUESTION 1) OLS BY HAND
# California test scores and class size (Lecture 3)
CASchools = read_xlsx("./data/caschool.xlsx")           # read the data from .XLSX file in directory ./data
X = CASchools$str
Y = CASchools$testscr
n = length(Y)
# a. OLS estimates from the closed form solutions
SSX  = sum( (X - mean(X))^2 )
SSXY = sum( (X - mean(X))*(Y - mean(Y)) )
b1.hat = SSXY/SSX
b0.hat = mean(Y) - b1.hat*mean(X)
lm.testscr_str = lm(testscr ~ str, data = CASchools)
cat(sprintf(' By hand: b0 = %.6f  b1 = %.6f\n', b0.hat, b1.hat))
cat(sprintf(' lm()   : b0 = %.6f  b1 = %.6f\n', coef(lm.testscr_str)[1], coef(lm.testscr_str)[2]))

# b. fitted values, residuals and the algebraic facts of OLS
Y.hat = b0.hat + b1.hat*X
u.hat = Y - Y.hat
TSS = sum( (Y - mean(Y))^2 );  ESS = sum( (Y.hat - mean(Y))^2 );  RSS = sum( u.hat^2 )
facts_df = data.frame(Fact  = c("mean(u.hat)", "sum(X*u.hat)", "mean(Y.hat) - mean(Y)", "sum(Y.hat*u.hat)", "TSS - ESS - RSS"),
                      Value = c(mean(u.hat), sum(X*u.hat), mean(Y.hat) - mean(Y), sum(Y.hat*u.hat), TSS - ESS - RSS) )
print(format(facts_df, digits = 4), row.names = FALSE)

# c. measures of fit
R2_df = data.frame( Measure = c("ESS/TSS", "1 - RSS/TSS", "Corr(X,Y)^2", "Corr(Y,Y.hat)^2", "SER = sqrt(RSS/(n-2))"),
                    Value   = c(ESS/TSS, 1 - RSS/TSS, cor(X,Y)^2, cor(Y,Y.hat)^2, sqrt(RSS/(n-2))) )
print(format(R2_df, digits = 8), row.names = FALSE)
prnt.testscr_str = print_lm_results(lm.testscr_str)

# d. reverse regression of X on Y
lm.str_testscr = lm(str ~ testscr, data = CASchools)
cat(sprintf(' Reverse regression: slope = %.6f, 1/b1 = %.6f, R2 = %.6f\n',
            coef(lm.str_testscr)[2], 1/b1.hat, summary(lm.str_testscr)$r.squared))
cat(sprintf(' Product of the two slopes = %.6f = Corr(X,Y)^2 = %.6f\n', coef(lm.str_testscr)[2]*b1.hat, cor(X,Y)^2))

# e. slope = Corr(X,Y) * sd(Y)/sd(X)  (Calc-2, Question 1f)
cat(sprintf(' Corr(X,Y)*sd(Y)/sd(X) = %.6f\n', cor(X,Y)*sd(Y)/sd(X)))

# %% QUESTION 2) GROWTH AND TRADE
# SW2020 Empirical Exercise E4.1 (AER data GrowthSW, 65 countries)
data("GrowthSW", package = "AER")
Malta = rownames(GrowthSW) == "Malta"
lm.growth_all = lm(growth ~ tradeshare, data = GrowthSW)
lm.growth_noM = lm(growth ~ tradeshare, data = GrowthSW[!Malta,])
cat(" Growth on TradeShare, all countries\n")
prnt.growth_all = print_lm_results(lm.growth_all)
cat(" Growth on TradeShare, excluding Malta\n")
prnt.growth_noM = print_lm_results(lm.growth_noM)
new_x = data.frame(tradeshare = c(0.5, 1.0))
pred_df = data.frame( TradeShare  = new_x$tradeshare,
                      All         = predict(lm.growth_all, new_x),
                      Excl.Malta  = predict(lm.growth_noM, new_x) )
print(format(pred_df, digits = 4, nsmall = 4), row.names = FALSE)
cat(sprintf(' Malta: TradeShare = %.4f, Growth = %.4f;  mean TradeShare of the others = %.4f\n',
            GrowthSW$tradeshare[Malta], GrowthSW$growth[Malta], mean(GrowthSW$tradeshare[!Malta])))

# %% PLOT SCATTER AND BOTH REGRESSION LINES
if (print2pdf == 1){ pdf( paste0(path2graphics, "Lab-03-Growth and Trade.pdf"), width = 9, height = 5 ) }
fnt = 1.35 # font size
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(5, 5, 2, 2),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.25)           # shorter tick marks

plot( GrowthSW$tradeshare, GrowthSW$growth, pch = 16, col = "steelblue", xlim = c(0, 2.05),
      xlab = "Trade share", ylab = "Average annual growth rate (%)", cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
points(GrowthSW$tradeshare[Malta], GrowthSW$growth[Malta], pch = 17, cex = 2, col = "red3")
text(GrowthSW$tradeshare[Malta], GrowthSW$growth[Malta], "Malta", pos = 2, cex = fnt-.1, col = "red3")
xgrd = seq(0, 2.05, length.out = 100)
lines(xgrd, predict(lm.growth_all, data.frame(tradeshare = xgrd)), col = "red3",  lwd = 2)
lines(xgrd, predict(lm.growth_noM, data.frame(tradeshare = xgrd)), col = "green4", lwd = 2, lty = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend( "bottomright", legend = c("OLS fit, all countries", "OLS fit, excluding Malta"), col = c("red3", "green4"),
        lwd = 2, lty = c(1, 2), bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

# %% QUESTION 3) CEO SALARY AND RETURN ON EQUITY
# Wooldridge (2020) Example 2.3 (data ceosal1)
data("ceosal1", package = "wooldridge")
cat(sprintf(' Mean salary = %.3f (in $1000s), mean roe = %.3f%%, n = %d\n', mean(ceosal1$salary), mean(ceosal1$roe), nrow(ceosal1)))
lm.salary_roe = lm(salary ~ roe, data = ceosal1)
prnt.salary_roe = print_lm_results(lm.salary_roe)
cat(sprintf(' Predicted salary at roe = 0: %.3f, at roe = 30: %.3f\n',
            predict(lm.salary_roe, data.frame(roe = 0)), predict(lm.salary_roe, data.frame(roe = 30))))

# %% QUESTION 4) MONTE CARLO: SAMPLING DISTRIBUTION OF OLS
# Wooldridge C2.8 and Lecture 3
# a. one sample, Y = 1 + 2X + u, X ~ U[0,10], u ~ N(0,36)
set.seed(123)
n  = 500
x  = runif(n, 0, 10)
u  = rnorm(n, 0, 6)
y  = 1 + 2*x + u
lm.sim = lm(y ~ x)
cat(sprintf(' One sample: b0 = %.4f, b1 = %.4f\n', coef(lm.sim)[1], coef(lm.sim)[2]))
cat(sprintf(' sum(u.hat) = %.2e, sum(x*u.hat) = %.2e   vs   sum(u) = %.4f, sum(x*u) = %.4f\n',
            sum(resid(lm.sim)), sum(x*resid(lm.sim)), sum(u), sum(x*u)))

# b./c. repeated samples with n = 50, for X ~ U[0,10] and X ~ U[0,20]
N.reps = 1e4
n      = 50
mc_df  = data.frame()
b1.store = matrix(NA, N.reps, 2)
for (j in 1:2){
  x.max = c(10, 20)[j]
  for (r in 1:N.reps){
    x = runif(n, 0, x.max)
    y = 1 + 2*x + rnorm(n, 0, 6)
    b1.store[r,j] = sum( (x - mean(x))*(y - mean(y)) ) / sum( (x - mean(x))^2 )
  }
  var.X = x.max^2/12                                  # variance of a uniform on [0, x.max]
  mc_df = rbind(mc_df, data.frame( X.range = paste0("U[0,", x.max, "]"),
                Mean.b1 = mean(b1.store[,j]), SD.b1 = sd(b1.store[,j]),
                SD.theory = sqrt( 36/(n*var.X) ) ))   # sqrt( sigma_u^2/(n*sigma_X^2) ), Lecture 4
}
print(format(mc_df, digits = 4, nsmall = 4), row.names = FALSE)

# %% d. PLOT THE SAMPLING DISTRIBUTIONS OF b1
if (print2pdf == 1){ pdf( paste0(path2graphics, "Lab-03-MC Sampling Distribution.pdf"), width = 9, height = 5 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0), mar = c(5, 5, 2, 2), oma = c(1, 1, 1, 1), tcl = -0.25)

h1 = hist(b1.store[,1], breaks = 50, plot = FALSE)
h2 = hist(b1.store[,2], breaks = 50, plot = FALSE)
plot( h1, freq = FALSE, col = rgb(.27,.51,.71,.6), border = "white", main = "", 
      xlim = c(0.8, 3.2), ylim = c(0, max(h2$density)*1.2),
      xlab = expression("OLS estimate " * hat(beta)[1] ~ "(true value 2)"), ylab = "Density",
      cex.lab = fnt, cex.axis = fnt-.1, xaxt = "n", yaxt = "n" )
plot(h2, freq = FALSE, col = rgb(.80,.10,.10,.4), border = "white", add = TRUE)
bgrd = seq(0.8, 3.2, length.out = 400)
lines(bgrd, dnorm(bgrd, 2, mc_df$SD.theory[1]), col = "steelblue", lwd = 2)
lines(bgrd, dnorm(bgrd, 2, mc_df$SD.theory[2]), col = "red3", lwd = 2)
set_axis_ticks(tcl = .15, cex = fnt-.1)
legend("topright", legend = paste0("X ~ U[0,", c(10, 20), "]"), 
        fill = c( rgb(.27,.51,.71,.6), 
                  rgb(.80,.10,.10,.4)),
        border = "white", bty = "o", bg = "white", cex = fnt-.2)
if (print2pdf == 1){ par(op);dev.off() }

#EOF
