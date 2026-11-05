# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc(); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(latex2exp)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = "EFF-02-Two Clusters Scatter.pdf"
path2graphics = "../_tex.source.files/graphics/"
# path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# legend text: fitted line and R^2 of each regression
fit_label = function(name, fit) {
  cf = coef(fit); r2 = summary(fit)$r.squared
  bquote(.(name)*":" ~~ hat(Y) == .(sprintf("%.2f", cf[1])) ~ .(ifelse(cf[2] < 0, "-", "+")) ~
           .(sprintf("%.2f", abs(cf[2])))*X * "," ~~ R^2 == .(sprintf("%.2f", r2)))
}

# %% SCRIPT STARTS HERE
# two clusters (Lecture 2): a scatter plot can show a strong relation between Y and X in the pooled data
# even though there is no relation between Y and X within either cluster. The pooled slope only picks up
# the difference between the two cluster means (an omitted group variable).

# %% SIMULATE THE TWO CLUSTERS AND RUN THE REGRESSIONS
n1 = 1450; n2 = 1550        # number of observations in cluster 1 (lower left) and cluster 2 (upper right)
b.within = 0                # slope of Y on X inside each cluster; try -0.5 to make the pooled and within slopes have opposite signs
set.seed(1234)
X1 = rnorm(n1, mean = -8, sd = 4)
Y1 = -6 + b.within*(X1 + 8) + rnorm(n1, mean = 0, sd = 4)
X2 = rnorm(n2, mean = 5, sd = 3)
Y2 =  8 + b.within*(X2 - 5) + rnorm(n2, mean = 0, sd = 2.5)
X  = c(X1, X2)
Y  = c(Y1, Y2)
D  = c(rep(0, n1), rep(1, n2))   # cluster dummy: 1 if the observation is in cluster 2

# OLS regressions: cluster 1 only, cluster 2 only, and both clusters pooled
lm.1    = lm(Y1 ~ X1)
lm.2    = lm(Y2 ~ X2)
lm.pool = lm(Y ~ X)
lm.D    = lm(Y ~ X + D)         # adding the cluster dummy removes the between-cluster part of the pooled slope

fits = rbind( "Cluster 1"       = c(coef(lm.1), summary(lm.1)$r.squared),
              "Cluster 2"       = c(coef(lm.2), summary(lm.2)$r.squared),
              "Pooled"          = c(coef(lm.pool), summary(lm.pool)$r.squared),
              "Pooled + dummy"  = c(coef(lm.D)[1:2], summary(lm.D)$r.squared) )
colnames(fits) = c("Intercept", "Slope on X", "R2")
print(round(fits, 4))
cat(sprintf(' Correlation of Y and X: cluster 1 = %.3f, cluster 2 = %.3f, pooled = %.3f\n',
            cor(X1, Y1), cor(X2, Y2), cor(X, Y)))

# %% SCATTER PLOT WITH THE REGRESSION LINES
col.1    = "steelblue"         # cluster 1 points
col.2    = "#E07B39"         # cluster 2 points
col.fit1 = "navy"              # cluster 1 regression line
col.fit2 = "red3"              # cluster 2 regression line
col.pool = "black"             # pooled regression line
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name), width = 10, height = 6.2 ) }
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(3.0, .5, 0),   # axis title, labels, line, position of y,x labels to axis
          mar = c(3.5, 4, 1, 4), # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(1, 1, 1, 1),   # outer margins for overall spacing
          tcl = -0.3)            # outward tick marks
x.ticks  = seq(-20, 15, by = 5)
y.ticks  = seq(-20, 20, by = 5)
plot( X, Y, type = "n", xlim = range(x.ticks), ylim = range(y.ticks), xaxs = "i", yaxs = "i",
      xlab = "", ylab = "", axes = FALSE )
abline(v = x.ticks, h = y.ticks, col = "grey80", lty = 3)
points(X1, Y1, col = col.1, pch = 1, cex = 0.9)
points(X2, Y2, col = col.2, pch = 1, cex = 0.9)
# cluster lines are drawn over the range of X in that cluster, the pooled line over the whole plot
x1.grid = range(X1); x2.grid = range(X2)
lines(x1.grid, predict(lm.1, newdata = data.frame(X1 = x1.grid)), col = col.fit1, lwd = 2)
lines(x2.grid, predict(lm.2, newdata = data.frame(X2 = x2.grid)), col = col.fit2, lwd = 2)
abline(lm.pool, col = col.pool, lwd = 2)
axis(1, at = x.ticks, cex.axis = fnt-.1)
axis(2, at = y.ticks, cex.axis = fnt-.1)
axis(4, at = y.ticks, cex.axis = fnt-.1)
axis(3, at = x.ticks, labels = FALSE, tcl = 0.3)
# mtext("X", side = 1, line = 2.2, cex = fnt)
# mtext("Y", side = 2, line = 2.6, cex = fnt)
mtext(latex2exp::TeX("$X$"), side = 1, line = 2.2, cex = fnt)
mtext(latex2exp::TeX("$Y$"), side = 2, line = 2.6, cex = fnt)
box()
# legend( "topleft", bty = "o", bg = "white", box.col = "grey60", cex = fnt-.35, inset = 0.01,
#         pch = c(1, 1, NA, NA, NA), lwd = c(NA, NA, 3, 3, 3),
#         col = c(col.1, col.2, col.fit1, col.fit2, col.pool),
#         legend = c(	"Cluster 1", "Cluster 2",
# 										do.call(expression, list(	fit_label("Cluster 1", lm.1), fit_label("Cluster 2", lm.2),
# 																							fit_label("Pooled", lm.pool)))) )

legend.cex = fnt - 0.35
models = list(lm.1, lm.2, lm.pool)

# LaTeX-style symbols, converted to R plotmath
yhat = latex2exp::TeX("$\\hat{Y}$")[[1]]
xmath = latex2exp::TeX("$X$")[[1]]
rmath = latex2exp::TeX("$R^{2}$")[[1]]

# Fitted equations, without R-squared
equations = as.expression(lapply(models, function(model) {
  b = coef(model)
  sign = if (b[2] < 0) "-" else "+"

  bquote(
    .(yhat) == .(sprintf("%.2f", b[1])) ~
      .(sign) ~ .(sprintf("%.2f", abs(b[2]))) * .(xmath)
  )
}))

# Separate R-squared column
r.squared = as.expression(lapply(models, function(model) {
  value = sprintf("%.2f", summary(model)$r.squared)
  bquote(.(rmath) == .(value))
}))

labels = c(	"Cluster 1", "Cluster 2",
						"Cluster 1:", "Cluster 2:", "Pooled:")

# Calculate widths for the three aligned columns
label.width = max(strwidth(labels, cex = legend.cex))
equation.width = max(strwidth(equations, cex = legend.cex))
r.width = max(strwidth(r.squared, cex = legend.cex))
gap = strwidth("  ", cex = legend.cex)

leg = legend(
  "topleft",
  legend = labels,
  bty = "o", bg = "white", box.col = "grey60",
  cex = legend.cex, inset = 0.01,
  pch = c(1, 1, NA, NA, NA),
  lwd = c(NA, NA, 3, 3, 3),
  col = c(col.1, col.2, col.fit1, col.fit2, col.pool),
  text.width = label.width + equation.width + r.width + 2 * gap
)
equation.x = leg$text$x[3:5] + label.width + gap
r.x = equation.x + equation.width + gap

text(equation.x, leg$text$y[3:5], labels = equations, adj = c(0, 0.5), cex = legend.cex)
text(r.x, leg$text$y[3:5], labels = r.squared,adj = c(0, 0.5), cex = legend.cex)

if (print2pdf == 1){ par(op);dev.off() }
