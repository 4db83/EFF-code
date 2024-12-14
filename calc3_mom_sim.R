# CLEAR THE CONSOLE
cat("\014"); rm(list = ls()); gc()
# SET DEFAULTS: DISPLAY OPTIONS, FONT AND Y AXIS LABEL ROTATION
options(digits = 8); options(scipen = 999);  options(max.print=10000)
windowsFonts("Palatino" = windowsFont("Palatino Linotype")); par(las = 1, family = "Palatino")
# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED 
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# SET WORKING DIRECTORY 
# setwd('D:/_research/_current/LW03_2024/code')
# LOAD HELPER FUNCTIONS STORED IN LOCAL DIRECTORY CALLED: ./local.Functions/
functions_path = c("./local.Functions/"); if (dir.exists(functions_path)){
invisible( lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ) ) }
# UNCOMMENT TO LOAD R BASELINE R_utility_functions.R FROM D:/matlab.tools/db.toolbox
# source("D:/matlab.tools/db.toolbox/R_utility_functions.R")
# LOAD REQUIRED PACKAGES
pacman::p_load(tictoc,matlab,ggplot2,gridExtra,scales); 
# set.seed(1234)

a <- 3
b <- 10
T <- 2e2      # sample size T
nSims <- 1e5  # number of simulations used for the sampling distribution

# Generate random gamma samples
tic()
y <- matrix(rgamma(T * nSims, shape = a, rate = b), nrow = T, ncol = nSims)
toc()
# print(end_time - start_time)

# Define xgrid over which to plot the density
xgrd <- seq(0, round(max(y)), length.out = 1e3)
# Gamma PDF function
py <- function(y, a, b) (b^a / gamma(a)) * y^(a - 1) * exp(-b * y)

## Compute moments
y_bar   <- colMeans(y)
y2_bar  <- colMeans(y^2)
y_1_bar <- colMeans(1/y)

# a)
a_hat1 <- y_bar^2 / (y2_bar - y_bar^2)
b_hat1 <- a_hat1 / y_bar
cat("    a1(se)    b1(se)\n")
print(rbind(c(mean(a_hat1), mean(b_hat1)), c(var(a_hat1), var(b_hat1))))

# b)
a_hat2 <- y_bar * y_1_bar / (y_bar * y_1_bar - 1)
b_hat2 <- a_hat2 / y_bar
cat("    a2(se)    b2(se)\n")
print(rbind(c(mean(a_hat2), mean(b_hat2)), c(var(a_hat2), var(b_hat2))))

# c)
SM <- y_1_bar^2 * y2_bar
a_hat3 <- 1 / 2 * ((2 * SM + 1) + sqrt(8 * SM + 1)) / (SM - 1)
b_hat3 <- (a_hat3 - 1) * y_1_bar
cat("    a3(se)    b3(se)\n")
print(rbind(c(mean(a_hat3), mean(b_hat3)), c(var(a_hat3), var(b_hat3))))

# Plotting

## Histogram of y with density plot ----
p1 <- ggplot(data = NULL, aes(x = matrix(y, ncol = 1))) +
  geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "blue", alpha = 0.5) +
  # stat_function(fun = function(x) py(x, a, b), color = "red", size = 1.5) +
  geom_line(data = NULL, aes(x = xgrd, y = py(xgrd, a, b)), color = "red", size = 1.2)  + 
  # ggtitle("Gamma Distribution") +
  xlim(c(0,3)) +
  xlab("y") + ylab("Density")
p1


## Histograms of estimates ----
# options(warn = -1)
plot_hist <- function(data, true_value, x_label, xlim, ylim) {
  ggplot(data = data.frame(x = data), aes(x = x)) +
    geom_histogram(aes(y = ..density..), bins = 300, color = "black", fill = "blue", alpha = 0.5) +
    geom_vline(xintercept = true_value, color = "red", size = 1.0) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    xlim(xlim) + ylim(ylim) + 
    xlab(x_label) + ylab("") + 
    theme_minimal(base_size = 15) + 
    theme(text = element_text(family = "Palatino")) 
}
# options(warn = 0)

XLMa <- c(0, 6)
XLMb <- c(2, 20)
YLMa =  c(0, 3.0)
YLMb =  c(0, 1.0)

p2 <- plot_hist(a_hat1, a, "alpha (a)", XLMa, YLMa)
p3 <- plot_hist(b_hat1, b, "beta (a)",  XLMb, YLMb)
p4 <- plot_hist(a_hat2, a, "alpha (b)", XLMa, YLMa)
p5 <- plot_hist(b_hat2, b, "beta (b)",  XLMb, YLMb)
p6 <- plot_hist(a_hat3, a, "alpha (c)", XLMa, YLMa)
p7 <- plot_hist(b_hat3, b, "beta (c)",  XLMb, YLMb)

# Arrange plots
grid.arrange(p2, p3, p4, p5, p6, p7, ncol = 2)



