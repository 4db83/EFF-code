# CLEAR SCREEN AND WORKSPACE ----
rm(list = ls()); cat("\014"); gc(); 
# SET DEFAULTS: display options, font and y axis label rotation
options(digits = 8); options(scipen = 999);  options(max.print=10000); 
windowsFonts("Palatino" = windowsFont("Palatino Linotype")); par(las=1) 
# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED (Note: may need to disable windows firewall for packages to install)
if (!require("pacman")) install.packages("pacman"); cat("pacman installed\n")
# LOAD REQUIRED PACKAGES 
pacman::p_load(tidyverse, ggplot2, scales , extrafont, ggpubr, ggfortify)
pacman::p_load(stats, ggpmisc)

# check the loaded data
head(anscombe)

# re-arrange the data to be able to plot it using the format belwo
tidy_anscombe <- anscombe %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)")
tidy_anscombe$set = paste0("Data set ", tidy_anscombe$set)
# sort on data set, and then ascending in x
tidy_anscombe     = arrange(tidy_anscombe, set, x)
print(tidy_anscombe,n=100)

### -------------
fnt = 12
my.formula <- y ~ x
ggplot(tidy_anscombe, aes(x = x, y = y)) + theme_bw() +
  facet_wrap(~set, scales='free', strip.position="bottom") +
  theme(axis.text.x = element_text(size = fnt, hjust  = 0.58, vjust = 0.5 , colour = "black"),
        axis.text.y = element_text(size = fnt, colour = "black"),
        text        = element_text(size = fnt, family = "Palatino"),
        axis.ticks.length = unit( -1.5, "mm"),
        strip.text = element_text(size = fnt), 
        strip.background = element_rect( colour="black", fill="white", linetype="blank"),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines") )  +
  geom_smooth(method = "lm", se = FALSE, formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               eq.with.lhs = c("italic(Y)~`=`~ "),
               eq.x.rhs = "~italic(X)",
               aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "*\",\"~~~")),
               parse = TRUE) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*1), name = "Y (dependent variable)") + 
  scale_x_continuous(name = "X (independent variable)") +
  coord_cartesian( ylim = c(2.5, 12.5), xlim = c(5, 20) ) +
  grids(linetype = "dashed") + 
  geom_point(size=3)













#EOF 