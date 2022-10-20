# CLEAR SCREEN AND WORKSPACE ----
# From: https://www.njtierney.com/post/2020/06/01/tidy-anscombe/
cat("\014"); rm(list = ls()); gc()
# load packages (if pacman not installed, install first with install.packages("pacman"))
pacman::p_load(tidyverse, ggplot2, scales , extrafont, ggpubr, ggfortify)
pacman::p_load(stats, ggpmisc)

head(anscombe)


tidy_anscombe <- anscombe %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)")
tidy_anscombe$set = paste0("Data set ", tidy_anscombe$set)
head(tidy_anscombe)

# tidy_anscombe_summary <- tidy_anscombe %>%
#   group_by(set) %>%
#   summarise(across(.cols = everything(),
#                    .fns = lst(min,max,median,mean,sd,var),
#                    .names = "{col}_{fn}"))
# 
# tidy_anscombe_summary
 
### -------------
windowsFonts("Palatino" = windowsFont("Palatino Linotype"))
my.formula <- y ~ x
fnt = 12
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
    # facet_wrap(~set) +
  # theme(strip.background = element_rect(colour="black", fill="white", 
  #                                       size=0.005, linetype="solid")) + 
  # geom_smooth(method = "lm", se = FALSE)
  geom_smooth(method = "lm", se = FALSE, formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               eq.with.lhs = c("italic(Y)~`=`~ "),
               # eq.y.rhs = "~italic(Y)",
               eq.x.rhs = "~italic(X)",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*\",\"~~~")),
               parse = TRUE) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*1), name = "Y (dependent variable)") + 
  scale_x_continuous(name = "X (independent variable)") +
  coord_cartesian( ylim = c(2.5, 12.5), xlim = c(5, 20) ) +
  grids(linetype = "dashed") + 
  geom_point(size=3)
