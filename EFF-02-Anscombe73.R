# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc(); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(tidyverse,scales,stats,ggpmisc,ggpubr,ggfortify)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = "EFF-02-Anscombe73.pdf"
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# check the loaded data
head(anscombe)

# re-arrange the data to be able to plot it using the format belwo
tidy_anscombe <- anscombe %>%
  pivot_longer(	cols = everything(),
                names_to = c(".value", "set"),
                names_pattern = "(.)(.)")
tidy_anscombe$set = paste0("Data set ", tidy_anscombe$set)
# sort on data set, and then ascending in xtraceback()
tidy_anscombe     = arrange(tidy_anscombe, set, x)
# print(tidy_anscombe,n=100)

# %% PLOT THE FOUR DATA SETS WITH THEIR REGRESSION LINES
fnt = 15 # font size (in points for ggplot)
if (print2pdf == 1){fnt = 12; pdf( paste0(path2graphics, pdf_name), width = 10, height = 7 ) }
my.formula <- y ~ x
p.anscombe = ggplot(tidy_anscombe, aes(x = x, y = y)) + theme_bw() +
  facet_wrap(~set, scales='free', strip.position="bottom") +
  theme(axis.text.x = element_text(size = fnt, hjust  = 0.58, vjust = 0.5 , colour = "black"),
        axis.text.y = element_text(size = fnt, colour = "black"),
        text        = element_text(size = fnt+3, family = "serif"),
        axis.ticks.length = unit( -1.5, "mm"),
        strip.text = element_text(size = fnt),
        strip.background = element_rect( colour="black", fill="white", linetype="blank"),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text( margin = margin(r = 14) ),
        # Remove inner facet axes
        # axis.text.x.top = element_blank(),
        # axis.text.y.right = element_blank(),
        )  +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula ) +
  stat_poly_eq(	formula = my.formula,
                eq.with.lhs = c("hat(Y)[i]~`=`~ "),
                eq.x.rhs = "~X[i]", family = "serif", size = 0.3*fnt, fontface = "italic",
                aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\",\"~~~")),
                parse = TRUE,
                ) +
  scale_y_continuous( name = "Y (dependent variable)", 
                      sec.axis = sec_axis( trans=~.*1)  ) +
  scale_x_continuous( name = "X (independent variable)" ) +
  coord_cartesian( ylim = c(2.5, 12.5), xlim = c(5, 20) ) +
  grids( linetype = "dashed" ) +
  geom_point( size=2.5 )
print(p.anscombe)
if (print2pdf == 1){ dev.off() }













#EOF 