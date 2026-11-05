# %% HEADER: CLEAR SCREEN, WORKSPACE, LOAD PACKAGES, SET DEFAULTS
rm(list = ls()); cat("\014"); gc(); 
# set defaults: display options, font and y axis label rotation
par(family = "serif", las = 1); options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# install pacman: if not installed 
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# load/install: other required packages
pacman::p_load(dplyr,readxl,sandwich,ggplot2,patchwork)
# load helper functions stored in functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))
# set to 1 to print to pdf instead of screen
print2pdf = 0
pdf_name  = c("Calc-02-Plots Panel d.pdf",
              "Calc-02-Plots.pdf")
path2graphics = "../_tex.source.files/graphics/"
path2graphics = ""

# IN-FILE FUNCTION DEFINITIONS
# none

# %% SCRIPT STARTS HERE
# Question 1) a to c.
# Colors similar to MATLAB clr(1:4)
clr <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

# Xgrid values
x <- seq(5, 100, length.out = 1000)
df <- data.frame(x = x,
                 y_a = 2 + 3 * log(x),
                 y_b = 2 - 3 * log(x),
                 y_c_z1 = 2 + 3 * log(x) + 4 * 1,
                 y_c_z0 = 2 + 3 * log(x) + 4 * 0,
                 y_d_z1 = 2 + 3 * log(x) + 4 * 1 - 1 * 1 * log(x),
                 y_d_z0 = 2 + 3 * log(x) + 4 * 0 - 1 * 0 * log(x),
                 y_e = 1 + 125 * x - 0.01 * x^2)

# %% FIRST PANEL: REGRESSION FUNCTIONS a) TO c) (GGPLOT)
p1 <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = y_a, color = "a"), size = 0.7) +
  geom_line(aes(y = y_b, color = "b"), size = 0.7) +
  geom_line(aes(y = y_c_z1, color = "c_z1"), size = 0.7) +
  geom_line(aes(y = y_c_z0, color = "c_z0"), linetype = "dashed", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.5) +
  scale_color_manual(
    name = NULL,
    values = c(a = clr[3], b = clr[2], c_z1 = clr[1], c_z0 = clr[4]),
    labels = c(
      a    = expression(hat(Y) == 2 + 3 * ln(X)),
      b    = expression(hat(Y) == 2 - 3 * ln(X)),
      c_z1 = expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z~","~Z == 1),
      c_z0 = expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z~","~Z == 0)
    ),
    guide = guide_legend(override.aes = list(linetype = c("solid","solid","solid","dashed")))
  ) +
  labs(x = expression(X), y = expression(hat(Y))) +
  theme_minimal() +
  theme(legend.position = "right",   # "best" is not a ggplot option and drops the legend
        legend.text.align = 0)

# # Second subplot -----
# p2 <- ggplot(df, aes(x = x)) +
#   geom_line(aes(y = y_d_z1, color = "d_z1"), size = 0.7) +
#   geom_line(aes(y = y_d_z0, color = "d_z0"), size = 0.7) +
#   scale_color_manual(
#     name = NULL,
#     values = c(d_z1 = clr[1], d_z0 = clr[2]),
#     labels = c(
#       d_z1 = expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 1),
#       d_z0 = expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 0)
#     )
#   ) +
#   labs(x = expression(italic(X)), y = expression(hat(italic(Y))) +
#   theme_minimal()
# print(p2)

# Second subplot -----
# p2 <- ggplot(df, aes(x = x)) +
#   geom_line(aes(y = y_d_z1, color = "d_z1"), size = 0.7) +
#   geom_line(aes(y = y_d_z0, color = "d_z0"), size = 0.7) +
#   scale_color_manual(
#     name = NULL,
#     values = c(d_z1 = clr[1], d_z0 = clr[2]),
#     labels = c(
#       d_z1 = expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 1),
#       d_z0 = expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 0)
#     )
#   ) +
#   labs(
#     x = expression(italic(X)),
#     y = expression(hat(italic(Y)))
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = c(0.05, 0.95),                # top-left
#     legend.justification = c("left", "top"),
#     legend.text.align = 0, 
#     legend.background = element_rect(             # box around legend
#       fill = "white",
#       color = "black",                            # border color
#       linewidth = 0.5
#     ),
#     legend.text.align = 0,
#     panel.border = element_rect(                  # box around the plot panel
#       color = "black",
#       fill = NA,
#       linewidth = 0.5
#     )
#   )

# %% SECOND PANEL: REGRESSION FUNCTION d) IN BASE R
x <- seq(5, 100, length.out = 1000)
# Compute curves
Z1 <- 1; Z0 <- 0
y_d_z1 <- 2 + 3 * log(x) + 4 * Z1 - Z1 * log(x)
y_d_z0 <- 2 + 3 * log(x) + 4 * Z0 - Z0 * log(x)

# Base R Plot
fnt = 1.8 # font size
if (print2pdf == 1){fnt = 1.35; pdf( paste0(path2graphics, pdf_name[1]), width = 10, height = 6 ) }
op = par( family = "serif", las = 1 )
plot(	x, y_d_z0, type = "l",
      col = "steelblue", lwd = 2,
      xlab = expression(X),
      ylab = expression(hat(Y)),
      main = "",
      cex.lab = fnt, cex.axis = fnt-.1
)
lines(x, y_d_z1,
      col = "darkred",
      lwd = 2)

# Add legend in top-left corner with a box around it
legend(	"topleft",
        legend = c(
          expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 0),
          expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 1)
        ),
        col = c("steelblue", "darkred"),   # same colours as the two lines
        lwd = 2,
        cex = fnt-.35,
        bty = "o",               # draw a box around legend
        box.lwd = 1,
        bg = "white")            # legend background white
if (print2pdf == 1){ par(op); dev.off() }

# Display plot
# print(p2)

# %% THIRD PANEL: REGRESSION FUNCTION e) (GGPLOT)
p3 <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = y_e, color = "e"), size = 0.7) +
  scale_color_manual(
    name = NULL,
    values = c(e = clr[3]),
    labels = c(e = expression(hat(Y) == 1 + 125 * X - 0.01 * X^2))
  ) +
  labs(x = expression(X), y = expression(hat(Y))) +
  theme_minimal() +
  theme(legend.position = "right",   # "best" is not a ggplot option and drops the legend
        legend.text.align = 0)

# %% COMBINE THE GGPLOT PANELS
# p2 is commented out above (panel d) is drawn in base R instead), so only p1 and p3 are combined
fnt = 16 # font size (in points for ggplot)
if (print2pdf == 1){fnt = 12; pdf( paste0(path2graphics, pdf_name[2]), width = 10, height = 8 ) }
# Combine vertically (2 rows x 1 column)
final_plot <- p1 / p3 + plot_layout(heights = c(1, 1)) & theme(text = element_text(size = fnt, family = "serif"))
print(final_plot)
if (print2pdf == 1){ dev.off() }