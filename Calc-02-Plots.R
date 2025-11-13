## PREAMBLE: clear screen/workspace -----
cat("\014"); rm(list = ls()); gc();
# SET DEFAULTS: display options, font and y axis label rotation
options(digits = 12); options(scipen = 999);  options(max.print=10000)
# INSTALL PACMAN: if not installed (note: may neeD:\_teaching\_current.teaching\_SU.EFF\code-EFF\helper_functions\print_results.Rd to disable windows firewall for packages to install)
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD/INSTALL: other required packages
pacman::p_load(dplyr,readxl,sandwich,patchwork)
# LOAD HELPER FUNCTIONS STORED IN functions_path
functions_path  = c("./helper_functions/")
invisible(lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ))

## Question 1) a to c. -----
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

# First subplot (3 rows, 1 col, first panel)
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
  theme(legend.position = "best",
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

# --- Data setup ---
x <- seq(5, 100, length.out = 1000)
# Compute curves
Z1 <- 1; Z0 <- 0
y_d_z1 <- 2 + 3 * log(x) + 4 * Z1 - Z1 * log(x)
y_d_z0 <- 2 + 3 * log(x) + 4 * Z0 - Z0 * log(x)

# --- Base R Plot ---
plot(x, y_d_z0, type = "l", 
     col = "steelblue", lwd = 2,
     xlab = expression(X),
     ylab = expression(hat(Y)),
     main = "",
)
lines(x, y_d_z1, 
      col = "darkred", 
      lwd = 2)

# Add legend in top-left corner with a box around it
legend("topleft",
       legend = c(
         expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 0),
         expression(hat(Y) == 2 + 3 * ln(X) + 4 * Z - Z %*% ln(X)~","~Z == 1)
       ),
       col = c(clr[2], clr[1]),
       lwd = 2,
       bty = "o",               # draw a box around legend
       box.lwd = 1,
       bg = "white")            # legend background white




# Display plot
# print(p2)

# Third subplot ----
p3 <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = y_e, color = "e"), size = 0.7) +
  scale_color_manual(
    name = NULL,
    values = c(e = clr[3]),
    labels = c(e = expression(hat(Y) == 1 + 125 * X - 0.01 * X^2))
  ) +
  labs(x = expression(X), y = expression(hat(Y))) +
  theme_minimal() +
  theme(legend.position = "best",
        legend.text.align = 0)

# Combine vertically (3 rows x 1 column)
final_plot <- p1 / p2 / p3 + plot_layout(heights = c(1, 1, 1))
print(final_plot)