use_dark_plots = function( background_color = NULL) {
  # set background_color dark as default
  if (is.null(background_color)) { background_color <- "Gray 4" }
    # Convert background color to RGB and calculate luminance
  bg_rgb <- col2rgb(background_color)
  bg_luminance <- (0.299 * bg_rgb[1,] + 0.587 * bg_rgb[2,] + 0.114 * bg_rgb[3,]) / 255

  # Define light gray threshold (you can adjust this value)
  light_gray_threshold <- .4  # 0.7 is a common threshold for light colors

  if (bg_luminance > light_gray_threshold) {
    # Background is light - use dark text
    par(bg = background_color,
        col.axis  = "black",
        col.lab   = "black", 
        col.main  = "black",
        col.sub   = "black",
        col       = "black",
        fg        = "black")
  } else {
    # Background is dark - use light text
    par(bg = background_color,
        col.axis  = "white",
        col.lab   = "white",
        col.main  = "white", 
        col.sub   = "white",
        col       = "white",
        fg        = "white")
  }

}