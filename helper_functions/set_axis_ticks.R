set_axis_ticks <- function( at1 = NULL,        # sequence of values
                            at2 = NULL,        # sequence of values
                            tcl = .25,        # ticksize 
                            cex = 1.35,       # font size
                            labels = TRUE,
                            suppress_default = TRUE) {

  if (is.null(at1)) { at1 <- axTicks(1) }
  if (is.null(at2)) { at2 <- axTicks(2) }

  # First axis (outside ticks)
  axis(side = 1, at = at1, cex.axis = cex, tcl = tcl, labels = labels)
  # First axis (inside ticks)
  axis(side = 1, at = at1, cex.axis = cex, tcl =-tcl, labels = FALSE)

  # Second axis (outside ticks)
  axis(side = 2, at = at2, cex.axis = cex, tcl = tcl, labels = labels)
  # Second axis (inside ticks)
  axis(side = 2, at = at2, cex.axis = cex, tcl =-tcl, labels = FALSE)


}

set_yaxis_ticks <- function( side = 1,         # which axis, 1 for y, 2 for x
                             at = NULL,        # sequence of values
                             tcl = .25,        # ticksize 
                             cex = 1.35,       # font size
                             labels = TRUE) {
  
  if (is.null(at)) { at <- axTicks(side) }

  # First axis (outside ticks)
  axis(side = side, at = at, cex.axis = cex, tcl = tcl, labels = labels)
  # First axis (inside ticks)
  axis(side = side, at = at, cex.axis = cex, tcl =-tcl, labels = FALSE)

}


set_xaxis_ticks <- function( side = 2,         # which axis, 1 for y, 2 for x
                             at = NULL,        # sequence of values
                             tcl = .25,        # ticksize 
                             cex = 1.35,       # font size
                             labels = TRUE) {
  
  if (is.null(at)) { at <- axTicks(side) }

  # Second axis (outside ticks)
  axis(side = side, at = at, cex.axis = cex, tcl = tcl, labels = labels)
  # Second axis (inside ticks)
  axis(side = side, at = at, cex.axis = cex, tcl =-tcl, labels = FALSE)

}
