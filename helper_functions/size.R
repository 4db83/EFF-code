size <- function(obj) {
# returns size of the object of interest  
  obj_name <- deparse(substitute(obj))
  obj_dims <- dim(obj)
  obj_length <- length(obj)
  obj_class <- class(obj)[1]
  
  if (!is.null(obj_dims)) {
    # cat( paste( obj_dims, collapse = " × "), "-", obj_class, "\n" )
    # cat( paste( obj_dims, collapse = " × "), "-", obj_class, "\n" )
    cat( paste( obj_dims, collapse = " × "), obj_class, "\n")
    return(obj_dims)
  } else {
    # cat("Length", obj_length, "-", obj_class, "\n")
    # cat( obj_class,"-", obj_length, "\n")
    cat( obj_length, obj_class, "\n")
    return(obj_length)
  }
}

# # Test with different data types
# my_matrix <- matrix(1:1000, nrow = 100, ncol = 10)
# size(my_matrix)
# 
# my_df <- data.frame(x = 1:50, y = letters[1:50], z = rnorm(50))
# size(my_df)
# 
# my_vector <- 1:100
# size(my_vector)
# 
# my_list <- list(a = 1:10, b = "hello", c = data.frame(x = 1:5))
# size(my_list)
# 
# my_ts <- ts(1:24, frequency = 12, start = c(2020, 1))
# size(my_ts)
# 
# my_date <- as.Date("2023-01-01")
# size(my_date)
# 
# my_factor <- factor(c("A", "B", "C", "A", "B"))
# size(my_factor)