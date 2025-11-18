# cat("\014")
Sales_2013 = 196
Sales_2014 = 500
percentage_change = 100*(Sales_2014 - Sales_2013)/Sales_2013
log_approx_change = 100*(log(Sales_2014)-log(Sales_2013))

print(round(percentage_change,4))
print(round(log_approx_change,4))

