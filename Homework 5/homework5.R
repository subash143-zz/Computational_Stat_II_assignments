#--------------------Initialization of dependencies----------------------
library(MASS)
attach(Boston)
library(boot)


#----------------------(a)-----------------------------------------------
mew = mean(medv)
mew


#----------------------(b)----------------------------------------------
standard_error = sqrt(var(medv)/nrow(Boston))
standard_error


#----------------------(c)-----------------------------------------------
mean_function = function(data, index){
  return (mean(data[index]))
}
boot(medv ,mean_function, R=1000)


#----------------------(e)-----------------------------------------------
med = median(medv)
med


#----------------------(f)-----------------------------------------------
median_function = function(data, index){
  return (median(data[index]))
}
boot(medv, median_function, R=1000)


