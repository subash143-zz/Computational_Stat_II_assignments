setwd("/home/subash/Documents/Computational Statistics II/Lab 3/Plots")
library(ISLR)
#--------------------9(a)--------------------------------------------------------------------------------------
pdf("filename.pdf")
pairs(Auto)
mtext("Figure 1", side = 3, line = -2, outer = TRUE)

#--------------------9(a)--------------------------------------------------------------------------------------

cor(Auto[1:8])

#--------------------9(c)--------------------------------------------------------------------------------------
#Omiting the name as the predictor as it is not a numeric value
linear_model <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=Auto)
#OR : linear_model <- lm(mpg~.-name, data=Auto)
summary(linear_model)

  #---------------------9(c)(i)--------------------------------------------------------------------------------
  # Answer: Of course, there exists a relation between the predictors and the model.Looking at
  #         the p-value of the differenet predictors, we can say that some of them have significant
  #         relation and some of them have negligible relation. But, at the end, there exists a
  #         relation among them.


  #---------------------9(c)(ii)--------------------------------------------------------------------------------
  # Answer: The predictor with the least p-value have the most significant relationship to the
  #         response. Looking at the output of the code, we can say that weight, year and origin have
  #         the most significant relationship to the response, i.e. the value of these predictors
  #         contribute to the response more than rest of those.


  #---------------------9(c)(iii)--------------------------------------------------------------------------------
  # Answer: The coefficient of the year variable suggest that for a single increase in value of year,
  #         the value for mpg is increased by 0.750773. It is the positive increment.


  #--------------------9(d)--------------------------------------------------------------------------------------

  par(mfrow=c(2,2))
  plot(linear_model)
  mtext("Figure 2", side = 3, line = -2, outer = TRUE)
  
  # Answer: Looking at the residual vs fitted graph, there is a almost horizontal line  but the relation is not
  #         linear. #323, #326 and #327 seem out of the line and they are the outliers. This implies that these
  #         plots might be a potential problem. Normal Q-Q plot is a straight line, it suggests that the 
  #         residuals are normally distributed except that #323, #326 and #327 are little off the line.
  #         Spread-location plot is a horizontal line with resudials equally spreaded except #323, #326 and #327
  #         off the mark. At last, the Residual-leverage plot displays most of the points inside the cooks curve
  #         but the points #327 and #394 are high leverage points.


  #--------------------9(e)--------------------------------------------------------------------------------------
  lm_interactions = lm(mpg ~ .+cylinders:origin + year:origin + displacement*weight
                              + acceleration*weight
                              +acceleration * horsepower +
                              + weight*origin + weight*year,
                              data = Auto[, 1:8])
  summary(lm_interactions)
  #Answer: The interactions suggest that displacement:weight, horsepower:acceleration and weight:year are statistically
  #        significant, which can be concluded form the p-value in the output. weight:origin is the most insignificant
  #        interaction among above checked interactions.
  
  #--------------------9(f)--------------------------------------------------------------------------------------
  #Answer: Lets take a variable weight in consideration for this example.
  #Normal
  par(mfrow=c(2,2))
  lm_cylinder = lm(mpg~weight, data=Auto[, 1:8])
  plot(lm_cylinder)
  mtext("Figure 3 : MPG~Weight", side = 3, line = -2, outer = TRUE)
  #Logarithmic
  par(mfrow=c(2,2))
  lm_cylinder_log = lm(mpg~log(weight), data=Auto[, 1:8])
  plot(lm_cylinder_log)
  mtext("Figure 4 : MPG~Log(Weight)", side = 3, line = -2, outer = TRUE)
  #Square root
  par(mfrow=c(2,2))
  lm_cylinder_root = lm(mpg~sqrt(weight), data=Auto[, 1:8])
  plot(lm_cylinder_root)
  mtext("Figure 5 : MPG~Sqrt(Weight)", side = 3, line = -2, outer = TRUE)
  #Square
  par(mfrow=c(2,2))
  lm_cylinder_square = lm(mpg~I(weight^2), data=Auto[, 1:8])
  plot(lm_cylinder_square)
  mtext("Figure 6: MPG~I(Weight^2)", side = 3, line = -2, outer = TRUE)
  #Observation: From the residual plots in Figure 3, we can say that the relation between the mpg and weights is
  #             non-linear. Using various transformations, we can see that the transformation in figure 4, which
  #             is logarithmic transformation gives the best linear output. 
  
  #---------------------------------13---------------------------------------------------------------------------
  
    #-------------------------------13(a)------------------------------------------------------------------------
    set.seed(1)
    x <- rnorm(100, 0, 1);
    
    #-------------------------------13(b)------------------------------------------------------------------------
    eps <- rnorm(100, 0, 0.25)
    
    #--------------------------------13(c)-----------------------------------------------------------------------
    y <- -1 + 0.5*x + eps
    length(y)
    # Answer: The length of vector y is 100. The values are:
    #   β0 = -1
    #   β1 = 0.5
  
    #--------------------------------13(d)-----------------------------------------------------------------------
    plot(x, y, main = "Figure 7 : Least Sq & Population Line")
    # Observation: The scatterplot shows somewhat linear relation between x and y. Though there are few outliers,
    #              they are not causing the linear relation to deviate much.
    
    #--------------------------------13(e)-----------------------------------------------------------------------
    least_square = lm(y~x)
    summary(least_square)
    #Observation: Using the Least Square linear model, we get the following the values:
    #   β0 = -1.00942
    #   β1 = 0.49973
    #Comparing the β0 and β1 used to generate the linear model with the values obtained from the modeling, we get
    #the same result. The values closely resemble to each other and it is not a surprise.
    
    #--------------------------------13(f)-----------------------------------------------------------------------
    abline(least_square, col="blue")
    abline(-1, 0.5, col="red")
    legend("topleft", legend=c("Least Sq. Line", "Population Line"),
           col=c("blue","red"), lty=1:2, cex=0.8)

    #--------------------------------13(g)-----------------------------------------------------------------------
    least_square_polynomial = lm(y~x+I(x^2))
    summary(least_square_polynomial)
    anova(least_square, least_square_polynomial)
    # Observation: The significant p-value associated with the quadratic term suggests that the model is not
    #               improved. Also, the anova shows that the F-statistic is 1.9682 and assicoated p-value is not zero.
    #               Thus, we can conclude that introducing the quadratic term is not helping to fit the data well.
      
    
    #--------------------------------13(h)----------------------------------------------------------------------- 
    #Putting less noise in the data
    x2 <- rnorm(100, 0, 1);
    eps2 <- rnorm(100, 0, 0.025)  
    y2 <- -1 + 0.5*x2 + eps2
    plot(x2, y2, main = "Figure 8 : Noise reduced")
    least_square2 = lm(y2~x2)
    summary(least_square2)
    abline(least_square2, col="blue")
    abline(-1, 0.5, col="red")
    legend("topleft", legend=c("Least Sq. Line", "Population Line"),
           col=c("blue","red"), lty=1:2, cex=0.8)
    #Observation: Adding less noise to the data decreases the number of outliers. This implies that the prediction
    #             line appears to be more close to the data. Also, the dependency of the response on the intercept
    #             increases drastically which can be interpreted from the p-value. The sum  of residuals is
    #             decreased too.
    
    
    #--------------------------------13(i)----------------------------------------------------------------------- 
    #Putting more noise in the data
    x3 <- rnorm(100, 0, 1);
    eps3 <- rnorm(100, 0, 2.5)  
    y3 <- -1 + 0.5*x3 + eps3
    plot(x3, y3, main = "Figure 9 : Noise added")
    least_square3 = lm(y3~x3)
    summary(least_square3)
    abline(least_square3, col="blue")
    abline(-1, 0.5, col="red")
    legend("topleft", legend=c("Least Sq. Line", "Population Line"),
           col=c("blue","red"), lty=1:2, cex=0.8)
    dev.off()
    #Observation: Adding more noise to the data increases the number of outliers. This means that the prediction 
    #             line appears to be separating the bunch of highly scattered data. Also, the dependency of the
    #             response on the intercept decreases drastically which can be interpreted from the p-value.
    
    
    #--------------------------------13(j)-----------------------------------------------------------------------
    #Normal
    confint(least_square)
    #Noise Reduced
    confint(least_square2)
    #Noise Added
    confint(least_square3)
    # Observation: The confidence interval shows that the range of confidence interval is increased for the predictors
    #              as well as the intercepts from the original data to the more noisy data(i). Whereas, the less noisy
    #              data(h) displays a drastic decrease in confidence interval. We can conclude that the more noise is 
    #              added to the data, the more the confidence interval increases.
    
    
    
    