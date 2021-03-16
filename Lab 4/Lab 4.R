#---------------------------------------------Lab_4 Assignment-----------------------------------
# Author: Subash Kharel
# Date: 23 February 2020
#------------------------------------------------------------------------------------------------

# Library for Auto data
library(ISLR)
#Library for lda() function
library(MASS)
#library for knn() function
library(class)

# Name column removed from the data
Auto$name = NULL;

# --------------------------------11(a)--------------------------------------------
# Binary variable mpg01 creation and assignment of values
mpg01 <- rep(0, nrow(Auto))
median_mpg = median(Auto$mpg)
mpg01[Auto$mpg >= median_mpg] = 1
as.factor(mpg01)
Auto$mpg01 = mpg01;


# --------------------------------11(b)--------------------------------------------
# Data Exploration to fund out the most statistically significant ones
pdf("lab_4.pdf")
pairs(Auto)
title(main="Figure 1: Scatterplot matrix")
boxplot(mpg01~cylinders, data=Auto, main = "Figure 2: Boxplot mpg01 vs cylinder")
boxplot(mpg01~acceleration, data=Auto, main = "Figure 3: Boxplot mpg01 vs acceleration")
boxplot(mpg01~horsepower, data=Auto, main = "Figure 4: Boxplot mpg01 vs horsepower")
boxplot(mpg01~weight, data=Auto, main = "Figure 5: Boxplot mpg01 vs weight")
dev.off()
# ---------------------------------11(c)------------------------------------------
# Data splitted into training and testing set
train = (Auto$weight < 3615)
training <- Auto[train,]
testing <- Auto[!train,]


#----------------------------------11(d)----------------------------------------
# Performing LDA on the data
lda.fit = lda(mpg01~horsepower+weight+acceleration, data=training, subset=train)
lda.pred = predict(lda.fit, testing)
lda.class = lda.pred$class
accuracy = mean(lda.class == testing$mpg01) * 100
cat("-------------------------------Accuracy for LDA : ", accuracy, fill = TRUE)


#----------------------------------11(e)----------------------------------------
# Performing QDA on the data
qda.fit = qda(mpg01~horsepower+weight+acceleration, data=training, subset=train)
qda.pred = predict(qda.fit, testing)
qda.class = qda.pred$class
accuracy = mean(qda.class == testing$mpg01) * 100
cat("-------------------------------Accuracy for QDA : ", accuracy, fill = TRUE)



#----------------------------------11(f)----------------------------------------
# Performing Logistic Regression on the data
glm.fit = glm(mpg01~horsepower+weight+acceleration, data=training, family = binomial, subset = train)
glm.probs = predict(glm.fit, testing, type="response")
glm.pred = rep(0, nrow(testing))
glm.pred[glm.probs > .5] = 1
accuracy = mean(glm.pred == testing$mpg01)
cat("-------------------------------Accuracy for LR : ", accuracy, fill = TRUE)



#----------------------------------11(g)----------------------------------------
# Performing KNN  classification on the data
train.X = training[,c("horsepower", "weight", "acceleration")]
test.X = testing[,c("horsepower", "weight", "acceleration")]
set.seed(1)
knn.pred = knn(train.X, test.X, training$mpg01, k = 1)
table(knn.pred, testing$mpg01)
accuracy = mean(knn.pred == testing$mpg01)
cat("-------------------------------Accuracy for KNN : ", accuracy, fill = TRUE)


#-----------------------------End of the Assignment---------------------------------------------------



