
#--------------------------------Question No. 8----------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

#------------8(a)----------------------------------------------------------------------------------------
setwd("D:/Computational Statistics/Lab 1/")
college = read.csv("College.csv", header=TRUE)
fix(college)

#------------8(b)----------------------------------------------------------------------------------------
rownames(college) = college[, 1]
fix(college)

college = college[, -1]
fix(college)


#------------8(c)(i)-------------------------------------------------------------------------------------
summary(college)


#------------8(c)(ii)------------------------------------------------------------------------------------
pairs(college[,1 : 10])


#------------8(c)(iii)------------------------------------------------------------------------------------
plot(college[, 1], college[, 9])


#------------8(c)(iv)-------------------------------------------------------------------------------------
Elite = rep("No", nrow(college));
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(college[ , "Outstate"], college[ , "Elite"], xlab = "Outstate", ylab = "Elite");


#------------8(c)(v)-------------------------------------------------------------------------------------
par(mfrow=c(2,2))
hist(college$Enroll, col = 2,main = "Histogram of College Enroll", xlab = "Enroll")
hist(college$Apps, col = 2,main = "Histogram of College Apps", xlab = "Apps")
hist(college$Accept, col = 2,main = "Histogram of College Accepts", xlab = "Accepts")
hist(college$Top25perc, col = 2,main = "Histogram of Top 25 Percent colleges", xlab = "Top 25 Percent")


#------------8(c)(vi)-------------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(college[, "Private"], college[, "Enroll"]);
#Conclusion: Enrollment in private college is relatively lower than in non-private colleges
plot(college[, "Private"], college[, "Grad.Rate"])
#Conclusion: Graduation rate in private college is relatively higher than in non-private colleges
plot(college[, "Elite"], college[, "Grad.Rate"])
#Conclusion: Graduation rate in Elite colleges is highes compared to others

#Calculating total expenditre done on student
TotalExpenditureByCollege = rep(0, nrow(college))
college$TotalExpenditureByCollege <- college$Room.Board + college$Books + college$Expend
college = data.frame(college, TotalExpenditureByCollege)
plot(college$Private, college$TotalExpenditureByCollege)
#Private colleges spend more money compared to non-private ones in terms of educational spending


plot(college$Grad.Rate, college$TotalExpenditureByCollege)
#Conclusion: Graduation rate of a particular college is not much dependent on the expenditure

plot(college$Apps, college$Enroll)
#Conclusion: The number of enroll is almost linearly dependent in the number of applicants

