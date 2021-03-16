

# Function to calculate mode of items in a vector. PS: Copied from stack overflow
#(Link : https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode#answer-8189441)
estimate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#Creating data frame to perform data operations
x1 <- c(0, 2, 0, 0, -1, 1)
x2 <- c(3, 0, 1, 1, 0, 1)
x3 <- c(0, 0, 3, 2, 1, 1)

y <- c("Red", "Red", "Red", "Green", "Green", "Red")
y <- as.factor(y)

data <- data.frame(x1, x2, x3, y)



#----------------------7(a)-----------------------------------
#Calculating eucledian distance for (0, 0, 0)
p <- c(0, 0, 0)
distance <- rep(0, 6)

for(i in 1:6) {
  q <- c(x1[i], x2[i], x3[i])
  z <- rbind(p, q);
  distance[i] <- dist(z, method = "euclidean")
}

#Adding distance vector to data frame and sorting the data frame by the distance column
data$Distance <- distance
data <- data[order(data$Distance),]



#----------------------7(b)-----------------------------------
#Using K= 1 for prediction
prediction = estimate_mode(data$y[1:1])
message("--->With K=1, the prediction is ", as.character(prediction))



#----------------------7(c)-----------------------------------
#Using K= 3 for prediction
prediction = estimate_mode(data$y[1:3])
message("-->With K=3, the prediction is ", as.character(prediction))
