
#-------------------a------------------------------------
x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
y = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col=y)

#-------------------b------------------------------------
abline(-0.5, 1)

#-------------------d------------------------------------
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

#-------------------g------------------------------------
plot(x1, x2, col=y)
abline(-0.7, 1)

#-------------------h------------------------------------
x1 = c(x1, 1.5)
x2 = c(x2, 2)
y = c(y, "blue")
plot(x1, x2, col=y)