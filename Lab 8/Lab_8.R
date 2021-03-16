#---------------a-------------------------------------------------
library(ISLR)
library(tree)
attach(OJ)


set.seed(1000)
train = sample(1:nrow(OJ), 800)
OJ.test = OJ[-train,]
OJ.train = OJ[train,]
Purchase.test = Purchase[-train]

#---------------b-------------------------------------------------
tree.oj = tree(Purchase~., OJ.train)
summary(tree.oj)

#---------------c-------------------------------------------------
tree.oj

#---------------d-------------------------------------------------
plot(tree.oj)
text(tree.oj, pretty = 1)

#---------------e-------------------------------------------------
pred = predict(tree.oj, OJ.test, type="class")
table(pred, Purchase.test)
test.error = round(mean(pred != Purchase.test)*100,2)
test.error
#---------------f-------------------------------------------------
cv.oj = cv.tree(tree.oj, FUN=prune.misclass)
cv.oj

#---------------g-------------------------------------------------
plot(cv.oj$size, cv.oj$dev, type="b")

#---------------h-------------------------------------------------
prune.oj =prune.misclass (tree.oj ,best=2)
plot(prune.oj)
text(prune.oj)

#---------------i-------------------------------------------------
tree.train.pruned = predict(prune.oj, OJ.train, type="class")
table(tree.train.pruned, OJ.train$Purchase)
train.error.pruned = round(mean(tree.train.pruned != OJ.train$Purchase)*100,2)
train.error.pruned
cat("Pruned: Train Error :", train.error.pruned)


tree.train.unpruned = predict(tree.oj, OJ.train, type="class")
table(tree.train.unpruned, OJ.train$Purchase)
train.error.unpruned = round(mean(tree.train.unpruned != OJ.train$Purchase)*100,2)
train.error.unpruned
cat("Unpruned: Train Error :", train.error.unpruned)

#---------------j-------------------------------------------------
tree.test.pruned = predict(prune.oj, OJ.test, type="class")
table(tree.test.pruned, Purchase.test)
test.error.pruned = round(mean(tree.test.pruned != Purchase.test)*100,2)
test.error.pruned
cat("Pruned: Test Error :", test.error.pruned)


tree.test.unpruned = predict(tree.oj, OJ.test, type="class")
table(tree.test.unpruned, Purchase.test)
test.error.unpruned = round(mean(tree.test.unpruned != Purchase.test)*100,2)
test.error.unpruned
cat("Unpruned: Test Error :", test.error.unpruned)


