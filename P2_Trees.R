
## Decision Trees
# Single Tress

tree.monster<-tree(type~.-color,data=Monster)
summary(tree.monster) # Give us internal nodes, the number of terminal nodes, and the (training) error rate
# Show the tree structure
plot(tree.monster)
# Show the node labels
text(tree.monster,pretty=0) 
# Include the category names for any qualitative predictors, 
#rather than simply displaying a letter for each category
set.seed(123)


# Evaluate the performance of the classification tree for Monster data
# Estimate the test error rather than simply computing the training error
# Split the obs into a training set and a test set, buid the tree using the training set, and evaluate its performance on the test data

# Use 10 fold cv on Decision tree to estimate the prediction accuracy
set.seed(2)
tree.cv = cv.trees(10, type~.-color,Monster, Monster$type, tree) 
tree.cv #error rate = 0.358

# Next, we consider whether pruning the tree might lead to improved results. 
# Use 10 fold cv on Decision tree with pruning to estimate the prediction accuracy
set.seed(2)
cv.prune = cv.prune.trees(10,type~.-color, Monster, Monster$type, tree)
cv.prune #error rate = 0.356
######################################## Classification Trees End #####################################################

######################################## Bagging and Random Forests ###################################################

# Use bagging and random forests to construct more powerful prediction models.
# bagging (mtry=p, p is the number of predictors)
set.seed(2)
bag.monster<-randomForest(type~.-color,data=Monster,
                          mtry=5,ntree=100,importance=TRUE)
bag.monster
bag.oob.error = 1-sum(diag(bag.monster$confusion))/371 #error rate = 0.2722
plot(bag.monster,main="")
legend("topright",c("OOB: Bagging","class.error: Goblin","class.error: Ghoul","class.error: Ghost"),
       lty=c(1,2,2,2),col=c("black","blue","green","red"),cex=0.8)

# View the importance of each variable
importance(bag.monster)
# Plots of these importance measures
varImpPlot(bag.monster)

# random forest (mtry=p^0.5)
set.seed(2)
rf.monster<-randomForest(type~.,data=Monster,ntree=100,mtry=3,importance=TRUE)
rf.monster
rf.oob.error = 1-sum(diag(rf.monster$confusion))/371 #error rate = 0.2776
rf.oob.error # error rate = 0.2534

plot(rf.monster,main="")
legend("topright",c("OOB: Random Forests","class.error: Goblin","class.error: Ghoul","class.error: Ghost"),
       lty=c(1,2,2,2),col=c("black","blue","green","red"),cex=0.8)

# View the importance of each variable
importance(rf.monster)
# Plots of these importance measures
varImpPlot(rf.monster)


# Adaboost
set.seed(2)
ada.cv.model = boosting.cv(type~.-color ,data = Monster,mfinal = 50)
ada.cv.model$error #.2803

ada.Monster = boosting(type~.-color, data = Monster, mfinal = 50)
error.evol = errorevol(ada.Monster, Monster)
plot(error.evol)
importanceplot(ada.Monster)



