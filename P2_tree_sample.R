#### Single Decision Tree
set.seed(1)
train<-sample(1:nrow(Monster),247) # 2/3 as train set and 1/3 as test set
Monster.test<-Monster[-train,]
Monster.train = Monster[train,]
type.test<-Monster.test$type
tree.ie<-tree(type~.-color,Monster,subset=train)

summary(tree.monster)
plot(tree.monster)
text(tree.monster)

tree.pred<-predict(tree.ie,Monster.test,type="class")
table(tree.pred,type.test)
# The correct prediction rate is 0.6532258
tree.sig.error = sum(diag(table(tree.pred,type.test)))/sum(table(tree.pred,type.test))#0.6613

#### Pruned tree
set.seed(1)
cv.tree.ie<-cv.tree(tree.ie,FUN=prune.misclass,K=10) # 10 folds of the cross-validation
names(cv.tree.ie)
cv.tree.ie # The tree with 9 terminal nodes results in the lowest cross-validation error rate

prune.monster<-prune.misclass(tree.ie,best=9)
summary(prune.monster)
par(mfrow=c(1,1))
plot(prune.monster)
text(prune.monster,pretty=1)

# The pruned tree perform on the test data set
tree.prune.ie<-predict(prune.monster,Monster.test,type="class")
table(tree.prune.ie,type.test)
# The correct prediction rate is 0.71
sum(diag(table(tree.prune.ie,type.test)))/sum(table(tree.pred,type.test)) #0.6612


plot(cv.tree.ie)
par(mfrow=c(1,2))
plot(cv.tree.ie$size,cv.tree.ie$dev,type="b")
plot(cv.tree.ie$k,cv.tree.ie$dev,type="b") # xlim=c(0,10) a bigger size between 0 and 10

#### Random Forest
set.seed(1)
bag.ie<-randomForest(type~.,data=Monster,subset=train,mtry=5,ntree=100,importance=TRUE)
bag.ie
plot(bag.ie,main="")
legend("topright",c("OOB: Bagging","class.error: Goblin","class.error: Ghoul","class.error: Ghost"),
       lty=c(1,2,2,2),col=c("black","blue","green","red"),cex=0.8)

bag.pred.ie<-predict(bag.ie,Monster.test,type="class")
table(bag.pred.ie,type.test)
# The correct prediction rate is 0.7258065.
sum(diag(table(bag.pred.ie,type.test)))/sum(table(bag.pred.ie,type.test))
# The accuracy is improved from 66.1% (pruned tree) to 72.58%

#### Adaboost

ada.ie = boosting(type~.-color, data =Monster.train, mfinal = 50)
ada.predict.ie = predict.boosting(ada.ie, Monster.test)# 0.30645
evol.error.ie = errorvol(ada.ie, Monster.train)
plot(evol.error.ie)


