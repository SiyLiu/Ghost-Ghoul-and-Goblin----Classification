
Monster = MM
#########################################-- linear kernel
set.seed(2)
cv.svm(10,type~.-color,Monster,Monster$type)
# svm.model <- svm(type~., data=Monster.interac, C=1, kernel='linear')
# svm.model$index
# summary(svm.model)
# use tune function to decide the best model (decide best C)
set.seed(2)
tune.out<-tune(svm, type~., data=MM, kernel='linear', 
               ranges=list(cost=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)))
summary(tune.out)

# get the information of the best model with Cost=1, error= 0.2369843 cv_scores = 0.763
bestmod = tune.out$best.model
bestmod = svm(type~., Monster,kernel = "linear",C = 1)
summary(bestmod)
 
# plot svm classification based on variables
plot(svm.model, Monster, bone_length~rotting_flesh)
plot(svm.model, Monster, bone_length~hair_length)
plot(svm.model, Monster, bone_length~has_soul)
plot(svm.model, Monster, hair_length~rotting_flesh)
plot(svm.model, Monster, has_soul~rotting_flesh)
plot(svm.model, Monster, hair_length~has_soul)

#########################################

# try another radial basis kernel
clf1 <- svm(type~ ., data=Monster, C=1, kernel='radial')
clf1$index
summary(clf1)

# use tune function to decide the best model (decide best C)
tune.out1<-tune(svm, type~., data=Monster, kernel='radial', ranges=list(cost=c(0.001,0.01,0.1,0.5,1,5,10)))
summary(tune.out1)

# get the information of the best model with C=5, error=0.264085 cv_scores=0.735915
bestmod1 = tune.out1$best.model
summary(bestmod1)
 
# plot svm classification based on variables
plot(clf1, Monster, bone_length~rotting_flesh)
plot(clf1, Monster, bone_length~hair_length)
plot(clf1, Monster, bone_length~has_soul)
plot(clf1, Monster, hair_length~rotting_flesh)
plot(clf1, Monster, has_soul~rotting_flesh)
plot(clf1, Monster, hair_length~has_soul)

# use the best model to make prediction, the score is 0.71644
pred_test1 <- predict(bestmod1,test)
summary(pred_test1)
write.csv(pred_test1, file="TEST1.csv")
# so much for the radial basis kernel

##########################################

# try another polynomial kernel degree=3, coef=0
clf2 <- svm(type~ ., data=Monster, C=1, kernel='polynomial')
clf2$index
summary(clf2)

# use tune function to decide the best model (decide best C)
tune.out2<-tune(svm, type~., data=Monster, kernel='polynomial', ranges=list(cost=c(0.001,0.01,0.1,0.5,1,5,10)))
summary(tune.out2)

# get the information of the best model with cost=10, error=0.2854908, cv_scores=0.71451
bestmod2 = tune.out2$best.model
summary(bestmod2)

# plot svm classification based on variables
plot(clf2, Monster, bone_length~rotting_flesh)
plot(clf2, Monster, bone_length~hair_length)
plot(clf2, Monster, bone_length~has_soul)
plot(clf2, Monster, hair_length~rotting_flesh)
plot(clf2, Monster, has_soul~rotting_flesh)
plot(clf2, Monster, hair_length~has_soul)

# use the best model to make prediction, the score is 0.68620
pred_test2 <- predict(bestmod2,test)
summary(pred_test2)
write.csv(pred_test2, file="TEST2.csv")
# so much for the polynomial kernel

#############################################

# try another sigmoid kernel  coef:0
clf3 <- svm(type~ ., data=Monster, C=1, kernel='sigmoid')
clf3$index
summary(clf3)

# use tune function to decide the best model (decide best C)
tune.out3<-tune(svm, type~., data=Monster, kernel='sigmoid', ranges=list(cost=c(0.001,0.01,0.1,0.5,1,5,10)))
summary(tune.out3)

# get the information of the best model with cost=0.1, error=0.2478663, cv_scores=0.7521337
bestmod3 = tune.out3$best.model
summary(bestmod3)

# plot svm classification based on variables
plot(clf3, Monster, bone_length~rotting_flesh)
plot(clf3, Monster, bone_length~hair_length)
plot(clf3, Monster, bone_length~has_soul)
plot(clf3, Monster, hair_length~rotting_flesh)
plot(clf3, Monster, has_soul~rotting_flesh)
plot(clf3, Monster, hair_length~has_soul)

# use the best model to make prediction, the score is 0.73724
pred_test3 <- predict(bestmod3,test)
summary(pred_test3)
write.csv(pred_test3, file="TEST3.csv")
# so much for the sigmoid kernel 
