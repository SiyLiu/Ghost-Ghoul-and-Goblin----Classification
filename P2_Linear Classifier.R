
## Logistic
#cv.logistic = function(num_fold,formula,data,cl,fun){
log.cv.error = rep(0,100)
for (i in 1:100){
logistic.cv = cv.logistic(10,type~.,Monster, Monster$type,multinom)#
logistic.cv
log.cv.error[i]=logistic.cv
}
log.cv.error.mean = mean(log.cv.error) #error rate = 0.2643
logistic.model = multinom(type~., data = Monster)

#significance
sd.error = summary(logistic.model)$standard.errors
coef.logistic = coef(logistic.model)
logistic.p = 1-pnorm(abs(coef.logistic/sd.error))
#Exclude color from the model

logistic.cv.error.2 = rep(0,100)
for (i in 1:100){
logistic.cv.2= cv.logistic(10,type~.-color, Monster, Monster$type,multinom)
logistic.cv.error.2[i] = logistic.cv.2
}
logistic.cv.error.2.mean= mean(logistic.cv.error.2) # error rate= 0.261

##LDA&QDA
#LDA
# Assumptions check before doing LDA 
# Every category has the same covariance matrix

# Normality check
par(xpd = FALSE)
par(mfrow = c(3,4))
for  (type in c("Ghost","Ghoul","Goblin")){
  for (i in 1:4){
    qqnorm(Monster[Monster$type==type,][,i], main = type )
    legend("topleft",colnames(Monster)[i],cex = 1.2,bty= "n")
    qqline(Monster[Monster$type==type,][,i])
  }
}
# Homogeneity test for covariance matrix
cov.gst = ggcorr(Monster[,1:4][Monster$type=="Ghost",],label = TRUE,label_round = 3)
cov.gul = ggcorr(Monster[,1:4][Monster$type=="Ghoul",],label = TRUE,label_round = 3)
cov.gin = ggcorr(Monster[,1:4][Monster$type=="Goblin",],label = TRUE, label_round=3)
# Different covariance matrix
multiplot(cov.gst,cov.gul,cov.gin, cols = 3)
boxM(Monster[,1:4],Monster$type) # p = 0.05874 - reject H0 - hetero..

# p-value is slightly greater than 0.05, we decide to try for it
lda.cv.error = rep(0,100)
for (i in 1:100){
lda.cv = cv(10, type~.-color, Monster, Monster$type, lda)  
lda.cv.error[i]=lda.cv 
}
lda.cv.error.mean = mean(lda.cv.error) # error rate = 0.2510
#QDA
qda.cv.error = rep(0,100)
for (i in 1:100){
qda.cv = cv(10, type~.-color, Monster, Monster$type, qda) # error rate = around 0.25 
qda.cv.error[i]=qda.cv
}
qda.cv.error.mean = mean(qda.cv.error) #error rate = 0.2530
attach(Monster)
data.inter = data.frame(bone_rot = bone_length*rotting_flesh,
                             bone_hair = bone_length*hair_length,
                             bone_soul = bone_length*has_soul,
                             rot_hair = rotting_flesh*hair_length,
                             rot_soul = rotting_flesh*has_soul,
                             hair_soul = hair_length*has_soul,
                             hair_soul_bone = hair_length*has_soul*bone_length,
                             bone_hair_rot = hair_length*rotting_flesh*bone_length,
                            bone_rot_soul = rotting_flesh*has_soul*bone_length,
                            hair_rot_soul = hair_length*has_soul*rotting_flesh
                        )
Monster.interac = cbind(data.inter,Monster)
detach(Monster)
attach(test)

test.inter = data.frame(bone_rot = bone_length*rotting_flesh,
                        bone_hair = bone_length*hair_length,
                        bone_soul = bone_length*has_soul,
                        rot_hair = rotting_flesh*hair_length,
                        rot_soul = rotting_flesh*has_soul,
                        hair_soul = hair_length*has_soul,
                        hair_soul_bone = hair_length*has_soul*bone_length,
                        bone_hair_rot = hair_length*rotting_flesh*bone_length,
                        bone_rot_soul = rotting_flesh*has_soul*bone_length,
                        hair_rot_soul = hair_length*has_soul*rotting_flesh
)

test.interac = cbind(test.inter,test)

Monster = Monster.interac[,c(1,2,3,4,5,6,7,8,9,10,15,16)]
