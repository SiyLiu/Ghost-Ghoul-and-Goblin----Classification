
library(nnet)
library(car)
library(mlogit)
library(adabag)
library(ggplot2)
library(dplyr)
library(RColorBrewer) 
library(car)
library(biotools)
library(MASS)
library(GGally)
library(purrr)
library(adabag)
library(tree)
library(randomForest)
library(e1071)

Monster = read.csv("L:\\3rdSemesterHere\\DM\\Project2\\train.csv")
test = read.csv("L:\\3rdSemesterHere\\DM\\Project2\\submit.csv")

Monster = Monster[,-1]


##EDA
# Boxplot

par(mfrow = c(2,2))
plot.bl<-ggplot(Monster,aes(Monster$type,Monster$bone_length))+geom_boxplot()+xlab("Type")+ylab("Bone Length")+geom_point()+
  theme(axis.title.y=element_text(size=11),axis.title.x=element_text(size=11),plot.title=element_text(hjust=0.5),panel.background = element_rect(fill = "white",colour="grey50"))+ggtitle("Type & Bone Length")+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_brewer(palette="Blues")

# rotting_flesh (severity of rot)
plot.rf<-ggplot(Monster,aes(Monster$type,Monster$rotting_flesh))+geom_boxplot()+xlab("Type")+ylab("Rotting Flesh")+geom_point()+
  theme(axis.title.y=element_text(size=11),axis.title.x=element_text(size=11),plot.title=element_text(hjust=0.5),panel.background = element_rect(fill = "white",colour="grey50"))+ggtitle("Type & Rotting Flesh")+theme(plot.title=element_text(hjust=0.5))

# hair_length (hair length measurements)
plot.hl<-ggplot(Monster,aes(Monster$type,Monster$hair_length))+geom_boxplot()+xlab("Type")+ylab("Hair Length")+geom_point()+
  theme(axis.title.y=element_text(size=11),axis.title.x=element_text(size=11),plot.title=element_text(hjust=0.5),panel.background = element_rect(fill = "white",colour="grey50"))+ggtitle("Type & Hair Length")+theme(plot.title=element_text(hjust=0.5))

# has_soul (extent of soullessness)
plot.hs<-ggplot(Monster,aes(Monster$type,Monster$has_soul))+geom_boxplot()+xlab("Type")+ylab("Extent of Soullessness")+geom_point()+
  theme(axis.title.y=element_text(size=11),axis.title.x=element_text(size=11),plot.title=element_text(hjust=0.5),panel.background = element_rect(fill = "white",colour="grey50"))+ggtitle("Type & Extent of Soullessness")+theme(plot.title=element_text(hjust=0.5))

## Put plots on one page
multiplot(plot.bl,plot.rf,plot.hl,plot.hs,cols = 2)


# Frequency distribution - color
Ghost<-Monster[Monster$type=="Ghost",]
Ghoul<-Monster[Monster$type=="Ghoul",]
Goblin<-Monster[Monster$type=="Goblin",]

plot.cl.Gst = ggplot()+geom_bar(data = Ghost, aes(x = color))+ggtitle("Ghost")+scale_fill_brewer(palette="Blues")
plot.cl.Gul = ggplot()+geom_bar(data = Ghoul, aes(x = color))+ggtitle("Ghoul")+scale_fill_brewer(palette="Blues")
plot.cl.Gin = ggplot()+geom_bar(data = Goblin, aes(x = color))+ggtitle("Goblin")+scale_fill_brewer(palette="Blues")

multiplot(plot.cl.Gst,plot.cl.Gul,plot.cl.Gin,cols = 3)

ggplot(Monster, aes(x = color, fill = type))+geom_bar()+scale_fill_brewer(palette="Set2")+ggtitle("Color v.s. Type")+
  theme(plot.title=element_text(hjust=0.5))


# Make the pairwise plot
par(xpd = TRUE)
par(mar = c(1,1,1,0.1))
my_colors <- brewer.pal(nlevels(as.factor(Monster$type)), "Set1")
scatterplotMatrix(~bone_length+hair_length+rotting_flesh+has_soul|type,data = Monster,reg.line="" , smoother="", col=my_colors , smoother.args=list(col="grey")  , pch=c(15,16,17) ,
                  main="Scatter plot with Three Cylinder Options",legend.plot = FALSE,cex = 0.7)
legend(0.6,0.8,c("Ghost","Ghoul","Gobin"),col = my_colors,pch = c(15,16,17),cex =1,bty = "n")





bl.dst=ggplot(data=Monster,aes(x = bone_length)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Bone length distribution per type')

hl.dst=ggplot(data=Monster,aes(x = hair_length)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Hair length distribution per type')

rf.dst = ggplot(data=Monster,aes(x = rotting_flesh)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Rotting flesh distribution per type')

hs.dst = ggplot(data=Monster,aes(x = has_soul)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Has soul distribution per type')

multiplot(bl.dst,hl.dst, rf.dst, hs.dst, cols = 2)


## Interaction terms

my_colors <- brewer.pal(nlevels(as.factor(Monster.interac$type)), "Set1")
scatterplotMatrix(~bone_rot+bone_hair+bone_soul+rot_hair+rot_soul+hair_soul|type,data = Monster.interac,reg.line="" , smoother="", col=my_colors , smoother.args=list(col="grey")  , pch=c(16,16,16) ,
                  main="Scatter plot with Interaction",legend.plot = FALSE,cex = 0.7)

#bone_hair, bone_soul, hair_soul should be kept
scatterplotMatrix(~bone_hair+bone_soul+hair_soul|type,data = Monster.interac,
                  reg.line="" , smoother="", col=my_colors , smoother.args=list(col="grey")  , pch=c(16,16,16) ,
                  main="Scatter plot with Interaction",cex = 0.7)







