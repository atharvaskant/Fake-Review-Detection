library(DMwR)
library(rpart)
library(caret)
library(SDMTools)
library(tree)
library(tidyverse)
library(e1071)

mydata = read.csv("C:\\Users\\athar\\Desktop\\FYP_Main\\Data\\YelpReviewData.csv")
c=which(is.na(mydata$cosinecost))
mydata$cosinecost[c]=0
data = mydata[c("review_length","abs_dev","MNRcount","cosinecost","Class")]
data <- data[sample(nrow(data)),]

ptrain=0.7
indices=sample(nrow(data), ptrain*nrow(data), replace = FALSE);
trdata=data[indices,]
tsdata=data[-indices,]
trdata <- trdata[sample(nrow(trdata)),]

nldx=c(50,100,150,200,300,500,700,900,1200, 1500, 2000, 2500)
nldata = c(0,50,150,300,500,800,1300,2000,2900,4100,5600,7600,10100)
maxldata=10500
uldata = trdata[(maxldata+1):nrow(trdata),]
uldata$Class = NA
result_dt = data.frame(LabeledData=integer(),SupervisedAcc=double(),SemiSupervisedAcc=double())

for (i in 2:length(nldata)-1){
  cat(nldx[i],"\n")
  ldatalow=nldata[i]+1
  ldatahigh=nldata[i+1]
  ldata=trdata[ldatalow:ldatahigh,]
  cat("length of ldata",nrow(ldata),"\n")
  model.dt=tree(Class~., data=ldata, method = "class")
  pred.dt=predict(model.dt, tsdata[,-5])
  pred.dt=ifelse(pred.dt>0.8, 1, 0)
  table.dt=confusion.matrix(tsdata$Class,pred.dt)
  sup.accuracy = (table.dt[1,1]+table.dt[2,2])/sum(table.dt)
  #Function for self train
  predfunc.nb <- function(m,d) { 
    l <- predict(m,d)
    c <- apply(predict(m,d),1,max)
    data.frame(cl=l,p=c)
  }
  semi.sup.data <- rbind(ldata,uldata)
  semi.sup.data[,5]=as.factor(semi.sup.data[,5])
  tsdata[,5]=as.factor(tsdata[,5])
  
  #SelfTrain
  nbST <- SelfTrain(Class ~ .,semi.sup.data,learner('rpart',list(method = "class")),'predfunc.nb',thrConf = 0.9, maxIts = 10, verbose = FALSE,percFull = 1)
  
  stpred.dt=predict(nbST,tsdata[,-5],type="class")
  #stpred.dt=ifelse(stpred.dt>0.8, 1, 0)
  stable.dt=confusion.matrix(tsdata$Class,stpred.dt)
  unsup.accuracy = (stable.dt[1,1]+stable.dt[2,2])/sum(stable.dt)
  
  result_dt[i,] = data.frame(nldx[i],sup.accuracy,unsup.accuracy)
  
}
xpdt=c(1,2,3,4,5,6,7,8,9,10,11,12)
dtr=ggplot(data=result_dt)+
  ggtitle("Supervised vs Semi-Supervised using Decision Tree Classifier (Random Sampling)")+
  geom_line(aes(x=xpdt,y=SupervisedAcc,colour="darkblue"),size=1.2)+
  geom_point(aes(x=xpdt,y=SupervisedAcc,colour="darkblue"),size=2)+
  geom_line(aes(x=xpdt,y=SemiSupervisedAcc,colour="red"),size=1.2)+
  geom_point(aes(x=xpdt,y=SemiSupervisedAcc,colour="red"),size=2)+
  scale_x_continuous(breaks=xpdt,label=c(50,100,150,200,300,500,700,900,1200,1500,2000,2500))+
  scale_y_continuous(limits = c(0.0,1.0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())+
  labs(y="Accuracy",x="No. of Labeled Data")+
  coord_fixed(ratio=4)+
  scale_color_discrete(name="",labels=c("Supervised Accuracy (No unlabeled data)","Semi-Supervised Accuracy (17k unlabeled data)"))+
  guides(colour=guide_legend(nrow=2))




dtr=ggplot(data=result_dt)+
  ggtitle("Supervised vs Semi-Supervised using Decision Tree Classifier (Random Sampling)")+
  geom_line(aes(x=xpdt,y=SupervisedAcc,colour="red"),size=1.2)+
  geom_point(aes(x=xpdt,y=SupervisedAcc,colour="red"),size=2)+
  geom_line(aes(x=xpdt,y=SemiSupervisedAcc,colour="darkblue"),size=1.2)+
  geom_point(aes(x=xpdt,y=SemiSupervisedAcc,colour="darkblue"),size=2)+
  scale_x_continuous(breaks=xpdt,label=c(50,100,150,200,300,500,700,900,1200,1500,2000,2500))+
  scale_y_continuous(limits = c(0.0,1.0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())+
  labs(y="Accuracy",x="No. of Labeled Data")+
  coord_fixed(ratio=4)+
  scale_color_discrete(name="",breaks=c("red","darkblue"),labels=c("Supervised Accuracy (No unlabeled data)","Semi-Supervised Accuracy (17k unlabeled data)"))+
  guides(colour=guide_legend(nrow=2))