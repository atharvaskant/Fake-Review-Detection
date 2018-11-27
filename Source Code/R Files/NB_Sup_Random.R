mydata = read.csv("C:\\Users\\athar\\Desktop\\FYP_Main\\Data\\YelpReviewData.csv")
c=which(is.na(mydata$cosinecost))
mydata$cosinecost[c]=0
library(DMwR)
library(e1071)
library(caret)
library(SDMTools)
library(ggplot2)
library(tidyr)
data = mydata[c("review_length","abs_dev","MNRcount","cosinecost","Class")]
##Shuffle data
data <- data[sample(nrow(data)),]
ptrain=0.7
indices=sample(nrow(data), ptrain*nrow(data), replace = FALSE)
trdata=data[indices,]
tsdata=data[-indices,]

trdata <- trdata[sample(nrow(trdata)),]

nldx=c(50,100,150,200,300,500,700,900,1200,1500,2000)
maxldata=sum(nldx)
uldata = trdata[(maxldata+1):nrow(trdata),]
uldata$Class = NA
result1 = data.frame(LabeledData=integer(),SupervisedAcc=double(),SemiSupervisedAcc=double())
for (i in 1:length(nldx)){
  cat(nldx[i],"\n")
  ldata = trdata[1:nldx[i],]
  ldata <- ldata[sample(nrow(ldata)),]
  model.nb=naiveBayes(Class~., data=ldata)
  pred.nb=data.frame(predict(model.nb, tsdata[,-5], type="raw"))
  pred.nb$label=sapply(1:nrow(pred.nb), function(x) ifelse(pred.nb[x,]$X0>=pred.nb[x,]$X1, 0, 1))
  table.nb=confusion.matrix(tsdata$Class,pred.nb$label, threshold = 0.5)
  sup.accuracy = (table.nb[1,1]+table.nb[2,2])/sum(table.nb)
  
  
  #Function for self train
  predfunc.nb <- function(m,d) {
    p <- predict(m,d,type='raw')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
  }
  semi.sup.data <- rbind(ldata,uldata)
  
  #SelfTrain
  nbST <- SelfTrain(Class ~ .,semi.sup.data,learner('naiveBayes',list()),'predfunc.nb',thrConf = 0.9, maxIts = 10, verbose = FALSE,percFull = 1)
  
  stpred.nb=data.frame(predict(nbST,tsdata[,-5],type="raw"))
  stpred.nb$label=sapply(1:nrow(stpred.nb), function(x) ifelse(stpred.nb[x,]$X0>=stpred.nb[x,]$X1, 0, 1))
  
  stable.nb=confusion.matrix(tsdata$Class,stpred.nb$label, threshold = 0.5)
  unsup.accuracy = (stable.nb[1,1]+stable.nb[2,2])/sum(stable.nb)
  
  result1[i,]=c(nldx[i],sup.accuracy,unsup.accuracy)
  
  
}


nbr=ggplot(data=result1)+
  ggtitle("Supervised vs Semi-Supervised using Naïve Bayes (Random Sampling)")+
  geom_line(aes(x=xp,y=SupervisedAcc,colour="darkblue"),size=1.2)+
  geom_point(aes(x=xp,y=SupervisedAcc,colour="darkblue"),size=2)+
  geom_line(aes(x=xp,y=SemiSupervisedAcc,colour="red"),size=1.2)+
  geom_point(aes(x=xp,y=SemiSupervisedAcc,colour="red"),size=2)+
  
  scale_x_continuous(breaks=xp,label=c(50,100,150,200,300,500,700,900,1200,1500,2000))+
  scale_y_continuous(limits = c(0.0,1.0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())+
  labs(y="Accuracy",x="No. of Labeled Data")+
  coord_fixed(ratio=4)+
  scale_color_discrete(name="",breaks=c("red","darkblue"),labels=c("Supervised Accuracy (No unlabeled data)","Semi-Supervised Accuracy (17k unlabeled data)"))+
  guides(colour=guide_legend(nrow=2))



nbr=ggplot(data=result1)+
  ggtitle("Supervised vs Semi-Supervised using Naïve Bayes (Random Sampling)")+
  geom_line(aes(x=xp,y=SupervisedAcc,colour="red"),size=1.2)+
  geom_point(aes(x=xp,y=SupervisedAcc,colour="red"),size=2)+
  geom_line(aes(x=xp,y=SemiSupervisedAcc,colour="darkblue"),size=1.2)+
  geom_point(aes(x=xp,y=SemiSupervisedAcc,colour="darkblue"),size=2)+
 
  scale_x_continuous(breaks=xp,label=c(50,100,150,200,300,500,700,900,1200,1500,2000))+
  scale_y_continuous(limits = c(0.0,1.0),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())+
  labs(y="Accuracy",x="No. of Labeled Data")+
  coord_fixed(ratio=4)+
  scale_color_discrete(name="",breaks=c("red","darkblue"),labels=c("Supervised Accuracy (No unlabeled data)","Semi-Supervised Accuracy (17k unlabeled data)"))+
  guides(colour=guide_legend(nrow=2))
  

