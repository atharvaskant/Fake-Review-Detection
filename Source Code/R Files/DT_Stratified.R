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
#nldata = c(0,50,150,300,500,800,1300,2000,2900,4100,5600,7600,10100)
maxldata=sum(nldx)
uldata = trdata[(maxldata+1):nrow(trdata),]
uldata$Class = NA
trlabeled = trdata[1:maxldata,]
trdataone=trlabeled[which(trlabeled$Class==1),]
trdatazero=trlabeled[which(trlabeled$Class==0),]
trdataone <- trdataone[sample(nrow(trdataone)),]   #Shuffle Training data One
trdatazero <- trdatazero[sample(nrow(trdatazero)),]   #Shuffle Training data Zero
result_rdt = data.frame(LabeledData=integer(),SupervisedAcc=double(),SemiSupervisedAcc=double())

for (i in 1:length(nldx)){
  cat(nldx[i],"\n")
  valone=0.9*nldx[i]
  valzero=0.1*nldx[i]
  if (valone > nrow(trdataone)){
    break
  }
  valone = ifelse(valone < nrow(trdataone), valone, nrow(trdataone))
  oneidx=sample(nrow(trdataone),valone,replace = FALSE)
  ldataone=trdataone[oneidx,]
  trdataone=trdataone[-oneidx,]
  valzero = ifelse(valzero < nrow(trdatazero), valzero, nrow(trdatazero))
  zeroidx=sample(nrow(trdatazero),valzero,replace = FALSE)
  ldatazero=trdatazero[zeroidx,]
  trdatazero=trdatazero[-zeroidx,]
  ldata = rbind(ldataone,ldatazero)
  model.rdt=tree(Class~., data=ldata, method = "class")
  pred.rdt=predict(model.rdt, tsdata[,-5])
  pred.rdt=ifelse(pred.rdt>0.8, 1, 0)
  table.rdt=confusion.matrix(tsdata$Class,pred.rdt,threshold = 0.5)
  sup.accuracy = (table.rdt[1,1]+table.rdt[2,2])/sum(table.rdt)
  
  
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
  
  stpred.rdt=predict(nbST,tsdata[,-5],type="class")
  #stpred.dt=ifelse(stpred.dt>0.8, 1, 0)
  stable.rdt=confusion.matrix(tsdata$Class,stpred.rdt)
  unsup.accuracy = (stable.rdt[1,1]+stable.rdt[2,2])/sum(stable.rdt)
  
  
  result_rdt[i,]=c(nldx[i],sup.accuracy,unsup.accuracy)
  
}
dts=ggplot(data=result_rdt)+
  ggtitle("Supervised vs Semi-Supervised using Decision Tree Classifier (Stratified Sampling)")+
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
  scale_color_discrete(name="",labels=c("Supervised Accuracy (No unlabeled data)","Semi-Supervised Accuracy (17k unlabeled data)"))+
  guides(colour=guide_legend(nrow=2))




dts=ggplot(data=result_rdt)+
  ggtitle("Supervised vs Semi-Supervised using Decision Tree Classifier (Stratified Sampling)")+
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


