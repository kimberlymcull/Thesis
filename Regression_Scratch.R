####Importing & formatting####
#import libraries 
library(data.table)
library(caTools)
library(gbm)
library(insol)
library(nnet)
library(glmnet)
library(dplyr)
library(knitr)
library(ggplot2)


#import track and complete signal data
Signal <- read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/CompleteData_BySignal.csv")
Track  <- read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/TrackData.csv")

#convert to data.table and other formatting
Signal=data.table(Signal)
Track=data.table(Track)

#manipulate time into usable format (julian dates)
options(digits=16)
Track$Time_min=JD(as.POSIXct(Track$Time_min), inverse=FALSE)
Track$Time_max=JD(as.POSIXct(Track$Time_max), inverse=FALSE)
Signal$Time=JD(as.POSIXct(Signal$Time), inverse=FALSE)
options(digits=7)
Track$increments=JD(as.POSIXct(Track$increments), inverse=FALSE)

#other change to appropriate class
Track$dim_a=as.numeric(Track$dim_a)
Track$dim_b=as.numeric(Track$dim_b)
Track$dim_c=as.numeric(Track$dim_c)
Track$dim_d=as.numeric(Track$dim_d)
Track$id=as.factor(Track$id)
Signal$dim_a=as.numeric(Signal$dim_a)
Signal$dim_b=as.numeric(Signal$dim_b)
Signal$dim_c=as.numeric(Signal$dim_c)
Signal$dim_d=as.numeric(Signal$dim_d)
Signal$id=as.factor(Signal$id)


####Training and test sets####
#set seed
set.seed(12)
Signal$sample = sample.split(Signal, SplitRatio = .90)
Track$sample= sample.split(Track, SplitRatio = .90)

#Training 90%
SigTrain= subset(Signal, Signal$sample==TRUE)
TrackTrain= subset(Track, Track$sample==TRUE)
SigTrain$sample=NULL
TrackTrain$sample=NULL
SigTrain$X=NULL
TrackTrain$X=NULL
#Test 10%
SigTest= subset(Signal, Signal$sample==FALSE)
TrackTest= subset(Track, Track$sample==FALSE)
SigTest$sample=NULL
TrackTest$sample=NULL
SigTest$X=NULL
TrackTest$X=NULL

#Classes of variables
str(TrackTrain)
str(SigTrain)

#output csvs
write.csv(TrackTest, file = "TrackTest.csv")
write.csv(SigTest,file="SignalTest.csv")
write.csv(TrackTrain, file = "TrackTrain.csv")
write.csv(SigTrain,file="SignalTrain.csv")

#import
TrackTest <- read.csv("~/Documents/OPERATIONS_RESEARCH/R/TrackTest.csv")
TrackTrain <- read.csv("~/Documents/OPERATIONS_RESEARCH/R/TrackTrain.csv")
TrackTest$X=NULL
TrackTrain$X=NULL
TrackTest=as.data.table(TrackTest)
TrackTrain=as.data.table(TrackTrain)
TrackTest$id=as.factor(TrackTest$id)
TrackTrain$id=as.factor(TrackTrain$id)

####Classification tree regression- Random Forest####
#TrackTrain data
TrackTrain.boost= gbm(Country~., data=TrackTrain[,-c(1,15,16,17)], distribution="multinomial",n.trees=500,interaction.depth=3)
best.iter <- gbm.perf(TrackTrain.boost,method="OOB")
print(best.iter)
summary(TrackTrain.boost,n.trees=best.iter)
print(pretty.gbm.tree(TrackTrain.boost,1))
print(pretty.gbm.tree(TrackTrain.boost,TrackTrain.boost$n.trees))
f.predict <- predict(TrackTrain.boost,TrackTest[,-c(1,15,16,17)],best.iter)
f.predict

TrackTrain1.boost= gbm(Country~., data=TrackTrain[,-c(1,15,16,17)], distribution="multinomial",n.trees=1000,interaction.depth=3)
best.iter <- gbm.perf(TrackTrain1.boost,method="OOB")


#colnames(TrackTrain)
#colnames(TrackTrain[,-c(1,15,16,17)])


####Neural network####
str(TrackTrain)
summary(TrackTrain)
#Table
data.frame(table(TrackTrain$Country))
#91 countries
#plot id by country
idgrp=group_by(TrackTrain, Country, id) %>% summarise(count=n()) %>%
  group_by(Country) %>% mutate(etotal=sum(count), proportion= count/etotal)
ggplot(idgrp, aes(x=Country, y= proportion, group=id, linetype=id)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))+
  ggtitle("Vessel Type by Country of Origin") +
  labs(y= "Proportion of Vessels", x="Vessel Country of Origin")
#regression
mmod= multinom(Country~., TrackTrain[,-c(1,15,16,17)], MaxNWts= 22000)
mmodi=step(mmod)








library(data.table)
#import
TrackTest <- read.csv("~/R/TrackTest.csv")
TrackTrain <- read.csv("~/R/TrackTrain.csv")
TrackTest$X=NULL
TrackTrain$X=NULL
TrackTest=as.data.table(TrackTest)
TrackTrain=as.data.table(TrackTrain)
TrackTest$id=as.factor(TrackTest$id)
TrackTrain$id=as.factor(TrackTrain$id)



#xgboost and h2o




####Break up Data into Validation set####
#Set Seed
set.seed(12)
#Split Training into 25%, 75%
library(caTools)
TrackTrain$sample = sample.split(TrackTrain, SplitRatio = .25)
#Validation 25%
TrackValidation= as.data.table(subset(TrackTrain, TrackTrain$sample==TRUE))
TrackTrain$sample=NULL
TrackValidation$sample=NULL


####Classification tree regression- Boosted Trees####
#Validation set 
library(gbm)
#default learning rate (shrinkage) .001
Validation.boost= gbm(Country~., data=TrackValidation[,-c(1,15,16,17)], distribution="multinomial",n.trees=1000,interaction.depth=3)
best.iter <- gbm.perf(Validation.boost,method="OOB")
print(best.iter)
summary(Validation.boost,n.trees=best.iter)
print(pretty.gbm.tree(Validation.boost,1))
print(pretty.gbm.tree(Validation.boost,Validation.boost$n.trees))
f.predict <- predict(Validation.boost,TrackValidation[,-c(1,14,15,16,17)],best.iter)
TrackValidation$Predicted=colnames(f.predict)[apply(f.predict, 1, which.max)]
TrackValidation$Correct= ifelse(TrackValidation$Country==TrackValidation$Predicted,1,0)
nrow(TrackValidation[TrackValidation$Correct==1])/nrow(TrackValidation)
#0.3784098

#add 100 trees
Validation.boost= gbm.more(Validation.boost, n.new.trees=100)
best.iter <- gbm.perf(Validation.boost,method="OOB")
print(best.iter)
summary(Validation.boost,n.trees=best.iter)
print(pretty.gbm.tree(Validation.boost,1))
print(pretty.gbm.tree(Validation.boost,Validation.boost$n.trees))
f.predict <- predict(Validation.boost,TrackValidation[,-c(1,15,16,17)],best.iter)
f.predict



