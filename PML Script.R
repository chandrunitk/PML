#Reading the data 
training<-read.csv("C:\\Users\\i067181\\Downloads\\PML\\pml-training (1).csv")
testing<-read.csv("C:\\Users\\i067181\\Downloads\\PML\\pml-testing.csv")
library(caret)

#Detecting near-zero variance variables and removing them
nzv<- nearZeroVar(training,saveMetrics=TRUE)
nzv$index<-seq.int(nrow(nzv))
indices<-nzv$index[nzv$nzv=="TRUE"]
trainingWithoutNZV<-training[,-indices]

#Removing other non-relevant variables
indicesNotRelevant<- c(1,2,3,4)
trainingWithoutNZV<-trainingWithoutNZV[,-indicesNotRelevant]

#Removing variables with mostly NA values
trainingWithoutNA<-trainingWithoutNZV[,!apply(trainingWithoutNZV,2,function(x)any(is.na(x)))]

#Applying the Random Forest training function on a reduced sample
trainingReducedTwentyPercent<-createDataPartition(y=trainingWithoutNA$classe,p=0.20,list=FALSE)
trainingReducedTwentyPercent<-trainingWithoutNA[trainingReducedTwentyPercent,]
modelFitReducedTwentyPercent<-train(classe ~.,data=trainingReducedTwentyPercent,method="rf",prox=TRUE)
predicted<-predict(modelFitReducedTwentyPercent,trainingReducedTwentyPercent)
table(predicted,trainingReducedTwentyPercent$classe)

#Predicting using the trained model
trainingReducedTwentyPercent<-trainingReducedTwentyPercent[,!(names(trainingReducedTwentyPercent) %in% "predRight")]
trainingReducedTwentyPercentWithoutClasse<-trainingReducedTwentyPercent[,!(names(trainingReducedTwentyPercent) %in% "classe")]
testingReallyReduced<-testing[,(names(testing) %in% requiredNames)]
predictedTesting<-predict(modelFitReducedTwentyPercent,testingReallyReduced)
