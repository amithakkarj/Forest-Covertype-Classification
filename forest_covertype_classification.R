# Data Preprocessing
rm(list=ls())


# Importing the dataset
dataset =read.csv('C:/Users/Ami Thakkar/Downloads/forest-cover-type-prediction/train.csv', head=T, stringsAsFactors=F, na.strings='')
#removing id,Soil_Type_7 and Soil_Type_15
dataset=dataset[,c(-1,-22,-30)]

#view(dataset)
length(boxplot(dataset$Slope)$out) #understand more

#check and delete outliers
i = 1
dataset[,1:53] = lapply(dataset[,1:53],as.numeric)
vizu = round(cor(dataset),1) #why does it give a warnng message? --> In cor(dataset) : the standard deviation is zero
library(ggcorrplot)
#checking for correlation
ggcorrplot(vizu)

#function for finding outliers
while(i <= ncol(dataset)){
  
  
  thirdQuantile = quantile(dataset[,i],0.75)
  upperOutlier = (IQR(dataset[,i]) * 3) + thirdQuantile
  #print(upperOutlier)
  x = length(which(dataset[,i]>upperOutlier))
  
  
  
  firstQuantile = quantile(dataset[,i],0.25)
  lowerOutlier =  firstQuantile -(IQR(dataset[,i])* 3)
  #print(lowerOutlier)
  y = length(which(dataset[,i] < lowerOutlier))
  
  print(paste(x+y," is the number of outliers in ", colnames(dataset)[i]))
  
  i = i+1
  
}
thirdQuantile = quantile(dataset$Horizontal_Distance_To_Hydrology,0.75)
upperOutlier = (IQR(dataset$Horizontal_Distance_To_Hydrology) * 3) + thirdQuantile
firstQuantile = quantile(dataset$Horizontal_Distance_To_Hydrology,0.25)
lowerOutlier =  firstQuantile -(IQR(dataset$Horizontal_Distance_To_Hydrology)* 3)

removeUpper = which(dataset$Horizontal_Distance_To_Hydrology > upperOutlier)
removeLower = which(dataset$Horizontal_Distance_To_Hydrology < lowerOutlier)
#removing upper outlier for Horizontal_Distance_To_Hydrology
if(length(removeUpper)>0){
  dataset = dataset[-removeUpper,]
}
#removing lower outlier for Horizontal_Distance_To_Hydrology
if(length(removeLower)>0){
  dataset = dataset[-removeLower,]
  
}

thirdQuantile = quantile(dataset$Horizontal_Distance_To_Roadways,0.75)
upperOutlier = (IQR(dataset$Horizontal_Distance_To_Roadways) * 3) + thirdQuantile
firstQuantile = quantile(dataset$Horizontal_Distance_To_Roadways,0.25)
lowerOutlier =  firstQuantile -(IQR(dataset$Horizontal_Distance_To_Roadways)* 3)
removeUpper = which(dataset$Horizontal_Distance_To_Roadways > upperOutlier)
removeLower = which(dataset$Horizontal_Distance_To_Roadways < lowerOutlier)
#removing upper outlier for Horizontal_Distance_To_Roadways
if(length(removeUpper)>0){
  dataset = dataset[-removeUpper,]
}
#removing lower outlier for Horizontal_Distance_To_Roadways
if(length(removeLower)>0){
  dataset = dataset[-removeLower,]
  
}

thirdQuantile = quantile(dataset$Horizontal_Distance_To_Fire_Points,0.75)
upperOutlier = (IQR(dataset$Horizontal_Distance_To_Fire_Points) * 3) + thirdQuantile
firstQuantile = quantile(dataset$Horizontal_Distance_To_Fire_Points,0.25)
lowerOutlier =  firstQuantile -(IQR(dataset$Horizontal_Distance_To_Fire_Points)* 3)
removeUpper = which(dataset$Horizontal_Distance_To_Fire_Points > upperOutlier)
removeLower = which(dataset$Horizontal_Distance_To_Fire_Points < lowerOutlier)
#removing upper outlier for Horizontal_Distance_To_Fire_Points
if(length(removeUpper)>0){
  dataset = dataset[-removeUpper,]
}
#removing lower outlier for Horizontal_Distance_To_Fire_Points
if(length(removeLower)>0){
  dataset = dataset[-removeLower,]
  
}

thirdQuantile = quantile(dataset$Vertical_Distance_To_Hydrology,0.75)
upperOutlier = (IQR(dataset$Vertical_Distance_To_Hydrology) * 3) + thirdQuantile
firstQuantile = quantile(dataset$Vertical_Distance_To_Hydrology,0.25)
lowerOutlier =  firstQuantile -(IQR(dataset$Vertical_Distance_To_Hydrology)* 3)
removeUpper = which(dataset$Vertical_Distance_To_Hydrology > upperOutlier)
removeLower = which(dataset$Vertical_Distance_To_Hydrology < lowerOutlier)
#removing upper outlier for Vertical_Distance_To_Hydrology
if(length(removeUpper)>0){
  dataset = dataset[-removeUpper,]
}
#removing lower outlier for Vertical_Distance_To_Hydrology
if(length(removeLower)>0){
  dataset = dataset[-removeLower,]
  
}
#checking dimensions for cleaned data
dim(dataset)

#scaling cleaned dataset
dataset[,c(1:10)]= scale(dataset[,c(1:10)])
rownames(dataset) = seq.int(nrow(dataset))
#importing libraries for CART and Random forest
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

#splitting dataset into test and train
set.seed(123)
splitting = sample.split(dataset$Cover_Type , SplitRatio = 0.6)
dataset_train = subset(dataset , splitting == TRUE)
dataset__test  = subset(dataset ,splitting == FALSE)
#applying CART on train data
classtree = rpart(Cover_Type ~., method = "class", data = dataset_train )
#predict full tree
predict_fulltree= predict(classtree , newdata = dataset__test, type = "class")
#confusion matrix for predicted full tree
confusion_matrix_fulltree = table(predict_fulltree, dataset__test$Cover_Type)
#finding accuracy for full tree
Accuracy_fulltree = sum(diag(confusion_matrix_fulltree))/nrow(dataset__test)

#plotting tree using prp()
prp(classtree)
#plotting tree using plot() and text()
plot(classtree)
text(classtree)
#plotting tree using rpart.plot()
rpart.plot(classtree, box.palette = 0)
names(classtree)
#finding CP for full tree
classtree$cptable
#prunning tree at lowest xerror 
prune_classtree = prune(classtree , cp =0.01715333 )
#plotting prune tree using rpart.plot()
rpart.plot(prune_classtree,box.palette = 0)
#plotting prune tree using prp()
prp(prune_classtree)
#predict test data using prune tree
predict_classification = predict(prune_classtree, newdata = dataset__test, type = "class")
#find accuracy for prune tree
confusion_matrix_class=table(dataset__test$Cover_Type, predict_classification)
confusion_matrix_class
Accuracy = sum(diag(confusion_matrix_class))/nrow(dataset__test)
Accuracy
#classification of test data with prune tree
plot(predict_classification, main ="Classification")
#CART using anova
regerss_tree = rpart(Cover_Type ~., method = "anova", data = dataset_train )
prp(regerss_tree)
rpart.plot(regerss_tree)
predicccc = predict(regerss_tree , newdata = dataset__test, type = "matrix")


#plotting random forest with nt=200
Random_tree = randomForest(factor(Cover_Type) ~. , data =dataset_train,ntree=200 )
plot(Random_tree)
#factoring Cover_Type
dataset__test$Cover_Type = as.factor(dataset__test$Cover_Type)
#predict test data with random forest
predict_randomforest = predict(Random_tree , newdata = dataset__test)
#finding accuracy for RAndom forest
confusion_matrix_RForest=table(dataset__test$Cover_Type , predict_randomforest)
confusion_matrix_RForest
Accuracy_RF = sum(diag(confusion_matrix_RForest))/nrow(dataset__test)
Accuracy_RF
#summary for predicted random forest
summary(predict_randomforest)
#plotting predicted random forest
plot(predict_randomforest, main = "Random Forest Classification")
 ##
probs= treeres



#KNN
#divide test and train data based on outcome variable
library(class)
Xtrain = dataset_train[1:52]
Xtest = dataset__test[1:52]
ytrain = dataset_train[53]
ytest = dataset__test[53]
#try k values for sqrt(train)
klen=round(sqrt(nrow(Xtrain)),0)

tpredict<-numeric() #Holding variable
for(j in 1:klen){
  #Apply knn with k = i
  ypred<-knn(Xtrain,Xtest,ytrain$Cover_Type,k=j)
  #finding accurately predicted values
  tpredict<-c(tpredict,
              mean(ypred==ytest$Cover_Type))
}
#validation error
error<-1-tpredict
#plotting validation error with K values
plot(1-tpredict,type="l",ylab="Error Rate",xlab="K",xaxt="n",main="Error Rate for cover type With Varying K")
axis(1, seq(0,klen, 1),las=2, font=2,cex.axis=0.8)

#predicting results and accuracy for k=4
ypred1 = knn(Xtrain, Xtest, ytrain$Cover_Type, k=4, prob=T)
cm1=as.matrix(table(ytest$Cover_Type, ypred1)) #confusion matrix
AccuracyK4=sum(diag(cm1))/length(ytest$Cover_Type) #accuracy

#predicting results and accuracy for k=12
ypred2 = knn(Xtrain, Xtest, ytrain$Cover_Type, k=12, prob=T)
cm2=as.matrix(table(ytest$Cover_Type, ypred2))
Accuracyk12=sum(diag(cm2))/length(ytest$Cover_Type)

#predicting results and accuracy for k=16
ypred3 = knn(Xtrain, Xtest, ytrain$Cover_Type, k=16, prob=T)
cm3=as.matrix(table(ytest$Cover_Type, ypred3))
Accuracyk16=sum(diag(cm3))/length(ytest$Cover_Type)

#test data classification with k=4
plot(ypred1, main = "knn  Classification")

