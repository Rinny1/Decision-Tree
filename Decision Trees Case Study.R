
1#------------------------------Preparing the environment for Decision Trees---------------------------------------#


list.of.packages <- c("caret", "e1071","ggplot2","rpart", "rpart.plot","pROC","ROCR","randomForest","caTools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)



2#==============================Setting the working directory===============================#
path1<-setwd("C:/Users/raveesh/Desktop/IVY/R/Decision Trees/letters_ABPR.csv")
getwd()


letters=read.csv("letters_ABPR.csv")
data1=letters

3#==============================Basic Exploration of the Data Set============================#
dim(letters)
str(letters)
summary(letters)
sapply(letters, function(x) sum(is.na(x)))

#==============================
#Dependent variable
#letter = the letter that the image corresponds to (A, B, P or R)  

#Independent Variables

#xbox = the horizontal position of where the smallest box covering the letter shape begins.  
#ybox = the vertical position of where the smallest box covering the letter shape begins. 
#width = the width of this smallest box.  
#height = the height of this smallest box.  
#onpix = the total number of "on" pixels in the character image  
#xbar = the mean horizontal position of all of the "on" pixels  
#ybar = the mean vertical position of all of the "on" pixels 
#x2bar = the mean squared horizontal position of all of the "on" pixels in the image 
#y2bar = the mean squared vertical position of all of the "on" pixels in the image 
#xybar = the mean of the product of the horizontal and vertical position of all of the "on" pixels in the image 
#x2ybar = the mean of the product of the squared horizontal position and the vertical position of all of the "on" pixels  
#xy2bar = the mean of the product of the horizontal position and the squared vertical position of all of the "on" pixels 
#xedge = the mean number of edges (the number of times an "off" pixel is followed by an "on" pixel, or the image boundary is hit) as the image is scanned from left to right, along the whole vertical length of the image 
#xedgeycor = the mean of the product of the number of horizontal edges at each vertical position and the vertical position 
#yedge = the mean number of edges as the images is scanned from top to bottom, along the whole horizontal length of the image 
#yedgexcor = the mean of the product of the number of vertical edges at each horizontal position and the horizontal position\ 


#==============================
letters$letter=as.factor(letters$letter)

# Q1===================Baseline Model

#===================Splitting the data into train and test data====================#
set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.6)
Train = subset(letters, spl==TRUE)
dim(Train)
str(Train)

Test = subset(letters, spl==FALSE)
dim(Test)
str(Test)


#=====================Testing the Accuracy of baselinemodel on test data set============================#
table(Test$letter)
321/(316+306+303+321)


#So baseline accuracy on testing set is 25.76%.


Q2#===============================CART Model============================================#
CART<-rpart(letter~.,data=Train, method = "class")
prp(CART)



1#-------------------------Checking the accuracy of the model in the test data------------------------------#
predictCART1<-predict(CART, newdata=Test, type = "class")
table(Test$letter,predictCART1)

(280+231+294+275)/nrow(Test)


# So accuracy of the model in  the test data set is 86.67%. 



2#========================Checking the accuracy of the model in the train data===============================#
predictCART2<-predict(CART, newdata=Train, type = "class")
table(Train$letter,predictCART2)


(424+365+438+425)/nrow(Train)


# So accuracy of the model in  the train data set is 88.34%. 



3#==========================ConfusionMatrix for test dataset
confusionMatrix(predictCART1,Test$letter)



4#==========================ConfusingMatrix for train dataset
confusionMatrix(predictCART2,Train$letter)


#============================End of Cart Model==========================================#



Q3#==========================Random  Forest Model


1#=========================== Building A Random Forest Model===========================================================#

set.seed(1000)
trainsmall<-Train[sample(nrow(Train),1870),]
str(trainsmall)

PredictForest1<-randomForest(letter~.,data = Train)
PredictForest1




2#=========================Checking the accuracy of the model on Test Data===================================#
predForest1<-predict(PredictForest1, newdata=Test, type = "class")
table(Test$letter,predForest1)
(316+298+321+293)/(316+294+12+321+14+1+288)

# So accuracy of the model in  the test data set is 98.55%. 



3#==================ConfusionMatrix
confusionMatrix(predForest1,Test$letter)



4#=====================Variable Importance chart in Random Forest
vu = varUsed(PredictForest1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(PredictForest1$forest$xlevels[vusorted$ix]), main = "Variable Importance Chart_Splitting")


#Y-edge is most important in terms of number of splits



5#====================Measuring Impurity in Randomforest Model
varImpPlot(PredictForest1, main = "Variable Importance Chart_Impurity Red")


#Xedgeycor is most important in terms of mean reduction in impurity



#=============================End of the Model=========================================#







