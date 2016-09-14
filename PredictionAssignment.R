library(caret)

# download the training and test data set files
if (!file.exists("pml-training.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                      "pml-training.csv", method = "curl")
}

if (!file.exists("pml-testing.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      "pml-testing.csv", method = "curl")
}

# inspect the csv files manually to see if they have NA values
# looks like we have NA and #DIV/0! are two possible values

trainingset <- read.csv("pml-training.csv", header = TRUE, 
                        na.strings=c("NA", "#DIV/0!") )
testingset <- read.csv("pml-testing.csv", header = TRUE, 
                       na.strings=c("NA", "#DIV/0!") )

str(trainingset)

# up to num_window variable they are not used in our model
trainingset <- trainingset[, -c(1,2,3,4,5,6,7)]

# remove na values from the training set as well
trainingset <-trainingset[, apply(trainingset, 2, function(x) !any(is.na(x)))]

# apply the variables for testset as well from the above
testingset <- testingset[,names(trainingset[,-53])]

# create training and test from training data for prediction analysis

inTrain<-createDataPartition(y=trainingset$classe, p=0.75,list=FALSE)
training<-trainingset[inTrain,] 
test<-trainingset[-inTrain,] 
#Training and test set dimensions
dim(training)

# we will use the following 3 models to predict
set.seed(18435)
fitControl <- trainControl(method="cv", number=5, allowParallel=TRUE, verbose=TRUE)

randomforestfit <- train(classe~.,data=training, method="rf", 
                       trControl=fitControl, verbose=FALSE)

randomforestpred <- predict(randomforestfit, newdata=test)

confusionMatrix(randomforestpred, test$classe)

# model 2

gmbfit <- train(classe~.,data=training, method="gbm", 
                trControl=fitControl, verbose=FALSE)

gmbpred <- predict(gmbfit, newdata=test)

confusionMatrix(gmbpred, test$classe)

# model 3

knnfit <- train(classe~.,data=training, method="knn") 

knnpred <- predict(knnfit, newdata=test)

confusionMatrix(knnpred, test$classe)

testingsetpred <-predict(randomforestfit, newdata=testingset)
# Output for the prediction of the 20 cases provided
testingsetpred
