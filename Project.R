  library(caret)
  library(randomForest)
  library(parallel)
  library(doParallel)
  #cluster <- makeCluster(detectCores() - 1)
  #registerDoParallel(cluster)
  set.seed(3859)
  
  if(!file.exists("./pml-training.csv")) {
  trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(trainUrl, destfile = "./pml-training.csv",method = "curl")
  }
  training <- read.csv("./pml-training.csv")
  
  if(!file.exists("./pml-testing.csv")) {
  testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(testUrl, destfile = "./pml-testing.csv",method = "curl")
  }
  testing <- read.csv("./pml-testing.csv")
  
  #Let's extract a validation set from the training set. 
  #We will use this validation set to test our model.
  partition <- createDataPartition(y=training$classe,p=0.8, list=FALSE) 
  validation <- training[ -partition , ]
  training <- training[ partition , ]
  
  ## Run exploratory analysis
  summary(training)
  length(colSums(is.na(training)))
  
  # Feature Selection
  # 7 first variables are not interesting for prediction
  training <- training[, -(1:7)]
  dim(training)[2]
  # Let's delete variables where more than 80% of the measures are NA
  largeNA <- which(colSums(is.na(training))/length(training$X) >= 0.8)
  training <- training[, -largeNA]
  dim(training)[2]
  # Let's use nearZeroVar to delete variables whose variance is low
  training <- training[, -nearZeroVar(training)]
  dim(training)[2]
  
  # Let's delete the variables highly correlated
  correlationMatrix <- cor(training[,-53])
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.8)
  training <- training[, -highlyCorrelated]
  dim(training)[2]
  
  train_control <- trainControl(method="cv", number=10,savePredictions = TRUE, allowParallel = TRUE) 
  #Train Random Forest : why rf ? How to tune ?
  rf <- train(classe ~ ., data=training, trControl=train_control, method="rf", prox = TRUE, ntree = 10)
  
  #Stop parallel processing
  #stopCluster(cluster)
  #registerDoSEQ()
  
  rf$resample 
  
  #Display one of the trees and the variables at the top of the tree
  tree2 <- getTree(rf$finalModel, k=2) 
  head(tree2)
  index_tree2 <- tree2[1:6,3]
  colnames(training)[index_tree2]
  
  #Let's use our model on the validation set to confirm its validity
  predictions <- predict(rf, validation)
  confusionMatrix(predictions , validation$classe)
  
  # Display out of sample error on validation test
  confusionMatrix(predictions , validation$classe)$overall["Accuracy"]
  1 - confusionMatrix(predictions , validation$classe)$overall["Accuracy"]
 