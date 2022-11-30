#Model.R


# Importing libraries
library(RCurl) # for downloading CSV file
library(randomForest)
library(caret)
library(ROSE)
library(rlang)
setwd("/Users/karankaushal/Oracle Content - Accounts/Oracle Content/Documents Karan/StFX/CSCI 225/ProjectWork/Testing webapp")
# Importing data set
clean_data <- read.csv("NewStrokeDataset.csv")

clean_data <- subset(clean_data, select = -c(id,Residence_type,Age_Category,work_type))


set.seed(666)

clean_data$gender <- as.factor(clean_data$gender)
clean_data$ever_married <- as.factor(clean_data$ever_married)
clean_data$smoking_status <- as.factor(clean_data$smoking_status)
clean_data$stroke <- as.factor(clean_data$stroke)
clean_data$heart_disease <- as.factor(clean_data$heart_disease)
clean_data$hypertension <- as.factor(clean_data$hypertension)
clean_data$bmi <- as.numeric(clean_data$bmi)


oversampled_data <- ovun.sample(as.factor(stroke)~.,data = clean_data, method = 'over',p = 0.3)$data
sample_index <- createDataPartition(oversampled_data$stroke, p = 0.7, 
                                    list = FALSE, 
                                    times = 1)

# Performs stratified random split of the data set

TrainingSet <- oversampled_data[sample_index,] # Training Set
TestingSet <- oversampled_data[-sample_index,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)

TrainSet <- TrainSet[,-1]


TrainSet[,"stroke"] <- as.factor(TrainSet[,"stroke"])
# Building Random forest model

model <- randomForest(stroke ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

pred_train <- predict(model)

result_train <- confusionMatrix(pred_train, TrainingSet$stroke)
(result_train$overall)['Accuracy']

# Save model to RDS file
saveRDS(model, "model.rds")
