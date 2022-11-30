install.packages("caret")
install.packages("imbalance")

library(caret)
library(imbalance)
library(randomForest)


data = subset(data, select = -c(Age_Category, Glucose_Category, BMI_Category))


dummy_data <- dummyVars(" ~ . ", data = data)
data <- data.frame(predict(dummy_data, newdata = data))


data %>% count(data$stroke)

#Majority Weighted Minority Oversampling Technique

sample_data <- round(mwmote(data, classAttr = "stroke", numInstances = 500))

set.seed(1203)

final_data <- rbind(data, sample_data)

final_data$stroke <- as.factor(final_data$stroke)

data_part <- createDataPartition(y = final_data$stroke, p = 0.8, list = FALSE)
training_data <- final_data[data_part, ]
test_data <- final_data[-data_part, ]

#we will be using 5-fold cross-validation.
training_control <- trainControl(method = "cv", number = 5)

RF_Model <- train(stroke ~ ., data = training_data, method = "rf", trControl = training_control)
RF_Model

model <- randomForest(stroke ~ ., data = training_data, ntree = 500, mtry = 4, importance = TRUE )
pred_train <- predict(model)
result_train <- confusionMatrix(pred_train, training_data$stroke)
(result_train$overall)['Accuracy']

test_data$prediction <- predict(RF_Model, newdata = test_data)

mysample <- data.frame(gender = c(0,1), 
                       age = c(67,69), 
                       hypertension = c(0,0), 
                       heart_disease = c(1,0), 
                       ever_married = c(1,0), 
                       work_type = c(2,2), 
                       Residence_type = c(0,0), 
                       avg_glucose_level = c(228.69,94.39), 
                       bmi = c(36.6,22.8), 
                       smoking_status = c(1,0))
mysample$predic <- predict(RF_Model, newdata = mysample)
mysample$predic


KNN_Model <- train(stroke~., data = training_data, method = "knn", trControl = training_control)
KNN_Model


LogisticRegression_Model <- train(stroke~., data = training_data, method = "glm", 
                            trControl = training_control,
                            family = "binomial")
LogisticRegression_Model
