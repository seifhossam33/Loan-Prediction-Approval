
######################################################################
############### BUILDING MODEL ###########################

library(caret)
library(rpart)
library(e1071)
library(randomForest)

# Shuffle the data
shuffled_df <- df[sample(nrow(df)), ]

#write.csv(shuffled_df, "shuffeled_data.csv",row.names=FALSE)

# Split the shuffled data into training and testing subsets
train_index <- createDataPartition(shuffled_df$Loan_Status, p = 0.8, list = FALSE)
train_set <- shuffled_df[train_index, ]
test_set <- shuffled_df[-train_index, ]




# Set the seed for reproducibility
set.seed(123)

# Logistic regression model gets accuracy with 82.79%
logit_model <- glm(Loan_Status ~ ., data = train_set, family = binomial())
logit_model_summary <- summary(logit_model)
logit_pred_probabilities <- predict(logit_model, newdata = test_set, type = "response")
logit_pred_values <- ifelse(logit_pred_probabilities >= 0.5, 1, 0)
logit_acc <- mean(logit_pred_values == test_set$Loan_Status)
print(paste("Logistic Regression Accuracy:", round(logit_acc * 100, 2), "%"))


logit_pred_values <- factor(logit_pred_values)
actuals <- factor(test_set$Loan_Status)
############ Confusion Matrix ################
confusionMatrix(logit_pred_values,actuals)

# SVM model     gets accuracy with 82.79%
svm_model <- svm(formula = Loan_Status ~., data = train_set, type = 'C-classification', kernel = 'linear')
svm_pred <- predict(svm_model, newdata = test_set, type = "response")
svm_acc <- mean(svm_pred == test_set$Loan_Status)
print(paste("SVM Accuracy:", round(svm_acc * 100, 2), "%"))


# Naive Bayes   gets accuracy with 78.69%
naive_model <- naiveBayes(formula = Loan_Status ~., data = train_set)
naive_pred <- predict(naive_model, newdata = test_set)
naive_acc <- mean(naive_pred == test_set$Loan_Status)
print(paste("Naive Accuracy:", round(naive_acc * 100, 2), "%"))

# Random forest model
rf_model <- randomForest(Loan_Status ~., data = train_set, type = "class")
rf_pred <- predict(rf_model, newdata = test_set)
rf_pred_values <- ifelse(rf_pred >= 0.5, 1, 0)
rf_acc <- mean(rf_pred_values == test_set$Loan_Status)
print(paste("Random Forest Accuracy:", round(rf_acc * 100, 2), "%"))

