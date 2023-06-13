# Team 47
# TFA Case

# Housecleaning
rm(list=ls())

# Loads data into R
library(readxl)
library(caret)
library(MASS)
data.A = read_excel("Teach for America Dataset Part A.xlsx", sheet = 2, col_names = TRUE)
data.B = read_excel("Teach for America Dataset Part B.xlsx", sheet = 2, col_names = TRUE)

# Shows that the variable "appyear" has only one unique value for all rows
sapply(lapply(data.A, unique), length)

# Removes the "personid", "appyear", and "schoolsel" variables
data.A = data.A[,-c(1,2,5)]
data.B = data.B[,-c(1,2,5)]

################################################################################

# Part A:
# Divides data into train and test
set.seed(47)
index <- createDataPartition(data.A$completedadm, p = .70, list = FALSE)
train <- data.A[index, ]
test <- data.A[-index, ]

# Runs the binary logistic regression model
model.A = glm(completedadm ~ ., family = binomial(), train)
summary(model.A)

# Removes non-significant variables and runs the model again

# Performs a likelihood ratio test to determine if "undergrad_uni" is 
# significant enough to be included in the model
model.1 = glm(completedadm ~ gpa+schoolsel_chr+major1+minor+undergrad_uni
              +essay3length+essayuniquewords+appdeadline+submitteddate
              +attendedevent,family = binomial(), train)
model.2 = glm(completedadm ~ gpa+schoolsel_chr+major1+minor
              +essay3length+essayuniquewords+appdeadline+submitteddate
              +attendedevent,family = binomial(), train)
anova(model.1, model.2, test="LRT")

# The p-value of the LRT test is 0.06569, which is greater than 0.05
# so "undergrad_uni: can be excluded from the model

# Further LRT tests concluded all other categorical variables can be
# included in the model as their p-values were below 0.05

# The final model for part A
model.final = glm(completedadm ~ gpa+schoolsel_chr+major1+minor
                  +essay3length+essayuniquewords+appdeadline+submitteddate
                  +attendedevent,family = binomial(), train)
summary(model.final)

# Tests the model on the test dataset
pred.test <- predict(model.final, test)

# Contingency table for train data
train$pred <- ifelse(model.final$fitted.values >= 0.5, 1, 0)
ctab_train <- table(train$completedadm, train$pred)
ctab_train

# Contingency table for test data
test$pred <- ifelse(pred.test >= 0.5, 1, 0)
ctab_test <- table(test$completedadm, test$pred)
ctab_test

# Accuracy rate for train data
accuracy_train <- sum(diag(ctab_train))/sum(ctab_train)
accuracy_train

# Accuracy rate for test data
accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)
accuracy_test

################################################################################

# Part B:

# Tests the model on the test dataset
pred_completedadm = predict(model.final, data.B)
pred.df = as.data.frame(pred_completedadm)

# Appends the predictions to data.B
final.df = cbind(data.B, pred.df)

# Determines the number and proportion of applicants that have a predicted score less than 0
nrow(final.df[final.df$pred_completedadm < 0,])
nrow(final.df[final.df$pred_completedadm < 0,]) / nrow(final.df)

# Determines the number and proportion of applicants that have a predicted score between 0 and 0.5
nrow(final.df[final.df$pred_completedadm > 0 & final.df$pred_completedadm < 0.5,])
nrow(final.df[final.df$pred_completedadm > 0 & final.df$pred_completedadm < 0.5,]) / nrow(final.df)

# Determines the number and proportion of applicants that have a predicted score between 0.5 and 1
nrow(final.df[final.df$pred_completedadm > 0.5 & final.df$pred_completedadm < 1,])
nrow(final.df[final.df$pred_completedadm > 0.5 & final.df$pred_completedadm < 1,]) / nrow(final.df)

# Determines the number and proportion of applicants that have a predicted score between 1 and 1.5
nrow(final.df[final.df$pred_completedadm > 1 & final.df$pred_completedadm < 1.5,])
nrow(final.df[final.df$pred_completedadm > 1 & final.df$pred_completedadm < 1.5,]) / nrow(final.df)

# Determines the number and proportion of applicants that have a predicted score between 1.5 and 2
nrow(final.df[final.df$pred_completedadm > 1.5 & final.df$pred_completedadm < 2,])
nrow(final.df[final.df$pred_completedadm > 1.5 & final.df$pred_completedadm < 2,]) / nrow(final.df)

# Determines the number and proportion of applicants that have a predicted score greater than 2
nrow(final.df[final.df$pred_completedadm > 2,])
nrow(final.df[final.df$pred_completedadm > 2,]) / nrow(final.df)


