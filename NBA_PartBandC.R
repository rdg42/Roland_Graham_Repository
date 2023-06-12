# NBA Part B and C
# Team 47

# Housecleaning
rm(list=ls())

# Reads in the data
# Note: I replaced Glover's 2013-14 season data with NA values in the raw file
library(readxl)
nba.data = read_excel("NBA Talent Analysis Part BandC data.xlsx", sheet = 2, col_names = TRUE)
forecast.data = read_excel("NBA Talent Analysis Part BandC data.xlsx", sheet = 3, col_names = TRUE)
glover.data = read_excel("NBA Talent Analysis Part BandC data.xlsx", sheet = 4, col_names = TRUE, skip = 1)
meyers.data = read_excel("NBA Talent Analysis Part BandC data.xlsx", sheet = 6, col_names = TRUE, skip = 1)
bowie.data = read_excel("NBA Talent Analysis Part BandC data.xlsx", sheet = 8, col_names = TRUE, skip = 1)

# Removes the "Player" variable from the NBA data
nba.data = subset(nba.data, select = -Player)
forecast.data = subset(forecast.data, select = -Player)

# For reference:
# Forecast 1: Danny Glover
# Forecast 2: Mike Meyers
# Forecast 3: David Bowie

# Adds underscores to the "Deal Average Salary" variable
colnames(nba.data)[1] = "Deal_Average_Salary"
colnames(forecast.data)[1] = "Deal_Average_Salary"

# Adds dummy variables for "Deal Year" in NBA and Forecast data
nba.data["Year_2017"] = ifelse(nba.data["Deal_Year"] == 2017, 1, 0)
nba.data["Year_2018"] = ifelse(nba.data["Deal_Year"] == 2018, 1, 0)
nba.data["Year_2019"] = ifelse(nba.data["Deal_Year"] == 2019, 1, 0)

forecast.data["Year_2017"] = ifelse(forecast.data["Deal_Year"] == 2017, 1, 0)
forecast.data["Year_2018"] = ifelse(forecast.data["Deal_Year"] == 2018, 1, 0)
forecast.data["Year_2019"] = ifelse(forecast.data["Deal_Year"] == 2019, 1, 0)

# Removes the Deal_Year variable
nba.data = subset(nba.data, select = -Deal_Year)
forecast.data = subset(forecast.data, select = -Deal_Year)

# Removes "Deal Average Salary" and "VORP" from Forecast data
forecast.data = subset(forecast.data, select = -Deal_Average_Salary)
forecast.data = subset(forecast.data, select = -VORP)

# Removes the "Lg" variable from the Glover, Meyers, and Bowie data sets
glover.data = subset(glover.data, select = -Lg)
meyers.data = subset(meyers.data, select = -Lg)
bowie.data = subset(bowie.data, select = -Lg)

# Correlation matrix for NBA data
cor(nba.data)

# We can see that PTS, FG, and WS are the three variables most correlated
# with Deal Average Salary

# With PTS (r=0.7877658410), with FG (r=0.7757720811),
# and with WS (r=0.7737194360)

# The three variables most correlated with each other are PTS, FG, and FGA 

# Between PTS and FG (r=0.99090766), PTS and FGA (r=0.98574246), and 
# FG and FGA (r=0.98182235)

# Divides into training and testing datasets
set.seed(47)
sample <- sample(c(TRUE, FALSE), nrow(nba.data), replace=TRUE, prob=c(0.8,0.2))
train  <- nba.data[sample, ]
test   <- nba.data[!sample, ]

################################################################################

# Performs PCA to reduce dimensionality
pca.data = subset(nba.data, select = -c(Deal_Average_Salary, RFA, Year_2017, Year_2018, Year_2019))
pca.result = prcomp(pca.data, scale = TRUE)
head(pca.result$x)
var_explained = pca.result$sdev^2 / sum(pca.result$sdev^2)

# Makes a Scree Plot
library(ggplot2)
qplot(c(1:29), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# We should use three PCs

# Prints the three loadings
loadings = pca.result$rotation
rownames(loadings) = colnames(pca.data)
loadings[,1:3]

# All loadings above this value are considered important
sqrt(1/ncol(pca.data))

# Runs a multiple linear regression using variables significant to PC1
model.pca = lm(Deal_Average_Salary ~G+GS+MP+FG+FGA+FT+FTA+DRB+TRB+STL+TOV+PF+PTS+OWS+DWS+WS, train)
summary(model.pca)

# Predicts test data based on PCA exclusion model
pred.pca= predict(model.pca, test)

# Mean square error of testing dataset for pca exclusion linear regression
sqrt((sum(test$Deal_Average_Salary - pred.pca)^2) / nrow(test))

################################################################################

# Runs a multiple linear regression on NBA Data
model.linear = lm(Deal_Average_Salary ~., train)
summary(model.linear)

# Predicts test data based on linear regression
pred.linear = predict(model.linear, test)

# Mean square error of testing dataset for linear regression
sqrt((sum(test$Deal_Average_Salary - pred.linear)^2) / nrow(test))

################################################################################

# Backwards Stepwise Selection
backward = step(model.linear, direction = "backward")
backward$coefficients

# Runs the linear regression model with only the selected variables
model.backward = lm(Deal_Average_Salary ~Age+G+FG+`FG%`+`3P`+`3PA`+`eFG%`+FT+AST+TOV+FTr+`USG%`+DWS+RFA+Year_2017+Year_2018+Year_2019, train)
summary(model.backward)

# We will also remove FG%, eFG%, and FTr because they show up as insignificant variables

model.backward.new = lm(Deal_Average_Salary ~Age+G+FG+`3P`+`3PA`+FT+AST+TOV+`USG%`+DWS+RFA+Year_2017+Year_2018+Year_2019, train)
summary(model.backward.new)

# Predicts test data based on linear regression with backwards-selection
pred.backward.new = predict(model.backward.new, test)

# Mean square error of testing dataset for linear regression with backwards-selection
sqrt((sum(test$Deal_Average_Salary - pred.backward.new)^2) / nrow(test))

################################################################################

# Performs k-fold cross validation
library(glmnet)
x = as.matrix(train[,-1])
y = as.matrix(train[,1])
cv_model = cv.glmnet(x, y, alpha = 1)

# Finds the optimal lambda value
lambda = cv_model$lambda.min
lambda

# Runs a LASSO regression on NBA Data
model.lasso = glmnet(x, y, alpha = 1, lambda = lambda)
coef(model.lasso)

# Predicts test data based on LASSO regression
test.matrix = as.matrix(test[,-1])
pred.lasso = predict(model.lasso, s = lambda, newx = test.matrix)

# R-Squared for LASSO
r.square = model.lasso$dev.ratio
r.square

# Mean squared error of testing dataset for LASSO
sqrt((sum(test$Deal_Average_Salary - pred.lasso)^2) / nrow(test))

################################################################################

# Linear regression with backwards-stepwise selection is the best model because
# it results in the lowest mean square error

# Predicts Forecast data based on linear regression using backwards selection
predict(model.backward.new, forecast.data)

################################################################################

# Generates standardized regression coefficients for the backwards selection model
library(QuantPsyc)
lm.beta(model.backward.new)

################################################################################

# Summary statistics for Glover, Meyers, and Bowie:

# FG%
glover.data$`FG%`[glover.data$Season == "Career"]
bowie.data$`FG%`[bowie.data$Season == "Career"]
meyers.data$`FG%`[meyers.data$Season == "Career"]

# 2P%
glover.data$`2P%`[glover.data$Season == "Career"]
bowie.data$`2P%`[bowie.data$Season == "Career"]
meyers.data$`2P%`[meyers.data$Season == "Career"]

# 3P%
glover.data$`3P%`[glover.data$Season == "Career"]
bowie.data$`3P%`[bowie.data$Season == "Career"]
meyers.data$`3P%`[meyers.data$Season == "Career"]

# eFG%
glover.data$`eFG%`[glover.data$Season == "Career"]
bowie.data$`eFG%`[bowie.data$Season == "Career"]
meyers.data$`eFG%`[meyers.data$Season == "Career"]

# FT%
glover.data$`FT%`[glover.data$Season == "Career"]
bowie.data$`FT%`[bowie.data$Season == "Career"]
meyers.data$`FT%`[meyers.data$Season == "Career"]

# ORB
glover.data$ORB[glover.data$Season == "Career"]
bowie.data$ORB[bowie.data$Season == "Career"]
meyers.data$ORB[meyers.data$Season == "Career"]

# AST
glover.data$AST[glover.data$Season == "Career"]
bowie.data$AST[bowie.data$Season == "Career"]
meyers.data$AST[meyers.data$Season == "Career"]

# STL
glover.data$STL[glover.data$Season == "Career"]
bowie.data$STL[bowie.data$Season == "Career"]
meyers.data$STL[meyers.data$Season == "Career"]

# PTS
glover.data$PTS[glover.data$Season == "Career"]
bowie.data$PTS[bowie.data$Season == "Career"]
meyers.data$PTS[meyers.data$Season == "Career"]
