# Install and load the required packages
# install.packages("tidyverse")
# install.packages("funModeling")
# install.packages("ggcorrplot")
# install.packages("caret")
# install.packages("e1071")
# install.packages("randomForest")
# install.packages("ipred")
# install.packages("tree")
# install.packages("ModelMetrics")
# install.packages("GGally")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggcorrplot")

library(tidyverse)
library(funModeling)
library(ggcorrplot)
library(caret)
library(e1071)
library(randomForest)
library(ipred)
library(tree)
library(ModelMetrics)
library(GGally)
library(dplyr)
library(ggplot2)
library(ggcorrplot)


# Set the working directory and then read the dataset
mydata <- read.csv("house-data.csv", header = TRUE)

# Numerical summaries of the data set
summary(mydata)

# Understand number of observations, variables, variable datatypes and sample values 
glimpse(mydata)

# Create a duplicate of 'mydata' in a new variable for further data processing while preserving the original data in the 'mydata' variable
mynewdata<-mydata

#NA in column "Alley" means "No alley access" => Replace "NA" with "None"
#NA in column "BsmtQual" means "No Basement" => Replace "NA" with "None"
#NA in column "BsmtCond" means "No Basement" => Replace "NA" with "None"
#NA in column "GarageType" means "No Garage" => Replace "NA" with "None"
#NA in column "GarageCond" means "No Garage" => Replace "NA" with "None"
#NA in column "PoolQC" means "No Pool" => Replace "NA" with "None"
#NA in column "Fence" means "No Fence" => Replace "NA" with "None"
#NA in column "MiscFeature" means "None" => Replace "NA" with "None"

#NA in column "LotFrontage" means Missing values
#NA in column "MasVnrArea" means Missing values

# Replace "NA" in specific columns to "None" as mentioned above
mynewdata$Alley[is.na(mynewdata$Alley)]="None"
mynewdata$BsmtQual[is.na(mynewdata$BsmtQual)]="None"
mynewdata$BsmtCond[is.na(mynewdata$BsmtCond)]="None"
mynewdata$GarageType[is.na(mynewdata$GarageType)]="None"
mynewdata$GarageCond[is.na(mynewdata$GarageCond)]="None"
mynewdata$PoolQC[is.na(mynewdata$PoolQC)]="None"
mynewdata$Fence[is.na(mynewdata$Fence)]="None"
mynewdata$MiscFeature[is.na(mynewdata$MiscFeature)]="None"

# Mean imputation for Missing values in the columns "LotFrontage" and "MasVnrArea"
mynewdata$LotFrontage[is.na(mynewdata$LotFrontage)]<-mean(mynewdata$LotFrontage,na.rm=TRUE)
mynewdata$MasVnrArea[is.na(mynewdata$MasVnrArea)]<-mean(mynewdata$MasVnrArea,na.rm=TRUE)

# Check if any columns in new dataset "mynewdata" has any NA values
cbind(
  lapply(
    lapply(mynewdata, is.na)
    , sum)
)

# Change character datatype to factor datatype
mynewdata <- mynewdata %>% mutate_if(is.character, as.factor)

# Drop ID column since it is redundant
mynewdata <- mynewdata[,-1]

# Confirm if character datatype is changed to factor 
glimpse(mynewdata)

# Average Sales Price with respect to neighbourhood
plot(mynewdata %>% group_by(Neighborhood) %>% summarise(mean(SalePrice)), ylab="Avg. SalePrice")

# Distribution of data for each variable
plot_num(mynewdata, bins=10)

# Select factor variables in a new dataframe
mynewdata_subset<- mynewdata %>% select(where(is.factor))

# Plot % distribution of categories in each variable
par(mfrow = c(3, 2))
plot(mynewdata_subset %>% group_by(Alley) %>% summarise((count=n()/nrow(mynewdata_subset))*100), ylab="% distribution")
plot(mynewdata_subset %>% group_by(Neighborhood) %>% summarise((count=n()/nrow(mynewdata_subset))*100), ylab="% distribution")
plot(mynewdata_subset %>% group_by(GarageCond) %>% summarise((count=n()/nrow(mynewdata_subset))*100), ylab="% distribution")
plot(mynewdata_subset %>% group_by(ExterQual) %>% summarise((count=n()/nrow(mynewdata_subset))*100), ylab="% distribution")
plot(mynewdata_subset %>% group_by(KitchenQual) %>% summarise((count=n()/nrow(mynewdata_subset))*100), ylab="% distribution")
plot(mynewdata_subset %>% group_by(GarageType) %>% summarise((count=n()/nrow(mynewdata_subset))*100), ylab="% distribution")

# Correlation matrix for numeric data
# Select numeric columns for correlation matrix
nums <- unlist(lapply(mynewdata, is.numeric), use.names = FALSE)
num_data <- mynewdata[ , nums]

correlation <- cor(num_data)
ggcorrplot(correlation,
           type = "lower",
           lab = TRUE,
           lab_size = 2,
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlation of numeric data",
           ggtheme=theme_bw)

# Visualise outliers for Sale Prices
boxplot(mynewdata$SalePrice)
housedt<- mynewdata

# Create a new variable called as condoverall with categories "Poor", "Average" and "Good" based on the existing variable OverallCond and then deleting the variable column OverallCond 
housedt<-housedt %>%
  mutate(condoverall = case_when(
    OverallCond<=3 & OverallCond>=1  ~ "Poor",
    OverallCond<=6 & OverallCond>=4 ~ "Average",
    OverallCond<=10 & OverallCond>=7 ~ "Good"))
housedatam<-subset(housedt, select=-OverallCond)
housedatam <- housedatam %>% mutate_if(is.character,as.factor)
summary(housedatam)

#boxplot to select features that affect condoverall:
boxplot(housedatam$condoverall~housedatam$Alley,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Condition1,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Condition2,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$GarageType,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$GarageCond,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$BsmtCond,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$BsmtQual,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$PoolQC,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Fence,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$LotFrontage,outline = TRUE)
#to eliminate
boxplot(housedatam$condoverall~housedatam$LotArea,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Street,outline = TRUE)
#to eliminate
boxplot(housedatam$condoverall~housedatam$Utilities,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$LotConfig,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Neighborhood,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$BldgType,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$HouseStyle,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$OverallQual,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$YearBuilt,outline = TRUE)
#to eliminate
boxplot(housedatam$condoverall~housedatam$RoofStyle,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$RoofMatl,outline = TRUE)
#to eliminate
boxplot(housedatam$condoverall~housedatam$Exterior1st,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$MasVnrArea,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$ExterQual,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$ExterCond,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Foundation,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$TotalBsmtSF,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Heating,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$X1stFlrSF,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$X2ndFlrSF,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$LowQualFinSF,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$GrLivArea,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$FullBath,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$BedroomAbvGr,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$KitchenAbvGr,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$KitchenQual,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Functional,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$Fireplaces,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$GarageArea,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$PavedDrive,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$PoolArea,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$MiscFeature,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$MiscVal,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$MiscVal,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$MoSold,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$YrSold,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$SaleType,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$SaleCondition,outline = TRUE)
boxplot(housedatam$condoverall~housedatam$SalePrice,outline = TRUE)

# Apply the MLR model
set.seed(50)
training.obs <- caret::createDataPartition(housedatam$condoverall, p = 0.8, list = FALSE)
train.housedata  <- housedatam[training.obs, ]
test.housedata   <- housedatam[-training.obs, ]

LR <- nnet::multinom(condoverall ~ ., data = train.housedata, family = "binomial")
summary(LR)

# Predict the testing data
predicted.classes <- predict(LR, test.housedata)
head(predicted.classes)

# Accuracy
mean(predicted.classes == test.housedata$condoverall)

# Reduce the features by removing Utilities and LotArea ,Exterior1st,RoofStyle
housedatamore<-subset(housedt, select=-c(Utilities, LotArea, Exterior1st, RoofStyle))

# Calculare regression again
set.seed(50)
trainingm.obs <- caret::createDataPartition(housedatamore$condoverall, p = 0.8, list = FALSE)
train.housedatamore  <- housedatamore[trainingm.obs, ]
test.housedatamore  <- housedatamore[-trainingm.obs, ]

LR1 <- nnet::multinom(condoverall ~ ., data = train.housedatamore, family = "binomial")
summary(LR1)

predicted1.classes <- predict(LR1, test.housedatamore)
head(predicted1.classes)

# Accuracy
mean(predicted1.classes == test.housedatamore$condoverall)
table(predicted1.classes,test.housedatamore$condoverall)

# Second method is Naive Bayes:
require(e1071)
NB3a = naiveBayes(condoverall ~ ., data = train.housedata)
NB3a      
NB3a.predicted = predict(NB3a, type = "class", newdata = test.housedata)

# Confusion Matrix
table(NB3a.predicted, test.housedata$condoverall)

# After applying correlation
NB13a = naiveBayes(condoverall ~ ., data = train.housedatamore)
NB13a     
NB13a.predicted = predict(NB13a, type = "class", newdata = test.housedatamore)

NB13a.predicted
# Confusion matrix
table(NB13a.predicted, test.housedatamore$condoverall)

# Use Inter Quartile Range method to remove the outliers in the Sale Prices column
Q1 <- quantile(mynewdata$SalePrice, .25)
Q3 <- quantile(mynewdata$SalePrice, .75)
IQR <- IQR(mynewdata$SalePrice)

mynewdata_revised <- subset(mynewdata, mynewdata$SalePrice > (Q1 - 1.5*IQR) & mynewdata$SalePrice < (Q3 + 1.5*IQR))

# Visualise boxplot again after removing outliers
boxplot(mynewdata_revised$SalePrice)

# Split training set and test set
set.seed(1234)
training.obs <- caret::createDataPartition(mynewdata_revised$SalePrice, p = 0.8, list = FALSE)
train.house   <- mynewdata_revised[training.obs, ]
test.house   <- mynewdata_revised[-training.obs, ]

# The first method is Random Forest Regression 
# STEP 1: Fit  without feature selections
set.seed(4567)
rd_full<-randomForest(formula = SalePrice ~ ., data=train.house , ntree=1000,
                      importance=TRUE, type=regression)
rd_full

# STEP 2: Fit Random Forest model with feature selections
# Method 1: Get variable importance based on MSE
set.seed(4567)
ImpData <- as.data.frame(importance(rd_full))
ImpData$Var.Names <- row.names(ImpData)

# Visualisation of the importance of features
ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# Check variable importance one more time based on the purity of node 
set.seed(4567)
rf <-  randomForest(x= train.house[,1:49],y= train.house[,50])
var_imp <- varImp(rf, scale = FALSE)

# Sort the score in decreasing order
var_imp_df <- data.frame(cbind(variable = rownames(var_imp), score = var_imp[,1]))
var_imp_df$score <- as.double(var_imp_df$score)
var_imp_df[order(var_imp_df$score,decreasing = TRUE),]

# Visualisation of those scores
ggplot(var_imp_df, aes(x=reorder(variable, score), y=score)) +
  geom_point() +
  geom_segment(aes(x=variable,xend=variable,y=0,yend=score)) +
  ylab("The node purity of features") +
  xlab("Variable Name") +
  coord_flip()

# Conclusion: top importance features including
# OverallQual, Neighborhood, GrLivArea, ExterQual, GarageArea,TotalBsmtSF,YearBuilt, KitchenQual
# BsmtQual, X1stFlrSF, LotArea,GarageType,X2ndFlrSF, FullBath,
# Fireplaces, Exterior1st, LotFrontage, OverallCond

# Fit new model with selected features
set.seed(4567)
rd_1<-randomForest(formula = SalePrice ~ OverallQual+Neighborhood+GrLivArea
                   +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
                   +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
                   +Fireplaces+Exterior1st+LotFrontage+OverallCond,
                   data=train.house, ntree=1000,
                   importance=TRUE, type=regression)
rd_1

# STEP 3: Evaluation using bootstrap to find the model with better performance
set.seed(4567)
errorest(SalePrice ~ ., data = train.house , model = randomForest, estimator = "boot")
errorest(SalePrice ~ OverallQual+Neighborhood+GrLivArea
         +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
         +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
         +Fireplaces+Exterior1st+LotFrontage+OverallCond, data = train.house, model = randomForest, estimator = "boot")
# Conclusion: Model with selected features has better performance

# Plot for error rate of rd_1
plot(rd_1)

# RMSE of this optimal random forest
sqrt(rd_1$mse[which.min(rd_1$mse)])
# Conclusion: The average difference between the predict Sales Price and the actual value is 21173.46

# STEP 4: Use rd_1 to predict house prices
predict_1 <- predict(rd_1, newdata = test.house)

# Root mean squared error (RMSE) for SalePrice using Random Forest
rf_RMSE <- rmse(test.house$SalePrice, predict_1)
rf_RMSE

# The second method is Support Vector Machince (SVM)
# STEP 1: Fit model with SVM without feature selections
set.seed(2345)
sv_full<-svm(formula = SalePrice ~ ., data=train.house, type="eps-regression",
             kernel="radial")
sv_full

# STEP 2: Fit model with feature selection
set.seed(2345)
sv1 <- svm(formula = SalePrice ~ OverallQual+Neighborhood+GrLivArea
           +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
           +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
           +Fireplaces+Exterior1st+LotFrontage+OverallCond, 
           data=train.house, type="eps-regression",
           kernel="radial")
sv1

# STEP 3: Evaluation using bootstrap to find the model with better performance
set.seed(2345)
errorest(SalePrice ~ ., data = train.house, model = svm, estimator = "boot")
errorest(SalePrice ~ OverallQual+Neighborhood+GrLivArea
         +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
         +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
         +Fireplaces+Exterior1st+LotFrontage+OverallCond, data = train.house , 
         model = svm, estimator = "boot")
# Conclusion: Model with selected features has better performance

# STEP 4: Make predictions
predict_2 <- predict(sv1, test.house)

# Plot the prediction
x = 1:length(test.house$SalePrice)
plot(x, test.house$SalePrice, pch=18, col="red")
lines(x, predict_2, lwd="1", col="blue")

# Root mean squared error (RMSE) of SVM model
svm_RMSE <- rmse(test.house$SalePrice, predict_2)
svm_RMSE

# Estimate the test error for Random Forest
# Use cross validation
mypredict <- function(object, newdata)
  predict(object, newdata = newdata, type = c("response"))

errorest(SalePrice ~ OverallQual+Neighborhood+GrLivArea
         +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
         +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
         +Fireplaces+Exterior1st+LotFrontage+OverallCond,
         data=test.house, model = randomForest,
         estimator = "cv", predict= mypredict)

# Use the Bootstrap 
errorest(SalePrice ~ OverallQual+Neighborhood+GrLivArea
         +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
         +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
         +Fireplaces+Exterior1st+LotFrontage+OverallCond, 
         data=test.house, model = randomForest,
         estimator = "boot", predict= mypredict)

# Estimate the test error for SVM
# Use Cross validation
mypredict <- function(object, newdata)
  predict(object, newdata = newdata, type = c("response"))

errorest(SalePrice ~ OverallQual+Neighborhood+GrLivArea
         +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
         +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
         +Fireplaces+Exterior1st+LotFrontage+OverallCond,
         data=test.house, model = svm,
         estimator = "cv", predict= mypredict)

# Use Bootstrap 
errorest(SalePrice ~ OverallQual+Neighborhood+GrLivArea
         +ExterQual+TotalBsmtSF+KitchenQual+GarageArea+YearBuilt
         +X1stFlrSF+LotArea+FullBath+X2ndFlrSF+GarageType+BsmtQual
         +Fireplaces+Exterior1st+LotFrontage+OverallCond, 
         data=test.house, model = svm,
         estimator = "boot", predict= mypredict)

housedata<-mynewdata

nums <- unlist(lapply(housedata, is.numeric), use.names = FALSE)
num_data_house <- housedata[ , nums]

# Numerical data without id column
str(num_data_house)

house_num <- num_data_house

# Plot correlation matrix
ggcorr(house_num[,-1], label = T, hjust = 1, layout.exp = 3)

# Delete YearSold, MoSold and MiscVal since the correlation equal to 0 and PCA is sensitive with lack of correlation
num_data_house<-house_num

# Deal with outliers, will give them the max value for each variable
# LotFrontage,SalePrice,PoolArea,GarageArea,GrLivArea,
# LowQualFinSF,X1stFlrSF,TotalBsmtSF,MasVnrArea,LotArea
boxplot(num_data_house$LotFrontage)
boxplot(num_data_house$SalePrice)
boxplot(num_data_house$PoolArea)
boxplot(num_data_house$GarageArea)
boxplot(num_data_house$Fireplaces)
boxplot(num_data_house$TotRmsAbvGrd)
boxplot(num_data_house$KitchenAbvGr)
boxplot(num_data_house$BedroomAbvGr)
boxplot(num_data_house$FullBath)
boxplot(num_data_house$GrLivArea)
boxplot(num_data_house$LowQualFinSF)
boxplot(num_data_house$X2ndFlrSF)
boxplot(num_data_house$X1stFlrSF)
boxplot(num_data_house$TotalBsmtSF)
boxplot(num_data_house$MasVnrArea)
boxplot(num_data_house$YearBuilt)
boxplot(num_data_house$OverallQual)
boxplot(num_data_house$LotArea)

# Detect outliers and replace them with upper or lower value in the whiskers
num_data_house_o<-num_data_house

# LotFrontage
upper_lot<-boxplot(num_data_house_o$LotFrontage)$stats[5]

# Replace the outliers with upper
num_data_house_o$LotFrontage[num_data_house_o$LotFrontage > upper_lot]  = upper_lot
num_data_house_o$LotFrontage
summary(num_data_house_o$LotFrontage)

# SalePrice
upper_SP<-boxplot(num_data_house_o$SalePrice)$stats[5]

# Replace the outliers with upper
num_data_house_o$SalePrice[num_data_house_o$SalePrice > upper_SP]  = upper_SP
summary(num_data_house_o$SalePrice)

# GarageArea
upper_gr<-boxplot(num_data_house_o$GarageArea)$stats[5]

# Replace the outliers with upper
num_data_house_o$GarageArea[num_data_house_o$GarageArea > upper_gr]  = upper_gr
summary(num_data_house_o$GarageArea)

# GrLivArea
upper_grl<-boxplot(num_data_house_o$GrLivArea)$stats[5]

# Replace the outliers with upper
num_data_house_o$GrLivArea[num_data_house_o$GrLivArea > upper_grl]  = upper_grl
summary(num_data_house_o$GrLivArea)

#X1stFlrSF
upper_X1<-boxplot(num_data_house_o$X1stFlrSF)$stats[5]

# Replace the outliers with upper
num_data_house_o$X1stFlrSF[num_data_house_o$X1stFlrSF > upper_X1]  = upper_X1
summary(num_data_house_o$X1stFlrSF)

# TotalBsmtSF
upper_bst<-boxplot(num_data_house_o$TotalBsmtSF)$stats[5]

# Replace the outliers with upper
num_data_house_o$TotalBsmtSF[num_data_house_o$TotalBsmtSF > upper_bst]  = upper_bst
summary(num_data_house_o$TotalBsmtSF)

# MasVnrArea
upper_vnr<-boxplot(num_data_house_o$MasVnrArea)$stats[5]

# Replace the outliers with upper
num_data_house_o$MasVnrArea[num_data_house_o$MasVnrArea > upper_vnr]  = upper_vnr
summary(num_data_house_o$MasVnrArea)

# LotArea
upper_vnr<-boxplot(num_data_house_o$LotArea)$stats[5]

num_data_house_o<-num_data_house_o[,-c(20,21)]

#select numerical variables and apply PCA 
apply(num_data_house_o[,-21], 2, mean)
apply(num_data_house_o[,-21], 2, var)

# First we scale the variables since we have different values of variances especially SalePrice and YearSold
Scale_house_num<-scale(num_data_house_o[,-21])

# Then we calculate the mean and the variance
cov(Scale_house_num)
cor(Scale_house_num)

# Perform PCA
pca <- prcomp(Scale_house_num,center=TRUE, scale. = TRUE)

# names(pca)
pca$rotation

# the role that each PC plays:
# Plot the first PCs
biplot(pca, scale = 0, xlabs=rep("·", nrow(num_data_house_o[,-21])),xlim=c(-5, 8), ylim=c(-8, 8),cex = 0.5)

abline(h = 0)
abline(v = 0)
PVE = pca$sdev^2 / sum(pca$sdev^2)
plot(PVE, type = 'o', col = "blue", xlab = "Principal Component", ylab = "Prop. Variance Explained")
plot(cumsum(PVE), type = 'o', col = "blue", xlab = "Principal Component", ylab = "Cumulative Prop. Variance Explained")
summary(pca)

         









graphical summaries of the data set
