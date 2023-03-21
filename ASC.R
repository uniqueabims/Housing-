##########################Question 1#################################
#Load library and Table
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("VIM")
install.packages("corrplot")
install.packages("plyr")
install.packages("ggplot2")
install.packages("naniar")
install.packages("ggrepel")
install.packages("ggfortify")
install.packages("reshape2")
install.packages('randomForest')
install.packages('Metrics')
install.packages('varImp')
install.packages("caret")
install.packages("sets")
install.packages("e1071")
install.packages("lattice")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(randomForest)
library(sets)
library(varImp)
library(reshape2)
library(VIM)
library(ggcorrplot)
library(GGally)
library(readr)
library(dplyr)
library(tidyr)
library(corrplot)
library(plyr)
library(naniar)
library(ggrepel)
library(ggfortify)
library(Metrics)

#Import the dataset
house_data_1 <- read.csv("Desktop/Applied Stats Groupwork/house-data (1).csv")
head(house_data_1)
str(house_data_1)
summary(house_data_1)

# Create histogram with normal distribution curve
ggplot(house_data_1, aes(x = SalePrice)) +
  geom_histogram(aes(y=..density..), binwidth = 10000, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean(house_data_1$SalePrice), sd = sd(house_data_1$SalePrice)),
                color = "red", size = 1) +
  labs(x = "Sale Price", y = "Density") +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = waiver())
#Few people can buy very expensive house because the sale price is right skewed

#Few other graphs
#Lot Frontage
ggplot(house_data_1, aes(x = LotFrontage)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(x = "Lot Frontage", y = "Frequency", title = "Frequency of Lot Frontage")

# Street 
# create a data frame with the count of each street
street_count <- data.frame(table(house_data_1$Street))
street_count

# plot the street count
ggplot(data=street_count, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="Street", y="Count") +
  ggtitle("Count of Houses by Street")

#Alley 
# create a data frame with the count of each street
Alley_count <- data.frame(table(house_data_1$Alley))

# plot the street count
ggplot(data=Alley_count, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="Alley", y="Count") +
  ggtitle("Count of Houses by Alley")

#Utilities
Utilities_count <- data.frame(table(house_data_1$Utilities))

# plot the street count
ggplot(data=Utilities_count, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="Utilities", y="Count") +
  ggtitle("Count of Houses by Utilities")

#LotConfig
LotConfig_count <- data.frame(table(house_data_1$LotConfig))
LotConfig_count <- LotConfig_count[order(-LotConfig_count$Freq),]

# plot the LotConfig count
ggplot(data=LotConfig_count, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="LotConfig", y="Count") +
  ggtitle("Count of Houses by LotConfig")

#Condition 1
Condition1_count <- data.frame(table(house_data_1$Condition1))
Condition1_count <- Condition1_count[order(-Condition1_count$Freq),]

# plot the LotConfig count
ggplot(data=Condition1_count, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="Condition 1", y="Count") +
  ggtitle("Count of Houses by Condition1")

#Condition 2
Condition2_count <- data.frame(table(house_data_1$Condition2))
Condition2_count <- Condition2_count[order(-Condition2_count$Freq),]

# plot the LotConfig count
ggplot(data=Condition2_count, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="Condition 2", y="Count") +
  ggtitle("Count of Houses by Condition2")

#BldgType
BldgType_count <- data.frame(table(house_data_1$BldgType))
BldgType_count <- BldgType_count[order(-BldgType_count$Freq),]

# plot the Bldg count
ggplot(data=BldgType_count, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="BldgType", y="Count") +
  ggtitle("Count of Houses by BldgType")

OverallQual_graph<- house_data_1 %>% group_by(OverallQual) %>% ggplot(aes(x = OverallQual, y = SalePrice)) + geom_col()
print(OverallQual_graph)

TotRmsAbvGrd_graph<- house_data_1 %>% group_by(TotRmsAbvGrd) %>% ggplot(aes(x = TotRmsAbvGrd, y = SalePrice)) + geom_col()
print(TotRmsAbvGrd_graph)

#Check the correlation

numeric_variable<- which(sapply(house_data_1, is.numeric)) #index vector numeric variables
numeric_variable_names <- names(numeric_variable) #saving names vector for use later on
cat('There are', length(numeric_variable), 'numeric variables')


# Select only numeric variables
all_numeric_variable <- house_data_1[, sapply(house_data_1, is.numeric)]
names(all_numeric_variable)

# Calculate correlation matrix for all numeric variables
correlation_numeric_variable <- cor(all_numeric_variable, use="pairwise.complete.obs")

# Sort variables on decreasing correlations with SalePrice
correlation_sorted <- sort(correlation_numeric_variable[,'SalePrice'], decreasing = TRUE)

# Select only highly correlated variables
Correlation_High <- names(correlation_sorted[abs(correlation_sorted) > 0.5])

# Calculate correlation matrix for highly correlated variables
correlation_numeric_variable <- correlation_numeric_variable[Correlation_High, Correlation_High]

# Plot the correlation matrix
ggcorrplot(correlation_numeric_variable,
           lab = TRUE,
           lab_size = 3,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Matrix of Numeric Variables")


#We can notice that the variable Overall Quality has 0.79 for its correlation with SalePrice
#We can then check if there are some outliers

ggplot(data=house_data_1[!is.na(house_data_1$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = waiver())

#We can see a upward curve. There aren't some really extreme values. 
#The only one we can take as outlier is the house with grade 4 for its Overall Quality

#The 2nd variable with the highest correlation with SalePrice is Above Grade Living Area
#That can be explained by the fact that big houses are most of time more expensive

ggplot(data=house_data_1[!is.na(house_data_1$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = waiver()) +
  geom_text_repel(data = house_data_1[!is.na(house_data_1$SalePrice) & house_data_1$GrLivArea > 4500, ],
                  aes(label = rownames(house_data_1[!is.na(house_data_1$SalePrice) & house_data_1$GrLivArea > 4500, ])))
#The house 1 and 2 has big living areas and low SalePrices are outliers. but we can see they got above the mean for the Overall Quality(7 and 6 respectivelly)

house_data_1[c(1,2), c('SalePrice', 'GrLivArea', 'OverallQual')]

#Checking for missing values 
Missing_values_columns <- which(colSums(is.na(house_data_1)) > 0)
View(Missing_values_columns)
sort(colSums(sapply(house_data_1[Missing_values_columns], is.na)), decreasing = TRUE)
cat('There are', length(Missing_values_columns), 'columns with missing values')


#Drop variables that have more than 75% of missing values
#so I drop PoolQC, MiscFeature, Alley, Fence
house_data_2 <-house_data_1[,c(-5,-43,-44,-45)]
str(house_data_2)

##Label encoding /Factorizing character variables

#Lot Frontage: 259 NA value
ggplot(house_data_2[!is.na(house_data_2$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (i in 1:nrow(house_data_2)){
  if(is.na(house_data_2$LotFrontage[i])){
    house_data_2$LotFrontage[i] <- as.integer(median(house_data_2$LotFrontage[house_data_2$Neighborhood==house_data_2$Neighborhood[i]], na.rm=TRUE)) 
  }
}

#Garage Type: Garage location

house_data_2$GarageType[is.na(house_data_2$GarageType)] <- 'No Garage'
house_data_2$GarageType <- as.factor(house_data_2$GarageType)
table(house_data_2$GarageType)

#GarageCond: Garage condition
# Define the levels of the ordinal variable
Qualities <- c("NA"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
house_data_2$GarageCond[is.na(house_data_2$GarageCond)] <- '0'
house_data_2$GarageCond<-as.integer(revalue(house_data_2$GarageCond, Qualities))
table(house_data_2$GarageCond)

#BsmtCond: ordinal transformation
Qualities <- c("NA"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
house_data_2$BsmtCond[is.na(house_data_2$BsmtCond)] <- '0'
house_data_2$BsmtCond<-as.integer(revalue(house_data_2$BsmtCond, Qualities))
table(house_data_2$BsmtCond)

#BsmtQual: ordinal transformation
Qualities <- c("NA"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
house_data_2$BsmtQual[is.na(house_data_2$BsmtQual)] <- '0'
house_data_2$BsmtQual<-as.integer(revalue(house_data_2$BsmtQual, Qualities))
table(house_data_2$BsmtQual)

#MasVnrArea
#fix this veneer Area by imputing the mode
house_data_2$MasVnrArea[is.na(house_data_2$MasVnrArea)] <-0

character_variables<- names(house_data_2[,sapply(house_data_2, is.character)])
character_variables
cat('There are', length(character_variables), ' columns with character values')


#Foundation
house_data_2$Foundation <- as.factor(house_data_2$Foundation)
table(house_data_2$Foundation)


#Heating 
house_data_2$Heating <- as.factor(house_data_2$Heating)
table(house_data_2$Heating)

#RoofStyle
house_data_2$RoofStyle <- as.factor(house_data_2$RoofStyle)
table(house_data_2$RoofStyle)

#RoofMatl
house_data_2$RoofMatl <- as.factor(house_data_2$RoofMatl)
table(house_data_2$RoofMatl)

#BldggType
house_data_2$BldgType <- as.factor(house_data_2$BldgType)
table(house_data_2$BldgType)

#HouseStyle
house_data_2$HouseStyle <- as.factor(house_data_2$HouseStyle)
table(house_data_2$HouseStyle)

#Neighborhood
house_data_2$Neighborhood <- as.factor(house_data_2$Neighborhood)
table(house_data_2$Neighborhood)

#Condition 1
house_data_2$Condition1 <- as.factor(house_data_2$Condition1)
table(house_data_2$Condition1)

#Condition 2
house_data_2$Condition2 <- as.factor(house_data_2$Condition2)
table(house_data_2$Condition2)

#Street 
house_data_2$Street <- as.integer(revalue(house_data_2$Street, c('Grvl'=0, 'Pave'=1)))
table(house_data_2$Street)

#PavedDrive
house_data_2$PavedDrive<-as.integer(revalue(house_data_2$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(house_data_2$PavedDrive)

#ExterCond
Qualities <- c("NA"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
house_data_2$ExterCond[is.na(house_data_2$ExterCond)] <- '0'
house_data_2$ExterCond<-as.integer(revalue(house_data_2$ExterCond, Qualities))
table(house_data_2$ExterCond)

#Functional
house_data_2$Functional <- as.factor(house_data_2$Functional)
table(house_data_2$Functional)

#Utilities
house_data_2$Utilities <- as.factor(house_data_2$Utilities)
table(house_data_2$Utilities)

#SaleType
house_data_2$SaleType <- as.factor(house_data_2$SaleType)
table(house_data_2$SaleType)

#Exterior1st
house_data_2$Exterior1st <- as.factor(house_data_2$Exterior1st)
table(house_data_2$Exterior1st)

#ExterQual
Qualities <- c("NA"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
house_data_2$ExterQual[is.na(house_data_2$ExterQual)] <- '0'
house_data_2$ExterQual<-as.integer(revalue(house_data_2$ExterQual, Qualities))
table(house_data_2$ExterQual)

#SaleCond
house_data_2$SaleCondition <- as.factor(house_data_2$SaleCondition)
table(house_data_2$SaleCondition)

#KitchenQual
Qualities <- c("NA"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd"=4, "Ex"=5)
house_data_2$KitchenQual[is.na(house_data_2$KitchenQual)] <- '0'
house_data_2$KitchenQual<-as.integer(revalue(house_data_2$KitchenQual, Qualities))
table(house_data_2$KitchenQual)

#There are some variables that are numeric but should be categorical
str(house_data_2$YrSold)
str(house_data_2$MoSold)
house_data_2$MoSold <- as.factor(house_data_2$MoSold)

#Lotconfig
house_data_2$LotConfig <- as.factor(house_data_2$LotConfig)
table(house_data_2$LotConfig)

#After the conversion of the different variables into factors or label encoded into numbers

numeric_variable <- which(sapply(house_data_2, is.numeric)) #index vector numeric variables
factor_variable <- which(sapply(house_data_2, is.factor)) #index vector factor variables
cat('There are', length(numeric_variable), 'numeric variables, and', length(factor_variable), 'categoric variables')


# Select only numeric variables
all_numeric_variable <- house_data_2[, sapply(house_data_2, is.numeric)]

# Calculate correlation matrix for all numeric variables
correlation_numeric_variable <- cor(all_numeric_variable, use="pairwise.complete.obs")

# Sort variables on decreasing correlations with SalePrice
correlation_sorted <- sort(correlation_numeric_variable[,'SalePrice'], decreasing = TRUE)

# Select only highly correlated variables
Correlation_High <- names(correlation_sorted[abs(correlation_sorted) > 0.5])

# Calculate correlation matrix for highly correlated variables
correlation_numeric_variable <- correlation_numeric_variable[Correlation_High, Correlation_High]

# Plot the correlation matrix
ggcorrplot(correlation_numeric_variable,
           lab = TRUE,
           lab_size = 3,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Matrix of Numeric Variables")


#I am about to drop the highly correlated variables
drop_high_cor_variables <- c('GrLivArea','TotalBsmtSF','OVerallQual')
house_data_2 <-house_data_2[,!(names(house_data_2) %in% drop_high_cor_variables)]
house_data_2 <-house_data_2[,-c(12)]
summary(house_data_2)
names(house_data_2)
str(house_data_2)

##########################Question 2#################################
#Transforming the "OverallCond" column
house_data_2$OverallCond <- ifelse(house_data_2$OverallCond >= 1 & house_data_2$OverallCond <= 3, "Poor",
                                   ifelse(house_data_2$OverallCond >= 4 & house_data_2$OverallCond <= 6, "Average", "Good"))

# Print the first 10 rows to check if the house_condition column is added correctly
head(house_data_2$OverallCond)
str(house_data_2)
#Converting all the character columns to factors
house_data_2 <- house_data_2 %>% 
  mutate_if(is.character, as.factor)
str(house_data_2)

######Question 2a####
# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
trainIndex <- createDataPartition(house_data_2$OverallCond, p = .8, list = FALSE)
trainData <- house_data_2[trainIndex, ]
testData <- house_data_2[-trainIndex, ]

#Setting a reference level to be used as baseline
trainData$OverallCond <- relevel(trainData$OverallCond, ref= "Poor")

#Fit a mutinomial logistic regression model on training data
log_model<-nnet::multinom(OverallCond ~ ., data = trainData, family ="binomial")
summary(log_model)

#Using the model to make predictions on the train data
Predictions_Train <- predict(log_model, trainData)
head(Predictions_Train)
Train_confusion_matrix <- table(Predictions_Train, trainData$OverallCond)
head(Train_confusion_matrix)

# Calculate the accuracy of the model on the train set
Accuracy_Train <- sum(diag(Train_confusion_matrix))/sum(Train_confusion_matrix)
Accuracy_Train

# Calculate the sensitivity of the model on the training set
sensitivity_Train <- diag(Train_confusion_matrix) / rowSums(Train_confusion_matrix)
sensitivity_Train

# Plot the sensitivity
par(mar=c(4,4,4,4))
barplot(sensitivity_Train, 
        ylim = c(0, 1.0), 
        col = c("#0072B2", "#D55E00", "#E69F00"),
        xlab = "Overall Condition Category",
        ylab = "Sensitivity",
        main = "Sensitivity of the Model on the Training Set")

# Add text labels to the bars
text(x = 1:3, y = sensitivity_Train, 
     labels = round(sensitivity_Train, 2), 
     pos = 1)

# Add the accuracy to the plot
text(x = 0.8, y = 1.05, 
     labels = paste0("Accuracy = ", round(Accuracy_Train, 2)), 
     cex = 1.2, 
     font = 2)

#Using the model to make predictions on the test data
Predictions_Test <- predict(log_model, testData)
head(Predictions_Test)
Test_confusion_matrix <- table(Predictions_Test, testData$OverallCond)
head(Test_confusion_matrix)

# Calculate the accuracy of the model on the test set
Accuracy_Test= mean(Predictions_Test == testData$OverallCond)
Accuracy_Test

# Calculate the sensitivity of the model on the test set
sensitivity_Test <- diag(Test_confusion_matrix) / rowSums(Test_confusion_matrix)
sensitivity_Test

# Plot the sensitivity
barplot(sensitivity_Test, 
        ylim = c(0, 1), 
        col = c("#0072B2", "#D55E00", "#E69F00"),
        xlab = "Overall Condition Category",
        ylab = "Sensitivity",
        main = "Sensitivity of the Model on the Test Set")

# Add text labels to the bars
text(x = 1:3, y = sensitivity_Test, 
     labels = round(sensitivity_Test, 2), 
     pos = 1)

# Add the accuracy to the plot
text(x = 0.8, y = 1.0, 
     labels = paste0("Accuracy = ", round(Accuracy_Test, 2)), 
     cex = 1.2, 
     font = 2)

####Question 2b####
##Fitting SVM model on the training data
svm_model <- svm( OverallCond~ ., data = trainData, kernel = "linear", cost = 1,probability = TRUE)
summary(svm_model)

#Using the model to make predictions on the train data
Predictions_Train1 <- predict(svm_model, trainData)
head(Predictions_Train1)
Train1_confusion_matrix <- table(Predictions_Train1, trainData$OverallCond)
head(Train1_confusion_matrix)

# Calculate the accuracy of the model on the train set
Accuracy_Train1 <- sum(diag(Train1_confusion_matrix))/sum(Train1_confusion_matrix)
Accuracy_Train1

# Calculate the sensitivity of the model on the training set
sensitivity_Train1 <- diag(Train1_confusion_matrix) / rowSums(Train1_confusion_matrix)
sensitivity_Train1

# Plot the sensitivity
barplot(sensitivity_Train1, 
        ylim = c(0, 1), 
        col = c("#0072B2", "#D55E00", "#E69F00"),
        xlab = "Overall Condition Category",
        ylab = "Sensitivity",
        main = "Sensitivity of the Model on the Training Set")

# Add text labels to the bars
text(x = 1:3, y = sensitivity_Train1, 
     labels = round(sensitivity_Train1, 2), 
     pos = 1)

# Add the accuracy to the plot
text(x = 0.8, y = 1.0, 
     labels = paste0("Accuracy = ", round(Accuracy_Train1, 2)), 
     cex = 1.2, 
     font = 2)

#Using the model to make predictions on the test data
Predictions_Test1 <- predict(svm_model, testData)
head(Predictions_Test1)
Test1_confusion_matrix <- table(Predictions_Test1, testData$OverallCond)
head(Test1_confusion_matrix)

# Calculate the accuracy of the model on the test set
Accuracy_Test1= mean(Predictions_Test1 == testData$OverallCond)
Accuracy_Test1

# Calculate the sensitivity of the model on the test set
sensitivity_Test1 <- diag(Test1_confusion_matrix) / rowSums(Test1_confusion_matrix)
sensitivity_Test1

# Plot the sensitivity
barplot(sensitivity_Test1, 
        ylim = c(0, 1), 
        col = c("#0072B2", "#D55E00", "#E69F00"),
        xlab = "Overall Condition Category",
        ylab = "Sensitivity",
        main = "Sensitivity of the Model on the Test Set")

# Add text labels to the bars
text(x = 1:3, y = sensitivity_Test1, 
     labels = round(sensitivity_Test1, 2), 
     pos = 1)

# Add the accuracy to the plot
text(x = 0.8, y = 1.0, 
     labels = paste0("Accuracy = ", round(Accuracy_Test1, 3)), 
     cex = 1.2, 
     font = 2)


##########################Question 3#################################
# split the data into training and test sets
set.seed(123)
train_idx <- sample(nrow(house_data_2), round(0.8 * nrow(house_data_2)))
train_data <- house_data_2[train_idx, ]
test_data <- house_data_2[-train_idx, ]

# Linear Regression Model
lr_model <- lm(SalePrice ~ ., data = house_data_2)

# Evaluation
lr_pred <- predict(lr_model, test_data)

lr_var_imp <- varImp(lr_model, scale = FALSE)
lr_var_imp <- data.frame(variables=row.names(lr_var_imp), importance=lr_var_imp$Overall)
lr_var_imp <- lr_var_imp[order(lr_var_imp$importance, decreasing = TRUE),]
lr_var <- head(lr_var_imp, 20)

ggplot(lr_var, aes(x=variables, y=importance)) +
  geom_segment( aes(x=variables, xend=variables, y=0, yend=importance), color="skyblue") +
  geom_point(aes(size = importance), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


#Checking the different between the Actual and the predicted 
lr_modelEval <- cbind(test_data$SalePrice, ceiling(lr_pred))
colnames(lr_modelEval) <- c('Actual', 'Predicted')
lr_modelEval <- as.data.frame(lr_modelEval)
head(lr_modelEval, 10)

#Plotting the Actual vs Predicted
ggplot(head(lr_modelEval, 20), aes(x = 1:20)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Linear Regression Actual vs Predicted", x = "index", y = "Value")

data.frame(
  lr_RSME = rmse(test_data$SalePrice, lr_pred),
  lr_MAE = mae(test_data$SalePrice, lr_pred),
  lr_R2 = R2(test_data$SalePrice, lr_pred))


# Random Forest
rf_model <- randomForest(SalePrice ~ ., data = train_data, importance=TRUE)
#importance(rf_model)
#varImpPlot(rf_model)

# Evaluate model on test data
rf_pred <- predict(rf_model, test_data)

data.frame(rf_RMSE = rmse(test_data$SalePrice, rf_pred),
           rf_MAE = mae(test_data$SalePrice, rf_pred),
           rf_R2 = R2(test_data$SalePrice, rf_pred))

rf_Eval <- cbind(test_data$SalePrice, ceiling(rf_pred))
colnames(rf_Eval) <- c('Actual', 'Predicted')
rf_Eval <- as.data.frame(rf_Eval)
head(rf_Eval, 10)

#Plotting the Actual vs Predicted
ggplot(head(rf_Eval, 20), aes(x = 1:20)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Random Forest Actual vs Predicted", x = "index", y = "Value")



rf_imp <- as.data.frame(importance(rf_model))
rf_imp <- rf_imp[order(rf_imp$`%IncMSE`, decreasing=TRUE),]
rf_imp$Var_Names <- row.names(rf_imp)

ggplot(rf_imp, aes(x=Var_Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var_Names, xend=Var_Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


#Cross Validation of LR
set.seed(123)
lr_cv_tc <- trainControl(method = "cv", number = 10)
# Train the model
lr_cv_model <- train(SalePrice ~., data = train_data, method = "lm",
                     trControl = lr_cv_tc)
# Summarize the results
print(lr_cv_model)


# Cross Validation of RF
rf_cv_tc <- trainControl(method = "cv", number = 10)
rf_cv_model <- train(SalePrice ~., data = train_data, method = "rf",
                     trControl = rf_cv_tc)
print(rf_cv_model)


# Bootstrap on LR
lr_bt_tc <- trainControl(method = "boot", number = 100)
lr_bt_model <- train(SalePrice ~., data = train_data, method = "lm",
                     trControl = lr_bt_tc)
print(lr_bt_model)


# Bootstrap on RF
rf_bt_tc <- trainControl(method = "boot", number = 100)
rf_bt_model <- train(SalePrice ~., data = train_data, method = "rf",
                     trControl = rf_bt_tc)
print(rf_bt_model)


##########################Question 4#################################
#Load dataset
house_data <- house_data_2
house_data <- house_data[,-1]

#Training
set(10111)
train_index <- sample(nrow(house_data), 0.8*nrow(house_data))
train_data <- house_data[train_index,]
test_data <- house_data[-train_index,]

# SVM
svm_model <- svm(Neighborhood ~ ., data = train_data, kernel = "linear", cost = 50)
svm_pred <- predict(svm_model, test_data)
confusionMatrix(svm_pred, test_data$Neighborhood)$overall['Accuracy']









