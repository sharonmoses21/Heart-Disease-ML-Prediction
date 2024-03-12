# Data taken from : https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset

#setting workspace
setwd("C:/Users/Sharon/Downloads/Heart Disease Dataset")

#---------------------------------------------------
# Install Library
#Installing and Loading library 
install.packages("ggthemes")
install.packages("ggsignif")
install.packages('skimr')
install.packages('caTools')
install.packages("rpart.plot")
install.packages('patchwork')
install.packages("gridExtra")
install.packages("rattle")
install.packages("ROSE")
install.packages("randomForest")
install.packages("glmnet")
install.packages("tidymodels")
installed.packages("yardstick")

library(glmnet)
library(yardstick)
library(tidymodels)
library(ROSE)
library(rattle)
library(RColorBrewer)
library(gridExtra)
library(patchwork)
library(tibble)
library(rpart)
library(rpart.plot)
library(readr)
library(caTools)
library(caret)
library(ROCR)
library(dplyr)
library(skimr)
library(ggplot2)
library(randomForest)
library(ROCR)
library(ggthemes)
library(ggsignif)
 
#-------------------------------------------------------------------------------------
#Reading and writing to Data Frame 

fileURL <- "https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset"
download.file(fileURL, destfile="heart_disease_health_indicators_BRFSS2015", method="curl")

df <- read.table("heart_disease_health_indicators_BRFSS2015.csv", na.strings = "?", sep=",", header = TRUE)
View (df)

#-------------------------------------------------------------------------------------
# EDA -Data Exploration
dim(df) 
str(df)
glimpse(df)
head(df, 5)
summary(df)
create_report(df)
#-------------------------------------------------------------------------------------
#EDA: Multi-Variate Analysis
  
#heat map
corr_matrix <- cor(df)
corr_df <- melt(corr_matrix)
ggplot(corr_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "orange", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Correlation Heatmap")

# Strong Correlations: there is a positive correlation between the Physical Health and General Health variables, suggesting that General Health may be a composite measure comprising Physical Health Sum and other related factors.
# Weak Correlations: Variables Income and General Health correlated negatively
# Furthermore, the analysis indicated a negative correlation between Income and General Health, implying that individuals with lower incomes may experience limited access to private medical care, potentially leading to negative impacts on their overall health outcomes.

# Calculate the correlation between all variables and the target variable
correlations <- cor(df)[, "HeartDiseaseorAttack"]
correlations <- correlations[order(abs(correlations), decreasing = TRUE)]

# Create a horizontal bar plot
library(ggplot2)
df_cor <- data.frame(variable = names(correlations), y = correlations)
ggplot(df_cor, aes(x = correlations, y = reorder(variable, correlations))) + 
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip()+
  labs(title = "Correlation with Diabetes_binary", x = "Correlation", y = "Variable") +
  theme(axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10, color = "black"),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#Target variables that have a very weak correlation with AnyHealthcare,NoDocbcCost, Fruits,Sex, Smoker, Veggies


#-------------------------------------------------------------------------------------
#EDA: Uni variate Analysis
#histogram Plot
plot_histogram(df)

# Density Plot
plot_density(df)

#Box plots for numerical variables
Age_Box_Plot <- ggplot(df, aes(Age))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Age Box Plot")+
  theme_tufte()

BMI_Box_Plot <- ggplot(df, aes(BMI))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "BMI Box Plot")+
  theme_tufte()

Edu_Box_Plot <- ggplot(df, aes(Education))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Education Box Plot")+
  theme_tufte()

GenHealth_Box_Plot <- ggplot(df, aes(GenHlth))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "General Box Plot")+
  theme_tufte()

Income_Box_Plot <- ggplot(df, aes(Income))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Income Box Plot")+
  theme_tufte()

MenHlth_Box_Plot <- ggplot(df, aes(MentHlth))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Mental Health Box Plot")+
  theme_tufte()

PhysHlth_Box_Plot <- ggplot(df, aes(PhysHlth))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Physical Health Box Plot")+
  theme_tufte()

Age_Box_Plot  + BMI_Box_Plot + Edu_Box_Plot+ GenHealth_Box_Plot+ 
  Income_Box_Plot +MenHlth_Box_Plot + PhysHlth_Box_Plot


#count plot for target variable
df$HeartDiseaseorAttack <- as.factor(df$HeartDiseaseorAttack)
print(ggplot(data = df, aes(x = HeartDiseaseorAttack, fill = HeartDiseaseorAttack)) +
        geom_bar() +
        scale_fill_manual(values = c("#00BFFF", "#FFA500")) +
        geom_text(stat = "count", aes(label = after_stat(count)), 
                  position = position_stack(vjust = 0.5)) +
        labs(title = "Count Plot of Target Variable", x = "Heart Attack", y = "Count"))

#count values and calculate percentage

counts <- table(df$HeartDiseaseorAttack)
percentages <- prop.table(counts) * 100
print(barplot(percentages, horiz=TRUE, xlim=c(0,100), main="Target Distribution (%)", 
              xlab="% of Patients", ylab="",
              col=c("cornflowerblue", "salmon"), names.arg=c("NoAttack", "HeartAttack"),
              las=1, cex.names=0.8, border=NA))
text(percentages, 1:2, labels=paste(round(percentages, 1), "%"), pos=4);

#-------------------------------------------------------------------------------------
#Data Pre-Processing

# Check Missing Data
plot_missing(df)

# Check Duplicated value
duplicates <- df[duplicated(df), ]
cat("Duplicate Rows: ", nrow(duplicates), "\n")

# Data Cleaning 
data_clean <- function(dataframe) {
  bins <- c(0, 18.5, 24.9, 29.9, Inf)
  labels <- c(1, 2, 3, 4)
  
  dataframe <- dataframe %>%
    distinct() %>%
    mutate(BMI_bins = cut(BMI, breaks = bins, labels = labels)) %>%
    rownames_to_column() %>%
    select(-rowname) %>%
    mutate(
      HighBP = as.integer(HighBP),
      HighChol = as.integer(HighChol),
      CholCheck = as.integer(CholCheck),
      BMI = as.integer(BMI),
      Smoker = as.integer(Smoker),
      Stroke = as.integer(Stroke),
      Diabetes = as.integer(Diabetes),
      PhysActivity = as.integer(PhysActivity),
      Fruits = as.integer(Fruits),
      Veggies = as.integer(Veggies),
      HvyAlcoholConsump = as.integer(HvyAlcoholConsump),
      AnyHealthcare = as.integer(AnyHealthcare),
      NoDocbcCost = as.integer(NoDocbcCost),
      GenHlth = as.integer(GenHlth),
      MentHlth = as.integer(MentHlth),
      PhysHlth = as.integer(PhysHlth),
      DiffWalk = as.integer(DiffWalk),
      Sex = as.integer(Sex),
      Age = as.integer(Age),
      Education = as.integer(Education),
      Income = as.integer(Income),
      BMI_bins = as.integer(BMI_bins)
    )
  return(dataframe)
}

df= data_clean(df)
str(df)

#Rename the columns
colnames(df) <-c("HeartDisease","High_BP","High_Chol", "Chol_Check", "BMI","Smoker","Stroke","Diabetes",
                 "Physical_Activity", "Consume_Fruits", "Consume_Veges","Heavy_Drinker","Health_Care_Access",
                 "Healthcare_CostIssue", "General_Health","Mental_Health","Physical_Health","Difficult_to_Walk",
                 "Sex","Age","Education","Income","BMI_bins")
str(df)

#Get the first column and remove it from the data frame and Add the first column as the last column
HeartDisease <- df[, 1]
df <- df[, -1]
df <- cbind(df, HeartDisease)

# Transform Heart Disease to a factor 
df$HeartDisease <- as.factor(df$HeartDisease)
str(df)

#-------------------------------------------------------------------------------------
# Feature Selection 

# features by importance using the caret
set.seed(7)
rf<-randomForest(HeartDisease~.,ntree=500,data=df)
rf
plot(rf$err.rate[,1],type="l",main="Random Forest Error Rate",xlab="Number of Trees")
varImpPlot(rf,main="Variable Importance Plot for Random Forest")

# drop unwanted columns
df <- select(df, -c(Chol_Check, Smoker, Consume_Veges, Heavy_Drinker, Health_Care_Access, Healthcare_CostIssue))
View(df)

#-------------------------------------------------------------------------------------
# Baseline model for imbalance data
set.seed(123) 
split <- sample.split(df$HeartDisease, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)
head(train,5)
head(test,5)

# Checking Class distribution
table(df$HeartDisease)
prop.table(table(df$HeartDisease))
prop.table(table(train$HeartDisease))
 
# LR
lr_model <- glm(HeartDisease ~ ., data = train, family = "binomial")
summary(lr_model)
#train
lr_train_pred <- predict(lr_model, newdata = train[,-17], type = "response")
lr_train_pred_class <- ifelse(lr_train_pred > 0.5, 1, 0)
# test
lr_test_pred <- predict(lr_model, newdata = test[,-17], type = "response")
lr_test_pred_class <- ifelse(lr_test_pred > 0.5, 1, 0)

# RF
rf_model <- randomForest(HeartDisease ~ ., data = train)
print(rf_model)
#train
rf_train_pred <- predict(rf_model, newdata = train[, -17])
#test
rf_test_pred <-predict(rf_model, newdata = test[, -17]) 
attributes(rf_model)

# DT with entropy
dt_model = rpart(HeartDisease~ ., train, method="class" ,parms = list(split = 'information'), 
                 control =rpart.control(minsplit = 1,minbucket=2, cp=0.00002))
dt_model_ptree<- prune(dt_model,cp= dt_model$cptable[which.min(dt_model$cptable[,"xerror"]),"CP"])
#summary(dt_model_ptree)
#fancyRpartPlot(dt_model_ptree, uniform=TRUE, main="Pruned Classification Tree")
# train
dt_train_pred <- predict(dt_model_ptree, train, type = "class")
#test 
dt_test_pred <- predict(dt_model_ptree,test, type = "class")


#-------------------------------------------------------------------------------------
# Baseline Model evaluation 

calc_metrics <- function(actual, predicted){
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  F1_score <- 2 * precision * sensitivity / (precision + sensitivity)
  
  metrics <- c(accuracy, sensitivity, precision, F1_score)
  return(metrics)
}

#train data
lr_train_metrics <- calc_metrics(train$HeartDisease, lr_train_pred_class)
rf_train_metrics <- calc_metrics(train$HeartDisease, rf_train_pred)
dt_train_metrics <- calc_metrics(train$HeartDisease, dt_train_pred)

# Create a data frame to store evaluation metrics on train data
train_metrics_df <- data.frame(Model = c("Logistic Regression", "Random Forest", "Decision Tree"),
                               Accuracy = c(lr_train_metrics[1], rf_train_metrics[1], dt_train_metrics[1]),
                               Sensitivity = c(lr_train_metrics[2], rf_train_metrics[2], dt_train_metrics[2]),
                               Precision = c(lr_train_metrics[3], rf_train_metrics[3], dt_train_metrics[3]),
                               F1_Score = c(lr_train_metrics[4], rf_train_metrics[4], dt_train_metrics[4]))
# Display the data frame without styles
knitr::kable(train_metrics_df, format = "markdown", row.names = FALSE, col.names = c("Model", "Accuracy", 
                                                                                     "Sensitivity", "Precision", 
                                                                                   "F1-Score"))

# Test Data
lr_metrics <- calc_metrics(test$HeartDisease, lr_test_pred_class)
rf_metrics <- calc_metrics (test$HeartDisease, rf_test_pred)
dt_metrics <- calc_metrics (test$HeartDisease, dt_test_pred)

# Create a data frame to store evaluation metrics
test_metrics_df <- data.frame(Model = c("Logistic Regression", "Random Forest", "Decision Tree"),
                              Accuracy = c(lr_metrics[1], rf_metrics[1], dt_metrics[1]),
                              Sensitivity = c(lr_metrics[2], rf_metrics[2], dt_metrics[2]),
                              Precision = c(lr_metrics[3], rf_metrics[3], dt_metrics[3]),
                              F1_Score = c(lr_metrics[4], rf_metrics[4], dt_metrics[4]))

# Display the data frame without styles
knitr::kable(test_metrics_df, format = "markdown", row.names = FALSE, col.names = c("Model", "Accuracy","Sensitivity", "Precision", "F1-Score"))
#-------------------------------------------------------------------------------------
# Feature Engineering 

# Data Normalization using ROSE on Training data
df_balanced_new <- ovun.sample(HeartDisease ~ ., data = df, method = "both", N = nrow(df), seed = 123)$data

# count plot of Heart Disease
print(ggplot(data = df_balanced_new, aes(x = HeartDisease, fill = HeartDisease)) +
        geom_bar() +
        scale_fill_manual(values = c("red", "blue")) +
        geom_text(stat = "count", aes(label = after_stat(count)), 
                  position = position_stack(vjust = 0.5)) +
        labs(title = "Target Variable After Data Balancing", x = "HeartDisease", y = "Count"))

df_balanced_norm <-df_balanced_new
df_balanced_norm[, c("BMI", "General_Health", "Mental_Health", "Physical_Health","Age","Education", "Income", "BMI_bins")] <- 
  apply(df_balanced_new[, c("BMI", "General_Health", "Mental_Health", 
                            "Physical_Health","Age","Education", "Income", "BMI_bins")], 2, function(x) (x - min(x)) / (max(x) - min(x)))
View(df_balanced_norm)

#-------------------------------------------------------------------------------------
# machine learning models - balance data

set.seed(1234) 
split <- sample.split(df_balanced_norm$HeartDisease, SplitRatio = 0.7)
train_balanced <- subset(df_balanced_norm, split == TRUE)
test_balanced <- subset(df_balanced_norm, split == FALSE)

# Checking Class distribution
table(df_balanced_norm$HeartDisease)
prop.table(table(df_balanced_norm$HeartDisease))
prop.table(table(train_balanced$HeartDisease))

#LR
lr_model_balanced  <- glm(HeartDisease ~ ., data = train_balanced, family = "binomial")
summary(lr_model_balanced)
#train
lr_train_pred_balanced <- predict(lr_model_balanced, newdata = train_balanced[, -17], type = "response")
lr_train_pred_class_balanced <- ifelse(lr_train_pred_balanced > 0.5, 1, 0)
#test
lr_test_pred_balanced <- predict(lr_model_balanced, newdata = test_balanced[,-17], type = "response")
lr_test_pred_class_balanced <- ifelse(lr_test_pred_balanced > 0.5, 1, 0)


#RF
rf_model_balanced  <- rf_model <- randomForest(HeartDisease ~ ., data = train_balanced, ntree = 500)
print(rf_model_balanced)
# train
rf_train_pred_balanced <- predict(rf_model_balanced, newdata = train_balanced[, -17], type = "class")
#test
rf_test_predicted <- predict(rf_model_balanced, newdata = test_balanced[,-17], type = "class")

#DT
dt_model_balanced = rpart(HeartDisease~ ., train_balanced, method="class" ,parms = list(split = 'information'), 
                          control =rpart.control(minsplit = 1,minbucket=2, cp=0.00002))
dt_model_ptree_balanced<- prune(dt_model_balanced,cp= dt_model_balanced$cptable[which.min(dt_model_balanced$cptable[,"xerror"]),"CP"])
#summary(dt_model_ptree_balanced)
#printcp(dt_model_balanced)
#fancyRpartPlot(dt_model_ptree_balanced, uniform=TRUE, main="Pruned Classification Tree")
# train
dt_train_pred_balanced  <- predict(dt_model_ptree_balanced, newdata = train_balanced, type = "class")
# test
dt_test_pred_balanced<- predict(dt_model_ptree_balanced, newdata = test_balanced[, -17], type = "class")


#-------------------------------------------------------------------------------------
# Model Evaluation

#train data
lr_train_balanced <- calc_metrics(train$HeartDisease, lr_train_pred_class_balanced)
rf_train_balanced <- calc_metrics(train$HeartDisease, rf_train_pred_balanced)
dt_train_balanced <- calc_metrics(train$HeartDisease, dt_train_pred_balanced)

# Create a data frame to store evaluation metrics on train data
train_metrics_balanced <- data.frame(Model = c("Logistic Regression", "Random Forest", "Decision Tree"),
                               Accuracy = c(lr_train_balanced[1], rf_train_balanced[1], dt_train_balanced[1]),
                               Sensitivity = c(lr_train_balanced[2], rf_train_balanced[2], dt_train_balanced[2]),
                               Precision = c(lr_train_balanced[3], rf_train_balanced[3], dt_train_balanced[3]),
                               F1_Score = c(lr_train_balanced[4], rf_train_balanced[4], dt_train_balanced[4]))
# Display the data frame without styles
knitr::kable(train_metrics_balanced, format = "markdown", row.names = FALSE, col.names = c("Model", "Accuracy", "Sensitivity", "Precision", "F1-Score"))

# Test data
lr_metrics_balanced <- calc_metrics(test_balanced$HeartDisease, lr_test_pred_class_balanced)
rf_metrics_balanced <- calc_metrics(test_balanced$HeartDisease, rf_test_predicted)
dt_metrics_balanced <- calc_metrics(test_balanced$HeartDisease, dt_test_pred_balanced)

# Create a data frame to store evaluation metrics
metrics_df_balanced <- data.frame(Model = c("Logistic Regression", "Random Forest", "Decision Tree"),
                                  Accuracy = c(lr_metrics_balanced[1], rf_metrics_balanced[1], dt_metrics_balanced[1]),
                                  Sensitivity = c(lr_metrics_balanced[2], rf_metrics_balanced[2], dt_metrics_balanced[2]),
                                  Precision = c(lr_metrics_balanced[3], rf_metrics_balanced[3], dt_metrics_balanced[3]),
                                  F1_Score = c(lr_metrics_balanced[4],rf_metrics_balanced[4], dt_metrics_balanced[4]))

# Display the data frame without styles
knitr::kable(metrics_df_balanced, format = "markdown", row.names = FALSE, col.names = c("Model", "Accuracy", "Sensitivity", "Precision", "F1-Score"))


#-------------------------------------------------------------------------------------
#ROC and AUC Plots
# Calculate predicted probabilities on test data
lr_prob_balanced <- predict(lr_model_balanced, newdata = test_balanced[, -17], type = "response")
rf_prob_balanced <- predict(rf_model_balanced, newdata = test_balanced[, -17], type = "prob")[,2]
dt_prob_balanced <- predict(dt_model_ptree_balanced, newdata = test_balanced[, -17], type = "prob")[,2]

# Create prediction objects
lr_pred_obj_balanced <- prediction(lr_prob_balanced, test_balanced$HeartDisease)
rf_pred_obj_balanced <- prediction(rf_prob_balanced, test_balanced$HeartDisease)
dt_pred_obj_balanced <- prediction(dt_prob_balanced, test_balanced$HeartDisease)

# Calculate AUC 
lr_auc_balanced <- performance(lr_pred_obj_balanced, measure = "auc")@y.values[[1]]
rf_auc_balanced <- performance(rf_pred_obj_balanced, measure = "auc")@y.values[[1]]
dt_auc_balanced <- performance(dt_pred_obj_balanced, measure = "auc")@y.values[[1]]

# function to plot the ROC curve and AUC
plot_ROC_and_AUC <- function(pred_obj, auc, col, title){
  perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  plot(perf, col = col, lwd = 2, main = title)
  abline(a = 0, b = 1, lty = 2)
  text(x = 0.5, y = 0.5, labels = paste("AUC =", round(auc, 3)), cex = 1.5)
}

# Plot ROC curve and AUC for each model separately
plot_ROC_and_AUC(lr_pred_obj_balanced, lr_auc_balanced, "blue", "ROC Curve - Logistic Regression")
plot_ROC_and_AUC(rf_pred_obj_balanced, rf_auc_balanced, "green", "ROC Curve - Random Forest")
plot_ROC_and_AUC(dt_pred_obj_balanced, dt_auc_balanced, "purple", "ROC Curve - Decision Tree")


#-------------------------------------------------------------------------------------
#HyperTune parameters 

set.seed(123)

# Define the training control
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Logistic Regression with hyperparameter tuning
lr_tune <- train(HeartDisease ~ ., data = train_balanced, method = "glmnet", trControl = ctrl, family = "binomial")
lr_model_tuned <- lr_tune$finalModel

# Random Forest with hyperparameter tuning
rf_tune <- train(HeartDisease ~ ., data = train_balanced, method = "rf", trControl = ctrl, ntree = 500)
rf_model_tuned <- rf_tune$finalModel

# Decision Tree with hyperparameter tuning
dt_tune <- train(HeartDisease ~ ., data = train_balanced, method = "rpart", trControl = ctrl, 
                 parms = list(split = 'information'), control = rpart.control(minsplit = 1, minbucket = 2, cp = 0.00002))
dt_model_tuned <- dt_tune$finalModel

# Print the best hyperparameters for each model
print("Logistic Regression - Best Hyperparameters:")
print(lr_tune$bestTune)

print("Random Forest - Best Hyperparameters:")
print(rf_tune$bestTune)

print("Decision Tree - Best Hyperparameters:")
print(dt_tune$bestTune)

# Evaluate the tuned models on the test set
lr_test_pred_tuned <- predict(lr_model_tuned, s = 0.003839986, newx = as.matrix(test_balanced[, -17]), type = "response")
lr_test_pred_class_tuned <- ifelse(lr_test_pred_tuned > 0.5, 1, 0)

rf_test_pred_tuned <- predict(rf_model_tuned, newdata = test_balanced[, -17], type = "class")

dt_test_pred_tuned <- predict(dt_model_tuned, newdata = test_balanced[, -17], type = "class")


# Test data for tuned models
lr_metrics_tuned <- calc_metrics(test_balanced$HeartDisease, lr_test_pred_class_tuned)
rf_metrics_tuned <- calc_metrics(test_balanced$HeartDisease, rf_test_pred_tuned)
dt_metrics_tuned <- calc_metrics(test_balanced$HeartDisease, dt_test_pred_tuned)

# Create a data frame to store evaluation metrics for tuned models
metrics_df_tuned <- data.frame(Model = c("Logistic Regression", "Random Forest", "Decision Tree"),
                               Accuracy = c(lr_metrics_tuned[1], rf_metrics_tuned[1], dt_metrics_tuned[1]),
                               Sensitivity = c(lr_metrics_tuned[2], rf_metrics_tuned[2], dt_metrics_tuned[2]),
                               Precision = c(lr_metrics_tuned[3], rf_metrics_tuned[3], dt_metrics_tuned[3]),
                               F1_Score = c(lr_metrics_tuned[4], rf_metrics_tuned[4], dt_metrics_tuned[4]))

# Display the data frame without styles
knitr::kable(metrics_df_tuned, format = "markdown", row.names = FALSE, col.names = c("Model", "Accuracy", 
                                                                                     "Sensitivity", "Precision", "F1-Score"))
#-----------------------------------------------------------------
# Install and load the pROC package
install.packages("pROC")
library(pROC)

# Create ROC curves for each tuned model
roc_lr <- roc(test_balanced$HeartDisease, lr_test_pred_tuned)
roc_rf <- roc(test_balanced$HeartDisease, as.numeric(rf_test_pred_tuned))
roc_dt <- roc(test_balanced$HeartDisease, as.numeric(dt_test_pred_tuned))

# Plot ROC curves separately for each tuned model
par(pty = "s")

# Plot for Logistic Regression
plot(roc_lr, col = "blue", main = "ROC Curve - Logistic Regression", col.main = "black", lwd = 2)
text(0.8, 0.2, paste("AUC = ", round(auc(roc_lr), 3)), adj = 0, cex = 1.2)

# Plot for Random Forest
plot(roc_rf, col = "green", main = "ROC Curve - Random Forest", col.main = "black", lwd = 2)
text(0.8, 0.2, paste("AUC = ", round(auc(roc_rf), 3)), adj = 0, cex = 1.2)

# Plot for Decision Tree
plot(roc_dt, col = "purple", main = "ROC Curve - Decision Tree", col.main = "black", lwd = 2)
text(0.8, 0.2, paste("AUC = ", round(auc(roc_dt), 3)), adj = 0, cex = 1.2)