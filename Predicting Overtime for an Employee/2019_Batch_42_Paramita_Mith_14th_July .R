## set the path
setwd("D:/INSOFE/Lab Machine Learning/Mith")
## remove all variables
rm(list = ls(all.names = T))

## import the libraries
library(vegan)
library(randomForest)
library(caret)
library(ROCR)
library(tidyverse)
library(gridExtra)
library(ggridges)
library(caret)
library(GGally)
library(hexbin)

#read the data
data <- read.csv("dataset.csv",stringsAsFactors = F)

## seggregate the train and test data set
train_data <- subset(data,(data$ExtraTime != 'NA') )
test_data <- data[40304:50341,]

## descriptive statistics
summary(train_data)
str(train_data)
head(train_data)
tail(train_data)

#check missing values and remove columns having > 10% of missing values
colSums(is.na(train_data))
## there are no missing values

##Feature Engineering
#remove columns with no variance
data_prep <- train_data
data_prep$RowID <- NULL
data_prep$istrain <- NULL
data_prep$Over18 <- NULL
data_prep$EmployeeCount <- NULL
data_prep$StandardHours <- NULL
data_prep$EmployeeID <- NULL
data_prep$datacollected <- NULL


#apply the same in test data
test_data$RowID <- NULL
test_data$istrain <- NULL
test_data$Over18 <- NULL
test_data$EmployeeCount <- NULL
test_data$StandardHours <- NULL
test_data$EmployeeID <- NULL
test_data$datacollected <- NULL


#check for class imbalance
table(data_prep$ExtraTime)
## there is no class imbalance

# verify the data set
summary(data_prep)
str(data_prep)
head(data_prep)
tail(data_prep)

## create 2 new features which is total no of working years and number of years working in current company
TotalWorkingYears <- 2018 - year(as.Date(data_prep$FirstJobDate, format = "%m/%d/%Y"))
YearsCurrentCompany <-  2018 - year(as.Date(data_prep$DateOfjoiningintheCurrentCompany, format = "%m/%d/%Y"))
data_prep$TotalWorkingYears <- TotalWorkingYears
data_prep$YearsCurrentCompany <- YearsCurrentCompany


#remove the date columns now
data_prep$FirstJobDate <- NULL
data_prep$DateOfjoiningintheCurrentCompany <- NULL

## do the same thing in test data
test_data$TotalWorkingYears <- 2018 - year(as.Date(test_data$FirstJobDate, format = "%m/%d/%Y"))
test_data$YearsCurrentCompany <- 2018 -  year(as.Date(test_data$DateOfjoiningintheCurrentCompany, format = "%m/%d/%Y"))
test_data$FirstJobDate <- NULL
test_data$DateOfjoiningintheCurrentCompany <- NULL

#verify
head(data_prep)
str(data_prep)
head(data_prep)
tail(data_prep)

## seggregate the numeric and categorical variables
num_attr <- c("DistancetoHome","MonthlyRate","YearsSinceLastPromotion","Hourlyrate","Age","YearsInCurrentRole"
              ,"NumberofCompaniesChanged","Emolumnet_in_Pecentage","DialyRate","MonthlyIncome"
              ,"TotalWorkingYears","YearsCurrentCompany")
cat_attr <- setdiff(colnames(data_prep),num_attr)

## convert the categorical variables to factors
data_prep[,names(data_prep) %in% cat_attr] <- lapply(data_prep[,names(data_prep) %in% cat_attr] , factor)
test_data[,names(test_data) %in% cat_attr] <- lapply(test_data[,names(test_data) %in% cat_attr] , factor)

## verify
str(data_prep)
summary(data_prep)
head(data_prep)
tail(data_prep)

##check correlation among numeric variables
ggcorr(data_prep[,names(data_prep) %in% num_attr],
       method = c("pairwise", "spearman"),
       nbreaks = 8,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

## A very high correlation (+0.9) is seen between YearsInCurrentRole and YearsCurrentCompany
## creating an interaction term for these 2 columns
data_prep$Interact_1 <- data_prep$YearsCurrentCompany*data_prep$YearsInCurrentRole
test_data$Interact_1 <- test_data$YearsCurrentCompany*test_data$YearsInCurrentRole

## drop the 2 columns YearsInCurrentRole and YearsCurrentCompany
data_prep$YearsInCurrentRole <- NULL
data_prep$YearsCurrentCompany <- NULL
test_data$YearsInCurrentRole <- NULL
test_data$YearsCurrentCompany <- NULL

## verify the data now
summary(data_prep)
str(data_prep)

## check the distribution of the numeric variables
##DistancetoHome is heavily right skewed
ggplot(data_prep, aes(DistancetoHome)) +
  geom_histogram()

##YearsSinceLastPromotion is 0 for >15000 employees
ggplot(data_prep, aes(YearsSinceLastPromotion)) +
  geom_histogram()

## Emolumnet_in_Pecentage is a bit right skewed, more employees are betwen 0 to 15
ggplot(data_prep, aes(Emolumnet_in_Pecentage)) +
  geom_histogram()

## MonthlyIncome is a bit right skewed, more employees are <5000
ggplot(data_prep, aes(MonthlyIncome)) +
  geom_histogram()

## YearsCurrentCompany is right skewed distribution
ggplot(data_prep, aes(YearsCurrentCompany)) +
  geom_histogram()


## It is observed that YearsCurrentCompany, YearsSinceLastPromotion and Age have an even impact on ExtraTime
## scatter plot - Bivariate relationship between YearsCurrentCompany and target variable
ggplot(data_prep, aes(x = YearsCurrentCompany, y = ExtraTime)) +
  geom_point(alpha = .3)

ggplot(data_prep, aes(MonthlyRate, TotalWorkingYears)) + geom_point(aes(color = JobRole)) + 
  scale_x_continuous("MonthlyRate", breaks = seq(0,25000,5000))+
  scale_y_continuous("TotalWorkingYears", breaks = seq(0,50,by = 10))+
  theme_bw() + labs(title="Scatterplot")

ggplot(data_prep, aes(TotalWorkingYears, Emolumnet_in_Pecentage)) + geom_point(aes(color = Joblevel)) + 
  scale_x_continuous("TotalWorkingYears", breaks = seq(0,50,10))+
  scale_y_continuous("Emolumnet_in_Pecentage", breaks = seq(0,30,by = 5))+ 
  theme_bw() + labs(title="Scatterplot") + facet_wrap( ~ Joblevel)

## check bivariate relationship between YearsSinceLastPromotion and target
ggplot(data = data_prep, aes(YearsSinceLastPromotion, fill = ExtraTime))+geom_bar()

## check bivariate relationship between Age and target
ggplot(data = data_prep, aes(Age, fill = ExtraTime))+geom_bar()

## It is observed that most employees who have ExtraTime='Yes' has MonthlyIncome <5000
## check bivariate relationship between MonthlyIncome and target
ggplot(data = data_prep, aes(MonthlyIncome,fill = ExtraTime))+geom_density()+facet_grid(~ExtraTime)

## check bivariate relationship between NumberofCompaniesChanged and target
ggplot(data = data_prep, aes(NumberofCompaniesChanged, fill = ExtraTime))+geom_bar()

## check bivariate relationship between Age and MaritalStatus
ggplot(data = data_prep, aes(Age,fill = MaritalStatus))+geom_bar()+facet_grid(~MaritalStatus)

## check bivariate relationship between Division and ExtraTime
ggplot(data = data_prep, aes(Division,fill = ExtraTime))+geom_bar()+facet_grid(~ExtraTime)


## check bivariate relationship between JobRole and ExtraTime (Laboratory Technician has more ExtraTime)
ggplot(data = subset(data_prep, (ExtraTime == 'Yes')) , aes(JobRole,fill = ExtraTime))+geom_bar()+facet_grid(~ExtraTime)
ggplot(data = subset(data_prep, (ExtraTime == 'No')) , aes(JobRole,fill = ExtraTime))+geom_bar()+facet_grid(~ExtraTime)
ggplot(data = subset(data_prep, (ExtraTime == 'Yes')) , aes(MonthlyIncome,fill = WorkLifeBalance))+geom_bar()+facet_grid(~ExtraTime)

## check the bivariate relationship between WorkLifeBalance and MonthlyIncome
ggplot(data_prep, aes(x = MonthlyIncome)) +
  geom_density(aes(color = WorkLifeBalance), alpha = 0.5) +
  theme_classic()

## check bivariate relationship between Age and MonthlyIncome
ggplot(data_prep, aes(x = Age, y = MonthlyIncome)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed") +
  geom_smooth(se = FALSE, lty = "dashed") 

## It is observerd that the Gender,Joblevel,Education,JobRole,Happynesslevelinjob,ESOps,PerformanceRating,Specialization, FrequencyofTravel
## of the employee w.r.t the target variable are evenly distributed
ggplot(data_prep, aes(Gender, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(Joblevel, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(Education, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(JobRole, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(Happynesslevelinjob, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(PerformanceRating, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(Specialization, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(FrequencyofTravel, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")
ggplot(data_prep, aes(ESOps, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")

## It is observed that 'Married' and 'Divorced' employees do less ExtraTime
ggplot(data_prep, aes(MaritalStatus, ..count..)) + geom_bar(aes(fill = ExtraTime), position = "dodge")

## check multivariate relationship between Age, MonthlyIncome and ExtraTime
## It is observed that employees having higher MonthlyIncome and older Age do less ExtraTime
## Age and Monthly income has a linear relationship
ggplot(data_prep, aes(x = Age, y = MonthlyIncome, color = ExtraTime)) +
  geom_point(alpha = .3)

## check box plots to identify any outliers for MonthlyIncome
ggplot(data_prep, aes(x = ExtraTime, y = MonthlyIncome)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

## check box plots to identify any outliers for MonthlyRate
ggplot(data_prep, aes(x = ExtraTime, y = MonthlyRate)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

## check box plots to identify any outliers for Age
ggplot(data_prep, aes(x = ExtraTime, y = Age)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

## check box plots to identify any outliers for Emolumnet_in_Pecentage
ggplot(data_prep, aes(x = ExtraTime, y = Emolumnet_in_Pecentage)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

## Split the data into train and validation data sets
set.seed(786)
train_rows <-  createDataPartition(data_prep$ExtraTime, p = 0.7, list = F)
train_data <-  data_prep[train_rows,]
val_data <- data_prep[-train_rows,]

rm(train_rows)

# Check how records are split with respect to target attribute (approx 0.05)
table(data_prep$ExtraTime)
table(train_data$ExtraTime)
table(val_data$ExtraTime)

############################### Model Building ##########################################

############## using glmnet - logistic regression elastic net ###########################
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid_params <- data.frame(alpha = c(0, 0.5, 1), lambda = c(0.1, 0.3, 0.5))

#train the model
model_glmnet <- caret::train(ExtraTime ~ ., train_data, method = "glmnet", 
                      trControl = ctrl, 
                      tuneGrid = grid_params)

## Predict on Validation Data
pred_test_glmnet <- predict(model_glmnet, val_data)
confusionMatrix(as.factor(pred_test_glmnet), as.factor(val_data$ExtraTime), positive ="Yes") 

#check auc 
pred_glmnet <- prediction(as.numeric(pred_test_glmnet),val_data$ExtraTime)
perf_glmnet <- ROCR::performance(pred_glmnet, measure = "tpr", x.measure ="fpr")

plot(perf_glmnet,col = rainbow(10),colorize = T)
auc <- ROCR::performance(pred_glmnet,measure = "auc")
auc@y.values[[1]]

## predict on the submission test data ##
pred_test_glmnet_sub <- predict(model_glmnet, test_data)
################ using XGBoost without tuning ##########################################
library(xgboost)
dummy_obj <- dummyVars( ~ . , train_data[, !(names(train_data) %in% c("ExtraTime"))])
dummy_obj_test <- dummyVars( ~ . , test_data[, !(names(test_data) %in% c("ExtraTime"))])
train_dummy_data <- as.data.frame(predict(dummy_obj, train_data))
test_data$ExtraTime <- 1
test_data[1,names(test_data) %in% c("ExtraTime")] <- 0
test_dummy_data <- as.data.frame(predict(dummy_obj, val_data))

test_data_sub <- as.data.frame(predict(dummy_obj_test, test_data))

test_data_sub$ExtraTime <- 1
test_data_sub[1,names(test_data_sub) %in% c("ExtraTime")] <- 0
test_data_sub$ExtraTime  <- as.factor(test_data_sub$ExtraTime)

train_dummy_data$ExtraTime <- train_data$ExtraTime
test_dummy_data$ExtraTime <- val_data$ExtraTime
train_dummy_data$ExtraTime <- ifelse(train_dummy_data$ExtraTime == "Yes",1,0)
test_dummy_data$ExtraTime <- ifelse(test_dummy_data$ExtraTime == "Yes",1,0)
train_dummy_data$ExtraTime <- as.factor(train_dummy_data$ExtraTime)
test_dummy_data$ExtraTime  <- as.factor(test_dummy_data$ExtraTime)

train_matrix <- xgb.DMatrix(data = as.matrix(train_dummy_data[, !(names(train_dummy_data) %in% c("ExtraTime"))]), 
                            label = as.matrix(train_dummy_data[, names(train_dummy_data) %in% "ExtraTime"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_dummy_data[, !(names(test_dummy_data) %in% c("ExtraTime"))]), 
                           label = as.matrix(test_dummy_data[, names(test_dummy_data) %in% "ExtraTime"]))

test_matrix_sub <- xgb.DMatrix(data = as.matrix(test_data_sub[, !(names(test_data_sub) %in% c("ExtraTime"))]), 
                               label = as.matrix(test_data_sub[, names(test_data_sub) %in% "ExtraTime"]))

##define parameter list
params_list <- list("objective" = "binary:logistic",
                    "eta" = 0.1,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 6,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.9,
                    "eval_metric" = "error",
                    "silent" = 1)

model_xgb <- xgboost(data = train_matrix, params = params_list, nrounds = 800
                     , early_stopping_rounds = 20)
prob_test_xgb <- predict(model_xgb, test_matrix, type="response",norm.votes=TRUE) 

## check auc
pred <- ROCR::prediction(prob_test_xgb,as.factor(test_dummy_data$ExtraTime))
perf <- ROCR::performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T,print.cutoffs.at= seq(0,1,0.2))
auc <- ROCR::performance(pred,measure = "auc")
auc@y.values[[1]]
pred_test_xgb_normal <- ifelse(prob_test_xgb > 0.4 , 1,0)
confusionMatrix(data = as.factor(pred_test_xgb_normal), reference = test_dummy_data$ExtraTime,positive ="1")


prob_test_xgb_sub <- predict(model_xgb, test_matrix_sub, type="response",norm.votes=TRUE) 
pred_test_xgb_sub <- ifelse(prob_test_xgb_sub > 0.4 , "Yes","No")


submission_data_xgboost_normal <- data.frame(pred_test_xgb_sub)
write.csv(x = submission_data_xgboost_normal,"D:/INSOFE/Lab Machine Learning/Mith/XGBoost_3.csv",row.names = F)

variable_importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = model_xgb)

xgb.plot.importance(variable_importance_matrix)
imp_attr <- variable_importance_matrix[1:20,1]
imp_attr <- data.frame(imp_attr)
colnames(imp_attr) <- c("Feature")
top_Imp_Attr = as.character(imp_attr$Feature[1:20])
top_Imp_Attr
top_Imp_Attr[length(top_Imp_Attr)+1] <- 'ExtraTime'
head(train_Data_std)
colnames(train_Data_std)
train_Data_std <- data.frame(train_dummy_data[,top_Imp_Attr])
test_Data_std <- data.frame(test_dummy_data[,top_Imp_Attr])
train_matrix <- xgb.DMatrix(data = as.matrix(train_Data_std[, !(names(train_Data_std) %in% c("ExtraTime"))]), 
                            label = as.matrix(train_Data_std[, names(train_Data_std) %in% "ExtraTime"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_Data_std[, !(names(test_Data_std) %in% c("ExtraTime"))]), 
                           label = as.matrix(test_Data_std[, names(test_Data_std) %in% "ExtraTime"]))

##define parameter list
params_list <- list("objective" = "binary:logistic",
                    "eta" = 0.1,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 6,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.65,
                    "eval_metric" = "error",
                    "silent" = 1)

model_xgb_imp <- xgboost(data = train_matrix, params = params_list, nrounds = 800
                     , early_stopping_rounds = 20)
prob_test_xgb_imp <- predict(model_xgb_imp, test_matrix, type="response",norm.votes=TRUE) 
## check auc
pred <- ROCR::prediction(prob_test_xgb_imp,as.factor(test_dummy_data$ExtraTime))
perf <- ROCR::performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T,print.cutoffs.at= seq(0,1,0.2))
auc <- ROCR::performance(pred,measure = "auc")
auc@y.values[[1]]
pred_test_xgb_imp <- ifelse(prob_test_xgb > 0.25 , 1,0)
confusionMatrix(data = as.factor(pred_test_xgb_imp), reference = test_dummy_data$ExtraTime,positive ="1")


################ using XGBoost with hyper parameter tuning #############################
library(mlr)
#convert characters to factors
fact_col <- colnames(train_data)[sapply(train_data,is.character)]

for(i in fact_col) set(train_data,j=i,value = factor(train_data[[i]]))
for (i in fact_col) set(val_data,j=i,value = factor(val_data[[i]]))
for (i in fact_col) set(test_data,j=i,value = factor(test_data[[i]]))

#create tasks
traintask <- makeClassifTask (data = train_data,target = "ExtraTime")
testtask <- makeClassifTask (data = val_data,target = "ExtraTime")

test_data$ExtraTime <- "dummy"
test_sub_task <- makeClassifTask (data = test_data, target = "ExtraTime")

#do one hot encoding
traintask <- createDummyFeatures(obj = traintask) 
testtask <- createDummyFeatures(obj = testtask)
test_sub_task <- createDummyFeatures(obj = test_sub_task)

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())
#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     measures = acc, par.set = params, control = ctrl, show.info = T)

#set hyperparameters
lrn_tune <- setHyperPars(lrn, par.vals = mytune$x)

#train model
xgmodel <- mlr::train(learner = lrn_tune, task = traintask)

#predict model on Validation data
pred_test_xgbtune <- predict(xgmodel,testtask)
confusionMatrix(pred_test_xgbtune$data$response,pred_test_xgbtune$data$truth, positive = "Yes")

#check auc 
pred_xgboost <- prediction(as.numeric(pred_test_xgbtune$data$response),val_data$ExtraTime)
perf_xgboost <- ROCR::performance(pred_glmnet, measure = "tpr", x.measure ="fpr")

plot(perf_xgboost,col = rainbow(10),colorize = T)
auc <- ROCR::performance(pred_xgboost,measure = "auc")
auc@y.values[[1]]

#predict model on submission test data
pred_test_xgbtune_sub <- predict(xgmodel,test_sub_task)
submission_data <- data.frame(rownames(test_data), pred_test_xgbtune_sub$data$response)
colnames(submission_data) <- c("RowID","ExtraTime")

#write to a file
write.csv(x = submission_data,"D:/INSOFE/Lab Machine Learning/Mith/XGBoost_2.csv",row.names = F)
# Stop parallel instance ~ Good practice to retire cores when training is complete
parallelStop()

################################# use random forest ###############################
set.seed(1234)
# Build the classification model using randomForest
model_rf = randomForest(ExtraTime ~ ., data=train_data, 
                        keep.forest=TRUE, ntree=100) 

# Print and understand the model
print(model_rf)
## Predict on validation data
pred_test_rf <- predict(model_rf, val_data[,setdiff(names(val_data),
                                                     "ExtraTime")],
                        type = "response" ,
                        norm.votes=TRUE)

# Build confusion matrix and find accuracy  
confusionMatrix(data=pred_test_rf,reference = val_data$ExtraTime, positive ="Yes")
# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model_rf$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
# plot (directly prints the important attributes) 
varImpPlot(model_rf)

## Build model using top 19 variables
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:22])
set.seed(123456)

# Build the classification model using randomForest
model_rf_imp = randomForest(ExtraTime~.,
                            data=train_data[,c(top_Imp_Attr,"ExtraTime")], 
                            keep.forest=TRUE,ntree=200, mtry = 8) 
# Predicton Validation Data
pred_test_rf_imp = predict(model_rf_imp, val_data[,top_Imp_Attr],
                           type="response", norm.votes=TRUE)

confusionMatrix(data=pred_test_rf_imp,reference = val_data$ExtraTime, positive ="Yes")

#check auc 
pred_rf <- prediction(as.numeric(pred_test_rf),val_data$ExtraTime)
perf_rf <- ROCR::performance(pred_rf, measure = "tpr", x.measure ="fpr")

plot(perf_rf,col = rainbow(10),colorize = T)
auc <- ROCR::performance(pred_rf,measure = "auc")
auc@y.values[[1]]

# Predict on Submission Test data
pred_test_rf_sub = predict(model_rf, test_data,
                           type="response", norm.votes=TRUE)

########################## use Decision Trees C5.0 #################################
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35)
                    ,.winnow = "TRUE")
set.seed(300)

model_c5.0 <- caret::train(ExtraTime ~ ., data = train_data, method = "C5.0",
                           trControl = ctrl
                           ,tuneGrid = grid)

summary(model_c5.0)

#Predict on validation data
pred_test_c5.0 <- predict(model_c5.0, val_data)

#check auc 
pred_c5.0 <- prediction(ifelse(pred_test_c5.0 == "Yes",1,0),val_data$ExtraTime)
perf_rf <- ROCR::performance(pred_c5.0, measure = "tpr", x.measure ="fpr")

plot(perf_rf,col = rainbow(10),colorize = T)
auc <- ROCR::performance(pred_c5.0,measure = "auc")
auc@y.values[[1]]

#confusion matrix
confusionMatrix(as.factor(pred_test_c5.0), as.factor(val_data$ExtraTime), positive ="Yes")
################################# use knn ######################################
model_knn <- knn3(ExtraTime ~ . , train_data, k = 5)

#Predict on Validation data
preds_k <- predict(model_knn, val_data)

preds_knn_test <- ifelse(preds_k[, 1] > preds_k[, 2], "No", "Yes")
confusionMatrix(as.factor(preds_knn_test), val_data$ExtraTime, positive ="Yes")

#check auc 
pred_knn <- prediction(ifelse(preds_knn_test == "Yes",1,0),val_data$ExtraTime)
perf_rf <- ROCR::performance(pred_knn, measure = "tpr", x.measure ="fpr")

plot(perf_rf,col = rainbow(10),colorize = T)
auc <- ROCR::performance(pred_knn,measure = "auc")
auc@y.values[[1]]

#Predict on the submission test data
preds_k_sub <- predict(model_knn, test_data)

preds_test_knn_sub <- ifelse(preds_k_sub[, 1] > preds_k_sub[, 2], "No", "Yes")

################### Build a stacked model using knn, random forest, xgboost ################
stacked_df_pred <- data.frame(preds_test_knn_sub,
                              pred_test_xgbtune_sub$data$response,
                              pred_test_rf_sub )
colnames(stacked_df_pred) <- c("knn","xgboost","randomforest")
head(stacked_df_pred)

# Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### mode of all models
i <- 1
for (i in seq(1,nrow(stacked_df_pred),1)){
  
  stacked_df_pred$predicted[[i]] <- ifelse((as.numeric(getmode(stacked_df_pred[i,!names(stacked_df_pred) %in% c("Attrition")]))) == 1,"No","Yes")
  i <- i + 1
}
head(stacked_df_pred,10)
## write it to file
submission_data_stacked <- data.frame(rownames(stacked_df_pred), stacked_df_pred$predicted)
colnames(submission_data_stacked) <- c("RowID","ExtraTime")

#write to a file
write.csv(x = submission_data_stacked,"D:/INSOFE/Lab Machine Learning/Mith/stacked_2.csv",row.names = F)
##################### clustering based on Division and then run Random forest##############
############################## HR division ###############################
train_hr_data_prep <- subset(data_prep,(data_prep$Division == 'Human Resources') )
train_hr_data_prep$Division <- NULL
## split into train and test 
train_hr_rows <- createDataPartition(train_hr_data_prep$ExtraTime, p = 0.7, list = F)
train_hr_data <- train_hr_data_prep[train_hr_rows,]
val_hr_data <- train_hr_data_prep[-train_hr_rows,]
library(caret)
set.seed(300)
# Build the classification model using randomForest
model_rf_hr <- randomForest(ExtraTime ~ ., data=train_hr_data, 
                             keep.forest=TRUE, ntree=100, mtry = 6) 

# Print and understand the model
print(model_rf_hr)
## Predict on test data
pred_test_rf_hr <- predict(model_rf_hr, val_hr_data[,setdiff(names(val_hr_data),
                                                                "ExtraTime")],
                            type = "response" ,
                            norm.votes=TRUE)
# Build confusion matrix and find accuracy  
confusionMatrix(data=pred_test_rf_hr,reference = val_hr_data$ExtraTime, positive ="Yes")

##################### Research & Development #####################
train_rd_data_prep <- subset(data_prep,(data_prep$Division == 'Research & Development') )
train_rd_data_prep$Division <- NULL
## split into train and test 
train_rd_rows <- createDataPartition(train_rd_data_prep$ExtraTime, p = 0.7, list = F)
train_rd_data <- train_rd_data_prep[train_rd_rows,]
val_rd_data <- train_rd_data_prep[-train_rd_rows,]
head(train_rd_data)
set.seed(786)
# Build the classification model using randomForest
model_rf_rd <- randomForest(ExtraTime ~ ., data=train_rd_data, 
                            keep.forest=TRUE, ntree=100, mtry = 6) 

# Print and understand the model
print(model_rf_rd)
## Predict on test data
pred_test_rf_rd <- predict(model_rf_rd, val_rd_data[,setdiff(names(val_rd_data),
                                                             "ExtraTime")],
                           type = "response" ,
                           norm.votes=TRUE)
# Build confusion matrix and find accuracy  
confusionMatrix(data=pred_test_rf_rd,reference = val_rd_data$ExtraTime, positive ="Yes")
########################### Sales ########################################
train_sal_data_prep <- subset(data_prep,(data_prep$Division == 'Sales') )
train_sal_data_prep$Division <- NULL
## split into train and test 
train_sal_rows <- createDataPartition(train_sal_data_prep$ExtraTime, p = 0.7, list = F)
train_sal_data <- train_sal_data_prep[train_sal_rows,]
val_sal_data <- train_sal_data_prep[-train_sal_rows,]

set.seed(786)
# Build the classification model using randomForest
model_rf_sal <- randomForest(ExtraTime ~ ., data=train_sal_data, 
                            keep.forest=TRUE, ntree=100, mtry = 6) 

# Print and understand the model
print(model_rf_sal)
## Predict on test data
pred_test_rf_sal <- predict(model_rf_sal, val_sal_data[,setdiff(names(val_sal_data),
                                                             "ExtraTime")],
                           type = "response" ,
                           norm.votes=TRUE)
# Build confusion matrix and find accuracy  
confusionMatrix(data=pred_test_rf_sal,reference = val_sal_data$ExtraTime, positive ="Yes")

# add all the predictions and get the final accuracy
val_hr_data$RowID <- row.names(val_hr_data)
val_rd_data$RowID <- row.names(val_rd_data)
val_sal_data$RowID <- row.names(val_sal_data)

hr_df <- cbind(val_hr_data[,names(val_hr_data) %in% c("RowID","ExtraTime")],pred_test_rf_hr)
colnames(hr_df) <- c("ExtraTime","RowID","prediction")

rd_df <- cbind(val_rd_data[,names(val_rd_data) %in% c("RowID","ExtraTime")],pred_test_rf_rd)
colnames(rd_df) <- c("ExtraTime","RowID","prediction")

sal_df <- cbind(val_sal_data[,names(val_sal_data) %in% c("RowID","ExtraTime")],pred_test_rf_sal)
colnames(sal_df) <- c("ExtraTime","RowID","prediction")

combined_data_frame <-  rbind(hr_df,rd_df,sal_df)
head(combined_data_frame)
## check the confusion matrix
confusionMatrix(data=combined_data_frame$prediction,reference = combined_data_frame$ExtraTime, positive ="Yes")

###################################### End #################################################