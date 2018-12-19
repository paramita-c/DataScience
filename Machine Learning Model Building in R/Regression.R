## set the path
setwd("D:/INSOFE/Lab Machine Learning/20180506_Batch42_CSE7302c_Lab02_Multiple_Linear_Regression_activity")
## remove all variables
rm(list = ls(all.names = T))

data <- read.csv("housing_data.csv")
summary(data)
str(data)
head(data)
tail(data)

colSums(is.na(data))

table(factor(data$CHAS))
table(factor(data$ZN))

ggcorr(data[,!names(data) %in% c("MV")],
       method = c("pairwise", "spearman"),
       nbreaks = 8,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

data$Interact_1 <- data$CRIM*data$INDUS*data$NOX*data$AGE
data$Interact_2 <- -(data$NOX*data$DIS*data$AGE*data$INDUS)
data$Sum_NAs <- rowSums(is.na(data[,!names(data) %in% c("Interact_1","Interact_2","MV")]))

cat_attr <- c("CHAS","RAD")
num_attr <- setdiff(colnames(data[,!names(data) %in% c("MV")]), cat_attr)

#convert into factors
data[,names(data) %in% cat_attr] <- lapply(data[,names(data) %in% cat_attr] , factor)

#imputation
library(DMwR)
data[, !names(data) %in% c("MV")] <- knnImputation(data[,!names(data) %in% c("MV")],k = 5)
sum(is.na(data))

summary(data)
## Spliting according to normal distribution sampling
set.seed(200)

data$Random <-runif(nrow(data),0,1)
train_data<-data[which(data$Random<=0.70),]
test_data<-data[which(data$Random>0.70),]

data$Random <- NULL

#################### use glmnet #################################################
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid_params <- data.frame(alpha = c(0, 0.5, 1), lambda = c(0.1, 0.3, 0.7))
model_glmnet <- caret::train(MV ~ ., train_data, method = "glmnet", 
                      preProcess = c("center", "scale"),
                      metric = "RMSE",
                      trControl = ctrl, 
                      tuneGrid = grid_params)
summary(model_glmnet)

pred_test_glmnet <- predict(model_glmnet, test_data)
pred_train_glmnet <- predict(model_glmnet, train_data)
#compare the performance metrices
library(DMwR)
regr.eval(test_data$MV, pred_test_glmnet)
regr.eval(train_data$MV, pred_train_glmnet)

##################### use random forest ############################################
set.seed(1234)
library(randomForest)
# Build the classification model using randomForest
model_rf = randomForest(MV ~ ., data=train_data, 
                        keep.forest=TRUE, ntree=200, mtry = 8) 
# Print and understand the model
print(model_rf)
## Predict on test data
pred_test_rf <- predict(model_rf, test_data[,setdiff(names(test_data),
                                                     "MV")],norm.votes=TRUE)
regr.eval(test_data$MV,pred_test_rf)
# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model_rf$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
# plot (directly prints the important attributes) 
varImpPlot(model_rf)

## build model using top 13 attributes
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:15])

set.seed(123456)

# Build the regression model using randomForest
model_rf_imp = randomForest(MV~.,
                            data=train_data[,c(top_Imp_Attr,"MV")], 
                            keep.forest=TRUE,ntree=220, mtry = 5) 
## Predict on test data
pred_test_rf_imp <- predict(model_rf_imp, test_data[,setdiff(names(test_data),
                                                     "MV")],norm.votes=TRUE)
regr.eval(test_data$MV,pred_test_rf_imp)
####################### use XGBoost ############################
library(xgboost)
dummy_obj <- dummyVars( ~ . , train_data[, !(names(train_data) %in% c("Attrition"))])
train_dummy_data <- as.data.frame(predict(dummy_obj, train_data))
test_dummy_data <- as.data.frame(predict(dummy_obj, test_data))

train_dummy_data$MV <- train_data$MV
test_dummy_data$MV <- test_data$MV
train_matrix <- xgb.DMatrix(data = as.matrix(train_dummy_data[, !(names(train_dummy_data) %in% c("MV"))]), 
                            label = as.matrix(train_dummy_data[, names(train_dummy_data) %in% "MV"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_dummy_data[, !(names(test_dummy_data) %in% c("MV"))]), 
                           label = as.matrix(test_dummy_data[, names(test_dummy_data) %in% "MV"]))
##define parameter list
params_list <- list("objective" = "reg:linear",
                    "eta" = 0.1,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 6,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.65,
                    "eval_metric" = "rmse",
                    "silent" = 1)


model_xgb <- xgboost(data = train_matrix, params = params_list, nrounds = 500
                     , early_stopping_rounds = 20)
pred_test_xgboost <- predict(model_xgb, test_matrix) 
regr.eval(test_dummy_data$MV ,pred_test_xgboost)

### tuning XGBoost ###
#load library for machine learning
library(mlr)

#create task
train.task <- makeRegrTask(data = train_data, target = "MV")
test.task <- makeRegrTask(data=test_data,target = "MV")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#do one hot encoding
traintask <- createDummyFeatures(obj = train.task) 
testtask <- createDummyFeatures(obj = test.task)

# Create repeated cross validation folds
cv_folds <- makeResampleDesc("CV",iters=5L)


# Define model tuning algorithm ~ Random tune algorithm
random_tune <-  makeTuneControlRandom(maxit = 10L)

library(parallelMap)
library(parallel)
# Define model
model <- makeLearner("regr.xgboost") # Regression XgBoost model

# Define parameters of model and search grid 
model_Params <- makeParamSet(
  makeIntegerParam("nrounds",lower=50L,upper=100L),
  makeIntegerParam("max_depth",lower = 3L,upper = 10L),
#  makeNumericParam("lambda",lower=0.55,upper=0.60),
#  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)
parallelStartSocket(cpus = detectCores())

# Tune model to find best performing parameter settings using random search algorithm
tuned_model <- tuneParams(learner = model,
                          task = traintask,
                          resampling = cv_folds,
                          measures = rsq,       # R-Squared performance measure, this can be changed to one or many
                          par.set = model_Params,
                          control = random_tune,
                          show.info = FALSE)
#View tuning results
tuned_model

# Apply optimal parameters to model
model <- setHyperPars(learner = model,
                      par.vals = tuned_model$x)

# Verify performance on cross validation folds of tuned model
resample(model,traintask,cv_folds,measures = list(rsq,mse))

# Train final model with tuned parameters
xgBoost <- train(learner = model,task = traintask)

# Predict on test set
pred_test_xgbtune <- predict(xgBoost,testtask)

# Stop parallel instance ~ Good practice to retire cores when training is complete
parallelStop()
str(pred_test_xgbtune)

regr.eval(pred_test_xgbtune$data$truth ,pred_test_xgbtune$data$response)
