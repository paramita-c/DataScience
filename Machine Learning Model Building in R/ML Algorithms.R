##########Scripts required#########
## Data Visualization
## Descriptive Statistics -- summary, structure, histogram, scatter plot, box plot, corr plot, PP and QQ plot
## Feature Extraction, Feature Engineering ###
## Data Preprocessing -- categorical, numeric, dummification, standardization, imputation ##
## Variable Importance -- XGBoost, Random Forest
## Build models using - logistic, C5.0, knn , Naive Bayes, bagged trees, random forest, tuned XGBoost

## set the path
setwd("D:/INSOFE/Lab Machine Learning/20180617_Batch42__CSE7120c_Lab02_AirFares_Tableau")
## remove all variables
rm(list = ls(all.names = T))

#read the data
data <- read.csv("HR_Attrition.csv")
#test_data_ff <- read.csv("test.csv")

#remove the ID and target columns
data_new <- data[, !names(data) %in% c("EmployeeNumber", "Attrition")]
head(data_new)

#check missing values and remove columns having > 10% of missing values
colSums(is.na(data))
# 2 columns have more than 10% of missing values
sum(colSums(is.na(data_new)) > 0.1 * nrow(data_new))
## 2 features which have more than 10% missing values, will drop those features
#data_f <- data_new[, colSums(is.na(data_new)) <= 0.1 * nrow(data_new)]
#data_new <- data_f
#rm(data_f)
summary(data_new)

## drop the columns having no variance
data_new$Over18 <- NULL
data_new$StandardHours <- NULL

num_attr <- c("Age","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears"
              ,"YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")

cat_attr <- setdiff(colnames(data_new),num_attr)
## convert categorical variables into factors
data_new[,num_attr]
data_new[,cat_attr] <- lapply(data_new[,cat_attr] , factor)
str(data_new)
head(data_new)
## appending the target column
data_new$Attrition <- as.factor(data$Attrition)
data_new$Attrition <- ifelse(data_new$Attrition == "Yes","1", "0")
head(data_new)
## Split the train_data data into train and validation data sets

set.seed(786)

train_rows =  createDataPartition(data_new$Attrition, p = 0.7, list = F)
train_data = data_new[train_rows,]
test_data = data_new[-train_rows,]

rm(train_rows)

## Spliting according to normal distribution sampling
set.seed(200)
data$Random <-runif(nrow(data),0,1)
train_data<-data_new[which(data$Random<=0.70),]
test_data<-data_new[which(data$Random>0.70),]

# Check how records are split with respect to target attribute (approx 0.05)
table(data_new$Attrition)
table(train_data$Attrition)
table(test_data$Attrition)

#Imputing missing values using centralImputation 
train_data <- centralImputation(train_data)
sum(is.na(train_data))
test_data <- centralImputation(test_data)
sum(is.na(test_data))
str(train_data)
train_data$Attrition <- as.factor(train_data$Attrition)
test_data$Attrition <- as.factor(test_data$Attrition)

## standardizing & binning 
## remove rows with more NA columns 
## same value for all rows, remove those columns -- optional

#####################Use logistic regression ###########################
#library(glmnet)
#X_train <- as.matrix(train_data[, !names(train_data) %in% c("Attrition")])
#y_train <- as.matrix(train_data[, c("Attrition")])

#model_log_lasso <- cv.glmnet(X_train, y_train, alpha = 1, type.measure = "auc", family ="binomial"
#                      , nfolds = 4)
#plot(model_log_lasso$glmnet.fit, xvar="lambda", label=TRUE)
#lasso_model <- glmnet(X_train, y_train, lambda = model_log_lasso$lambda.min, alpha = 1
#                      , family ="binomial")
#summary(lasso_model)
#preds_glm <- predict(lasso_model, val_data)
#confusionMatrix(preds_glm, val_data$target)
## store the predictions
#preds_train_glm <- predict(lasso_model)

### use simple logistic
model_log_reg <- glm(formula = Attrition~.
               , data = train_data, family = binomial(link ='logit'))

summary(model_log_reg)
prob_test_log <- predict(model_log_reg, test_data,
                    type="response", norm.votes=TRUE)


#check auc 
pred <- prediction(as.numeric(prob_test_log),test_data$Attrition)
perf <- ROCR::performance(pred, measure = "tpr","fpr")


plot(perf,col = rainbow(10),colorize = T, print.cutoffs.at= seq(0,1,0.2))
auc <- ROCR::performance(pred,measure = "auc")
auc@y.values[[1]]
pred_test_log <- ifelse(pred_test > 0.5 , "1","0")
confusionMatrix(data=as.factor(pred_test_log),reference = test_data$Attrition, positive ="1")

# Predictions on train data
pred_log_prob <- predict(model_log_reg, train_data,
                    type="response", norm.votes=TRUE)
pred_log <- ifelse(pred_log_prob > 0.5 , "1","0")

## using glmnet - logistic regression elastic net
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid_params <- data.frame(alpha = c(0, 0.5, 1), lambda = c(0.2, 0.3, 0.4))
model_glmnet <- train(Attrition ~ ., train_data, method = "glmnet", 
      #preProcess = c("center", "scale", "pca"),
      trControl = ctrl, 
      tuneGrid = grid_params)
pred_test_glmnet <- predict(model_glmnet, test_data)

confusionMatrix(as.factor(pred_test_glmnet), as.factor(test_data$Attrition), positive ="Yes")
#######################use C5.0 #################################
#ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35)
                    ,.winnow = "TRUE"
                    )
set.seed(300)

model_c5.0 <- caret::train(Attrition ~ ., data = train_data, method = "C5.0",
           metric = "Accuracy",
           trControl = ctrl
           ,tuneGrid = grid
           )

summary(model_c5.0)

pred_test_c5.0 <- predict(model_c5.0, test_data)

confusionMatrix(as.factor(pred_test_c5.0), as.factor(test_data$Attrition), positive ="Yes")

# Predictions on train data
pred_c5.0 <- predict(model_c5.0, train_data)
####################use knn ##################################
model_knn <- knn3(Attrition ~ . , train_data, k = 5)

preds_k <- predict(model_knn, test_data)

preds_knn_test <- ifelse(preds_k[, 1] > preds_k[, 2], "No", "Yes")

confusionMatrix(as.factor(preds_knn_test), test_data$Attrition, positive ="Yes")

# Predictions on train data
pred_knn_prob <- predict(model_knn, train_data)
pred_knn <- ifelse(pred_knn_prob[, 1] > pred_knn_prob[, 2], 0, 1)
## using knn with cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid_params <- data.frame(k = c(3, 10, 1))
model_knn_cv <- train(Attrition ~ ., train_data, method = "knn", 
                      #preProcess = c("center", "scale", "pca"),
                      trControl = ctrl, 
                      tuneGrid = grid_params)
pred_test_knn_cv <- predict(model_knn_cv, test_data)

confusionMatrix(as.factor(pred_test_knn_cv), as.factor(test_data$Attrition), positive ="Yes")
#################### use Naive Bayes #########################
model_naive_bayes <- naiveBayes(Attrition ~., data = train_data )
#test the model on test data
pred_naive_bayes <- predict(model_naive_bayes,test_data)
confusionMatrix(pred_naive_bayes,test_data$Attrition,positive = "1" )
####################### bagged trees #####################
library(caret)
set.seed(300)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_bagged <-  train(Attrition ~ ., data = train_data, method = "treebag",
                       trControl = ctrl)

firstprediction <-predict(model_bagged, train_data)
# Predicton Test Data
pred_test_bagtree <- predict(model_bagged, test_data)

confusionMatrix(pred_test_bagtree, test_data$Attrition, positive ="1")
####################use random forest ###############
set.seed(1234)

# Build the classification model using randomForest
model_rf = randomForest(Attrition ~ ., data=train_data, 
                     keep.forest=TRUE, ntree=100) 
# Print and understand the model
print(model_rf)
## Predict on test data
pred_test_rf <- predict(model_rf, test_data[,setdiff(names(test_data),
                                                   "Attrition")],
                     type = "response" ,
                     norm.votes=TRUE)

# Build confusion matrix and find accuracy  
conf_matrix <- confusionMatrix(data=pred_test_rf,reference = test_data$Attrition, positive ="1")

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model_rf$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
# plot (directly prints the important attributes) 
varImpPlot(model_rf)

## build model using top 20 attributes
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:20])

set.seed(123456)

# Build the classification model using randomForest
model_rf_imp = randomForest(Attrition~.,
                         data=train_data[,c(top_Imp_Attr,"Attrition")], 
                         keep.forest=TRUE,ntree=100, mtry = 8) 

# Prediction on Train Data
pred_rf_imp <- predict(model_rf_imp, train_data[,top_Imp_Attr],
                       type="response", norm.votes=TRUE)
# Predicton Test Data
pred_test_rf_imp = predict(model_rf_imp, test_data[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)

confusionMatrix(data=pred_test_rf_imp,reference = test_data$Attrition, positive ="1")

#check auc 
pred <- prediction(as.numeric(pred_test_rf_imp),test_data$Attrition)
perf <- performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T)
auc <- performance(pred,measure = "auc")
auc@y.values[[1]]

####################### use XGBoost ####################
library(xgboost)
dummy_obj <- dummyVars( ~ . , train_data[, !(names(train_data) %in% c("Attrition"))])
train_dummy_data <- as.data.frame(predict(dummy_obj, train_data))
test_dummy_data <- as.data.frame(predict(dummy_obj, test_data))

train_dummy_data$Attrition <- train_data$Attrition
test_dummy_data$Attrition <- test_data$Attrition
train_matrix <- xgb.DMatrix(data = as.matrix(train_dummy_data[, !(names(train_dummy_data) %in% c("Attrition"))]), 
                            label = as.matrix(train_dummy_data[, names(train_dummy_data) %in% "Attrition"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_dummy_data[, !(names(test_dummy_data) %in% c("Attrition"))]), 
                           label = as.matrix(test_dummy_data[, names(test_dummy_data) %in% "Attrition"]))
##define parameter list
params_list <- list("objective" = "binary:logistic",
                    "eta" = 0.1,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 6,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.65,
                    "eval_metric" = "auc",
                    "silent" = 1)

model_xgb <- xgboost(data = train_matrix, params = params_list, nrounds = 500
                                 , early_stopping_rounds = 20)
prob_test_xgb <- predict(model_xgb, test_matrix,type="response", norm.votes=TRUE) 

#check auc 
pred <- prediction(prob_test_xgb,test_dummy_data$Attrition)
perf <- performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T,print.cutoffs.at= seq(0,1,0.2))
auc <- performance(pred,measure = "auc")
auc@y.values[[1]]
pred_test_xgb <- ifelse(prob_test_xgb > 0.7 , 1,0)
pred_test_xgb <- as.factor(pred_test_xgb)
confusionMatrix(data=pred_test_xgb ,reference = test_dummy_data$Attrition, positive ="1")
####################### use XGBoost tuning####################
#convert characters to factors
fact_col <- colnames(train_data)[sapply(train_data,is.character)]

for(i in fact_col) set(train_data,j=i,value = factor(train_data[[i]]))
for (i in fact_col) set(test_data,j=i,value = factor(test_data[[i]]))

#create tasks
traintask <- makeClassifTask (data = train_data,target = "Attrition")
testtask <- makeClassifTask (data = test_data,target = "Attrition")

#do one hot encoding
traintask <- createDummyFeatures(obj = traintask) 
testtask <- createDummyFeatures(obj = testtask)

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                       # makeNumericParam("lambda",lower=0.05,upper=0.2),
                       # makeNumericParam("lambda_bias",lower = 0.04 ,upper =0.4),
                       # makeNumericParam("alpha", lower = 0.05, upper = 0.4),
                       # makeNumericParam("eta", lower = 0.01, upper = 0.4),
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

mytune$y 
mytune$x
#set hyperparameters
lrn_tune <- setHyperPars(lrn, par.vals = mytune$x)

#train model
library(mlr)
xgmodel <- mlr::train(learner = lrn_tune, task = traintask)

#predict model
pred_test_xgbtune <- predict(xgmodel,testtask)

confusionMatrix(pred_test_xgbtune$data$response,pred_test_xgbtune$data$truth, positive = "1")

# Stop parallel instance ~ Good practice to retire cores when training is complete
parallelStop()
## Build a stacked model
stacked_df_pred <- data.frame(pred_test_log,
                              pred_test_c5.0,
                              preds_knn_test,
                              pred_test_rf_imp,
                              pred_test_bagtree,
                              pred_naive_bayes,
                              #pred_test_xgbtune$data$response 
                               pred_test_xgb
                              , test_data$Attrition)
head(stacked_df_pred)
# Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

stacked_df_pred <- data.frame(apply(stacked_df_pred,2,FUN = function (x) {as.numeric(as.character(x))}))
stacked_df_pred$predicted <- NULL
head(stacked_df_pred)
str(stacked_df_pred)
### mode of all models
i <- 1
for (i in seq(1,nrow(stacked_df_pred),1)){

  stacked_df_pred$predicted[[i]] <- (as.numeric(getmode(stacked_df_pred[i,!names(stacked_df_pred) %in% c("Attrition")])))
  i <- i + 1
}

head(stacked_df_pred)
str(stacked_df_pred)
confusionMatrix(as.factor(stacked_df_pred$predicted),test_data$Attrition, positive = "1")
write.csv(x = stacked_df_pred,"D:/INSOFE/Lab Machine Learning/20180617_Batch42__CSE7120c_Lab02_AirFares_Tableau/mode_result_code.csv")
head(data)
############## Boruta for feature importance ###########
set.seed(7777)
train_data$Attrition <- as.factor(train_data$Attrition)
Boruta.Attr <- Boruta(Attrition ~ ., data = train_data,doTrace = 2, ntree = 500)
#getNonRejectedFormula(Boruta.Attr)
plot(Boruta.Attr)
df_output <- data.frame(attStats(Boruta.Attr))
df_output$Attributes <- rownames(df_output)
Column_vector <- ifelse(df_output$decision != "Rejected",df_output$Attributes,"0")
Column_vector <- Column_vector[!Column_vector =="0"]
Column_vector
############## Recursive Feature Elimination + Random Forest ###########
## imputation
data <- centralImputation(data)
## recursive feature selection
target <- data$V16
data$V16 <- NULL
data$V16 <- as.factor(target)
head(data)
str(data)
set.seed(7)
# load the library
library(mlbench)
library(caret)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
length(colnames(data))
results <- rfe(data[,1:18], data[,19],sizes = c(1:18), rfeControl=control)

# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
Column_vector <- predictors(results)
set.seed(786)
head(data)
train_rows =  createDataPartition(data$V16, p = 0.7, list = F)
train_data = data[train_rows,]
test_data = data[-train_rows,]

rm(train_rows)

##build random forest 
set.seed(123)

# Build the classification model using randomForest
model_rf = randomForest(V16 ~ ., data=train_data, 
                        keep.forest=TRUE, ntree=100, mtry = 5) 
# Print and understand the model
print(model_rf)
## Predict on test data
pred_test_rf <- predict(model_rf, test_data[,setdiff(names(test_data),
                                                     "V16")],
                        type = "response" ,
                        norm.votes=TRUE)

# Build confusion matrix and find accuracy  
confusionMatrix(data=pred_test_rf,reference = test_data$V16, positive ="+")
