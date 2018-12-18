############################Data Pre Processing, Feature Engineering############################
setwd("C:/Users/papachak/Downloads/CSE7305c_CUTe")
rm(list = ls(all.names = T))
preprocess_model <- function() {
  
  
  ## import the libraries
  library(DMwR)
  library(vegan)
  library(randomForest)
  library(caret)
  library(ROCR)
  library(e1071)
  library(corrplot)
  library(Hmisc)
  library(tidyverse)
  library(gridExtra)
  library(ggridges)
  library(caret)
  library(GGally)
  library(hexbin)
  library(ggplot2)
 
  #read the data
  data <- read.csv("train.csv")
  t_data <- read.csv("test.csv")
  test_data_ff <- read.csv("test.csv")
  
  summary(data)
  str(data)
  head(data)
  tail(data)
  
  #check missing values
  colSums(is.na(data))
  
  data_new <- data[, !names(data) %in% c("ID", "target")]
  test_data_ff <-  test_data_ff[,!names(test_data_ff) %in% c("ID")]
  data_new <- rbind(data_new,test_data_ff)
  ## check the correlation
  res <- cor(data[,2:30], use = "complete.obs")
  
  corrplot(res, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  res_1 <- cor(data[,31:65], use = "complete.obs")
  
  corrplot(res_1, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  res_2 <- cor(data[,15:45], use = "complete.obs")
  
  corrplot(res_2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  ## Following correlations were observed
  #Attribute 18, 7, 14 and 2 - +1
  #Attribute 10,2, 18, 7, 14, 38 - -1
  
  #Attribute 58, 62, 30, 43, 44, 31,39, 42, 57 -- +1
  #Attribute 49, 58, 62, 30, 43, 44, 31, 39, 42 -- -1
  #Attribute 56, 58, 62, 30, 43, 44, 31, 39, 42  -- -1
  
  #Attr 19 and 23 -- 0.8
  #Attr 8 and 17 -- 0.8
  ## add the new interaction terms
  #Attribute 18, 7, 14 and 2 ~ +1
  data_new$Interact1 <- (data_new$Attr18 * data_new$Attr7 * data_new$Attr14 * data_new$Attr2)
  #Attribute 10,2, 18, 7, 14, 38 ~ -1
  data_new$Interact2 <- -(data_new$Attr10 * data_new$Attr2 * data_new$Attr18 * data_new$Attr7
                          * data_new$Attr14* data_new$Attr38)
  #Attribute 58, 62, 30, 43, 44, 31,39, 42, 57 -- +1
  data_new$Interact3 <- (data_new$Attr58 * data_new$Attr62 * data_new$Attr30 * data_new$Attr43
                         * data_new$Attr44* data_new$Attr31* data_new$Attr39* data_new$Attr42
                         * data_new$Attr57)
  #Attribute 49, 58, 62, 30, 43, 44, 31, 39, 42 -- -1
  #Attribute 56, 58, 62, 30, 43, 44, 31, 39, 42  -- -1
  data_new$Interact4 <- -(data_new$Attr49 * data_new$Attr58 * data_new$Attr62 * data_new$Attr30
                          * data_new$Attr43* data_new$Attr44* data_new$Attr31* data_new$Attr39
                          * data_new$Attr42 * data_new$Attr56)
  #Attr 19 and 23 -- 0.8
  data_new$Interact5 <- (data_new$Attr19 * data_new$Attr23)
  #Attr 8 and 17 -- 0.8
  data_new$Interact6 <- (data_new$Attr8 * data_new$Attr17)
  head(data_new)
  ## check the disttibution of the interaction terms
  hist(data_new$Interact1)
  hist(data_new$Interact2)
  hist(data_new$Interact3)
  hist(data_new$Interact4)
  hist(data_new$Interact5)
  hist(data_new$Interact6)
  ## interaction terms are also heavily skewed
  
  #check missing values
  sum(colSums(is.na(data_new)) > 0.1 * nrow(data_new))
  colSums(is.na(data_new)) > 0.1 * nrow(data_new)
  ## 2 features which have more than 10% missing values, will drop those features
  data_f <- data_new[, colSums(is.na(data_new)) <= 0.1 * nrow(data_new)]
  data_new <- data_f
  head(data_new)
  ## imputing the missing values with 1-hot encoding, which is a new synthetic feature is devised 
  ## (for each feature having missing values) where the feature value is 1 if the value is missing
  ## else it's 0
  nCol_Counter <- as.numeric(length(colnames(data_new[, !names(data_new) %in% c("ID", "target")])))
  
  i <- 1
  Col_Counter <- ncol(data_new) + 1
  colname = "Col_"
  
  for (i in seq(1,nCol_Counter,1)) {
    
    if (sum(is.na(data_new[,i])) > 0 ){
      new_COl_v <- ifelse(is.na(data_new[,i]) == TRUE, 1, 0)
      data_new[gsub(" ","",paste(colname, Col_Counter))] <- new_COl_v
      new_COl_v <- NULL
      Col_Counter <- Col_Counter + 1
    }
    
    i <- i + 1
  }
  
  rm(Col_Counter,colname,i,new_COl_v, nCol_Counter)
  head(data_new)
  colnames(data_new)
  
  ## One more synthetic feature is added which has the sum of all the NAs for that observation(row)
  data_new[gsub(" ","",paste("Sum","_NAs"))] <- rowSums(is.na(data_new))
  head(data_new)
  
  ##separate the test and data sets
  test_data_ff <- data_new[seq(nrow(data)+1,nrow(data_new),1),]
  test_data_final <- test_data_ff
  data_final <- data_new[seq(1,nrow(data),1),]
  
  ## appending the target column
  data_final$target <- data$target
  head(data_final)
  
  ## Splitting the data now into train and test
  
  set.seed(786)
  
  train_RowIDs =  createDataPartition(data_final$target, p = 0.7, list = F)
  train_Data = data_final[train_RowIDs,]
  test_Data = data_final[-train_RowIDs,]
  
  # Check how records are split with respect to target attribute (approx 0.05)
  table(data_final$target)
  table(train_Data$target)
  table(test_Data$target)
  
  #Imputing missing values using centralImputation for the given attributes only
  train_Data <- centralImputation(train_Data)
  sum(is.na(train_Data))
  test_Data <- centralImputation(test_Data)
  sum(is.na(test_Data))
  
  ## Numerical attributes are heavily skewed, so applying z normalization
  #head(train_Data)
  #train_Data_std <- decostand(train_Data[,seq(1,70,1)],"standardize",2)
  
  #train_Data_std <- cbind(train_Data_std, train_Data[,seq(71,136,1)])
  #head(train_Data_std)
  
  #test_Data_std <- decostand(test_Data[,seq(1,70,1)],"standardize",2)
  #head(test_Data_std)
  #test_Data_std <- cbind(test_Data_std, test_Data[,seq(71,136,1)])
  #head(test_Data_std)
  train_Data_std <- train_Data
  train_Data_std$target <- as.factor(as.character(train_Data_std$target))
  test_Data_std <- test_Data
  test_Data_std$target <- as.factor(as.character(test_Data_std$target))
  
}

######################Randfom Forest model#################################################
## Model Building
set.seed(1234)
head(train_Data_std)
colnames(train_Data_std)

# Build the classification model using randomForest
model = randomForest(target ~ ., data=train_Data_std, 
                     keep.forest=TRUE, ntree=100) 

# Print and understand the model
print(model)
head(test_Data_std)
## Predict on test data
pred_Test <- predict(model, test_Data_std[,setdiff(names(test_Data_std),
                                                   "target")],
                     type = "response" ,
                     norm.votes=TRUE)

# Build confusion matrix and find accuracy  
cm_Test <- table("actual"=test_Data_std$target, "predicted"=pred_Test)
conf_matrix <- confusionMatrix(data=pred_Test,reference = test_Data_std$target, positive ="1")
print(conf_matrix)
print(cm_Test)

precision <- 174/(174+27)
recall <- 174/(174+363)

f1_score = 2*precision*recall/(precision+recall)

## analyze the variable importance
# Important attributes
model$importance  
round(importance(model), 2)   

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
rf_Imp_Attr
# plot (directly prints the important attributes) 
varImpPlot(model)

## build model using top 30 attributes
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:30])

set.seed(123456)

# Build the classification model using randomForest
model_Imp = randomForest(target~.,
                         data=train_Data_std[,c(top_Imp_Attr,"target")], 
                         keep.forest=TRUE,ntree=100, mtry = 8) #100, 8-- 57
print(model_Imp)
# Predicton Test Data
pred_Test = predict(model_Imp, test_Data_std[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual" = test_Data_std$target, 
                "predicted" = pred_Test)

cm_Test

conf_matrix <- confusionMatrix(data=pred_Test,reference = test_Data_std$target, positive ="1")
print(conf_matrix)
#check auc 

pred <- prediction(as.numeric(pred_Test),test_Data_std$target)
perf <- performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T)
auc <- performance(pred,measure = "auc")
auc@y.values[[1]]

precision <- 227/(227+21)
recall <- 227/(227+310)

f1_score = 2*precision*recall/(precision+recall) ## 0.58 fscore
test_data_ff <- centralImputation(test_data_ff)
##test_data_ff_1 <- decostand(test_data_ff[,seq(1,64,1)],"standardize",2)
head(test_data_ff)
pred_Test = predict(model_Imp, test_data_ff[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)
pred_Test
Summary_output <- data.frame(cbind(t_data,pred_Test ))
Summary_output <- Summary_output[,colnames(Summary_output) %in% c("ID","pred_Test")]
head(Summary_output)
#write to a file
write.csv(x = Summary_output,"C:/Users/papachak/Downloads/CSE7305c_CUTe/Random_Forest.csv")
####################################XGBoost################################################
preprocess_model()
library(xgboost)

train_matrix <- xgb.DMatrix(data = as.matrix(train_Data_std[, !(names(train_Data_std) %in% c("target"))]), 
                            label = as.matrix(train_Data_std[, names(train_Data_std) %in% "target"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_Data_std[, !(names(test_Data_std) %in% c("target"))]), 
                           label = as.matrix(test_Data_std[, names(test_Data_std) %in% "target"]))


sumposclass <- sum(ifelse(train_Data_std$target == 1, 1,0))
sumnegclass <- sum(ifelse(train_Data_std$target == 0, 1,0))
##define parameter list
params_list <- list("objective" = "binary:logistic",
                    "eta" = 0.1,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 6,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.65,
                    "eval_metric" = "auc",
                    "silent" = 1, scale_pos_weight = sumnegclass/sumposclass )

xgb_model_with_params <- xgboost(data = train_matrix, params = params_list, nrounds = 500
                                 , early_stopping_rounds = 20)
#saving the model with 75% f1 score
#xgb.save(xgb_model_with_params, "xgb_model_75")

prob_Test <- predict(xgb_model_with_params, test_matrix,type="response", norm.votes=TRUE) 

#check auc 

pred <- prediction(prob_Test,test_Data_std$target)
perf <- performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T,print.cutoffs.at= seq(0,1,0.2))
auc <- performance(pred,measure = "auc")
auc@y.values[[1]]
pred_Test <- ifelse(prob_Test > 0.5 , 1,0)

# Build confusion matrix and find accuracy   
cm_Test = table("actual" = test_Data_std$target, 
                "predicted" = pred_Test)

cm_Test
precision <- 342/(342+81)
recall <- 342/(342+195)

precision <- 332/(332+78)
recall <- 332/(332+205)

f1_score = 2*precision*recall/(precision+recall) ## 0.71 fscore
test_data_ff <- centralImputation(test_data_ff)
##test_data_ff_1 <- decostand(test_data_ff[,seq(1,64,1)],"standardize",2)
head(test_data_ff)
prob_Test <- predict(xgb_model_with_params, as.matrix(test_data_ff),type="response", norm.votes=TRUE) 
pred_Test <- ifelse(prob_Test > 0.2 , "1","0")
Summary_output <- data.frame(cbind(t_data,pred_Test ))
Summary_output <- Summary_output[,colnames(Summary_output) %in% c("ID","pred_Test")]
head(Summary_output)
#write to a file
write.csv(x = Summary_output,"C:/Users/papachak/Downloads/CSE7305c_CUTe/XGBoost.csv")
#############################trying with imp variables only###################
## 30 variables give 76%
## 40 variables give 76.71%
## 50 variables give 77%
variable_importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model_with_params)

xgb.plot.importance(variable_importance_matrix)
imp_attr <- variable_importance_matrix[1:50,1]
imp_attr <- data.frame(imp_attr)
colnames(imp_attr) <- c("Feature")
top_Imp_Attr = as.character(imp_attr$Feature[1:50])
top_Imp_Attr
top_Imp_Attr[length(top_Imp_Attr)+1] <- 'target'
head(train_Data_std)
colnames(train_Data_std)
train_Data_std <- data.frame(train_Data_std[,top_Imp_Attr])
test_Data_std <- test_Data_std[,top_Imp_Attr]
train_matrix <- xgb.DMatrix(data = as.matrix(train_Data_std[, !(names(train_Data_std) %in% c("target"))]), 
                            label = as.matrix(train_Data_std[, names(train_Data_std) %in% "target"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_Data_std[, !(names(test_Data_std) %in% c("target"))]), 
                           label = as.matrix(test_Data_std[, names(test_Data_std) %in% "target"]))

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
#    nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#108      20         6 0.3   0.6              0.6                1       0.7
xgb_model_with_params <- xgboost(data = train_matrix, params = params_list, nrounds = 500
                                 , early_stopping_rounds = 20)
#saving the model with 77% f1 score
#xgb.save(xgb_model_with_params, "xgb_model_77")
print(xgb_model_with_params)
prob_Test <- predict(xgb_model_with_params, test_matrix,type="response", norm.votes=TRUE) 

#check auc 

pred <- prediction(prob_Test,test_Data_std$target)
perf <- performance(pred, measure = "tpr", x.measure ="fpr")

plot(perf,col = rainbow(10),colorize = T,print.cutoffs.at= seq(0,1,0.2))
auc <- performance(pred,measure = "auc")
auc@y.values[[1]]
pred_Test <- ifelse(prob_Test > 0.2 , 1,0)

# Build confusion matrix and find accuracy   
cm_Test = table("actual" = test_Data_std$target, 
                "predicted" = pred_Test)

cm_Test
pred_Test <- as.factor(pred_Test)
confusionMatrix(data = pred_Test, reference = test_Data_std$target,positive ="1")

precision <- 348/(348+69)
recall <- 351/(351+ 189)  # .72

f1_score = 2*precision*recall/(precision+recall) ## 0.71 fscore

#test_data_ff <- test_data_final
test_data_ff <- centralImputation(test_data_ff)
##test_data_ff_1 <- decostand(test_data_ff[,seq(1,64,1)],"standardize",2)
test_Imp_Attr <- top_Imp_Attr[1:50]
test_data_ff <- data.frame(test_data_ff[, test_Imp_Attr])
head(test_data_ff)
prob_Test <- predict(xgb_model_with_params, as.matrix(test_data_ff),type="response", norm.votes=TRUE) 
pred_Test <- ifelse(prob_Test > 0.2 , "1","0")
Summary_output <- data.frame(cbind(t_data,pred_Test ))
Summary_output <- Summary_output[,colnames(Summary_output) %in% c("ID","pred_Test")]
colnames(x=Summary_output) <- c("ID","prediction")
head(Summary_output)
#write to a file
write.csv(x = Summary_output,"C:/Users/papachak/Downloads/CSE7305c_CUTe/XGBoost_1.csv")
##############################using gbm############################
