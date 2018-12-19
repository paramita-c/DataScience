## set the path
setwd("D:/INSOFE/Lab Machine Learning")
## remove all variables
rm(list = ls(all.names = T))

data <- read.csv("cmc.csv", header = F)
summary(data)
str(data)
table(factor(data$V10))
head(data)
tail(data)
num_attr <- c("V1")
cat_attr <- setdiff(colnames(data[,!names(data) %in% c("V10")]), num_attr)
#convert the categorical variables to factors
data[,names(data) %in% cat_attr] <- lapply(data[,names(data) %in% cat_attr] , factor)
str(data)
head(data)
tail(data)
data$V10 <- as.factor(data$V10)
# check missing values
sum(is.na(data))
ggplot(data = data, aes(V4, fill = V10))+geom_bar()

## bin it to 0, 1-4, 5-8, 9-16
data <- data %>%
  mutate(V4_New = factor(ifelse(V4=="0",0,ifelse(V4=="1"|V4=="2"|V4=="3"|V4=="4","1",
                                                 ifelse(V4=="5"|V4=="6"|V4=="7"|V4=="8","2","3")))))


#data <- data %>%
#  mutate(target=factor(ifelse(V10=="1" | V10=="3","1","2")))

data$V1 <- NULL
#data$V10 <- NULL    
head(data)
data$V4 <- NULL
str(data)
summary(data)
set.seed(786)

train_rows =  createDataPartition(data$V10, p = 0.7, list = F)
train_data = data[train_rows,]
test_data = data[-train_rows,]

table(data$V10)
table(train_data$V10)
table(test_data$V10)

####################### use XGboost ####################
dummy_obj <- dummyVars( ~ . , train_data[, !(names(train_data) %in% c("V10"))])
train_dummy_data <- as.data.frame(predict(dummy_obj, train_data))
test_dummy_data <- as.data.frame(predict(dummy_obj, test_data))

train_dummy_data$V10 <- as.factor(train_data$V10)
test_dummy_data$V10 <- as.factor(test_data$V10)

train_matrix <- xgb.DMatrix(data = as.matrix(train_dummy_data[, !(names(train_dummy_data) %in% c("V10"))]), 
                            label = as.matrix(train_dummy_data[, names(train_dummy_data) %in% "V10"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_dummy_data[, !(names(test_dummy_data) %in% c("V10"))]), 
                           label = as.matrix(test_dummy_data[, names(test_dummy_data) %in% "V10"]))

numberOfClasses <- length(unique(train_dummy_data$V10)) 
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses+1)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = as.numeric(train_dummy_data$V10) + 1)

head(OOF_prediction)

# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)
# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = as.numeric(test_dummy_data$V10) + 1,
         max_prob = max.col(., "last"))

# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

######################## use random forest #########################
set.seed(1234)

# Build the classification model using randomForest
model_rf = randomForest(V10 ~ ., data=train_data, 
                        keep.forest=TRUE, ntree=200) 
# Print and understand the model
print(model_rf)
## Predict on test data
pred_test_rf <- predict(model_rf, test_data[,setdiff(names(test_data),
                                                     "V10")],
                        type = "response" ,
                        norm.votes=TRUE)

# Build confusion matrix and find accuracy  
confusionMatrix(data=pred_test_rf,reference = test_data$V10, positive ="1")
# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model_rf$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
# plot (directly prints the important attributes) 
varImpPlot(model_rf)

## build model using top 20 attributes
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:5])

set.seed(123456)

# Build the classification model using randomForest
model_rf_imp = randomForest(V10~.,
                            data=train_data[,c(top_Imp_Attr,"V10")], 
                            keep.forest=TRUE,ntree=200) 
# Predicton Test Data
pred_test_rf_imp = predict(model_rf_imp, test_data[,top_Imp_Attr],
                           type="response", norm.votes=TRUE)

confusionMatrix(data=pred_test_rf_imp,reference = test_data$V10, positive ="1")
########################### use caret's multinom ##########################
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
grid_params <- data.frame(decay = c(0.01, 0.5, 0.05))
model_multinom_cv <- train(V10 ~ ., train_data, method = "multinom", 
                      #preProcess = c("center", "scale", "pca"),
                      trControl = ctrl, 
                      tuneGrid = grid_params)
pred_test_multinom_cv <- predict(model_multinom_cv, test_data)

confusionMatrix(as.factor(pred_test_multinom_cv), as.factor(test_data$V10))

## lda, gda, gaussian() -- family multinomial- lasso, knn, ada