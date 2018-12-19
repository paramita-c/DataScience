## set the path
setwd("D:/INSOFE/Lab Machine Learning")
## remove all variables
rm(list = ls(all.names = T))

## import the libraries
library(Boruta)
library(permute)
library(vegan)
library(infotheo)
library(e1071)
library(corrplot)
library(Hmisc)
library(tidyverse)
library(gridExtra)
library(ggridges)
library(GGally)
library(hexbin)
library(ggplot2)
library(unbalanced)
library(DMwR)
library(vegan)
library(randomForest)
library(caret)
library(ROCR)

data <- read.csv("crx.csv", header = F, na.strings = c("?"),stringsAsFactors = F)
head(data)
tail(data)
summary(data)
str(data)

## drop the columns having no variance (if any)
#data$Over18 <- NULL
#data$StandardHours <- NULL

#check missing values and remove columns having > 10% of missing values
colSums(is.na(data))
# 2 columns have more than 10% of missing values
sum(colSums(is.na(data)) > 0.1 * nrow(data))
## 2 features which have more than 10% missing values, will drop those features
data_f <- data[, colSums(is.na(data)) <= 0.1 * nrow(data)]
data_new <- data_f
rm(data_f)

head(data)
tail(data)
summary(data)
str(data)
# seggregate the categorical and numerical attributes
cat_attr <- c("V1","V4","V5","V6","V7","V9","V10","V12","V13")
num_attr <- setdiff(colnames(data[,!names(data) %in% c("V16")]),cat_attr)
#data[,names(data) %in% cat_attr] <- data.frame(apply(data[,names(data) %in% cat_attr] ,2,FUN = function (x) {as.character(x)}))

# check unique values for a columns
table(factor(data$V10))
#check for NA values
colSums(is.na(data))

#put a value of "Unavailable" where the column value is NA for a character attribute
for (i in cat_attr){
  if (sum(is.na(data[,names(data) %in% i])) > 0){
    for (j in seq(1,nrow(data),1)){
      if (is.na(data[j,names(data) %in% i])) {
       
        data[j,names(data) %in% i] <- "Unavailable"
        #print(data[j,names(data) %in% i])
      }
      
    }
    
  }
  
}

#convert the categorical variables to factors
data[,names(data) %in% cat_attr] <- lapply(data[,names(data) %in% cat_attr] , factor)
#verify if the data frame is proper
summary(data)
str(data)

# Change categorical variable V7 10 levels to 6 distinct values, club the ones with less rows
data <- data %>%
  mutate(V7_New = factor(ifelse(V7 == "dd" 
                  | V7 == "j" | V7 =="n" | V7 == "o" | V7 == "z", "less"
                  , ifelse(V7 == "bb","bb",ifelse(V7 == "ff","ff", ifelse(V7 == "h", "h",
                                                                          ifelse(V7 == "v","v","Unavailable")))))))


data$V7 <- NULL

str(data)
#check for NA values
colSums(is.na(data))

#add a column which stores the NA values for each row
data$Sum_NAs <- rowSums(is.na(data))

## imputing the missing values with 1-hot encoding, which is a new synthetic feature is devised 
## (for each feature having missing values) where the feature value is 1 if the value is missing
## else it's 0
nCol_Counter <- as.numeric(length(colnames(data[, !names(data) %in% c("V16")])))

i <- 1
Col_Counter <- ncol(data) + 1
colname = "Col_"

for (i in seq(1,nCol_Counter,1)) {
  
  if (sum(is.na(data[,i])) > 0 ){
    new_COl_v <- ifelse(is.na(data[,i]) == TRUE, 1, 0)
    data[gsub(" ","",paste(colname, Col_Counter))] <- new_COl_v
    new_COl_v <- NULL
    Col_Counter <- Col_Counter + 1
  }
  
  i <- i + 1
}

rm(Col_Counter,colname,i,new_COl_v, nCol_Counter)
head(data)
colnames(data)
summary(data)
str(data)

##binning 
library(vegan)
library(infotheo)

# Using Equal Frequency Convert numeric attributes into categorical. 
num_2_Cat_Data = data.frame(sapply(data[,num_attr],  
                                   function(x){discretize(x, disc = "equalfreq", 
                                                          nbins = 4)}))
names(num_2_Cat_Data) = num_attr
num_2_Cat_Data = data.frame(sapply(num_2_Cat_Data, as.factor))

head(num_2_Cat_Data)
data = cbind(num_2_Cat_Data, data[,names(data) %in% cat_attr])
rm(num_2_Cat_Data)
head(data)
## standardizing (if required) use range or normalize
data_new <- decostand(x = data[,num_attr],method = "range",MARGIN = 2)
head(data_new)
## remove rows with more NA columns 