# plot the histograms of some numeric attributes
setwd("C:/Users/papachak/Downloads/CSE7305c_CUTe")
rm(list = ls(all.names = T))

library(tidyverse)
library(gridExtra)
library(ggridges)
library(caret)
library(GGally)
library(hexbin)
# Attr5, Attr15, Attr17, Attr20, Attr30, Attr32, Attr36, Attr37, Attr40, Attr44, Attr45, Attr47
# Attr50, Attr53, Attr56, Attr58, Attr59, Attr60, Attr63, Attr64

## evenly
## Attr55, Attr62
data <- read.csv("train.csv")
table(data$target)
1765/34788 * 100
summary(data)
ggplot(data, aes(Attr64)) +
  geom_histogram()
ggplot(data, aes(log(Attr64))) +
  geom_histogram()


data_impute <- centralImputation(data)

ggplot(data_impute, aes('Company', Attr55)) +
  geom_boxplot(outlier.alpha = .05)  +
  scale_y_log10(
    labels = as.character(breaks),
    breaks = quantile(data_impute$Attr55)
  )
  

data_missing_values_1 <- as.matrix(colSums(is.na(data[data$target == 1,])))
data_missing_values_1 <- data.frame(data_missing_values_1)

data_missing_values_target_1 <- data.frame(colSums(is.na(data[data$target == 1,])))
data_missing_values_target_1$target <- rep(1,66)
head(data_missing_values_target_1)


data_missing_values_target_1$Attribute <- rownames(data_missing_values_target_1)
colnames(x = data_missing_values_target_1) <- c("Missing_Values","target", "Attribute")
head(data_missing_values_target_1)

data_missing_values_target_1 <- data_missing_values_target_1[data_missing_values_target_1$Missing_Values >= 100,]
head(data_missing_values_target_1)

ggplot(data_missing_values_target_1,aes(Attribute,Missing_Values, col = Missing_Values, size = 0.15))+geom_point()+
  scale_color_gradient(low="black",high="red")


data_missing_values_0 <- as.matrix(colSums(is.na(data[data$target == 0,])))
data_missing_values_0 <- data.frame(data_missing_values_0)

data_missing_values_0 <- data.frame(colSums(is.na(data[data$target == 0,])))
data_missing_values_0$target <- rep(1,66)
head(data_missing_values_0)

data_missing_values_0$Attribute <- rownames(data_missing_values_0)
colnames(x = data_missing_values_0) <- c("Missing_Values","target", "Attribute")
head(data_missing_values_0)

data_missing_values_0 <- data_missing_values_0[data_missing_values_0$Missing_Values >= 100,]
head(data_missing_values_0)

ggplot(data_missing_values_0,aes(Attribute,Missing_Values, col = Missing_Values, size = 0.15))+geom_point()+
  scale_color_gradient(low="black",high="red")

## check the correlation
res <- cor(data[,2:30], use = "complete.obs")

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res_1 <- cor(data[,31:65], use = "complete.obs")

corrplot(res_1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res_2 <- cor(data[,15:45], use = "complete.obs")

corrplot(res_2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


ggplot(data, aes(target)) + 
  geom_bar()

