# Data Visualization
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

summary(data)
ggplot(data, aes(Attr64)) +
  geom_histogram()
ggplot(data, aes(log(Attr64))) +
  geom_histogram()



## plot the missing values for the Attributes as per the target variable
data_missing_values_1 <- as.matrix(colSums(is.na(data[data$target == 1,])))
data_missing_values_1 <- data.frame(data_missing_values_1)

data_missing_values_target_1 <- data.frame(colSums(is.na(data[data$target == 1,])))
data_missing_values_target_1$target <- rep(1,ncol(data))
head(data_missing_values_target_1)

data_missing_values_target_1$Attribute <- rownames(data_missing_values_target_1)
colnames(x = data_missing_values_target_1) <- c("Missing_Values","target", "Attribute")
head(data_missing_values_target_1)

# plot missing values > 100
data_missing_values_target_1 <- data_missing_values_target_1[data_missing_values_target_1$Missing_Values >= 100,]
head(data_missing_values_target_1)

ggplot(data_missing_values_target_1,aes(Attribute,Missing_Values, col = Missing_Values, size = 0.15))+geom_point()+
  scale_color_gradient(low="black",high="red")


data_missing_values_0 <- as.matrix(colSums(is.na(data[data$target == 0,])))
data_missing_values_0 <- data.frame(data_missing_values_0)

data_missing_values_0 <- data.frame(colSums(is.na(data[data$target == 0,])))
data_missing_values_0$target <- rep(1,ncol(data))
head(data_missing_values_0)

data_missing_values_0$Attribute <- rownames(data_missing_values_0)
colnames(x = data_missing_values_0) <- c("Missing_Values","target", "Attribute")
head(data_missing_values_0)

# plot missing values > 100
data_missing_values_0 <- data_missing_values_0[data_missing_values_0$Missing_Values >= 100,]
head(data_missing_values_0)

ggplot(data_missing_values_0,aes(Attribute,Missing_Values, col = Missing_Values, size = 0.15))+geom_point()+
  scale_color_gradient(low="black",high="red")


#normal bar
ggplot(data, aes(target)) + 
  geom_bar()

#normal bar to find out the levels (with target split)
ggplot(data = data, aes(V7, fill = V16))+geom_bar()

#check the distribution of the numeric columns 
## bar and distribution plots
AttrV2<-ggplot(data = data, aes(V2,fill = V16))+geom_density()+facet_grid(~V16)
AttrV6<-ggplot(data = data, aes(V6, fill = V16))+geom_bar()
AttrV14<-ggplot(data = data, aes(V14, fill = V16))+geom_bar()
V8<-ggplot(data = data, aes(V8,V16))+geom_point()
grid.arrange(AttrV2, AttrV6, AttrV14)

# box plot gender working time
ggplot(data, aes(x = V1, y = V14)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

ggplot(data, aes(x = factor(V1), y = V14)) +
  geom_boxplot()

data_impute <- centralImputation(data)

ggplot(data_impute, aes('Company', Attr55)) +
  geom_boxplot(outlier.alpha = .05)  +
  scale_y_log10(
    labels = as.character(breaks),
    breaks = quantile(data_impute$Attr55)
  )


# density plots for continuous variable
ggplot(data, aes(x = V15)) +
  geom_density(alpha = .2, fill = "#FF6666")


ggplot(data, aes(x = V15)) +
  geom_density(aes(color = V16), alpha = 0.5) +
  theme_classic()

ggplot(data, aes(x = V14, color = V7)) +
  geom_freqpoly() +
  scale_x_log10(breaks = c(50, 150, 400, 750) * 1000, labels = scales::dollar)

#non-linearity
ggplot(data, aes(x = V11, y = V8)) +
  geom_point(aes(color = V1),
             size = 0.5) +
  stat_smooth(method = 'lm',
              formula = y~poly(x, 2),
              se = TRUE,
              aes(color = V1)) +
  theme_classic()

#correlation
ggcorr(data[,names(data) %in% num_attr],
       method = c("pairwise", "spearman"),
       nbreaks = 8,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

## check the correlation in chunks
res <- cor(data[,2:30], use = "complete.obs")

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res_1 <- cor(data[,31:65], use = "complete.obs")

corrplot(res_1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res_2 <- cor(data[,15:45], use = "complete.obs")

corrplot(res_2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

## check average V15(numeric) grouped by V4(categorical)
data %>%
  group_by(V4) %>%
  summarize(avg = mean(V15),
            count = n()) %>%
  arrange(avg)

## scatter plot - Bivariate relationship
ggplot(data, aes(x = V14, y = V2)) +
  geom_point(alpha = .3)


ggplot(data, aes(x = V8, y = V2)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed") +
  geom_smooth(se = FALSE, lty = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
   ggtitle("log-transformed variables")
