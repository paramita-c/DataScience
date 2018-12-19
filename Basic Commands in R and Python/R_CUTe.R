#remove all variables
rm(list=ls(all.names = TRUE))

#Question 1
my_vect <- seq(1000,10000)
my_vect[my_vect %% 7==0 & my_vect %% 19==0]

#Question 2
my_func <- function(n){
  i <- 1 
  a <- 1
  b <- 1
  c <- 1
  while (i<n-1){
    c <- a+b
    a <- b
    b <- c

    i <- i + 1
    
  }
  print(c)
  
}
my_func(3)

setwd("C:/CUTe_21st April/20180421_Batch42_CSE7212c_CUTe/data")
#Question 3
COL1 <- read.csv("COL1.csv")
head(COL1)
COL2 <- read.csv("COL2.csv")
head(COL2)
COL3 <- read.csv("COL3.csv")
head(COL3)
COL4 <- read.csv("COL4.csv")
head(COL4)
COL5 <- read.csv("COL5.csv")
head(COL5)

#Question 4a,b
summary(COL1)
#dominance of one Extra Curr Actvity over the other
table(factor(COL1$Extra.Curr)) #Drama takes domiance
#dominance of one Extra Curr Actvity over the other
summary(COL2)
table(factor(COL2$Extra.Curr))#writing takes dominance
#dominance of one Extra Curr Actvity over the other
summary(COL3)
table(factor(COL3$Extra.Curr))
#dominance of one Extra Curr Actvity over the other
summary(COL4)
table(factor(COL4$Extra.Curr))
#dominance of one Extra Curr Actvity over the other
summary(COL5)
table(factor(COL5$Extra.Curr))

#Question 3c
#Count of Null values
colSums(is.na(COL1)) #Acad_Score and Behavior_type
#Count of Null values
colSums(is.na(COL2))
#Count of Null values
colSums(is.na(COL3))
#Count of Null values
colSums(is.na(COL4))
#Count of Null values
colSums(is.na(COL5))

#Question 3d
#attributes missing in each file
sum(is.na(COL1))
sum(is.na(COL2))
sum(is.na(COL3))
sum(is.na(COL4))
sum(is.na(COL5))

#Question 5
#Imputing missing values njumeric is replaced by median and categorical by mode
library(DMwR)
COL1_Imputed <- centralImputation(COL1)
COL2_Imputed <- centralImputation(COL2)
COL3_Imputed <- centralImputation(COL3)
COL4_Imputed <- centralImputation(COL4)
COL5_Imputed <- centralImputation(COL5)

#Quesstion 6
#combine all the data frames
consolidated_data <- data.frame(rbind(COL1_Imputed,COL2_Imputed,COL3_Imputed,COL4_Imputed,COL5_Imputed))

#Question 7
#range normalization for numeric variables
Data_Numeric <- consolidated_data[c('Acad_Score','Overall_Score')]
library(vegan)
Data_Numeric_1 <- decostand(Data_Numeric,"range",2)
consolidated_data <- consolidated_data[-c(3,6)]
final_data <- data.frame(cbind(consolidated_data,Data_Numeric_1))
head(final_data)