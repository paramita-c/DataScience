setwd("D:/DataVisualize")
library(tidyverse)
library(dplyr)
library(ggplot2)

iris %>% View()

iris %>% 
  ggplot(aes(x= Sepal.Length, y = Sepal.Width)) + geom_point(size = 4)

iris %>% str()

iris %>% 
  ggplot(aes(x= Sepal.Length, y = Sepal.Width, color = Species)) + geom_point(size = 2)

univariate data visualization in ggplot
continuous - cont, density, rug plot, vline, boxplot, histogram
categorical - geom_call, geom_bar, geom_point (re-order), geom_segment
install.packages("hexbin")
library(caret)
library(tidyverse)
library(gridExtra)
library(ggridges)
library(caret)
library(GGally)
library(hexbin)