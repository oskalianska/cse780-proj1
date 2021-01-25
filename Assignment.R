rm(list=ls())

#loading libraries
library(arules)
library(DataExplorer)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr) 
library(glimpse)

#read data
data <- read.csv("/Users/olena.skalianska/Desktop/McMaster/CSE 780/week 1/data.csv", sep = ",")
head(data)

#check for any missing values 
plot_missing(data)

#mutate function is from dplyr package
data %>% mutate(W_edu = as.factor(W_edu))
data %>% mutate(H_edu = as.factor(H_edu))
data %>% mutate(W_relig = as.factor(W_relig))
data %>% mutate(W_work = as.factor(W_work))
data %>% mutate(L_index = as.factor(L_index))
data %>% mutate(Media = as.factor(Media))
data %>% mutate(Contraceptive = as.factor(Contraceptive))

#convert and edit variances into numeric
W_age <- as.numeric(as.character(data$W_age))
Num_of_child <- as.numeric(as.character(data$Num_of_child))

#bind new columns into dataframe
cbind(data,W_age)
cbind(data,Num_of_child)

#get a glimpse of data
glimpse(data)

#store data into a .csv 
write.csv(data,"/Users/olena.skalianska/Desktop/McMaster/CSE 780/week 1/data_new.csv", quote = FALSE, row.names = FALSE)

#load transaction data into an object of the transaction class
trans <- read.transactions('/Users/olena.skalianska/Desktop/McMaster/CSE 780/week 1/data_new.csv', format = 'basket', sep=',')
inspect(trans)

summary(trans)
itemLabels(trans)

#set parameters 
params<-list(support=0.005,confidence=0.8,minlen=2,maxlen=6)

#mine the rules using the APRIORI algorithm
fit1<-apriori(trans,parameter=params)
summary(fit1)
inspect(sort(fit1[1:10], by="lift"))

#suppose we are interested in rules with consequent Contraceptive (Long-term use)
app<-list(default="lhs",rhs="Long-term")
fit2<-apriori(trans,parameter=params,appearance=app)
inspect(sort(fit2[1:10], by="lift"))

#suppose we are interested in rules with consequent Contraceptive (Short-term use)
app<-list(default="lhs",rhs="Short-term")
fit3<-apriori(trans,parameter=params,appearance=app)
inspect(sort(fit3[1:10], by="lift"))

#suppose we are interested in rules with consequent Contraceptive (No-use)
app<-list(default="lhs",rhs="No-use")
fit4<-apriori(trans,parameter=params,appearance=app)
inspect(sort(fit4[1:10], by="lift"))

#suppose we are interested in rules with antecedent Standard-of-living (High)
app <- list(default="rhs",lhs="High standard-of-living")
fit5<-apriori(trans,parameter=params,appearance=app)
inspect(sort(fit5, by="lift"))

plot(fit3)

plot(fit3, method="grouped")

plot(fit3, method="graph")

plot(fit3,method="matrix3D")


