#Install and load packages
packages <- c("tidyverse","tabplot","VIM","rpart","rpart.plot")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
}

for (package in packages) {
  library(package, character.only=T)
}

install.packages("rpart.plot")
install.packages("expss")

library("expss")
library("tidyverse")
library("tabplot")
library("VIM")
library("rpart")
library("rpart.plot")

## import data here.
setwd("/Users/abigailcartus/Box/numom2b diet and machine learning")
getwd()
a<- read.csv("numom_small_everything we need.csv", header=TRUE, sep=",")

#Don't need to remove anything, just specify what we want when modeling

#Still using complete cases
a <- a[complete.cases(a), ]

#Converting integer variables to factors
a$sptb37 <- factor(a$sptb37, levels=c(0,1), labels=c("Not preterm", "Preterm"))
a$smokerpre <- factor(a$smokerpre, levels=c(1,2), labels=c("Nonsmoker","Smoker"))
a$agecat3 <- factor(a$agecat3)
a$bmicat <- factor(a$bmicat)
a$black <- factor(a$black)
a$college <- factor(a$college)
a$married <- factor(a$married)

#Trees for: gdm, preeclampsia (both versions), sga with total healthy eating index score
tree1 <- rpart(gdm ~ smokerpre + agecat3 + bmicat + black + college + married + heix_tot, data=a, method="class", control=rpart.control(minsplit=10, minbucket=3, cp=0.001))

#Plotting tree
rpart.plot(tree1, extra=1)
