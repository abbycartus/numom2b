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

library("tidyverse")
library("tabplot")
library("VIM")
library("rpart")
library("rpart.plot")

## import data here.
setwd("/Users/abigailcartus/Box/numom2b diet and machine learning")
getwd()
a<- read.csv("numom_small HEI total score.csv", header=TRUE, sep=",")

# need to create a dataset that has only the stuff we want
# Removing studyid, sptb37_alt, and the income variable (pctfedpov)
a <- a[c(-1,-4,-6)]

# general graphical overview of data
tableplot(a)

# visualization of missing
aggr(a)

#Low proportion missing; using only complete cases (?)
a <- a[complete.cases(a), ]

#Converting integer variables to factors
a$sptb37 <- as.factor(a$sptb37)
a$smokerpre <- as.factor(a$smoker)
a$agecat3 <- as.factor(a$agecat3)
a$bmicat <- as.factor(a$bmicat)
a$white <- as.factor(a$white)
a$college <- as.factor(a$college)
a$married <- as.factor(a$married)

# classification and regression tree
# to use the "Class ~ ." notation, make sure you only have things in the data that you want in teh model
# the outcome here is called "Class" (case matters)
# We have only the variables we want in the model, outcome = sptb37
tree1 <- rpart(sptb37 ~ ., data=a, method="class", control=rpart.control(minsplit=10, minbucket=3, cp=0.001))

# here is a crude plot of the tree
## we can make this nicer for publication, if desired
plot(tree1)   # "Error in plot.rpart(tree1) : fit is not a tree, just a root"
text(tree1)


# measuring "impact"
## suppose we were interested in a 1 unit change in the variable heix_tot
hist(a$heix_tot)
median(a$heix_tot)

# create two diff datasets, each with diff level of heix_tot
a0 <- a; a0$heix_tot <- median(a$heix_tot) - 33.75262
a1 <- a; a1$heix_tot <- median(a$heix_tot)

# predict from each
y0 <- predict(tree1,newdata = a0)
y1 <- predict(tree1,newdata = a1)

# here's the "impact" of heix_tot on sptb37 expressed on the risk difference scale
mean(y1[,2]) - mean(y0[,2])

# On the Risk Ratio scale
mean(y1[,2]) / mean(y0[,2])

# On the Odds Ratio scale
(mean(y1[,2])/(1 - mean(y1[,2]))) / (mean(y0[,2])/(1 - mean(y0[,2])))

