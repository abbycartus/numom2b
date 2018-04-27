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
a$sptb37 <- factor(a$sptb37, levels=c(0,1), labels=c("Not preterm", "Preterm"))
a$smokerpre <- factor(a$smokerpre, levels=c(1,2), labels=c("Nonsmoker","Smoker"))
a$agecat3 <- factor(a$agecat3)
a$bmicat <- factor(a$bmicat)
a$white <- factor(a$white)
a$college <- factor(a$college)
a$married <- factor(a$married)

#Labeling variables so they'll look nicer on the tree
a = apply_labels(a,
                 sptb37 = "Preterm birth",
                 smokerpre = "Smoker",
                 agecat3 = "Age group",
                 bmicat = "BMI group",
                 white = "Race",
                 college = "Education",
                 married = "Marital status",
                 heix_tot = "HEI score"
                 )

#Switching columns in the data frame to see if column order matters
b <- a[c("bmi", "agecat3", "smokerpre", "white", "bmicat", "college","married","sptb37", "heix_tot")]

# classification and regression tree
# to use the "Class ~ ." notation, make sure you only have things in the data that you want in teh model
# the outcome here is called "Class" (case matters)
# We have only the variables we want in the model, outcome = sptb37
tree1 <- rpart(sptb37 ~ ., data=a, method="class", control=rpart.control(minsplit=25, minbucket=3, cp=0.001))

#Plotting the tree
plot(tree1, branch=0.1, uniform=TRUE, compress=TRUE)  
text(tree1, use.n=TRUE, all=TRUE, cex=0.9)
rpart.plot(tree1, extra=1)

# measuring "impact"
## suppose we were interested in a 1 unit change in the variable heix_tot
hist(a$heix_tot)
median(a$heix_tot)

# create two diff datasets, each with diff level of heix_tot
a0 <- a; a0$heix_tot <- median(a$heix_tot) - 33.75262
a1 <- a; a1$heix_tot <- median(a$heix_tot)

#creating different datasets for different contrasts: choose your own adventure
a0 <- a; a0$heix_tot <- 79.1 #median of HEI quintile 5 
a1 <- a; a1$heix_tot <- 45.8 #median of HEI quintile 1 

a0 <- a; a0$heix_tot <- median(a$heix_tot)
a1 <- a; a1$heix_tot <- 38.9 #overall split score median(a$heix_tot)

a0 <- a; a0$heix_tot <- 79.1 #median of HEI quintile 5
a1 <- a; a1$heix_tot <- 38.9 #overall split score

# predict from each
y0 <- predict(tree1,newdata = a0)
y1 <- predict(tree1,newdata = a1)

# here's the "impact" of heix_tot on sptb37 expressed on the risk difference scale
mean(y1[,2]) - mean(y0[,2])

# On the Risk Ratio scale
mean(y1[,2]) / mean(y0[,2])

# On the Odds Ratio scale
(mean(y1[,2])/(1 - mean(y1[,2]))) / (mean(y0[,2])/(1 - mean(y0[,2])))

