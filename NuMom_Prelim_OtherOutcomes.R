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
setwd("/Users/abigailcartus/Box/numom2b diet and machine learning/Data")
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
a$pree_acog <- factor(a$pree_acog, levels=c(0,1), labels=c("No", "Yes"))


#Trees for: gdm, preeclampsia (both versions), sga with HEI score or food componentns
#For outcome gestage, change method to anova
tree1 <- rpart(pree_acog ~ smokerpre + agecat3 + bmicat + black + insurpub + college + married + v_totdens + f_totdens + d_totdens + dt_protdens + g_whldens + g_nwhldens, data=a, method="class", control=rpart.control(minsplit=30, minbucket=10, cp=0.001))

#Plotting tree
rpart.plot(tree1, extra=1)


# measuring "impact"

# median of quintiles of each variable
a$fq <- ntile(a$f_totdens, 5)  #creating quintile
aggregate(a$f_totdens, list(a$fq), median) #getting the median of each quintile (I think!)

#creating different datasets for different contrasts: choose your own adventure
a0 <- a; a0$f_totdens <- 1.5442625 #median of quintile 5   
a1 <- a; a1$f_totdens <- 0.2765270 #median of quintile 1

# predict from each
y0 <- predict(tree1,newdata = a0)
y1 <- predict(tree1,newdata = a1)

# here's the "impact" of heix_tot on sptb37 expressed on the risk difference scale
mean(y1[,2]) - mean(y0[,2])

# On the Risk Ratio scale
mean(y1[,2]) / mean(y0[,2])

# On the Odds Ratio scale
(mean(y1[,2])/(1 - mean(y1[,2]))) / (mean(y0[,2])/(1 - mean(y0[,2])))

