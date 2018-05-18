#Install and load packages
packages <- c("tidyverse","tabplot","VIM","rpart","rpart.plot","expss","here","boot")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
}

for (package in packages) {
  library(package, character.only=T)
}

## import data here.
a<- read.csv("./numom_small HEI total score.csv", header=TRUE, sep=",")

head(a)

# need to create a dataset that has only the stuff we want
# Removing studyid, sptb37_alt, and the income variable (pctfedpov)
a <- subset(a,select=-c(studyid,sptb37_alt,pctfedpov,bmicat))

# general graphical overview of data
tableplot(a)

# visualization of missing
aggr(a)
nrow(a)

#Low proportion missing; using only complete cases (?)
a <- a[complete.cases(a), ]
nrow(a)

#Converting integer variables to factors
a$sptb37 <- factor(a$sptb37, levels=c(0,1), labels=c("Not preterm", "Preterm"))
a$smokerpre <- factor(a$smokerpre, levels=c(1,2), labels=c("Smoker","Nonsmoker"))
a$agecat3 <- factor(a$agecat3)
#a$bmicat <- factor(a$bmicat)
a$white <- factor(a$white)
a$college <- factor(a$college)
a$married <- factor(a$married)

#Labeling variables so they'll look nicer on the tree
a = apply_labels(a,
                 sptb37 = "Preterm birth",
                 smokerpre = "Smoker",
                 agecat3 = "Age group",
                 #bmicat = "BMI group",
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

names(a)
ggplot(a) + geom_histogram(aes(x=heix_tot))
ggplot(a) + geom_histogram(aes(x=log(bmi)))
ggplot(a) + geom_point(aes(x=bmi,y=heix_tot))

table(a$smokerpre)
table(a$agecat3)
table(a$sptb37)
table(a$white)
table(a$college)
table(a$married)

tree1 <- rpart(sptb37 ~ ., data=a, method="class", control=rpart.control(minsplit=5, minbucket=3, cp=0.001))

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

pFunc <- function(data,indices,outcome){
  
  varname <- enquo(outcome)
  tree1_call <- expr(rpart(!!varname ~ ., data=data[indices], method="class"),control=rpart.control(minsplit=25, minbucket=3, cp=0.001))
  tree1 <- eval_bare(tree1_call)
  
  suppressMessages(
  medians <- data[indices,] %>% 
    group_by(quantiles=ntile(heix_tot,5)) %>% 
    mutate(medians=median(heix_tot)) %>% 
    arrange(quantiles) %>%
    slice(1) %>%
    select(medians)
  )
  
  #print(medians)
  
  a0 <- data[indices,]
  a0 <- a0 %>% mutate(heix_tot=as.numeric(medians[1,2]))
  a1 <- data[indices,]
  a1 <- a1 %>% mutate(heix_tot=as.numeric(medians[5,2]))
  
  y0 <- predict(tree1,newdata = a0)
  y1 <- predict(tree1,newdata = a1)
  
  # here's the "impact" of heix_tot on sptb37 expressed on the risk difference scale
  RD<-mean(y1[,2]) - mean(y0[,2])
  
  # On the Risk Ratio scale
  RR<-mean(y1[,2]) / mean(y0[,2])
  
  # On the Odds Ratio scale
  OR<-(mean(y1[,2])/(1 - mean(y1[,2]))) / (mean(y0[,2])/(1 - mean(y0[,2])))
  
  results<-cbind(RD,RR,OR)
  return(results)
  
}

pFunc(a,1:nrow(a),quo(sptb37))

# bootstrapping
boot_res <- boot(data=a,statistic=pFunc,R=1000,outcome=sptb37)

## SE of RD, RR, OR
boot_res$t[,2] <- log(boot_res$t[,2])
boot_res$t[,3] <- log(boot_res$t[,3])
apply(boot_res$t,2,sd)

