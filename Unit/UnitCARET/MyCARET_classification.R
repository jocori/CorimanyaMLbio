### CARET assignment

rm(list=ls())
graphics.off()

#set seed for reproducibility
set.seed(123)
#set working directory
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/UnitCARET")
#these are all packages from models I tried.......didn't use all of them
library(caret) #for modelling
library(corrplot) #for plotting correlation matrix
library(bnclassify)
library(ada)
#library(plyr)
library(xgboost)
library(dplyr)
library(rpart)
library(bnclassify)
library(sparseLDA)
library(HiDimDA)
library(VGAM)
library(randomForest)
library(rocc)
library(C50)
library(glmnet)
library(Matrix)
#read in frog data
frog<-read.csv("Frogs_MFCCs.csv")

#inspect data
head(frog)
dim(frog)
##ensure response is a factor
frog$Species <- as.factor(frog$Species)
#make a histogram of each numeric variable
colnames <- colnames(frog)
for (i in colnames) {
  ##Check if the column is numeric
  if (is.numeric(frog[[i]])) {
    #create a histogram for the numeric column
    hist(frog[[i]], main = paste("Histogram of", i), xlab = i)
  }
}

#Check for correlations
numeric_data <- frog[, sapply(frog, is.numeric)]
correlation_matrix <- cor(numeric_data)

##Plot the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45,
         type = "upper")
#.3 v .5, .7 v .9, .9 v 11, 11 v 13, 15 v 17
#remove highly correlated predictor variables and recordID column
frog<-frog[, !(names(frog) %in% c("MFCCs_.5","MFCCs_.9","MFCCs_11","MFCCs_17","RecordID","Family","Genus"))]

#shuffle the data and split into training and validation sets
f_split <- frog %>%
  sample_frac(size = 1) %>%  #shuffle the entire dataset
  mutate(row_number = row_number()) %>%  #create a row number column for indexing
  mutate(split = ifelse(row_number <= floor(0.75 * n()), "train", "vault")) %>%  #assign 'train' or 'vault'
  select(-row_number)  #drop the row number column

#create the training and validation datasets
f_train <- f_split %>% filter(split == "train") %>% select(-split)
f_vault <- f_split %>% filter(split == "vault") %>% select(-split)
y_train <- f_train[,19] # y value, response
x_train <- f_train[,1:18] #predictor variables

#speed up run time by doing parallel processing
library(doParallel)
cl <- makePSOCKcluster(3) #the argument is basically the number of cores/processes to use
registerDoParallel(cl)


#k-fold cross validation through caret
cont <- trainControl(method = "repeatedcv", number = 10, repeats = 5)


#find out tuning parameters, whether model is for classification or regression, and if it's classification, it's probabilities
mods <- c("rocc", ### ROC Curves, one tuning parameter
          "RFlda", ### Factor-Based Linear Discriminant Analysis, 1 tuning parameter
          "glmnet",### glmnet, 2 tuning parameters
          "C5.0Rules",### Single C5.0 Ruleset, no tuning parameters
          "rpart", ###CART, 1 tuning parameter
          "rf") ### Random Forest, 1 tuning parameter
#look up all the models
for (i in 1:length(mods)){
  print(modelLookup(mods[i]))
}

#set tune length for each model based on number of tuning parameters
tune.l <- c(1, #rocc
            5, #RFlda
            2, #glmnet
            1, #C5.0Rules
            1, #rpart
            1)#rf
            
results <- list() # a list in which to store results
models<-data.frame(model = mods, 
                   Accuracy = NA*numeric(length(mods)),
                   Kappa=NA*numeric(length(mods)),
                   AccuracySD=NA*numeric(length(mods)),
                   KappaSD=NA*numeric(length(mods)))
for (i in 1:length(mods)){
  m <- train(
    x = x_train, 
    y = y_train, 
    method = mods[i], 
    preProcess = c("center", "scale"), 
    tuneLength = tune.l[i], 
    trControl = cont
  )
  #store model results
  results[[mods[i]]] <- m
  #extract and store best results
  best <- m$results[which.max(m$results$Accuracy),]
  #save best results in the models dataframe
  models$Accuracy[i] <- best$Accuracy
  models$Kappa[i] <- best$Kappa
  models$AccuracySD[i] <- best$AccuracySD
  models$KappaSD[i] <- best$KappaSD
}
models
#RFLDA is best...increasing from tune length of 1 to 5
#RFlda accuracy did not change at all
stopCluster(cl)
write.csv(models, "class_mods.csv")
