#DAN: Overall, great job, the code is good and looks right. I appreciated your write up.
#Just one mistake, I believe you selected the *worst* classification model instead of the best!
#Accuracy should be maximized. Grade: S+ 

#DAN: Nice header, nice well-organized code. 

#Caret Regression assignment for machine learning
#Joanna Corimanya

### predict wine quality (from 1-10) from continuous physiochemical and sensory predictors ###
### Data citation: 
#P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
#Modeling wine preferences by data mining from physicochemical properties.
#In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.

#Available at: [@Elsevier] http://dx.doi.org/10.1016/j.dss.2009.05.016
#[Pre-press (pdf)] http://www3.dsi.uminho.pt/pcortez/winequality09.pdf
#[bib] http://www3.dsi.uminho.pt/pcortez/dss09.bib

#set seed for reproducibility
set.seed(123)

#load packages
#library(plyr) # for relaxed lasso
library(dplyr) #for data organization
library(corrplot) #for plotting correlation matrix

#set working directory
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/UnitCARET")

#read in data
wine<- read.csv("winequality-red.csv",header = TRUE)

#inspect data
head(wine)
dim(wine)

#make a histogram of each numeric variable
colnames <- colnames(wine)
for (i in colnames) {
  ##Check if the column is numeric
  if (is.numeric(wine[[i]])) {
    #create a histogram for the numeric column
    hist(wine[[i]], main = paste("Histogram of", i), xlab = i)
  }
}

#Check for correlations
numeric_data <- wine[, sapply(wine, is.numeric)]
correlation_matrix <- cor(numeric_data)

##Plot the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45,
         type = "upper")
#no evidence of autocorreltation
#shuffle the data and split into training and validation sets
w_split <- wine %>%
  sample_frac(size = 1) %>%  #shuffle the entire dataset
  mutate(row_number = row_number()) %>%  #create a row number column for indexing
  mutate(split = ifelse(row_number <= floor(0.75 * n()), "train", "vault")) %>%  #assign 'train' or 'vault'
  select(-row_number)  #drop the row number column

#create the training and validation datasets
w_train <- w_split %>% filter(split == "train") %>% select(-split)
w_vault <- w_split %>% filter(split == "vault") %>% select(-split)
y_train <- w_train[,12] # y value, response
x_train <- w_train[,1:11] #predictor variables

#load more packages that cause bugs if I load them earlier
library(caret) #for modelling
library(doParallel) #parallel processing
library(brnn) #for Bayesian Regularized Neural Networks
library(elasticnet) #for Elasticnet
library(fastICA) #for Independent Component Regression
library(leaps) #for Linear Regression with Stepwise Selection
library(monomvn) #for Bayesian Ridge Regression

#speed up run time by doing parallel processing

cl <- makePSOCKcluster(3) #the argument is basically the number of cores/processes to use
registerDoParallel(cl)

#k-fold cross validation through caret
cont <- trainControl(method = "repeatedcv", number = 10, repeats = 15)
mods <- c("brnn",#Bayesian Regularized Neural Networks, 1 tuning parameter
          "enet",#Elasticnet, 2 tuning parameters
          "icr",#Independent Component Regression, 1 tuning parameter
          "leapSeq", #Linear Regression with Stepwise Selection, 1 tuning parameter
          "bridge" #Bayesian Ridge Regression, No tuning parameters for this model
          )

#look up all the models
for (i in 1:length(mods)){
  print(modelLookup(mods[i]))
}

#tune length based on number of tuning parameters
tune.l <- c(5, #brnn
            2, #enet
            1, #icr
            1, #leapSeq
            1) #bridge

results <- list() # a list in which to store results

#dataframe to save best results
models <- data.frame(model = mods, 
                     RMSE = NA, 
                     Rsquared = NA, 
                     RMSESD = NA, 
                     RsquaredSD = NA)

#run models
for (i in 1:length(mods)){
  #train models with caret and get error by k-fold by cross validation
  m<-train(x = x_train, y = y_train, method = mods[i], preProcess = c("center","scale"),
           tuneLength = tune.l[i], trControl = cont,metric = "RMSE")
  #store model results
  results[[mods[i]]] <- m
  #extract and store best results
  best <- m$results[which.min(m$results$RMSE),] 
  #save best results in the models dataframe
  models$RMSE[i] <- best$RMSE
  models$Rsquared[i] <- best$Rsquared
  models$RMSESD[i] <- best$RMSESD
  models$RsquaredSD[i] <- best$RsquaredSD
}
print(models)
#put core use back to normal settings
stopCluster(cl)
write.csv(models, "reg_mods.csv")
