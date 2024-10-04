## Set working directory to class folder
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_CART")
#set seed for reproducibility
set.seed(123)
##Load packages
library(corrplot) #for plotting correlation matrix
library(rpart) #to fit CARTs
library(rpart.plot) #for plotting classification trees
library(ipred) #bagging
library(randomForest) #random forests
library(adabag) #adaptive boosting
library(xgboost) #gradient boosting

## Import yeast data for assignment
yeast<-read.csv("yeast.data.csv")

##### DATA EXPLORATION ####
##ensure response is a factor
yeast$localization_site <- as.factor(yeast$localization_site)
#inspect data
head(yeast)
dim(yeast)
#make a histogram of each numeric variable
colnames <- colnames(yeast)
for (i in colnames) {
  ##Check if the column is numeric
  if (is.numeric(yeast[[i]])) {
    #create a histogram for the numeric column
    hist(yeast[[i]], main = paste("Histogram of", i), xlab = i)
  }
}

## Examine variables for multicollinearity using correlation matrix
numeric_data <- yeast[, sapply(yeast, is.numeric)]
correlation_matrix <- cor(numeric_data)

##Plot the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45,
         type = "upper")

##### DATA PREPARATION #####
## All variables are quite different! Only need to remove the ID column
yeast<-yeast[,-1]

##Split data into testing and training data
y_perm <- yeast[sample(dim(yeast)[1],dim(yeast)[1], replace = FALSE),]
y_train <- y_perm[1:floor(0.75*(dim(yeast)[1])),]
y_vault <- y_perm[(floor(0.75*(dim(yeast)[1]))+1):(dim(yeast)[1]),]

##### MAIN ANALYSIS #####
## Fit default CART to training data
m <- rpart(localization_site~.,y_train, method = "class")
m
m_pred <- predict(m, type = "class")
table(m_pred, y_train$localization_site) #table of predicted vs real 
sum(m_pred!=y_train$localization_site)/dim(y_train)[1] #error = 0.4043127
# plot the tree
prp(m)

## Fit a full CART
m_f <- rpart(localization_site~.,y_train, method = "class",control =
             rpart.control(minsplit = 1,cp = 0))
m_f
prp(m_f)
#predicted values
mf_pred <- predict(m_f, type = "class")
table(mf_pred, y_train$localization_site)
sum(mf_pred!=y_train$localization_site)/dim(y_train)[1] #0

## K-Fold Cross Validation
nfolds <- 10
folds <- rep(1:nfolds, length.out = dim(y_train)[1])
x_err <- NA*numeric(nfolds)
x_err_f <- NA*numeric(nfolds)

for (counter in 1:nfolds){
  m <- rpart(localization_site~.,y_train[folds!=counter,], method = "class")
  mf<- rpart(localization_site~.,y_train[folds!=counter,], method = "class",control =
                     rpart.control(minsplit = 1,cp = 0))
  m_pred <- predict(m, y_train[ folds== counter,],type = "class")
  mf_pred <- predict(m_f, y_train[folds==counter,] ,type = "class")
  x_err[counter] <- sum(m_pred!=y_train$localization_site[folds == counter])/
    sum(folds == counter)
  x_err_f[counter] <- sum(mf_pred!=y_train$localization_site[folds == counter])/
    sum(folds == counter)
}
mean(x_err) #0.4392777
mean(x_err_f) #0

##Error summary
#Within sample tree:0.4043127
#Within sample full tree: 0
#Out of sample tree: 0.4392777
#Out of sample full tree: 0

## The out of sample error is higher than the within sample error. For the full tree,
## The out of sample error and within sample error are each 0.
## This suggests that the full tree is vastly overfit!
## The simple tree performed similarly within sample and out of sample, 
## though the error rate is slightly lower within sample. 

## Compared to the breast cancer data, the fit is much worse for the yeast data.
## The complex trees are so overfit that cross validaiton didn't work. 
## Also, the simple trees produced a 40% error rate compared to <10% error rates
## overall for the breast cancer data. In both the yeast and breast cancer data,
## The within sample error rate was better for the simple tree.


## Pruning
plotcp(m_f)
printcp(m_f)
x_err_pr <- NA*numeric(nfolds)

for (counter in 1:nfolds){
  mpr <-rpart(localization_site~.,y_train[folds!=counter,],method = "class",
              control = rpart.control(minsplit = 1, cp = 0.0049))
  pred_pr <- predict(mpr, y_train[folds == counter,], type = "class")
  x_err_pr[counter] <- sum(pred_pr!=y_train$localization_site[counter == folds])/
    sum(folds == counter)
}
mean(x_err_pr) #0.4276384
## So far, my best model is the pruned tree if you only consider cross-validation results
## and if you note that the error was lowest for the full tree, BUT it was 0 error
## indicating overfitting. Therefore, I would not call that model the best
## even though it is "perfect".

## Bagging
mb<-bagging(localization_site~.,y_train, nbagg = 500, coob = TRUE,
        method = "class",
        control = rpart.control(minsplit = 1, cp = 0, xval = 0),
        aggregation = "majority")
mb$err #0.4267745
pred_b <- predict(mb, y_train, type = "class", aggregation = "majority")
sum(pred_b!=y_train$localization_site)/dim(y_train)[1] #0

x_err_b <- NA*numeric(nfolds)

for (counter in 1:nfolds){
  mb <- bagging(localization_site~.,y_train[counter!=folds,], nbagg = 500, coob = TRUE,
                method = "class",
                control = rpart.control(minsplit = 1, cp = 0, xval = 0),
                aggregation = "majority")
  pred_b <- predict(mb, y_train[counter == folds,], type = "class", 
                    aggregation = "majority")
  x_err_b[counter]<-sum(pred_b!=y_train$localization_site[folds == counter])/
    sum(folds == counter)
}
mean(x_err_b) #0.4267535

## The bagging model performed slightly better than the pruned tree model.
## The out of sample error after cross validation was 42.6735% in the bagging model and 42.7638%
## in the pruned tree (the next best model).

## Random Forests
mrf <- randomForest(localization_site~.,y_train, ntrees = 1000)
mrf$err.rate
mrf_pred<-predict(mrf, y_train, type = "class")
sum(mrf_pred!=y_train$localization_site)/dim(y_train)[1] #0.007187781

x_err_rf <-NA*numeric(nfolds)

for (counter in 1:nfolds){
  mrf <- randomForest(localization_site~.,y_train[folds!=counter,], ntrees = 1000)
  mrf_pred<-predict(mrf, y_train[folds == counter,], type = "class")
  x_err_rf[counter] <-sum(mrf_pred!=y_train$localization_site[folds == counter])/
    sum(folds == counter)
}
mean(x_err_rf) #0.3782014

## The random forest algorithm is the best so far if you do not consider the "perfect" (overfit)
## complex tree. At 37.82% error after cross validation, classification errors
## happen less frequently than in the next best algorithm,
##which was achieved through bagging and had an error rate of 42.67%.

## Boosting
### adaptive boosting
mada <- boosting(localization_site~.,y_train)
mada_pred <- predict(mada,y_train[,1:8],type = "class")$class
sum(mada_pred!=y_train$localization_site)/dim(y_train)[1] #0.3674753

## x-val adaptive boosting
xerr_ada <- NA*numeric(nfolds)

for (counter in 1:nfolds){
  mada <- boosting(localization_site~.,y_train[counter != folds,],mfinal =150)
  mada_pred <- predict(mada,y_train[counter == folds,],type = "class")$class
  xerr_ada[counter]<-sum(mada_pred!=y_train$localization_site[counter ==folds])/
    sum(counter == folds)
}
mean(xerr_ada) #0.4051239

### extreme gradient boosting
num_classes <- length(unique(y_val))
x_val<-as.matrix(y_train[,1:8])
y_val<-as.integer(y_train[,9])-1
mxg<-xgboost(data = x_val, label = y_val, max_depth =7,eta=0.15,nrounds = 40,
             objective="multi:softmax",num_class = num_classes,nthread=2,verbose=2)
mxg_pred<-predict(mxg,x_val)
# Calculate classification error
xgb_error <- sum(mxg_pred != y_val) / length(y_val)
xgb_error #0.1248877

# x-val extreme gradient boosting
xerr_xgb <- NA*numeric(nfolds)

for (counter in 1:nfolds){
  mxg<-xgboost(data = x_val[counter!=folds,], label = y_val[counter!=folds], 
               max_depth =6,eta=0.1,nrounds = 35,
               objective="multi:softmax",
               num_class = num_classes,nthread=1,verbose=2)
  mxg_pred<-predict(mxg,x_val[counter ==folds,])
  # Calculate classification error
  xerr_xgb[counter] <- sum(mxg_pred != y_val[counter == folds]) / sum(counter == folds)
}
mean(xerr_xgb) #0.3916828

## Extreme gradient boosting works on this dataset better than most other algorithms
## Aside from random forests, which is the best algorithm for this dataset.
## However, given that the best algorithm still misclassifies the training data 37% of the time
## None of the methods I've tried in the CART unit are totally appropriate for
## predicting the protein site on yeast cells.

mean(xerr)
mean(xerr_f)
mean(xerr_b)
mean(xerr_pr)
mean(xerr_rf) #0.3782014, best model!
mean(xerr_ada)#0.40
mean(xerr_xgb)#0.39

### Testing best model on test dataset!!
testpred_rf<-predict(mrf,y_vault[,1:8],type="class")
sum(testpred_rf!=y_vault$localization_site)/dim(y_vault)[1] #0.3746631

## The random forest model performed equally well on the training and test dataset!
## This is great news from an overfitting perspective!
## The news is not so great for yeast protein classification overall, 
## given the 37% error rate from the testing and training dataset!
