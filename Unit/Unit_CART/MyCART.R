setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_CART")
data<- read.csv("Abalone_data.csv")
head(data)
str(data)
is.na(data)

colnames <- colnames(data)
for (i in colnames) {
  ##Check if the column is numeric
  if (is.numeric(data[[i]])) {
    # Create a histogram for the numeric column
    hist(data[[i]], main = paste("Histogram of", i), xlab = i)
  }
}

##Check correlation betweeen each pair of variables by building a correlation matrix
for (i in colnames) {
  for (j in colnames) {
    # Check if both columns are numeric and not the same column
    if (is.numeric(data[[i]]) && is.numeric(data[[j]]) && i != j) {
      # Calculate the correlation between the two columns
      correlation <- cor(data[[i]], data[[j]])
      # Print the correlation result
      cat("Correlation between", i, "and", j, ":", correlation, "\n")
    }
  }
}
library(corrplot)             ##Load the corrplot package

##Calculate the correlation matrix for numeric columns
numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)


##Plot the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45,  ##Text label color and rotation
         type = "upper")                 ##Only show the upper triangle of the matrix
##whole weight highly correlated witha few other variable...keep an eye on this one

##Split data into calibration validation data
set.seed(123)
ab_perm <- data[sample(dim(data)[1],dim(data)[1], replace = FALSE),]
ab_cal <- ab_perm[1:floor(0.75*(dim(data)[1])),] #calibration
ab_val <- ab_perm[(floor(0.75*(dim(data)[1]))+1):(dim(data)[1]),]
write.csv(ab_val, "ab_val.csv") #write to folder so I can pull later for evaluation
write.csv(ab_cal, "ab_cal.csv")

##Fit a CART with default algorithmic parameters to validation dataset
library(rpart)
ab_m<-rpart(Rings~., data = ab_cal, method = "class")
summary(ab_m)
print(m_d)
#examine error
ab_m_pred<-predict(ab_m, type = "class")
table(ab_m_pred, ab_cal$Rings)
sum(ab_m_pred!=ab_cal$Rings)/dim(ab_cal)[1] #74% error....not good!
##plot the tree
install.packages("rpart.plot")
library(rpart.plot)
prp(ab_m) #didn't predict most of the potential number of rings
#abalone in this dataset can have from 1 to 29 rings

##Fit a full CART
ab_fm<-rpart(Rings~.,data = ab_cal, method = "class", 
      control = rpart.control(minsplit = 1, cp = 0))
summary(ab_fm)
print(ab_fm)
ab_fm_pred<-predict(ab_fm, type = "class")
table(ab_fm_pred, ab_cal$Rings)
sum(ab_fm_pred!=ab_cal$Rings)/dim(ab_cal)[1] #0% error...overfitted!
prp(ab_fm) #plot

##K-fold cross validation
nfolds<- 10
folds<-rep(1:nfolds, length.out = dim(ab_cal)[1])
xerr <- NA*numeric(nfolds)

for (i in 1:nfolds){
  mod<-rpart(Rings~., data = ab_cal[folds !=i,], method = "class")
  pred<- predict(mod, ab_cal[folds == i,], type = "class")
  xerr[i] <- sum(pred!=ab_cal$Rings[folds==i])/
    sum(folds == i)
}
mean(xerr) #0.7531949

##K-fold cross validation (full tree)
nfolds<- 10
folds<-rep(1:nfolds, length.out = dim(ab_cal)[1])
xerr <- NA*numeric(nfolds)

for (i in 1:nfolds){
  mod<-rpart(Rings~., data = ab_cal[folds !=i,], method = "class",
             control = rpart.control(minsplit = 1, cp = 0))
  pred<- predict(mod, ab_cal[folds == i,], type = "class")
  xerr[i] <- sum(pred!=ab_cal$Rings[folds==i])/
    sum(folds == i)
}
mean(xerr) #0.8096996

#within sample simple: 0.7353129
#within sample complex: 0
#out of sample simple: 0.7531949
#out of sample complex: 0.8096996

## The out of sample error is very slightly lower than 
##the within sample error rate for the simple model. For the full tree,
##the out of sample error rate from cross-validation was higher.

##The within sample error rate was lower for the full tree.

##The out-of-sample error rate was lower for the simple tree.

##The complex tree did perfectly within sample and very badly out of sample.
## This suggests it is overfit. The simple tree did equally poorly within sample 
##and out-of-sample. This suggests that the default tree is underfit.
##The full tree is overly complex.

##The Abalone dataset showed a similar pattern to the breast cancer data 
##with regard to general patterns among within and out-of-sample 
##error rates and the simple and complex models. However,
##the error rates for the abalone dataset were overall MUCH higher
##than in the breast cancer dataset.

###Day 4:Pruning
plotcp(ab_fm)
printcp(ab_fm)

xerr_pr <- NA*numeric(nfolds)

for (i in 1:nfolds){
  mod<-rpart(Rings~., data = ab_cal[folds !=i,], method = "class",
             control = rpart.control(minsplit = 1, cp = 0.002))
  pred<- predict(mod, ab_cal[folds == i,], type = "class")
  xerr_pr[i] <- sum(pred!=ab_cal$Rings[folds==i])/
    sum(folds == i)
}
mean(xerr_pr) #0.7289341

##So far, the pruned classification tree is the best model. 
##From K-fold cross validation, the out of sample error rate was
##0.7289341, which is lower than the out of sample error for the full 
##tree (0.80) and for the simple tree (0.75)
