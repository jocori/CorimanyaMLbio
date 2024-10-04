##Set working directory
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_CART")
##Read in breast cancer data
data<- read.csv("data.csv")
head(data)
str(data)
colnames <- colnames(data)
set.seed(101)
for (i in colnames) {
  ##Check if the column is numeric
  if (is.numeric(data[[i]])) {
    # Create a histogram for the numeric column
    hist(data[[i]], main = paste("Histogram of", i), xlab = i)
  }
}

##Check correlation between each pair of variables by building a correlation matrix
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

##Visualize correlation
#install.packages("corrplot")  ##Install corrplot package
library(corrplot)             ##Load the corrplot package

##Calculate the correlation matrix for numeric columns
numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)

##Plot the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45,  ##Text label color and rotation
         type = "upper")                 ##Only show the upper triangle of the matrix

##Save heatmap to local device
pdf("correlation_matrix.pdf")
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black", type = "upper")
dev.off()

##Missing values?
is.na(data)

##Remove ID and worst columns
head(data)
str(data)
data<- data[ , -c(1)] #Remove ID column
##Remove columns that contain the word "worst" in their names
data <- data[ , !grepl("worst", names(data))]

str(data)
data<-data[,-c(22)]
##Split into test and training data
d_perm<-data[sample(dim(data)[1],dim(data)[1], replace = FALSE),]
d_val<-d_perm[1:floor(0.75*(dim(data)[1])),]
d_test<-d_perm[(floor(0.75*(dim(data)[1]))+1):(dim(data)[1]),]

##Fit CART
#install.packages("rpart") #package for fitting carts
library(rpart)
m_d<-rpart(diagnosis~., data = d_val, 
           method = "class") #because it's a classification tree
print(m_d)
m_d_pred<-predict(m_d, type = "class")
table(m_d_pred, d_val$diagnosis)
sum(m_d_pred!=d_val$diagnosis)/dim(d_val)[1] #0.05164319

##Build full tree
m_f<-rpart(diagnosis~., data = d_val, 
           method = "class",
           control = rpart.control(cp=0, minsplit=1))
print(m_f)
m_f_pred<-predict(m_f, type = "class")
table(m_f_pred, d_val$diagnosis)
sum(m_f_pred!=d_val$diagnosis)/dim(d_val)[1] #0

##K-fold cross validation (manual) on simple model
numgp <- 10 #number of folds
gp <- rep(1:numgp, length.out=dim(d_val)[1])
xerrs<-NA*numeric(numgp)
for (counter in 1:numgp) {
  #fit model on all data excluding one group
  m<-rpart(diagnosis~., data = d_val[gp!=counter,], method = "class")
  #get predictions on the left out group and get error rates
  pred <- predict(m,d_val[gp == counter,], type = "class")
  xerrs[counter] <- sum(pred!=d_val$diagnosis[gp==counter])/
    sum(gp==counter)
}
mean(xerrs) #0.08925803

## K-fold cross validation on complex model
numgp <- 10 #number of folds
gp <- rep(1:numgp, length.out=dim(d_val)[1])
xerrs_f<-NA*numeric(numgp)
for (counter in 1:numgp) {
  #fit model on all data excluding one group
  m<-rpart(diagnosis~., data = d_val[gp!=counter,], method = "class",
           control = rpart.control(minsplit = 1, cp = 0))
  #get predictions on the left out group and get error rates
  pred <- predict(m,d_val[gp == counter,], type = "class")
  xerrs_f[counter] <- sum(pred!=d_val$diagnosis[gp==counter])/
    sum(gp==counter)
}
xerrs_f
mean(xerrs_f) #0.103433

##Pruning
##Plot CP on full tree
plotcp(m_f)
##print CP to see that numerically
printcp(m_f)

##cp of 0.018 is ideal
m_f_5 <- rpart(diagnosis~., data = d_val, method = "class",
               control = rpart.control(cp = 0.018, minsplit = 1))
xerrs_5<-NA*numeric(numgp)
for (counter in 1:numgp) {
  #fit model on all data excluding one group
  m<-rpart(diagnosis~., data = d_val[gp!=counter,], method = "class",
           control = rpart.control(minsplit = 1, cp = 0.018))
  #get predictions on the left out group and get error rates
  pred <- predict(m,d_val[gp == counter,], type = "class")
  xerrs_5[counter] <- sum(pred!=d_val$diagnosis[gp==counter])/
    sum(gp==counter)
}
xerrs_5
mean(xerrs_5) #0.07524917

## Day 5: Bagging
#install.packages("ipred")
library(ipred)
## ipred can't handle character, so convert diagnosis to factor
d_val$diagnosis <- as.factor(d_val$diagnosis)
bagres<-bagging(diagnosis~., 
        data = d_val, nbagg=500, coob = TRUE, method = "class",
        control = rpart.control(minsplit = 1, cp = 0, xval=0),
        aggregation = "majority")
## k-fold cross validation
numgp <-10
gp<-rep(1:numgp, length.out = dim(d_val)[1])
xerr_b <- NA*numeric(numgp)

for (counter in 1:numgp) {
  m<-bagging(diagnosis~., 
             data = d_val[gp!=counter,], nbagg=500, coob = TRUE, method = "class",
             control = rpart.control(minsplit = 1, cp = 0, xval=0),
             aggregation = "majority")
  pred<- predict(m, d_val[gp == counter,], type = "class", aggregation = "majority")
  xerr_b[counter] <- sum(pred!=d_val$diagnosis[gp == counter])/sum(gp == counter)
}
mean(xerr_b) #0.06101883

## Random forest
#install.packages("randomForest")
library(randomForest)
m_rf<-randomForest(diagnosis~., data = d_val, ntree= 1000)
plot(m_rf) #information on how many trees you really need
m_rf

#k-fold cross validation on random forest
xerr_rf <- NA*numeric(numgp)

for (counter in 1:numgp) {
  m<-randomForest(diagnosis~.,
                  data = d_val[gp!=counter,], ntree = 1000, 
                  control = rpart.control(minsplit = 1,cp=0,xval=0))
  pred <- predict(m, newdata = d_val[gp == counter,], type = "class")
  xerr_rf[counter] <- sum(pred!=d_val$diagnosis[gp == counter])/sum(gp == counter)
}
mean(xerr_rf) #0.07264673

##random forest is worse than bagging, but better than pruned, simple, and full tree

### Boosting
install.packages("adabag")
library(adabag)
m_ada <- boosting(diagnosis~.,d_val)
m_ada
m_ada$weights
ada_pred<-predict(m_ada, d_val[,2:dim(d_val)[2]])$class
sum(ada_pred!=d_val$diagnosis)/dim(d_val)[1] #0

xerr_ada <- NA*numeric(numgp)

for (counter in 1:numgp){
  m_ada <- boosting(diagnosis~.,data =d_val[counter!=gp,])
  ada_pred <- predict(m_ada,d_val[counter == gp,], type = "class")$class
  xerr_ada[counter] <-sum(ada_pred!=d_val$diagnosis[counter == gp])/
    sum(gp == counter)
}
mean(xerr_ada) #0.05398671

#gradient boosting
install.packages("xgboost")
library(xgboost)
## convert from categorical to integer
x_val<-as.matrix(d_val[,2:(dim(d_val)[2])])
y_val<-as.integer(d_val[,1])-1

m_xgb <- xgboost(data = x_val, label = y_val, max_depth =6,
                 eta=0.3,nrounds = 20,objective="binary:logistic",
                 nthread =2,verbose=2)
xgb_pred <- predict(m_xgb, x_val)
# Convert probabilities to class predictions (threshold 0.5)
xgb_class_pred <- ifelse(xgb_pred > 0.5, 1, 0)
# Calculate classification error
xgb_error <- sum(xgb_class_pred != y_val) / length(y_val)
xgb_error

#x val
xerr_xgb<- NA*numeric(numgp)

for (counter in 1:numgp){
  m_xgb <- xgboost(data = x_val[counter!=gp,], label = y_val[counter !=gp], max_depth =6,
                   eta=0.3,nrounds = 20,objective="binary:logistic",
                   nthread =2,verbose=2)
  xgb_pred <- predict(m_xgb, x_val[counter==gp,])
  # Convert probabilities to class predictions (threshold 0.5)
  xgb_class_pred <- ifelse(xgb_pred > 0.5, 1, 0)
  # Calculate classification error
  xgb_error[counter] <- sum(xgb_class_pred != y_val[counter ==gp]) / sum(counter ==gp)
}

mean(xgb_error) #0.05636766
