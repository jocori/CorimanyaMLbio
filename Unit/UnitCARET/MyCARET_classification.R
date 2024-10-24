#set seed for reproducibility
set.seed(123)
library(caret) #for modelling
library(corrplot) #for plotting correlation matrix
library(dplyr) #for data organization

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
.3 v .5, .7 v .9, .9 v .11, .11 v .13, .15 v .17
#remove highly correlated predictor variables and recordID column
frog<-frog[, !(names(frog) %in% c("MFCCs_.5","MFCCs_.9","MFCCs_11","MFCCs_17","RecordID"))]

#shuffle the data and split into training and validation sets
f_split <- frog %>%
  sample_frac(size = 1) %>%  #shuffle the entire dataset
  mutate(row_number = row_number()) %>%  #create a row number column for indexing
  mutate(split = ifelse(row_number <= floor(0.75 * n()), "train", "vault")) %>%  #assign 'train' or 'vault'
  select(-row_number)  #drop the row number column

#create the training and validation datasets
f_train <- f_split %>% filter(split == "train") %>% select(-split)
f_vault <- f_split %>% filter(split == "vault") %>% select(-split)

## Adjacent Categories Probability Model for Ordinal Data
#method = 'vglmAdjCat'
#Tuning parameters: parallel (Parallel Curves),link (Link Function)
library(VGAM)

## Factor-Based Linear Discriminant Analysis
# method = 'RFlda'
#Tuning parameters: q (# of factors)
library(HiDimDA)

## Sparse Mixture Discriminant Analysis
#method = 'smda'
#Tuning parameters:NumVars (# Predictors), lambda (Lambda), R (# Subclasses)
library(sparseLDA)
  
## Semi-Naive Structure Learner Wrapper
#method = 'nbSearch'
#Tuning parameters: k (#Folds), epsilon (Minimum Absolute Improvement),smooth (Smoothing Parameter),
#smooth (Smoothing Parameter), final_smooth (Final Smoothing Parameter), direction (Search Direction)
library(bnclassify)

