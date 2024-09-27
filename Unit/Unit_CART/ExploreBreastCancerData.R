##Set working directory
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_CART")
##Read in breast cancer data
data<- read.csv("data.csv")
head(data)
str(data)
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

##Visualize correlation
install.packages("corrplot")  ##Install corrplot package
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
