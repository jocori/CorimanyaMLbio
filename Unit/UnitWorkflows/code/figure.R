# Define the color palette for body regions
colors <- c("th" = "#ce6700", "be" = "#98c9e8", "ba" = "#999999")
library(ggplot2)
# Create the ggplot
library(here)
png(here("results","Rplot.png"))
urban <- belly[belly$urbanization == "urban",]
rural <- belly[belly$urbanization == "rural",]
plot(belly$B2~ belly$bodycond, pch = 16,
     xlab = "Body condition",
     ylab = "Mean Brightness",
     col = ifelse(belly$urbanization == "urban",  "#98c9e8","#ce6700"))
legend("bottomleft", bty = "n",
       pch = 16, 
       c("Rural", "Urban"), 
       col = c("#ce6700", "#98c9e8")) 
abline(lm(rural$B2 ~ rural$bodycond), col = "#ce6700", lwd = 2)
abline(lm(urban$B2 ~ urban$bodycond), col = "#98c9e8", lwd = 2) 
dev.off()