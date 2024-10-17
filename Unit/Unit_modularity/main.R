## Modularity assignment
#
## Joanna Corimanya
## Overall goal: see climate change
#
# Set up
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_modularity")
library(dplyr) #tidyverse for data management
## Read in data
precip<-readRDS("USAAnnualPcpn1950_2008.rds")
temp<-readRDS("USAAnnualTemp1950_2008.rds")
head(precip)
head(temp)
plot(data~year, data = precip)
plot(data~year, data = temp)
### Can't really see any strong trends just looking at the data

#Separately for temperature and precipitation, separately for each location with at least 40 measurements
#regress climate variable against year and take the slope. Make a histogram of all slopes for each climate variable.
#make a map for each climate variable where the slopes are depicted in color.
#The “maps” package may come in handy. Based on these results, can you see climate
#change? What is happening with precipitation?
# combine data
colnames(precip)[5]<-"precip"
colnames(temp)[5]<-"temp"
#check for NA values and keep only rows with precipitation data
precip<-precip[is.na(precip$precip) == "FALSE",]
#check for NA values and keep only rows with temperature data
temp<-temp[is.na(temp$temp) == "FALSE",]
library(dplyr)
#clim<-merge(precip,temp, by = c("state","name","lon","lat","year"), all = TRUE)
## 1. Filter locations to only ones with at least 40 measurements
### input: data: precipitation or temperature data
### output: dataset with same column numbers and names as temp or precip that only has rows where each location has at least 40 measurements
filter_locs<-function(data){
  data %>%
    group_by(name) %>%
    filter(n()>=40) %>%
    ungroup
}
## 2. Regress climate variable against the year & take the slopes from that model
### input: data: filtered temperature or precipitation data, variable name ("temp" or "precip")
### output: slope: matrix containing slope, climate varible, latitude, and longitude
get_slopes <- function(data, var_name){
  slopes<-data%>%
    group_by(name, lat, lon) %>%
    do({
      model<-lm(as.formula(paste(var_name, "~year")), data =.)
      data.frame(slope= coef(model)[2]) #Extract slope 
    }) %>%
    ungroup()
  return(slopes)
}
## 3. Make a histogram of all slopes for each climate variable
### input:
### output:
## 4. Make a map for each climate variable where the slopes are depicted in color