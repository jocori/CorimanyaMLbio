## Modularity assignment
#
## Joanna Corimanya
## Overall goal: see climate change
#
# Set up
setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_modularity")
library(dplyr) #tidyverse for data management
library(maps) #for making maps
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

## 1. Filter locations to only ones with at least 40 measurements
### input: data: dataset with precipitation and temperature data, var_name (variable name as string ("temp" or "precip")),
### min_count (minimum number of measurements)
### output: dataset with same column numbers and names as temp or precip that only has rows where each location has at least min_count measurements
filter_locations <- function(data, var_name, min_count = 40) {
  #filter out rows with NA values in the variable
  data %>%
    filter(!is.na(!!sym(var_name))) %>%
    #group data by location name to count measurements per location
    group_by(name) %>%
    #keep those locations that have at least min_count measurements
    filter(n() >= min_count) %>%
    #remove grouping
    ungroup()
}
## 2. Regress climate variable against the year & take the slopes from that model
### input: data: filtered dataset containing temperature and precipitation data, var_name (vector of variable names as strings("temp" or "precip"))
### output: slope: a list in which each list element is a dataframe for each variable containing name, slope, climate varible, latitude, and longitude
get_slopes <- function(data, var_name) {
    #Do linear regression for each location to get the slope
  slopes <- data %>%
    group_by(name, lon, lat) %>%
    do({
      model <- lm(as.formula(paste(var_name, "~ year")), data = .)  #run linear model
      data.frame(slope = coef(model)[2])                            #extract slope
    }) %>%
    ungroup()
  
  return(slopes)
}

## 3. Make a histogram of all slopes for each climate variable
### input: slopes: slope dataframe from the get_slopes function, 
###        variable name ("temp" or "precip")
### output: none. output is the side effect of the function (the histograms)
plot_histograms <- function(slopes, var_name){
  #plot histogram of slope for each variable
    hist(slopes$slope, main = paste("Slope Histogram for", var_name),
         xlab = "Slope", ylab = "Frequency", col = "pink", border = "black")
}

## 4. Make a map for each climate variable where the slopes are depicted in color
###   input: slope dataframe from the get_slopes function, 
###        variable name ("temp" or "precip")
###   output: None. Output is a "side effect" of the funciton (color-coded slopes)
make_map<- function(slopes,var_name){
    #plot map of region of interest (USA here)
    map("state")
    #plot location points, color coded by slope direction
    points(slopes$lon,slopes$lat, col = ifelse(slopes$slope >0, "lightgreen","coral"), pch = 16,cex = 0.5)
    #add a title
    title(main=paste("Slope maps showing", var_name))
}

###5. Using the new functions!
#### input: data_list (list of datasets), variables (vector of variable names)
# output: Histograms and maps for each variable's slopes
analysis <- function(data, var_name) {
  
  #filter locations for each dataset and variable
    #filter the dataset for the current variable and store it in the list
    filtered_data <- filter_locations(data, var_name)
  
  #get slopes for each variable using the filtered data
  slopes <- get_slopes(filtered_data, var_name)
  
  #plot histograms for each variable
  plot_histograms(slopes,var_name)
  
  #plot maps for each variable
  make_map(slopes,var_name)
}


#run the analysis on precipitation data
analysis(precip, "precip")
#run the analysis on temperature data
analysis(temp, "temp")

