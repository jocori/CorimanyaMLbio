library(here)

# Paths to folders within UnitWorkflows
paper_path <- here("paper")
results_path <- here("results")
code_path <- here("code")
data_path <- here("data")

#loading a data file within the data folder
data_file <- here("data", "feath.csv")
feath <- read.csv(data_file)

feath$sex<-as.factor(feath$sex)
feath$urbanization <- as.factor(feath$urbanization)
str(feath)
feath$nestid <- as.character(feath$nestid)

#DAN: I did not run this on my machine because of all these dependencies and I did not 
#want to install them all.
#Are they all really necessary? I see you have a comment next to most, so that's good. 

#DAN: When you have a lot of dependencies, there are several risks. One is, functions in
#namespace of one package get masked by functions in the namespace of a package loaded 
#subsequently. You can address that byt not using library(, instead use :: whenever you 
#call a function from a package. Another, bigger risk is that packages get updated and
#then your code does not work. 

#DAN: Anyway, you may want to handle packages by listing all dependencies and/or having
#all calls to library() in the main.R script, so at least the user can see them all in 
#one place. 


library(rsq) #to calculate r-squared values
library(pavo) #for importing, analyzing, and making figures from spectral readings
library(effects) #for extracting model predictions and confidence intervals for plotting
library(lmerTest)
library(car)
library(kimisc) # has the nlist function to create a named list
library(AICcmodavg) # has the aictab function
library(tibble)
library(lme4)
library(broom.mixed)  # For tidying mixed models
library(dplyr) # for plots


### separate the body regions
feath$bodreg <- tolower(feath$bodreg)
throat <-feath[feath$bodreg == "th",]
belly <-feath[feath$bodreg == "be",]
back <-feath[feath$bodreg == "ba",]