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