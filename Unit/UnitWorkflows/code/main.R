######## Workflow assignment using analysis over how iridescent feathers are affected by urbanization
######## author: Joanna Corimanya
######## date: 5 Nov 2024

# Load the here package
library(here)

# Check if the results directory exists; if not, create it
results_dir <- here("results")
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

# Read in data, clean data, load packages
source(here("code", "data_clean.R"))

# Run models and save model outputs to tables in results folder
source(here("code", "models.R"))

#Make a figure and save it to figure folder
print("Running figure.R")
source(here("code","figure.R"))

