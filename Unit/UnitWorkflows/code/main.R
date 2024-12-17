#DAN: Overall, pretty good, but the goal of making a reproducible workflow was thwarted 
#by all the dependencies. Soemtimes those are necessary. In this case, the effect was, I 
#got there and said to myself "I am not going to install all these packages just to see if
#this runs because my disk is too full and I don't have time to do all those installations."
#So the end result was, your workflow was less accessible. Perhaps that was unavoidable if
#all those dependencies are really needed, but perhaps it was avoidable. You decide.
#
#Also, I note you used R markdown for the SI. I used R markdown for years, and eventually 
#decided it's a trap, as I explained in class. It's not a good idea to unify your writeup and
#your code because then every time you change a comma you have to re-run your code. You can
#do what you want, but I anticipate if you start to use R markdown more and more for full-scale
#scientific workflow, you will more and more notice its shortcomings. 
#
#Otherwise it looks pretty good. Grade, S.

######## Workflow assignment using analysis over how iridescent feathers are affected by urbanization
######## author: Joanna Corimanya
######## date: 5 Nov 2024

#DAN: Nice job having pretty much just source commands, here.

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

