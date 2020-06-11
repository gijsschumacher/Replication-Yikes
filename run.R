rm(list=ls())
root <- getwd()

# Load functions
source("Scripts/Functions.R")   ### NOTE: if you receive warning messages here it means that some packages were built under different R versions. This should not affect behaviour.

# Knit data together
source("Scripts/combinedata.R") ### NOTE: Line 52 of this scripts produces 44 warnings. This is due to reading in some data that we do not need.

# Run validation checks on physiological data
source("Scripts/validate phys.R")

# Construct variables from dataset
source("Scripts/add_vars.R")

# Perform main analysis
source("Scripts/analysis.R")

# Create appendix
source("Scripts/Appendix.R") ### There may be several warnings that Lavaan will no longer be using melt in a future version. Whatever.
# Warning one: the disgust scales do not fit well. Lavaan package throws a warning.
# Warning two: we fit a dichotomous variable using an lm regression. This throws a warning.