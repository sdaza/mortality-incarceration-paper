##############################
# mortality and incarceration
# source files
# author: sebastian daza
##############################

# load utilities
source("src/utils/utils.R")

##############################
# psid analysis
##############################

# load data
source("src/psid/01_load_individual_data_13.R")

# reformat data and variables
source("src/psid/02_define_variables_long_13.R")

# models
source("src/psid/03_survival_models_psid_13.R")


##############################
# nlsy analysis
##############################

# load data
source("src/nlsy79/01_load_data.R")

source("src/nlsy79/03_survival_models_nlsy_adjusted.R")
