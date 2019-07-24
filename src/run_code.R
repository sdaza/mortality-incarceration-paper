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
source("src/psid/01_load_individual_data_psid13.R")

# reformat data and variables
source("src/psid/02_define_variables_long_psid13.R")

# models
source("src/psid/03_survival_models_psid_psid13.R")

# multiple imputation
source("src/psid/04_imputation_model_long_format_psid13.R")

# summary tables using imputed data
source("src/psid/05_process_imputations_psid13.R")


##############################
# nlsy analysis
##############################

# load data and format variables
source("src/nlsy/01_load_data_nlsy79.R")

# models
source("src/nlsy/02_survival_models_nlsy79.R")
