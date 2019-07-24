##############################
# health and incarceration
# pooling models using imputed data
# author: sebastian daza
##############################

# libraries
library(sdazar)
library(ggplot2)
library(lattice)
library(survival)
library(mitools)
library(texreg)
library(survey)

source('src/utils/utils.R')

# load imputation results

# observations and deaths
load("output/imputations/obs_deaths.Rdata")

# weighted models

# models_wt
load("output/imputations/models_wt.Rdata")

# number of imputations
n_imputations <- length(coeff_wt)

m1 <- MIcombine(coeff_wt, vcov_wt)
screenreg(m1)

# models_h_wt
load("output/imputations/models_h_wt.Rdata")

m2 <- MIcombine(coeff_h_wt, vcov_h_wt)
screenreg(m2)

# models_wt_msm
load("output/imputations/models_wt_msm.Rdata")

m3 <- MIcombine(coeff_wt_msm, vcov_wt_msm)
screenreg(m3)

# models_h_wt_msm
load("output/imputations/models_h_wt_msm.Rdata")

m4 <- MIcombine(coeff_h_wt_msm, vcov_h_wt_msm)
length(coeff_h_wt_msm)
screenreg(m4)

# unweighted models
load("output/imputations/models_uwt.Rdata")

m5 <- MIcombine(coeff_uwt, vcov_uwt)
screenreg(m5)

# models_h_uwt
load("output/imputations/models_h_uwt.Rdata")

m6 <- MIcombine(coeff_h_uwt, vcov_h_uwt)
screenreg(m6)

# models_uwt_msm
load("output/imputations/models_uwt_msm.Rdata")

m7 <- MIcombine(coeff_uwt_msm, vcov_uwt_msm)
screenreg(m7)

# models_h_uwt_msm
load("output/imputations/models_h_uwt_msm.Rdata")

m8 <- MIcombine(coeff_h_uwt_msm, vcov_h_uwt_msm)
screenreg(m8)

# summary list for texreg
modelsw <- list(m1,m3,m2,m4)
modelsuw <- list(m5,m7,m6,m8)

# tables with imputation results

name.map  <- list(gprison = "Prison",
                  cagei = "Age",
                  male = "Male",
                  fraceiBlack = "Black",
                  fraceiOther = "Other race + Unknown",
                  linca = "Log Income, centered",
                  "ieduchigh school" = "High school",
                  "ieducsome college" = "Some college",
                  "ieduccollege" = "College",
                  dghealth = "Poor health")
texreg(modelsuw,
       obs = N$observations,
       events = N$deaths,
       stars = 0,
       custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
       groups = list("Race (ref. White)" = 4:5, "Education (ref. $<$ HS)" = 7:9),
       custom.coef.map = name.map,
       custom.note = "Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "models_psid_imp_1",
       caption = paste0("Cox Survival Models on the effect of Imprisonment on Mortality, \\newline ", n_imputations, " Imputations, Unweighted, PSID 1968-2013"),
       caption.above = TRUE,
       fontsize = "scriptsize",
       float.pos = "htp",
       file = "output/models_psid_imp_1.tex"
)


texreg(modelsw,
       obs = N$observations,
       events = N$deaths,
       stars = 0,
       custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
       groups = list("Race (ref. White)" = 4:5, "Education (ref. $<$ HS)" = 7:9),
       custom.coef.map = name.map,
       custom.note = "Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "models_psid_imp_2",
       caption = paste0("Cox Survival Models on the effect of Imprisonment on Mortality, \\newline ", n_imputations, " Imputations, Weighted, PSID 1968-2013"),
       caption.above = TRUE,
       fontsize = "scriptsize",
       float.pos = "htp",
       file = "output/models_psid_imp_2.tex"
)
