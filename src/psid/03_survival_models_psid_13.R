##############################
# health and incarceration
# cox models
# author: sebastian daza
##############################

# libraries
library(sdazar)
library(survival)
library(forcats)
library(survey)
library(survminer)
library(texreg)
library(ipw)
library(ggplot2)
library(weights)

# load data
ldat <- readRDS("output/psid_long_format_covariates_year_13.rds")

##############################
# compute time variables for survival analysis
##############################

summary(ldat$year)
cop <- copy(ldat)

# remove cases with previous covariate to imprisonment
ldat <- ldat[select == 1]

ldat[, myear := min(as.numeric(year)), by = pid]
ldat[, start := year - myear, by = pid]
ldat[, stop := start + 1]

summary(ldat[, start, stop])

ids <- sample(unique(ldat$pid), 1)
ldat[pid == ids, .(pid, response, agei, year, start, stop, liinc)]

# explore some examples
ids <- unique(ldat[death == 1, pid])
ldat[pid == sample(ids, 1), .(pid, year, death, ydeath, died)]

# age constant
ldat[, mage := Min(agei), pid]
ldat[, cage := mage - 18]

summary(ldat$cage)
table(ldat$cage, useNA = "ifany")

# categories of education
ldat[iedu %in% 1:11, ieduc := "less high school"]
ldat[iedu == 12, ieduc := "high school"]
ldat[iedu %in% 13:15, ieduc := "some college"]
ldat[iedu > 15, ieduc := "college"]

ldat[, ieduc := factor(ieduc, levels = c("less high school", "high school",
                                       "some college", "college"))]
table(ldat$ieduc, useNA = "ifany")

# explore variables by year

# respondents
(tot <- length(unique(ldat[, pid]))) # 54046
(s <- length(unique(ldat[smember == 1, pid])))
(ns <- length(unique(ldat[smember == 0, pid])))
s / tot # 60%
ns / tot # 30%


summary(ldat$iwt)
hist(ldat$iwt)
hist(ldat$fwt)

summary(ldat$fwt)
length(unique(ldat[is.na(fwt), pid])) # 279 cases


table(duplicated(ldat, by = c("family_id")))
summary(ldat$agei)

# create timing variables
summary(ldat$year)

setkey(ldat, year, pid)

ldat[, myear := min(as.numeric(year)), by = pid]
ldat[, start := year - myear, by = pid]
# ldat[, stop := ifelse(year < 1997, start + 1, start + 2)]
ldat[, stop := start + 1]

summary(ldat$start)
summary(ldat$stop) # 46
ids <- sample(unique(ldat$pid), 1)
ldat[pid == ids, .(pid, response, agei, cage, year, start, stop, liinc)]

# explore some examples
ids <- unique(ldat[death == 1, pid])
ldat[pid == sample(ids, 1), .(pid, year, death, ydeath, died)]


# recoding some variables

# race
table(ldat$racei, useNA = "ifany")
ldat[, fracei := factor(racei, labels = c("White", "Black", "Other"))]
table(ldat$fracei, useNA = "ifany")

table(ldat$ieduc, useNA = "ifany")

# gender
ldat[, male := gender]

# time between death and last imprisonment recorded (descriptives)

ldat[pid == sample(unique(ldat$pid), 1), .(pid, year, agei, prison95, nrprison)]

table(ldat$nrprison)

ldat[, s_nrprison := cumsum(nrprison), pid]
ldat[, s_nrprison := ifelse(nrprison > 1, 1, 0)]

ldat[, s_prison95 := cumsum(prison95), pid]
ldat[, s_prison95 := ifelse(s_prison95 > 1, 1, 0)]
table(ldat$s_prison95, useNA = "ifany")

ldat[, temp_prison := ifelse(is.na(nrprison), 0, nrprison)]
summary(ldat$temp_prison)
ldat[, temp_prison := pmax(s_prison95, temp_prison)]
summary(ldat$temp_prison)

setorder(ldat, -year, pid)
ldat[, cum_temp_prison := cumsum(temp_prison), pid]
ldat[, anyprison := ifelse(sum(iprison, na.rm = TRUE) > 0, 1, 0), pid]
ldat[, anydeath := ifelse(sum(died, na.rm = TRUE) > 0, 1, 0), pid]
setorder(ldat, year, pid)

table(ldat[start == 0, .(anyprison, anydeath)]) # 94
ldat[, cum_temp_prison := ifelse(cum_temp_prison > 0, 0, 1)]

#########################
# survival analysis
#########################

# only prison why not response

# two sets (with and without health)

vars1 <- c("pid", "family_id", "anrprison", "cage", "male", "fracei", "ieduc",
          "liinc", "start", "stop", "agei", "year", "died", "dropout", "fwt")

set1 <- ldat[complete.cases(ldat[, vars1, with = FALSE]), ]
table(set1[, min(stop), by = pid][, V1])
table(set1[, max(stop), by = pid][, V1])
table(set1[, max(start), by = pid][, V1])
table(set1[, min(start), by = pid][, V1])

# descriptives for set1

length(unique(set1$pid))

wpct(set1[start == 0, male])
wpct(set1[start == 0, fracei])

table(set1[, max(died), pid]$V1, set1[, max(anrprison), by = pid]$V1) # 46
table(set1[, max(died), by = pid]$V1, set1[, max(iprison), by = pid]$V1) # 91

summary(set1$ydeath)

set1[, select95 := 0]
set1[ydeath >= 1995 | death == 0, select95 := 1]
table(set1$select95, useNA = "ifany")

set195 <- set1[select95 == 1]
length(unique(set195$pid)) # 44349
table(set195$ydeath, useNA = "ifany")
table(set195$year, useNA = "ifany")

table(set195[, max(died), pid]$V1, set195[, max(anrprison), by = pid]$V1) # 27
table(set195[, max(died), by = pid]$V1, set195[, max(iprison), by = pid]$V1) # 72

#  people who died before 1995
table(set195[, min(stop), by = pid][, V1])
table(set195[, max(stop), by = pid][, V1])
table(set195[, max(start), by = pid][, V1])
table(set195[, min(start), by = pid][, V1])

# import to stata to run gompertz model
write_dta(set195, "output/psid_gompertz_data_95.dta", version = 14)

#######################
# 1995 model
########################

m1.1 <- coxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei +  liinc + ieduc  + cluster(pid),
              data = set195)

# interaction prisonn age

m1.1.1 <- coxph( Surv(start, stop, died) ~ iprison * cage
                 + male + fracei +  liinc + ieduc  + cluster(pid),
                data = set195)

ds <- svydesign(id = ~ cluster, weights = ~ fwt, strata=~stratum, data = set195, nest = TRUE)

m1.2 <- svycoxph( Surv(start, stop, died) ~ iprison + cage
                 + male + fracei +  liinc + ieduc  + cluster(pid),
                 design = ds)

# add health
m2.1 <- coxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei +  liinc + ieduc  + idghealth +  cluster(pid),
              data = set195)


m2.2 <- svycoxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei +  liinc + ieduc  + idghealth + cluster(pid),
                design = ds)


# marginal structural model

temp1 <- ipwtm(exposure = dropout, family = "survival",
              numerator = ~ male + cage + fracei,
              denominator = ~ male + cage + fracei + iprison + liinc + ieduc,
              id = pid,
              tstart = start, timevar = stop,
              type = "first",
              data = set195)

temp2 <- ipwtm(exposure = iprison, family = "survival",
              numerator = ~ male + cage + fracei,
              denominator = ~ male + cage + fracei + liinc + ieduc,
              id = pid,
              tstart = start, timevar = stop,
              type = "first",
              data = set195)

summary(temp1$ipw.weights)
summary(temp2$ipw.weights)

# create weigths
set195[, iwt := temp1$ipw.weights * temp2$ipw.weights]
set195[, nwt := fwt * temp1$ipw.weights * temp2$ipw.weights]

# only MSM weights
m3.1 <- coxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei +  cluster(pid),
               weights = set195$iwt,
               data = set195)

summary(m3.1)

# sampling and MSM weights
ds <- svydesign(id = ~ cluster, weights = ~ nwt, strata = ~stratum, data = set195, nest = TRUE)

m3.2 <- svycoxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei + cluster(pid),
               design = ds)

##################
# add health
#################

vars2 <- c("pid", "family_id", "iprison", "cage", "male", "fracei", "ieduc",
          "liinc", "start", "stop", "agei", "year", "died", "dropout", "fwt",
          "idghealth")

set195h <- set195[complete.cases(set195[, vars2, with = FALSE]), ]

table(set195h[, max(died), pid]$V1, set195h[, max(anrprison), by = pid]$V1) # 27
table(set195h[, max(died), by = pid]$V1, set195h[, max(iprison), by = pid]$V1) # 72

temp1 <- ipwtm(exposure = dropout, family = "survival",
              numerator = ~ male + cage + fracei,
              denominator = ~ male + cage + fracei + iprison + liinc + ieduc + idghealth,
              id = pid,
              tstart = start, timevar = stop,
              type = "first",
              data = set195h)

temp2 <- ipwtm(exposure = iprison, family = "survival",
              numerator = ~ male + cage + fracei,
              denominator = ~ male + cage + fracei + liinc + ieduc + idghealth,
              id = pid,
              tstart = start, timevar = stop,
              type = "first",
              data = set195h)

# calculate weights
summary(temp1$ipw.weights)
summary(temp2$ipw.weights)
set195h[, iwt := temp1$ipw.weights * temp2$ipw.weights]
set195h[, nwt := fwt * temp1$ipw.weights * temp2$ipw.weights]

# only MSM weights
m4.1 <- coxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei + cluster(pid),
               weights = set195h$iwt,
               data = set195h)

# sampling and MSM weights
ds <- svydesign(id = ~ cluster, weights = ~ nwt, strata=~stratum, data = set195h, nest = TRUE)

m4.2 <- svycoxph( Surv(start, stop, died) ~ iprison + cage
               + male + fracei +  cluster(pid),  design = ds)

######################################
# create tables (without stars!)
######################################

screenreg(list(m1.1, m1.2, m2.1, m2.2, m3.1, m3.2, m4.1, m4.2), include.zph = FALSE)

name.map  <- list(iprison = "Prison",
                 cage = "Age",
                 male = "Male",
                 fraceiBlack = "Black",
                 fraceiOther = "Other race + Unknown",
                 liinc = "Log Income, centered",
                 "ieduchigh school" = "High school",
                 "ieducsome college" = "Some college",
                 "ieduccollege" = "College",
                 idghealth = "Poor health")

length(name.map)

models_unw <- list(m1.1, m3.1, m2.1, m4.1)
models_w <- list(m1.2, m3.2, m2.2, m4.2)

screenreg(models_unw, include.zph = FALSE)

texreg(models_unw, include.rsquared = FALSE, include.aic = FALSE,
    include.maxrs = FALSE, include.events = TRUE, include.nobs = TRUE,
    include.missings = FALSE, include.zph = FALSE,
    stars = 0,
    custom.gof.names =c("Deaths", "Person-years"),
    custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
    groups = list("Race ( .ref. White)" = 4:5, "Education (ref. $<$ HS)" = 7:9),
    custom.coef.map = name.map,
    custom.note = "Robust standard errors in parenthesis.",
    booktabs = TRUE,
    dcolumn = TRUE,
    use.packages = FALSE,
    label = "models_psid_1",
    caption = "Cox Survival Models on the effect of Imprisonment on Mortality, \\newline Unweighted, PSID 1968-2013",
    caption.above = TRUE,
    fontsize = "scriptsize",
    float.pos = "htp",
    file = "output/models_psid_1.tex"
    )

texreg(models_w, include.rsquared = FALSE, include.aic = FALSE,
    include.maxrs = FALSE, include.events = TRUE, include.nobs = TRUE,
    include.missings = FALSE, include.zph = FALSE,
    stars = 0,
    custom.gof.names =c("Deaths", "Person-years"),
    custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
    groups = list("Race (ref. White)" = 4:5, "Education (ref. $<$ HS)" = 7:9),
    custom.coef.map = name.map,
    custom.note = "Robust standard errors in parenthesis.",
    booktabs = TRUE,
    dcolumn = TRUE,
    use.packages = FALSE,
    label = "models_psid_2",
    caption = "Cox Survival Models on the effect of Imprisonment on Mortality, \\newline Weighted, PSID 1968-2013",
    caption.above = TRUE,
    fontsize = "scriptsize",
    float.pos = "htp",
    file = "output/models_psid_2.tex"
    )

#########################
# plots (transition probabilities)
#########################

# 18 years old
newdata1 <- data.frame(iprison = 1,
                      cage = 0,
                      male = 1,
                      fracei = "Black" ,
                      liinc = 0,
                      ieduc = "less high school")

newdata2 <- data.frame(iprison = 0,
                      cage = 0,
                      male = 1,
                      fracei = "Black" ,
                      liinc = 0,
                      ieduc = "less high school")

plot_m1_1_18 <- getComparisonPlot(m1.1, data1 = newdata1,
  data2 = newdata2,
  pos1 = c(30, 42),
  pos2 = c(0.3, 0.08))

savepdf("output/plot_psid_m1_1_18")
print(plot_m1_1_18)
dev.off()

# 30 years old

newdata1 <- data.frame(iprison = 1,
                      cage = 12.0,
                      male = 1,
                      fracei = "Black" ,
                      liinc = 0,
                      ieduc = "less high school")

newdata2 <- data.frame(iprison = 0,
                      cage = 12.0,
                      male = 1,
                      fracei = "Black" ,
                      liinc = 0,
                      ieduc = "less high school")

plot_m1_1_30 <- getComparisonPlot(m1.1, data1 = newdata1, data2 = newdata2,
                                    pos1 = c(25, 42), pos2 = c(0.3, 0.20))


savepdf("output/plot_psid_m1_1_30")
print(plot_m1_1_30)
dev.off()

#############################
# end script
#############################
