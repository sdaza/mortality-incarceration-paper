##############################
# health and incarceration
# exploratory models NSLY 79
# author: sebastian daza
##############################

#+ libraries

library(sdazar)
library(haven)
library(forcats)
# library(VIM)
# library(simPH)
library(survival)
library(survey)
# library(eha)
library(survminer)
library(texreg)
# library(parfm)
library(ipw)
library(ggplot2)
library(weights)

# paths
# path <- "/Users/sdaza/Google Drive/01Projects/01IncarcerationHealth/06WorkingPapers/Manuscript/"

#+ load data
load("output/rdata/nlsy/nlsy79_longformat.Rdata")
source("src/nlsy79/functions.R")

# original model and analysis (PAA)

##############################
# compute time variables for survival analysis
##############################

summary(ldat$year)
cop <- copy(ldat)

# remove cases with previous covariate to imprisonment
ldat <- ldat[select == 1]

ldat[, myear := min(as.numeric(year)), by = id]
ldat[, start := year - myear, by = id]
ldat[, stop := start + 1]

summary(ldat[, start, stop])

ids <- sample(unique(ldat$id), 1)
ldat[id == ids, .(id, response, agei, year, start, stop, liinc)]

# age constant
ldat[, mage := Min(agei), id]
ldat[, cage := mage - 18]

summary(ldat$cage)
table(ldat$cage, useNA = "ifany")

lookvar(ldat, "edu")
# categories of education
ldat[iedu %in% 0:11, ieduc := "less high school"]
ldat[iedu == 12, ieduc := "high school"]
ldat[iedu %in% 13:15, ieduc := "some college"]
ldat[iedu > 15, ieduc := "college"]

ldat[, ieduc := factor(ieduc, levels = c("less high school", "high school",
                                       "some college", "college"))]
table(ldat$ieduc, useNA = "ifany")

# race
ldat[, race := fct_relevel(race, c("non-hispanic/non-black", "black", "hispanic"))]
table(ldat$race, useNA = "ifany")

# gender
table(ldat$gender, useNA = "ifany")
ldat[, male := ifelse(gender == 1, 1, 0)]
table(ldat$male, useNA = "ifany")

# parent's education
ldat[pedu %in% 0:11, peduc := "less high school"]
ldat[pedu == 12, peduc := "high school"]
ldat[pedu %in% 13:15, peduc := "some college"]
ldat[pedu > 15, peduc := "college"]

ldat[, peduc := factor(peduc, levels = c("less high school", "high school",
                                       "some college", "college"))]
table(ldat$peduc, useNA = "ifany")

table(ldat[, .(peduc, ieduc)])

# respondents
(tot <- length(unique(ldat[, id]))) # 12619

summary(ldat$wt)
hist(ldat$wt)

# time between release and death

# time between death and last imprisonment recorded (descriptives)

ldat[id == sample(unique(ldat$id), 1), .(id, year, agei, tprison)]

ldat[, temp_prison := ifelse(is.na(tprison), 0, tprison)]
summary(ldat$temp_prison)

setorder(ldat, -year, id)
ldat[, cum_temp_prison := cumsum(temp_prison), id]
ldat[, anyprison := ifelse(sum(tprison, na.rm = TRUE) > 0, 1, 0), id]
ldat[, anydeath := ifelse(sum(died, na.rm = TRUE) > 0, 1, 0), id]
setorder(ldat, year, id)

table(ldat[start == 0, .(anyprison, anydeath)]) # 81


ldat[, cum_temp_prison := ifelse(cum_temp_prison > 0, 0, 1)]



ldat[id == sample(unique(ldat$id), 1), .(id, year, agei, prison)]
summary(ldat$rprison)
summary(ldat$tprison)


x <- ldat[anyprison == 1 & anydeath == 1,
  .(N = sum(cum_temp_prison)), .(id, male)]

ldat[id == 7461, .(id, year, agei, died,
  tprison, cum_temp_prison)]
table(x$male)

summary(x$N)
table(x$N, useNA = "ifany")
hist(x$N, breaks = 12)

ids <- x[N < 5, id]
nrow(x[N < 5])/nrow(x) # 42%
summary(ldat[id %in% ids, .(age = max(agei)), id]$age)

ids <- x[male == 0, id]
summary(x[id %in% ids, N])

ids <- x[, id]
summary(ldat[id %in% ids, .(age = max(agei)), id]$age)

y  <- ldat[id %in% ids, .(age = max(agei)), id]$age
quantile(y, c(0.25, 0.5, 0.75))
summary(ldat[anyprison == 1, agei])

#########################
#+ survival analysis
#########################

summary(ldat$cdeltot)

vars <- c("id", "prison", "cage", "male", "race", "ieduc",
          "liinc", "start", "ijob", "imarried",
           "stop", "agei", "year", "died", "dropout", "peduc",
           "deltot", "wt", "ihealthw")

countmis(ldat[, vars, with = FALSE])

set1 <- ldat[complete.cases(ldat[, vars, with = FALSE]), ]

table(set1[, min(stop), by = id][, V1])
table(set1[, max(stop), by = id][, V1])
table(set1[, max(start), by = id][, V1])
table(set1[, min(start), by = id][, V1])

ldat[id == 12253, .(id, year, agei, iprison, ijob, ieduc, liinc, imarried, ihealthw)]

# descriptives for set1

length(unique(set1$id)) # 11372

wpct(set1[start == 0, male])
wpct(set1[start == 0, race])

#######################
# models
########################

m1.1 <- coxph( Surv(start, stop, died) ~ prison + as.factor(cage) +
              male + race +  liinc + ieduc + peduc + deltot
              + ijob + imarried + cluster(id),
              data = set1)


ds <- svydesign(id = ~ cluster, weights = ~ wt, strata=~stratum, data = set1, nest = TRUE)

m1.2 <- svycoxph( Surv(start, stop, died) ~ prison + as.factor(cage) +
              male + race +  liinc + ieduc + peduc + deltot
              + ijob + imarried + cluster(id),
               design = ds)


# plus health
m2.1 <- coxph( Surv(start, stop, died) ~ prison + as.factor(cage) +
              male + race +  liinc + ieduc + peduc + deltot
              + ijob + imarried + ihealthw + cluster(id),
              data = set1)

m2.2 <- svycoxph( Surv(start, stop, died) ~ prison + as.factor(cage) +
              male + race +  liinc + ieduc + peduc + deltot
              + ijob + imarried + ihealthw + cluster(id),
               design = ds)


# marginal structural model

temp1 <- ipwtm(exposure = dropout, family = "survival",
              numerator = ~ male + as.factor(cage) + race + peduc + deltot,
              denominator = ~ male + as.factor(cage) + race + peduc + deltot + liinc +
              ieduc + ijob + imarried + prison,
              id = id,
              tstart = start, timevar = stop,
              type = "first",
              data = set1)

temp2 <- ipwtm(exposure = prison, family = "survival",
              numerator = ~ male + as.factor(cage) + race + peduc + deltot,
              denominator = ~ male + as.factor(cage) + race + peduc + deltot + liinc +
              ieduc + ijob + imarried,
              id = id,
              tstart = start, timevar = stop,
              type = "first",
              data = set1)


summary(temp1$ipw.weights)
summary(temp2$ipw.weights)

# create weigths
set1[, iwt := temp1$ipw.weights * temp2$ipw.weights]
set1[, nwt := wt * temp1$ipw.weights * temp2$ipw.weights]

# only MSM weights
m3.1 <- coxph( Surv(start, stop, died) ~ prison +  male + as.factor(cage) + race
              + cluster(id),
              data = set1, weights = set1$iwt)

summary(m3.1)
# sampling and MSM weights
ds <- svydesign(id = ~ cluster, weights = ~ nwt, strata = ~stratum, data = set1, nest = TRUE)

m3.2 <- svycoxph( Surv(start, stop, died) ~ prison + male + as.factor(cage) + race + cluster(id),
              design = ds)

screenreg(list(m3.1, m3.2), include.zph = FALSE)

# add health

temp1 <- ipwtm(exposure = dropout, family = "survival",
              numerator = ~ male + as.factor(cage) + race + peduc + deltot,
              denominator = ~ male + as.factor(cage) + race + peduc + deltot + liinc +
              ieduc + ijob + imarried + prison + ihealthw,
              id = id,
              tstart = start, timevar = stop,
              type = "first",
              data = set1)

temp2 <- ipwtm(exposure = prison, family = "survival",
              numerator = ~ male + as.factor(cage) + race + peduc + deltot,
              denominator = ~ male + as.factor(cage) + race + peduc + deltot + liinc +
              ieduc + ijob + imarried + ihealthw,
              id = id,
              tstart = start, timevar = stop,
              type = "first",
              data = set1)


summary(temp1$ipw.weights)
summary(temp2$ipw.weights)

# create weigths
set1[, iwt := temp1$ipw.weights * temp2$ipw.weights]
set1[, nwt := wt * temp1$ipw.weights * temp2$ipw.weights]


# only MSM weights
# only MSM weights
m4.1 <- coxph( Surv(start, stop, died) ~ prison + as.factor(cage) +
              male + race + cluster(id),
              data = set1, weights = set1$iwt)

# sampling and MSM weights
ds <- svydesign(id = ~ cluster, weights = ~ nwt, strata = ~stratum, data = set1, nest = TRUE)

m4.2 <- svycoxph( Surv(start, stop, died) ~ prison + as.factor(cage) +
              male + race + cluster(id),
              design = ds)

# tables with results
screenreg(list(m1.1, m1.2, m2.1, m2.2, m3.1, m3.2, m4.1, m4.2), include.zph = FALSE)

name.map  <- list(prison = "Prison",
                 male = "Male",
                 raceblack = "Black",
                 racehispanic = "Hispanic",
                 # deltot = "Delinquency 1980",
                 # "peduchigh school" = "High school",
                 # "peducsome college" = "Some college",
                 # "peduccollege" = "College",
                 liinc = "Log Income, centered",
                 "ieduchigh school" = "High school",
                 "ieducsome college" = "Some college",
                 "ieduccollege" = "College",
                 ijob = "Working",
                 imarried = "Married",
                 ihealthw = "Poor health")

length(name.map)

models_unw <- list(m1.1, m3.1, m2.1, m4.1)
models_w <- list(m1.2, m3.2, m2.2, m4.2)

screenreg(models_unw, include.zph = FALSE)
screenreg(models_w, include.zph = FALSE)

# create tables (without stars!!!)

texreg(models_unw, include.rsquared = FALSE, include.aic = FALSE,
    include.maxrs = FALSE, include.events = TRUE, include.nobs = TRUE,
    include.missings = FALSE, include.zph = FALSE,
    stars = 0,
    custom.gof.names =c("Deaths", "Person-years"),
    custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
    groups = list("Race (ref. Non-Hispanics/Blacks)" = 3:4, "Education (ref. $<$ HS)" = 6:8),
    custom.coef.map = name.map,
    custom.note = "Robust standard errors in parenthesis. Age, Delinquency at 1980 and Parent's education coefficient omitted.",
    booktabs = TRUE,
    dcolumn = TRUE,
    use.packages = FALSE,
    label = "models_nlsy_1",
    caption = "Cox Survival Models on the effect of Imprisonment on Mortality, \\newline Unweighted, NLSY79 1980-2014",
    caption.above = TRUE,
    fontsize = "scriptsize",
    float.pos = "htp",
    file = paste0(path, "tables/models_nlsy_1.tex")
    )

texreg(models_w, include.rsquared = FALSE, include.aic = FALSE,
    include.maxrs = FALSE, include.events = TRUE, include.nobs = TRUE,
    include.missings = FALSE, include.zph = FALSE,
    stars = 0,
    custom.gof.names =c("Deaths", "Person-years"),
    custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
    groups = list("Race (ref. Non-Hispanics/Blacks)" = 3:4, "Education (ref. $<$ HS)" = 6:8),
    custom.coef.map = name.map,
    custom.note = "Robust standard errors in parenthesis. Age, Delinquency at 1980 and Parent's education coefficient omitted.",
    booktabs = TRUE,
    dcolumn = TRUE,
    use.packages = FALSE,
    label = "models_nlsy_2",
    caption = "Cox Survival Models on the effect of Imprisonment on Mortality, \\newline Weighted, NLSY79 1980-2014",
    caption.above = TRUE,
    fontsize = "scriptsize",
    float.pos = "htp",
    file = paste0(path, "tables/models_nlsy_2.tex")
    )

# plots

# newdata1 <- data.frame(iprison = 1,
#                       cage = 0,
#                       male = 1,
#                       fracei = "Black" ,
#                       liinc = 0,
#                       ieduc = "less high school")

# newdata2 <- data.frame(iprison = 0,
#                       cage = 0,
#                       male = 1,
#                       fracei = "Black" ,
#                       liinc = 0,
#                       ieduc = "less high school")

# plot1 <- getComparisonPlot(m1.1, data1 = newdata1, data2 = newdata2,
#                   pos1 = c(25, 42), pos2 = c(0.3, 0.09))


# TODO: create descriptive tables

####################################
####################################
