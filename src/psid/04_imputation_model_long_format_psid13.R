##############################
# health and incarceration
# imputation models
# author: sebastian daza
##############################

# libraries
library(sdazar)
library(foreach)
library(doParallel)
library(survival)
library(forcats)
library(ipw)
library(survey)

# load data
ldat <- readRDS("output/psid_long_format_covariates_year_13.rds")

# linstat
# library(here)
# source("functions.R")
# load("long_selection_covariates_year_unit_13.Rdata")

# number of cores
number_cores <- detectCores()
number_imputations <- 100

#################################
# start imputation procedure
##################################

# initial checks
setkey(ldat, year, pid)
summary(ldat$year)

# remove cases with previous covariate to imprisonment
ldat <- ldat[select == 1]
nrow(ldat)

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

summary(ldat$cagei)
table(ldat$cagei)

# categories of education
ldat[iedu %in% 1:11, ieduc := "less high school"]
ldat[iedu == 12, ieduc := "high school"]
ldat[iedu %in% 13:15, ieduc := "some college"]
ldat[iedu > 15, ieduc := "college"]

ldat[, ieduc := factor(ieduc, levels = c("less high school", "high school",
                                       "some college", "college"))]
table(ldat$ieduc)

# race
table(ldat$racei)
ldat[, fracei := factor(racei, labels = c("White", "Black", "Other"))]
table(ldat$fracei)

table(ldat$ieduc)

# gender
ldat[, male := gender]

# respondents
(tot <- length(unique(ldat[, pid]))) # 54046
(s <- length(unique(ldat[smember == 1, pid])))
(ns <- length(unique(ldat[smember == 0, pid])))
s / tot # 60%
ns / tot # 40%

summary(ldat$iwt)
summary(ldat$fwt)
length(unique(ldat[is.na(fwt), pid])) # 279 cases

# remove cases without sampling weight
ldat <- ldat[!is.na(fwt)]
nrow(ldat)

# create time variable
table(ldat[, any18])

anyDuplicated(ldat[, family_id])
summary(ldat$agei)

table(ldat[, min(stop), by = pid][, V1])
table(ldat[, max(stop), by = pid][, V1])
table(ldat[, max(start), by = pid][, V1])
table(ldat[, min(start), by = pid][, V1]) # right!


table(ldat[, .(year, nrprison)])
summary(ldat$start)
summary(ldat$stop) # 48

ids <- sample(unique(ldat$pid), 1)
ldat[pid == ids, .(pid, response, agei, year, start, stop, linca)]

table(ldat$male)
table(ldat$fracei)
table(ldat$myear)
table(ldat$response)
table(ldat$year)

# create variables for imputation
ldat[, cyear := year - 1990]
ldat[, cyear2  := cyear ^ 2]
ldat[, cagei := scale(agei, center = TRUE, scale = FALSE)]
ldat[, cagei2 := cagei ^ 2 ]

ex <- ldat[, .(pid, whynr, response, smember, itrack, male, year, cyear, cyear2,
               start, stop, nrprison, prison95, ydeath, died, dropout, death, myear, agei,
               cagei, cagei2, cage, dghealth, linca, fracei, iedu, family_id,
               stratum, cluster, fwt)]

# ex[, maxage := max(agei), by = pid]
ex[, adropout := max(dropout), by = pid]
countmis(ex) # max 38%
length(unique(ex$pid)) # 18 years or older, 52050, I use all the data to impute

##################################
# some descriptives for paper
##################################

temp <- copy(ex)
temp[, select95 := 0]
temp[ydeath >= 1995 | death == 0, select95 := 1]
table(temp$select95)
temp <- temp[select95 == 1]

length(unique(temp$pid))
length(temp[start==0, pid]) # same as above number
min(temp$start)
max(temp$start)

# gender
prop.table(table(temp[start == 0, male]))

# age start
mean(temp[start == 0, agei])
median(temp[start == 0, agei])

# prison
temp[, tprison := pmax(nrprison, prison95, na.rm = TRUE)]
table(temp$tprison)
table(temp[, .(prison = any(tprison == 1), died = any(died == 1)), pid][, .(prison, died)])

# average exposure
mean(temp[, .(exposure = max(stop)), by = pid][, exposure])
median(temp[, .(exposure = max(stop)), by = pid][, exposure])

# race
prop.table(table(temp[start == 0, fracei]))

# deaths
sum(temp$died)

# release descriptives

# time between death and last imprisonment recorded
temp[, tprison95 := cumsum(prison95), pid]
temp[, tprison95 := ifelse(tprison95 == 1, 1, 0)]

# time between death and last imprisonment recorded
setorder(temp, year, pid)
temp[, nrprison := ifelse(is.na(nrprison), 0, nrprison)]
table(temp$tprison95)
table(temp$nrprison)

temp[, temp_prison := pmax(tprison95, nrprison, na.rm = TRUE)]
summary(temp$temp_prison)
table(temp[, temp_prison])

setorder(temp, -year, pid)
temp[, cum_temp_prison := cumsum(temp_prison), pid]
temp[, anyprison := ifelse(sum(temp_prison, na.rm = TRUE) > 0, 1, 0), pid]
temp[, anydeath := ifelse(sum(died, na.rm = TRUE) > 0, 1, 0), pid]
setorder(temp, year, pid)

table(temp[start == 0, .(anyprison, anydeath)]) # 94
temp[, cum_temp_prison := ifelse(cum_temp_prison > 0, 0, 1)]

x <- temp[anyprison == 1 & anydeath == 1, .(N = sum(cum_temp_prison), age = max(agei)), .(pid, male)]

temp[pid == 2083004, .(pid, year, prison95, tprison95, nrprison,
                      died, temp_prison, cum_temp_prison)]

# get some descriptives for paper
summary(x$N) # 14
table(x$N)
hist(x$N, breaks = 12)
summary(x$age)
nrow(x[N < 5])/nrow(x) # 22%
summary(x[N < 5, age])

# remove temp objects
remove(x, temp)
ex[, ydeath := NULL]

########################################
# to use mice for imputing
########################################

imp <- mice(ex, maxit = 0)
mat <- imp$predictorMatrix
meth <- imp$meth

meth
mat

# define matrix using fixed effects and specific methods
mat[,] <- 0

# random variables

# individual identifier
mat[c("fracei", "nrprison", "iedu", "linca", "dghealth"), "pid"] <- -2 # cluster variable

# model per variable
mat["fracei", c("male", "cage", "myear", "died", "dropout", "linca",
               "iedu", "nrprison", "prison95")] <- 1

which(mat["fracei", ] != 0)

mat["linca", c("male", "fracei", "cyear", "cyear2", "cagei", "cagei2",
               "nrprison", "prison95", "dghealth", "died", "dropout", "iedu")] <- 1

which(mat["linca", ] != 0)

mat["iedu", c("male", "fracei", "cyear", "cyear2", "cagei", "cagei2", "nrprison", "prison95",
               "dghealth", "died", "dropout", "linca")] <- 1

which(mat["iedu", ] != 0)

mat["nrprison", c("male", "fracei", "prison95", "cyear", "cyear2", "cagei", "cagei2",
                  "iedu", "dghealth",  "death", "adropout", "linca")] <- 1
which(mat["nrprison", ] != 0)

mat["dghealth", c("male", "fracei", "cyear", "cyear2", "cagei", "cagei2", "iedu",
                  "nrprison", "prison95", "died", "dropout", "linca")] <- 1
which(mat["dghealth", ] != 0)

countmis(ex)
meth["linca"] <- "2l.pmm"
meth["iedu"] <- "2l.pmm"
meth["fracei"] <- "2lonly.pmm"
meth["dghealth"] <- "2l.pmm"
meth["nrprison"] <- "2l.pmm"

# imputation and analysis

coeff_wt <- list()
vcov_wt <- list()

coeff_h_wt <- list()
vcov_h_wt <- list()

coeff_wt_msm <- list()
vcov_wt_msm <- list()

coeff_h_wt_msm <- list()
vcov_h_wt_msm <- list()

coeff_uwt <- list()
vcov_uwt <- list()

coeff_h_uwt <- list()
vcov_h_uwt <- list()

coeff_uwt_msm <- list()
vcov_uwt_msm <- list()

coeff_h_uwt_msm <- list()
vcov_h_uwt_msm <- list()

# create imputation, no seeds
# chunks of 10 imputations

max_value_loop <- ceil(number_imputations / number_cores)

for (i in 1:max_value_loop) {

    print(paste0("Imputing chunk ", i))

    temp_imp <- parmice(data = ex, predictorMatrix = mat, method = meth,
                  maxit = 20, n.core = number_cores, n.imp.core = 1)

    tdat <- data.table(complete(temp_imp, "long", inc = FALSE))
    setkey(tdat, .imp, pid, start)
    nimp <- max(tdat[, as.numeric(as.character(.imp))])

    # create cumulative prison variable
    tdat[, cprison := cumsum(nrprison), by = .(.imp, pid)][, prison := ifelse(cprison > 0, 1, 0)]
    tdat[, gprison := pmax(prison, prison95)] # prison 95

    table(tdat$gprison)

    # recode education
    tdat[iedu %in% 1:11, ieduc := "less high school"]
    tdat[iedu == 12, ieduc := "high school"]
    tdat[iedu %in% 13:15, ieduc := "some college"]
    tdat[iedu > 15, ieduc := "college"]
    tdat[, ieduc := fct_relevel(ieduc, c("less high school", "high school", "some college", "college"))]

    print(paste0("Processing imputations chunk ", i))

    # models
    for (j in 1:nimp) {
        t <- tdat[.imp == j]
        summary(t$fwt)
        print(paste0("Models from imputation ", j))

        # standard
        ds <- svydesign(id = ~cluster, weights = ~fwt, strata=~stratum, data = t, nest = TRUE)
        temp <- svycoxph(Surv(start, stop, died) ~ gprison + male + cagei + fracei +
                         + linca + ieduc + cluster(pid), design = ds)

        if ( file.exists("output/models_wt.Rdata") ) { load("output/models_wt.Rdata") }
        coeff_wt <- c(coeff_wt, list(coefficients(temp)))
        vcov_wt <- c(vcov_wt, list(vcov(temp)))
        save(coeff_wt, vcov_wt, file = "output/models_wt.Rdata")
        remove(temp)

        temp <- svycoxph( Surv(start, stop, died) ~ gprison + male + cagei + fracei +
                               + linca + ieduc + dghealth + cluster(pid), design = ds)

        if ( file.exists("output/models_h_wt.Rdata") ) { load("output/models_h_wt.Rdata") }
        coeff_h_wt <- c(coeff_h_wt, list(coefficients(temp)))
        vcov_h_wt <- c(vcov_h_wt, list(vcov(temp)))
        save(coeff_h_wt, vcov_h_wt, file = "output/models_h_wt.Rdata")
        remove(temp)

        # msm
        w1 <- ipwtm(exposure = dropout, family = "survival",
                    numerator = ~ male + fracei + cagei,
                    denominator = ~  gprison + male + cagei + fracei + linca + ieduc,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)
        w2 <- ipwtm(exposure = gprison, family = "survival",
                    numerator = ~  male + fracei + cagei,
                    denominator = ~  male + cagei + fracei + linca + ieduc,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)

        t[, nwt := fwt * w1$ipw.weights * w2$ipw.weights]
        ds <- svydesign(id = ~cluster, weights = ~nwt, strata = ~stratum, data = t, nest = TRUE)
        temp <- svycoxph(Surv(start, stop, died) ~ gprison + male
                         + cagei + fracei + cluster(pid), design = ds)

        if ( file.exists("output/models_wt_msm.Rdata") ) { load("output/models_wt_msm.Rdata") }
        coeff_wt_msm <- c(coeff_wt_msm, list(coefficients(temp)))
        vcov_wt_msm <- c(vcov_wt_msm, list(vcov(temp)))
        save(coeff_wt_msm, vcov_wt_msm, file = "output/models_wt_msm.Rdata")
        remove(temp)

        w1 <- ipwtm(exposure = dropout, family = "survival",
                    numerator = ~ male + fracei + cagei,
                    denominator = ~  gprison + male + cagei + fracei + linca + ieduc + dghealth,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)
        w2 <- ipwtm(exposure = gprison, family = "survival",
                    numerator = ~  male + fracei + cagei,
                    denominator = ~  male + cagei + fracei + linca + ieduc + dghealth,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)

        t[, nwt := fwt * w1$ipw.weights * w2$ipw.weights]
        ds <- svydesign(id = ~cluster, weights = ~nwt, strata = ~stratum, data = t, nest = TRUE)
        temp <- svycoxph(Surv(start, stop, died) ~ gprison + male
                         + cagei + fracei + cluster(pid), design = ds)

        if ( file.exists("output/models_h_wt_msm.Rdata") ) { load("output/models_h_wt_msm.Rdata") }
        coeff_h_wt_msm <- c(coeff_h_wt_msm, list(coefficients(temp)))
        vcov_h_wt_msm <- c(vcov_h_wt_msm, list(vcov(temp)))
        save(coeff_h_wt_msm, vcov_h_wt_msm, file = "output/models_h_wt_msm.Rdata")
        remove(temp)

        # unweighted models

        temp <- coxph( Surv(start, stop, died) ~ gprison + male + cagei + fracei +
                            + linca + ieduc + cluster(pid), data = t)

        if ( file.exists("output/models_uwt.Rdata") ) { load("output/models_uwt.Rdata") }
        coeff_uwt <- c(coeff_uwt, list(coefficients(temp)))
        vcov_uwt <- c(vcov_uwt, list(vcov(temp)))
        save(coeff_uwt, vcov_uwt, file = "output/models_uwt.Rdata")
        remove(temp)

        temp <- coxph(Surv(start, stop, died) ~ gprison + male + cagei + fracei +
                            + linca + ieduc + dghealth + cluster(pid), data = t)

        if ( file.exists("output/models_h_uwt.Rdata") ) { load("output/models_h_uwt.Rdata") }
        coeff_h_uwt <- c(coeff_h_uwt, list(coefficients(temp)))
        vcov_h_uwt <- c(vcov_h_uwt, list(vcov(temp)))
        save(coeff_h_uwt, vcov_h_uwt, file = "output/models_h_uwt.Rdata")
        remove(temp)

        # msm
        w1 <- ipwtm(exposure = dropout, family = "survival",
                    numerator = ~ male + fracei + cagei,
                    denominator = ~  gprison + male + cagei + fracei + linca + ieduc,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)
        w2 <- ipwtm(exposure = gprison, family = "survival",
                    numerator = ~  male + fracei + cagei,
                    denominator = ~  male + cagei + fracei + linca + ieduc,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)

        t[, nwt := w1$ipw.weights * w2$ipw.weights]
        temp <- coxph(Surv(start, stop, died) ~ gprison + male
                      + cagei + fracei + cluster(pid), data = t, weights = t$nwt)

        if ( file.exists("output/models_uwt_msm.Rdata") ) { load("output/models_uwt_msm.Rdata") }
        coeff_uwt_msm <- c(coeff_uwt_msm, list(coefficients(temp)))
        vcov_uwt_msm <- c(vcov_uwt_msm, list(vcov(temp)))
        save(coeff_uwt_msm, vcov_uwt_msm, file = "output/models_uwt_msm.Rdata")
        remove(temp)

        w1 <- ipwtm(exposure = dropout, family = "survival",
                    numerator = ~ male + fracei + cagei,
                    denominator = ~  gprison + male + cagei + fracei + linca + ieduc + dghealth,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)
        w2 <- ipwtm(exposure = gprison, family = "survival",
                    numerator = ~  male + fracei + cagei,
                    denominator = ~  male + cagei + fracei + linca + ieduc + dghealth,
                    id = pid,
                    tstart = start, timevar = stop,
                    type = "first",
                    data = t)

        t[, nwt :=  w1$ipw.weights * w2$ipw.weights]
        temp <- coxph( Surv(start, stop, died) ~ gprison + male
                            + cagei + fracei + cluster(pid), data = t, weights = t$nwt)

        if ( file.exists("output/models_h_uwt_msm.Rdata") ) { load("output/models_h_uwt_msm.Rdata") }
        coeff_h_uwt_msm <- c(coeff_h_uwt_msm, list(coefficients(temp)))
        vcov_h_uwt_msm <- c(vcov_h_uwt_msm, list(vcov(temp)))
        save(coeff_h_uwt_msm, vcov_h_uwt_msm, file = "output/models_h_uwt_msm.Rdata")
        remove(temp)

        } # end loop by imputation

    print(paste0("Finishing chunk ", i))

} # end loop by chunks


################################
# end imputation
################################
