# preliminary models NLSY79
# selected variables

library(data.table)
library(sdazar)
library(survival)
library(texreg)
library(ipw)

setwd("/Users/sdaza/Google Drive/01Projects/01IncarcerationHealth/")
load("04Data/NLSY79/rdata/nlsy79_selection_longformat.RData")

table(dat[, death2012], useNA = "ifany")

sum(is.na(ldat$inc)) / nrow(ldat) # 20 percent
sum(is.na(ldat$pov)) / nrow(ldat) # 20 percent

table(ldat$pov, useNA = "ifany")
table(ldat$death)

table(ldat[, .(prison, prison1980)])

# create prison variable
setkey(ldat, id, year)
ldat[, cumprison := cumsum(prison), by = id]
ldat[id == sid, .(id, time, agei, prison, prison1980, release1980, cumprison)]
ldat[, inprison := ifelse(cumprison > 0, 1, 0)]
ldat[id == sid, .(id, time, agei, prison, prison1980,
                  release1980, cumprison, inprison)]

# recode some variables
ldat[, sex := ifelse(gender == 1, 1, 0)]

setkey(ldat, year, id)
ldat[, s := 1:.N, by = id]
ldat[s == 1, .(year, agei)]
ldat[, mage := Min(agei), by = id]
table(ldat$mage, useNA = "ifany")

# only survival respondents by 1980
table(ldat[year == 1979, ldeath], useNA = "ifany") # selection
removeids <- unique(ldat[ldeath == 1 & year == 1979, id])
ids <- unique(ldat[, id])

length(ids) - length(removeids)
ldat <- ldat[id %in% ids[-removeids]]
ldat <- ldat[agei >= 18] # 18 years or older
summary(ldat$agei)

# preliminary cox model

# define time
setkey(ldat, id, year)
ldat[, end := 0:.N, by = id]
ldat[, start := tstartfun("id", "end", ldat)]

m1 <- coxph(Surv(start, end, ldeath) ~ inprison + sex + agei + I(agei^2) +
            cluster(id), data = ldat)

m2 <- coxph(Surv(start, end, ldeath) ~ I(cumprison/10) + sex + mage
            + cluster(id), data = ldat)

km.by.sex <- survfit(Surv(start, end, ldeath) ~ sex, data = ldat,
                          conf.type = "log-log")

male <- ldat[sex == 1]
female <- ldat[sex == 0]

km.by.prison.male <- survfit(Surv(start, end, ldeath) ~ inprison, data = male ,
                          conf.type = "log-log")
km.by.prison.female <- survfit(Surv(start, end, ldeath) ~ inprison, data = female ,
                          conf.type = "log-log")


ggsurvplot(km.by.prison.male, risk.table = TRUE, fun = "cumhaz", conf.int = TRUE)
ggsurvplot(km.by.prison.female, risk.table = TRUE, fun = "cumhaz", conf.int = TRUE)


plot(km.by.prison)
plot(km.by.prison, fun = "cumhaz")

ggsurvplot(km.by.prison, risk.table = TRUE)

## Load rms package
library(simPH)
library(survminer)

library(rms)
survplot(km.by.sex)

m2 <- coxph(Surv(start, end, ldeath) ~ prison + sex + mage
            + cluster(id), data = ldat)


ggfitStrata(m2Fit, )

summary(m2)
summary(ldat$cumprison)

library(flexsurvreg)

m2 <- coxph(Surv(start, end, ldeath) ~ prison + sex + mage
            + cluster(id), data = ldat)

summary(m1)
summary(m2)
screenreg(m1)

length(unique(ldat$id))

# dropout
table(ldat$dropout, useNA = "ifany") # 2536 out of 12677, 20%
temp <- ipwtm(exposure = dropout, family = "survival",
  numerator = ~ sex + agei + as.factor(race),
  denominator = ~ inprison + sex + agei + as.factor(race),
  id = id,
  tstart = start, timevar = end, type = "first", data = ldat)

summary(temp$num.mod)
summary(temp$den.mod)

m2 <- coxph(Surv(start, end, ldeath) ~ inprison + sex + agei + as.factor(race) +
            cluster(id), data = ldat, weights = temp$ipw.weights)

summary(m2)

m3 <- coxph(Surv(start, end, ldeath) ~ inprison + sex + agei + as.factor(race) +
            cluster(id), data = ldat)

summary(m3)

m1 <- coxph(Surv(tstart, end, death) ~ sex  + mage inprison, data = ldat)
screenreg(m1)

names(dat)
table(duplicated(dat$caseid_1979))

# years
library(ipw)
nn <- as.data.table(haartdat)
table(nn[, .(event, dropout)])

summary(coxph(Surv(tstart, fuptime, event) ~ haartind + cluster(patient),
              data = haartdat))

temp <- ipwtm(exposure = haartind, family = "survival",
  numerator = ~ sex + age,
  denominator = ~ cd4.sqrt + sex + age,
  id = patient, tstart = tstart,
  timevar = fuptime, type = "first", data = haartdat)

temp$num.mod
temp$den.mod


ldat
summary(coxph(Surv(tstart, fuptime, event) ~ haartind + cluster(patient),
              data = haartdat, weights = temp$ipw.weights))

temp2 <- ipwtm(exposure = dropout, family = "survival",
  numerator = ~ sex + age,
  denominator = ~ cd4.sqrt + sex + age,
  id = patient, tstart = tstart, timevar = fuptime, type = "first",
  data = haartdat)

temp2$num.mod
temp2$den.mod

summary(coxph(Surv(tstart, fuptime, event) ~ haartind + cluster(patient),
              data = haartdat, weights = temp$ipw.weights*temp2$ipw.weights))


years <- c(1979:1994, seq(1996,2012, 2))

# rename variables
setnames(dat, "caseid_1979", "id")
setnames(dat, "sample_race_78scrn", "race")
setnames(dat, "sample_sex_1979", "gender")

summary(dat$id)
table(dat$gender, useNA = "ifany")
table(dat$race, useNA = "ifany")

# age
vars <- lookvar(dat, "age")
length(vars)
nvars <- paste0("age", years)
length(nvars)
setnames(dat, vars, nvars)

table(dat$age1979)
table(dat$age2012)

# type of residence
vars <- lookvar(dat, "hh1")
nvars <- paste0("resid", years)
setnames(dat, vars, nvars)

table(dat$resid1979, useNA = "ifany")

# ever incarcerated 1980
setnames(dat, "police-7_1980", "prison1980")
dat[, prison1980 := ifelse(is.na(prison1980), 0, prison1980)]
table(dat$prison1980, useNA = "ifany") # impute NA to 0

# last released (year)
setnames(dat, "police-7c_y_1980", "release1980")
table(dat$release1980, useNA = "ifany")


# code of jail / prison
jail <- c(3, rep(5, 24)) # different coding during first year
resid <- lookvar(dat, "resid")

for (i in seq_along(years)) {
  dat[, paste0("jail", years[i]) := ifelse(get(resid[i]) == jail[i], 1, 0)]
}

table(dat$resid2004, useNA = "ifany" )
table(dat$jail2004, useNA = "ifany")

table(dat$resid1979, useNA = "ifany" )
table(dat$jail1979, useNA = "ifany" )

jail <- paste0("jail", years)

dat[ , (jail) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
     .SDcols = jail]

table(dat$jail1979, useNA = "ifany")
table(dat$jail2004, useNA = "ifany")

dat[, anyjail := apply(.SD, 1, function(x) as.numeric(any(x == 1))), .SDcols = jail]
table(dat$anyjail, useNA = "ifany") # 693

# deaths
rni <- lookvar(dat, "rni")
length(rni)
death <- paste0("death", years[-1])
length(death)

dat[, (death) := lapply(.SD, function(x) ifelse(x == 65, 1, 0)), .SDcols = rni]
dat[, (death) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
	.SDcols = death]

table(dat$death1980, useNA = "ifany")
table(dat$death2004, useNA = "ifany")

dat[, anydeath := apply(.SD, 1, function(x) as.numeric(any(x == 1))), .SDcols = death]
table(dat$anydeath, useNA = "ifany") # 715, higher than jail

# incarcerated
rni <- lookvar(dat, "rni")
length(rni)
incarcerated <- paste0("incarcerated", years[-1])
length(incarcerated)

dat[, (incarcerated) := lapply(.SD, function(x) ifelse(x == 73, 1, 0)), .SDcols = rni]
dat[, (incarcerated) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
	.SDcols = incarcerated]

table(dat$incarcerated1980, useNA = "ifany")
table(dat$incarcerated2004, useNA = "ifany")
table(dat$incarcerated2006, useNA = "ifany")

table(dat[, .(incarcerated2006, jail2006)])
table(dat[, .(incarcerated2008, jail2008)])
table(dat[, .(incarcerated2010, jail2010)])
table(dat[, .(incarcerated2012, jail2012)])

table(dat[, .(resid2012, rni_2012)], useNA = "ifany")

dat[, anyincarcerated := apply(.SD, 1, function(x) as.numeric(any(x == 1))), .SDcols = incarcerated]

table(dat$anyincarcerated, useNA = "ifany") # 130, not a small number

# censored
# rni 68, 69, 70
rni <- lookvar(dat, "rni")
length(rni)
censored <- paste0("censored", years[-1])
length(censored)

dat[, (censored) := lapply(.SD, function(x) ifelse(x %in% 68:70, 1, 0)), .SDcols = rni]
dat[, (censored) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
	.SDcols = censored]

table(dat$censored2004)
table(dat$censored2012)

# non-response
rni <- lookvar(dat, "rni")
length(rni)
nonresponse <- paste0("nonresponse", years[-1])
length(nonresponse)

values <- c(60:64, 66:67, 71:72, 74)

dat[, (nonresponse) := lapply(.SD, function(x) ifelse(x %in% values, 1, 0)), .SDcols = rni]
dat[, (nonresponse) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
	.SDcols = nonresponse]

table(dat$nonresponse1980)
table(dat$nonresponse2012)

# poverty
pov <- lookvar(dat, "pov")
nvars <- paste0("pov", years)
setnames(dat, pov, nvars)

table(dat$pov1979, useNA = "ifany")
table(dat$pov2004, useNA = "ifany")
table(dat$pov2012, useNA = "ifany")

# income
inc <- lookvar(dat, "tnfi")
nvars <- paste0("inc", years)
setnames(dat, inc, nvars)
summary(dat$inc1979)

# set long format

# create null variables
vars <- c("nonresponse1979", "incarcerated1979", "censored1979", "death1979")
dat[, (vars)  := 0]

age <- paste0("age", years)
jail <- paste0("jail", years)
inc <- paste0("inc", years)
pov <- paste0("pov", years)
nonresponse <- paste0("nonresponse", years)
incarcerated <- paste0("incarcerated", years)
censored <- paste0("censored", years)
death <- paste0("death", years)

length(nonresponse)
length(incarcerated)
length(censored)
length(death)
length(age)
length(jail)
length(inc)
length(pov)

vars <- c("id", "gender", "race", "anydeath", "prison1980", "release1980", nonresponse, censored, incarcerated, death, age, jail, inc, pov)

vars
wdat <- copy(dat[, vars, with = FALSE])
names(wdat)
dim(wdat) # 12686

# patterns and variables
setkey(wdat, id)
summary(wdat$id)

# same order
patt <- c("^nonresponse", "^censored", "^incarcerated", "^death",
	 "^age", "^jail", "^inc[0-9]*$", "^pov")
vnames <- c("nonresponse", "censored", "incarcerated", "death",
	"age", "jail", "inc", "pov")



# to avoid melt warnings
length(patt) == length(vnames)

vars <- c("id", "gender", "race", "anydeath", "prison1980", "release1980")
ldat <- data.table::melt(wdat, id.vars = vars,
			 measure.vars = patterns(patt),
             value.name = vnames,
             variable.name = "time")

for (i in seq_along(years)) {
  ldat[time == i, year := years[i]]
}

ldat[id == 2]
?melt
ldat

# explore
ids <- unique(ldat$id)
sid <- sample(ids, 1)
ldat[id == sid, .(id, year, age, nonresponse, censored, incarcerated, death, inc)]

# censored
setkey(ldat, id, year)
ldat[, cumcensored := cumsum(censored), by = id]
ldat[, tcensored := max(censored), by = id]
ldat[id == sid, .(id, year, age, nonresponse, cumcensored, tcensored, censored, incarcerated, death)]

ldat <- ldat[cumcensored < 1]

# death
ldat[, cumdeath := cumsum(death), by = id]
table(ldat$death)
sid <- sample(ids, 1)
ldat[id == sid, .(id, death, cumdeath)]

ldat <- ldat[cumdeath < 2]
table(ldat$cumdeath, useNA = "ifany") # 715
table(ldat$death, useNA = "ifany") # 715

# dropouts (nonresponse)
setorder(ldat, -year)
ldat[, nonresponse := ifelse(death == 1, 0, nonresponse)]
ldat[, invnonresponse := ifelse(nonresponse == 1, 0, 1)]
ldat[, cumdrop := cumsum(invnonresponse), by = id]
ldat[, tnonresponse := max(nonresponse), by = id]

sid <- sample(ids, 1)
ldat[id == sid, .(id, year, age, death, cumdrop, nonresponse)]

table(ldat$cumdrop)

ldat <- ldat[cumdrop > 0]

sid <- sample(ids, 1)
ldat[id == sid, .(id, year, age, death, cumdrop, nonresponse)]

ldat[, dropout := 0]
ldat[, s := 1:.N, by = id]
ldat[, dropout := ifelse(s == 1
	& year < 2012 & tcensored == 0, 1, dropout)]

length(unique(ldat[age >= 24 & age <= 47, ids]))
table(ldat[age >= 24 & age <= 47, dropout])
1424 /  12686 # 20% versus 10%

sid <- sample(ids, 1)
ldat[id == sid, .(id, year, age, death, tcensored, dropout)]

prop.table(table(ldat[, .(gender, death)])) # higher among men

# incarcerated
setkey(ldat, id, year)
ldat[, tincarcerated := max(incarcerated), by = id]

table(ldat$incarcerated, useNA = "ifany")

ids <- unique(ldat[tincarcerated == 1, id])
sid <- sample(ids, 1)
ldat[id == sid, .(id, year, age, death, tcensored, dropout,
	nonresponse, jail, incarcerated)]

# prison variable (just combine both records)

names(ldat)
ldat[, prison := apply(.SD, 1, max), .SDcols = c("jail", "incarcerated")]
table(ldat$prison)
table(ldat$death)

ldat[id == sid, .(id, year, age, death, tcensored, dropout,
	nonresponse, jail, incarcerated, prison)]

# impute age
setkey(ldat, id, year)

impute.age <- function(age, year) {
  min.age <- Min(age[age > 0 & age < 999])
  min.year <- year[ which (age == min.age)][1]  # some ages are tied
  nage <- age
  nage[nage == 0 | nage == 999] <- NA
  for (i in seq_along(nage)) {
    if (is.na(nage[i]) & !is.na(min.age)) {
      nage[i] <- min.age + (year[i] - min.year)
    }
  }
return(nage)
}

ldat[, age := as.numeric(age)]
ldat[, agei := impute.age(age, year), by = id]

sid <- sample(ids, 1)
ldat[id == sid, .(id, year, age, agei, death, tcensored, dropout,
	nonresponse, inc, jail, incarcerated, prison)]

# save data
save(ldat, dat, file = "nlys79_selection_longformat.RData")


