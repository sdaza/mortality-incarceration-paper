##########################
# load and processing NLSY79
#########################

# library
library(sdazar)
library(ggplot2)
library(zoo)

# load data

dat <- readRDS("data/nlsy79selection.rds")
nrow(dat)
wt <- readRDS("data/nlsy79weights.rds")

# dat <- readRDS("data/nlsy79selection_missing.rds")
# newdata <- readRDS("data/type_residence.rds")
# setkey(dat, caseid_1979)
# setkey(newdata, caseid_1979)
# dat = newdata[dat]
# saveRDS(dat, "data/nlsy79selection.rds")

####################
# define variables
####################

# identifiers
lookvar(dat, "id")
setnames(dat, "caseid_1979", "id")

summary(dat$id)
anyDuplicated(dat[, id])

setnames(dat, "sample_id_1979", "type")
table(dat$type)

years = c(1979:1994, seq(1996,2014, 2))

# merge weights
setkey(wt, id)
setkey(dat, id)
nrow(dat)

dat <- wt[dat]
nrow(dat)

# rename some demographic variables
setnames(dat, "sample_race_78scrn", "race")
setnames(dat, "sample_sex_1979", "gender")

table(dat$gender)
table(dat$race)
dat[, sample := ifelse(type %in% 1:8, 1, ifelse(type %in% 9:14, 2, 3))]
table(dat[, .(sample)])

# age
vars <- lookvar(dat, "^age")
vars

length(vars)
nvars = paste0("age", years)
length(nvars)
setnames(dat, vars, nvars)
table(dat$age1979)
table(dat$age2014)

# missing data codes
# -1  Refused
# -2  Dont know
# -3  Invalid missing
# -4  Valid missing
# -5  Non-interview
# 1 to 4 are item non-response

# NA for non-interview cases
dat = dat[, lapply(.SD, function(x) ifelse(x == -5, NA, x))]
table(dat$age1979)
table(dat$age1980)
table(dat$age1981)

##################################################
# define variables and then change to long format
##################################################

# deaths ok
# incarceration ok
# age ok
# race ok
# gender ok
# parents' education ok max
# respondent's education ok
# welfare
# job
# income ok
# marriage ok
# health insurance (1984) ok
# health issues (job) ok

# substance use (1980)
# cigarette use
# delinquency (1980) ok
# violence
# locus of control
# criminal contact ok

# weights ok

# parents' education
lookvar(dat, "hgc")
setnames(dat, c("hgc-mother_1979", "hgc-father_1979"), c("medu", "fedu"))
table(dat$fedu)
table(dat$medu)

dat[, c("fedu", "medu") := lapply(.SD, function(x) ifelse(x %in% c(-4, -3, -2, -1), NA, x)),
      .SDcols = c("fedu", "medu")]

dat[, pedu := pmax(fedu, medu, na.rm = TRUE)]
table(dat$pedu)

# education
vars = lookvar(dat, "hgc")
setnames(dat, vars, paste0("edu", years))

table(dat$edu1979)
table(dat$edu2014)

# welfare (pending), transform this variable separately
# vars = paste0("welfare-amt-", years, "_revised_xrnd")
# nvars = paste0("welfare.years")
# setnames(dat, vars, nvars)
# lookvar(dat, "welfare")
# names(dat)

# income
vars = lookvar(dat, "tnfi")
setnames(dat, vars, paste0("income", years))
summary(dat$income1979)
summary(dat$income2014)

# poverty
vars = lookvar(dat, "povst")
setnames(dat, vars, paste0("poverty", years))
table(dat$poverty1979)
table(dat$poverty2014)

# job
vars = lookvar(dat, "hrs")
nvars = paste0("hoursworked", years)
length(vars)
length(nvars)

setnames(dat, vars, nvars)
# vars = lookvar(dat, "esr")
# nvars = paste0("working", years[years<2007])
# setnames(dat, vars, nvars)

table(dat$hoursworked1979)
# hist(dat$hoursworked1979)
# dat[, mean(hoursworked1979), esr_col_1979]

# marriage
vars = lookvar(dat, "marst")
nvars = paste0("mstatus", years)
setnames(dat, vars, nvars)

# health coverage
vars = lookvar(dat, "q11-79")
nvars = paste0("healthcare", years[years > 1988 & years != 1991])

length(nvars)
length(vars)
setnames(dat, vars, nvars)

# health problems
vars = lookvar(dat, "q11-4")
nvars = paste0("healthw", years)
setnames(dat, vars, nvars)

table(dat$healthw1979)
table(dat$healthw2014)

# delinquency
vars = lookvar(dat, "(delin-[1][0-9])|(delin-[4-9])|delin-20")

length(vars)
nvars = paste0("del", 1:17)
setnames(dat, vars, nvars)

dat[, (nvars) := lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = nvars]
table(dat$del1)
table(dat$del17)

dat[, deltot := rowscore(dat, nvars, p = 0)] # all
summary(dat$deltot)

# hist(log(dat$deltot))

# criminal justice experiences
lookvar(dat, "police")
table(dat[, "police-7_1980", with = FALSE])
table(dat[, "police-7c_y_1980", with = FALSE])
setnames(dat, "police-7_1980" , "ever_prison1980")
setnames(dat, "police-7c_y_1980" , "ever_prison1980_release")

# weights
vars = lookvar(dat, "sampweight")
nvars = paste0("wgt", years)
setnames(dat, vars, nvars)

# type of residence (key incarceration variable)
vars = lookvar(dat, "hh1")
nvars = paste0("resid", years)
setnames(dat, vars, nvars)

table(dat$resid1979)
table(dat$resid2000)

# code of jail / prison
jail = c(3, rep(5, 25)) # different coding during first year, 3 versus 5
resid = lookvar(dat, "resid")
length(jail)
length(resid)

for (i in seq_along(years)) {
    dat[, paste0("prison", years[i]) := ifelse(get(resid[i]) == jail[i], 1, 0)]
}

table(dat$resid2004 )
table(dat$prison2004)

table(dat$resid1979 )
table(dat$prison1979 )

# deaths
rni = lookvar(dat, "rni")
length(rni)
death = paste0("death", years[-1])
death

table(dat$rni_1980)
dat[, (death) := lapply(.SD, function(x) ifelse(x == 65, 1, 0)), .SDcols = rni]
table(dat$death1980)
table(dat$death2014)

# not interviewed people because incarcerated
rni = lookvar(dat, "rni")
length(rni)

incarcerated = paste0("incarcerated", years[-1])
length(incarcerated)

dat[, (incarcerated) := lapply(.SD,
                               function(x) ifelse(x == 73, 1, 0)),
                               .SDcols = rni]

dat[, (incarcerated) := lapply(.SD,
                               function(x) ifelse(is.na(x), 0, x)),
                               .SDcols = incarcerated]

table(dat$incarcerated1980)
table(dat$prison2014)

# different records
table(dat[, .(incarcerated2006, prison2006)])
table(dat[, .(incarcerated2014, prison2014)])

# non-response (excluding incarceration)
rni = lookvar(dat, "rni")
length(rni)
nonresponse = paste0("nonresponse", years[-1])
length(nonresponse)

values = c(60:64, 66:67, 71:72, 74)

dat[, (nonresponse) := lapply(.SD,
                              function(x) ifelse(x %in% values, 1, 0)),
                              .SDcols = rni]
dat[, (nonresponse) := lapply(.SD,
                              function(x) ifelse(is.na(x), 0, x)),
                              .SDcols = nonresponse]

table(dat$nonresponse1980)
table(dat$nonresponse2012)

#########################################
# set long format
########################################

# create null variables
vars = c("nonresponse1979", "incarcerated1979", "death1979")
dat[, (vars)  := 0]
dat[, rni_1979 := -4]

vars = paste0("healthcare", years)
ovars = lookvar(dat, "healthcare")
dat[, (vars[-which(vars %in% ovars)]) := NA]

lookvar(dat, "care")

# create variable names

list_variables = c('age', 'prison', 'income', 'poverty', 'healthcare', 'nonresponse',
                    'incarcerated', 'death', 'mstatus', 'edu', 'healthw', 'wgt',
                    'hoursworked', 'rni_', 'resid')


list_names = c('rage', 'rprison', 'rinc', 'rpov', 'rhealthcare', 'rnonresponse',
               'rincarcerated', 'rdeath', 'rmstatus', 'redu', 'rhealthw',
               'rwgt', 'rhoursworked', 'rrni', 'rresid')

create_name_years = function(x, years) { paste0(x, years) }

for (i in seq_along(list_variables)) {
  assign(list_names[i], paste0(list_variables[i], years))
}

lookvar(dat, "ever")

vars = c("id", "sample", "wt", "cluster",
         "stratum", "gender", "race", "pedu", "ever_prison1980", "ever_prison1980_release",
         "deltot", rrni, rresid, rhealthw, rhealthcare, rwgt, rhoursworked,
         rmstatus, redu, rage, rnonresponse, rprison, rincarcerated, rdeath, rinc, rpov)

# vars
wdat = copy(dat[, vars, with = FALSE])
names(wdat)
nrow(wdat) # 12686

# patterns and variables
setkey(wdat, id)

# same order
patt = c("^rni", "^resid", "^nonresponse", "^incarcerated", "^death", "^edu",
          "^age", "^prison", "^income", "^poverty", "^mstatus",
          "^healthw", "^healthcare", "^wgt", "^hoursworked")
vnames = c("whynr", "resid", "nonresponse", "incarcerated", "death", "edu",
            "age", "prison", "income", "poverty", "mstatus",
            "healthw", "healthcare", "wgt", "hoursworked")

length(patt) == length(vnames)


# adjust variable types before melting
vars =  names(wdat)[names(wdat) %like% '^rni_']
wdat[, c(vars) := lapply(.SD, as.numeric), .SDcols = vars]

vars =  names(wdat)[names(wdat) %like% '^healthcare[0-9]+']
wdat[, c(vars) := lapply(.SD, as.numeric), .SDcols = vars]

# melt data
vars = c("id", "sample", "stratum", "cluster", "wt", "gender", "race",
         "pedu", "deltot", "ever_prison1980", "ever_prison1980_release")

ldat = data.table::melt(wdat, id.vars = vars,
                         measure.vars = patterns(patt),
                         value.name = vnames,
                         variable.name = "time")
head(ldat)

# set variables in long format
for (i in seq_along(years)) {
    ldat[time == i, year := years[i]]
}

table(ldat[, .(year)])

# explore cases
ids = unique(ldat$id)
sid = sample(ids, 1)
ldat[id == sid,
  .(id, deltot, wgt, year, age, gender, nonresponse, prison, incarcerated,
    death, poverty, income)]

# death
setkey(ldat, id, year)
table(ldat$death)
ldat[, ldeath := shift(death, type = "lead"), by = id]
table(ldat$ldeath)

replace_na_with_last = function(x,a=!is.na(x)) {
    x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

ldat[, ldeath := replace_na_with_last(ldeath), by = id]
table(ldat$ldeath)
table(ldat$death)

sid = sample(ids, 1)
ldat[id == sid, .(id, death, ldeath)]

ldat[, cumdeath := cumsum(ldeath), by = id]
table(ldat$cumdeath)

ldat = ldat[cumdeath < 2]
table(ldat$cumdeath) # 816
table(ldat$ldeath)

# define non-response and dropouts
ldat[, response := 0]
ldat[!is.na(age), response := 1]
ldat[incarcerated == 1, response := 1]
table(ldat$response)

setorder(ldat, -year)
ldat[, cresp := cumsum(response), by = id]
ldat = ldat[cresp > 0]
table(ldat$incarcerated)
table(ldat$ldeath)

# dropouts
ldat[, dropout := 0]
ldat[, cresp := cumsum(cresp), id]
ldat[, dropout := ifelse(year < 2014 & cresp == 1 & ldeath == 0,
                         1, dropout)]
table(ldat$dropout)

setorder(ldat, year, id)
sid = sample(ids, 1)
ldat[id == sid,
  .(id, cresp, response, wgt, year, age, dropout, ldeath,
    prison, incarcerated, death, poverty, income)]

table(ldat[, .(sample, dropout)])
table(ldat[, .(year, dropout)])
table(ldat$dropout)

table(ldat[, cumsum(dropout), id]$V1) # 4901

# combine incarceration records
ldat[, rprison := pmax(prison, incarcerated, na.rm = TRUE)]
table(ldat$incarcerated)

# prison variable (just combine both records)
ldat[ever_prison1980_release > 0 & ever_prison1980_release < 81, yprison := as.numeric(ever_prison1980_release) + 1900]
table(ldat$yprison)
ldat[, prison1980 := ifelse(year == yprison, 1, 0)][is.na(prison1980), prison1980 := 0]
table(ldat$prison1980)

table(ldat$rprison)
ldat[, rprison := pmax(rprison, prison1980, na.rm = TRUE)]

table(ldat[, .(whynr, rprison)])
ldat[is.na(age) & year < 2004, rprison := NA]
table(ldat$rprison)

setorder(ldat, year, id)
ldat[id == sample(ids, 1),
  .(id, whynr ,response, year, age, dropout, ldeath, prison1980, prison,
    rprison, incarcerated, death, income)]

# expand dataset
setkey(ldat, id, year)
ldat[, myear := min(year), id]
table(ldat$myear)
ldat[, start := (year - myear) + 1, id]
ldat[, stop := ifelse(year < 1994, start + 1, start + 2)]
ldat[, count := stop - start]
table(ldat[, .(year, count)])
ldat[ldat[, .I[.N], id][, V1], count := 1]
table(ldat$count)

ids = sample(unique(ldat$id), 1)
ldat[id %in% ids, .(id, year, count, start, stop)]

# expand data
xx = ldat[rep(seq(1, nrow(ldat)), ldat$count)]
xx[, nyear := year[1]:year[.N], id]

ids = sample(unique(xx$id), 1)
xx[id %in% ids, .(id, response, start, stop, year, nyear, age, count)]
xx[xx[, .I[2], .(id, year)][, V1], age := NA] # remove repeated age
ldat = xx

ldat[, oyear := year]
ldat[, year := nyear]

ldat[id %in% ids, .(id, start, stop, year, age, count)]
table(ldat$year)
table(ldat[year == 2014, whynr]) # all observed

# impute age function
setkey(ldat, id, year)

impute.age = function(age, year) {

    if (any(is.na(age))) {
        min.age = Min(age)
        position = which(age == min.age)[1] # ties
        if (!is.na(position)) {
            if (position > 1) { # initial values
                for (i in 1:(position-1)) {
                    age[position - i] = age[position] - i
                }
            }
            missing = which(is.na(age)) # missing data position
            for (i in missing) {
               age[i] = age[i-1] + (year[i] - year[i-1])
            }
        }  else { age = as.numeric(NA) }
    }
    return(age)
    }

ldat[, age := as.numeric(age)]
ldat[, age := ifelse(age < 0, NA, age)]
ldat[, agei := impute.age(age, year), by = id]

sid = sample(ids, 1)
ldat[id == sid, .(id, year, age, agei, death, dropout,
                  nonresponse, incarcerated, rprison)]

# ggplot(ldat, aes(x = agei, y = agei)) + geom_jitter()
summary(ldat$agei) # ok!

##############################
# assign missing data values
##############################

# gender
ldat[, male := ifelse(gender == 1, 1, 0)]
table(ldat[, .(male)])

# income
summary(ldat$income)
ldat[, income := ifelse(income < 0, NA, income)]

# died variable
ldat[, died := ldeath]
table(ldat$died) # 681

# education
setkey(ldat, id, year)
table(ldat$edu)

ldat[, edu := ifelse(edu < 0, NA, edu)]
table(ldat$edu)

# parents education
table(ldat$pedu) # time invariant

# married
table(ldat[, .(mstatus)])
ldat[, mstatus := ifelse(mstatus < 0, NA, mstatus)]
ldat[, married := ifelse(mstatus == 1 | mstatus == 5, 1, 0)]
table(ldat[, .(married)])

# health
table(ldat[, .(healthw)])
ldat[healthw == -4, healthw := 0][, healthw := ifelse(healthw < 0, NA, healthw)]
table(ldat[, .(healthw)])

# jobs
table(ldat[, .(hoursworked)])
ldat[hoursworked == -4, hoursworked := 0]
ldat[, job := ifelse(hoursworked > 0, 1, 0)][hoursworked < 0, job := NA]
table(ldat$job)

prop.table(table(ldat[year == 2000, job])) # not sure this is the best

# by age
x = ldat[, mean(job, na.rm = TRUE), agei]
setorder(x, agei)
remove(x)

# race
ldat[, race := factor(race, labels = c("hispanic", "black", "non-hispanic/non-black"))]
table(ldat$race)

# select records 18 or above
x = ldat[year >= 1980 & agei >= 18]

table(x[, cumsum(died), id]$V1) # only ones
table(x[, cumsum(dropout), id]$V1) # only ones

summary(x$agei)
x[id == sample(unique(x$id), 1), .(id, year, age, agei, died, rprison, dropout)]
ldat = x

# create time variables
setkey(ldat, id, year)
ldat[, myear := min(as.numeric(year)), id]
ldat[, start := year - myear, id]
ldat[, stop := start + 1]

summary(ldat$start)
summary(ldat$stop) # 35, I removed 1979 to avoid adjusting by the future
table(ldat$start)
table(ldat$stop)

ldat[, magei := min(agei), id]
table(ldat$male)

# create cumulative prison variables!
setkey(ldat, id, year)
ldat[, tprison := rprison][is.na(tprison), tprison := 0]
table(ldat$tprison)

table(ldat[, .(tprison, died)]) # okey
ldat[, cprison := cumsum(tprison), id][, cprison := ifelse(cprison > 0, 1, 0)]
table(ldat$cprison)

table(ldat[, .(cprison, died)]) # 81 cases
table(ldat[male == 1, .(cprison, died)]) # 64
table(ldat[male == 0, .(cprison, died)]) # 17

mean(ldat[, max(stop), id]$V1) # 26 years
hist(ldat[died == 1, agei])

# time-varying variables forward and backward
# impute forward and backward

lookvar(ldat, "prison")
ldat[, iprison := ifelse(is.na(prison), 0, prison)]
ldat[, prison := cumsum(iprison), id][, prison := ifelse(prison > 0, 1, 0)]
ldat[, index_prison := cumsum(iprison), id]
table(ldat$index_prison)

ldat[, select := 1]

# education
table(ldat$edu)

# education before imprisonment
ldat[, index_edu := cumsum(!is.na(edu)), id]
if ('before' %in% names(ldat)) { ldat[, before := NULL] }
ldat[, before := as.numeric(any(index_prison <= index_edu)), id]
table(ldat$before)

# define selection variable
ldat[before == 0, select:= 0]
table(ldat$select)

# check
nrow(ldat[, max(select), .(select, id)]) == length(unique(ldat$id))
table(ldat$select)
ldat[select == 0, .(id, year, agei, prison, edu)]

# impute education forward, and then backward
ldat[, iedu:= na.locf(edu, na.rm = FALSE), id]
ldat[select == 1, iedu := na.locf(iedu, fromLast=TRUE), id] # all cases, it is not relevant

# income

# income before imprisonment
ldat[, index_inc := cumsum(!is.na(income)), id]
ldat[, before := NULL]
ldat[, before := as.numeric(any(index_prison <= index_inc)), id]
table(ldat$before)

# impute education forward, and then backward
ldat[, income := ifelse(income == 0, 1, income)]
ldat[, iinc := na.locf(income, na.rm = FALSE), id]
ldat[select == 1, iinc := na.locf(iinc, fromLast=TRUE), id] # all cases, it is not relevant

cpi = fread("data/nlsy_cpi.csv")
setkey(cpi, year)
setkey(ldat, year)

ldat = cpi[ldat]
ldat[, cpi := cpi / 100][, iinc := iinc* cpi]
ldat[, liinc := scale(log(iinc), center = TRUE, scale = FALSE)]

# health at working place

# health before imprisonment
ldat[, index_health := cumsum(!is.na(healthw)), id]
ldat[, before := NULL]
ldat[, before := as.numeric(any(index_prison <= index_health)), id]
table(ldat$before)

# define selection variable
ldat[before == 0, select:= 0]
table(ldat$select)

# check
nrow(ldat[, max(select), .(select, id)]) == length(unique(ldat$id))
table(ldat$select)
ldat[select == 0, .(id, year, agei, prison, healthw)]
countmis(ldat[, .(healthw)])

# impute education forward, and then backward
ldat[, ihealthw := na.locf(healthw, na.rm = FALSE), id]
ldat[select == 1, ihealthw := na.locf(ihealthw, fromLast=TRUE), id] # all cases, it is not relevant

summary(ldat$ihealthw)

# job

# job before imprisonment
ldat[, index_job := cumsum(!is.na(job)), id]
ldat[, before := NULL]
ldat[, before := as.numeric(any(index_prison <= index_job)), id]
table(ldat$before)

# define selection variable
ldat[before == 0, select:= 0]
table(ldat$select)

# check
nrow(ldat[, max(select), .(select, id)]) == length(unique(ldat$id))
table(ldat$select)
ldat[select == 0, .(id, year, agei, prison, job)]
countmis(ldat[, .(job)])

# impute education forward, and then backward
ldat[, ijob := na.locf(job, na.rm = FALSE), id]
ldat[select == 1, ijob := na.locf(ijob, fromLast=TRUE), id] # all cases, it is not relevant

summary(ldat$ijob)

# married

# married before imprisonment
ldat[, index_married := cumsum(!is.na(married)), id]
ldat[, before := NULL]
ldat[, before := as.numeric(any(index_prison <= index_married)), id]
table(ldat$before)

# define selection variable
ldat[before == 0, select:= 0]
table(ldat$select)

# check
nrow(ldat[, max(select), .(select, id)]) == length(unique(ldat$id))
table(ldat$select)
ldat[select == 0, .(id, year, agei, prison, married)]
countmis(ldat[, .(married)])

# impute education forward, and then backward
ldat[, imarried := na.locf(married, na.rm = FALSE), id]
ldat[select == 1, imarried := na.locf(imarried, fromLast=TRUE), id] # all cases, it is not relevant
summary(ldat$imarried)

# save data
saveRDS(ldat, file = "output/nlsy79_long_format_covariates.rds")
