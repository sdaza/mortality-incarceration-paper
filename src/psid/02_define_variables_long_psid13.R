##############################
# health and incarceration
# define variables in long format
# author: sebastian daza
##############################

# libraries
library(sdazar)
library(zoo)
library(ggplot2)
library(ggthemes)
library(ggpubr)

# load data
ldat <- readRDS('output/psid_ind_long_selection_13.rds')
wdat <- readRDS('output/psid_ind_wide_selection_13.rds')

#########################
# define variables
#########################

length(unique(ldat$pid)) # 77223

table(ldat[year == 2013, whynr], useNA = "ifany") # right

ldat[pid == 452035, .(pid, year, age) ]
ldat[pid == 1540031, .(pid, year, age) ]
ldat[pid == 452035, .(pid, year, age) ]

names(ldat)
table(ldat[year == 1999, .(whynr, tysample)])
table(ldat[year == 2001, .(whynr, tysample)])
table(ldat[year == 2003, .(whynr, tysample)])

# missed years
missed_years <- seq(1998, 2013, by = 2) # to adjust this at the end
missed_years

##########################
# expand records two years
##########################

# create time variable
setkey(ldat, pid, year)

ldat[, myear := min(year), by = pid]
ldat[, start := (year - myear) + 1, by = pid]
ldat[, stop := ifelse(year < 1997, start + 1, start + 2)]
ldat[, count := stop - start]
ldat[ldat[, .I[.N], by = pid][, V1], count := 1]
table(ldat$count)

ids <- sample(unique(ldat$pid), 1)
ldat[pid %in% ids, .(pid, year, count, start, stop)]

# expand data
xx <- ldat[rep(seq(1, nrow(ldat)), ldat$count)]
xx[, nyear := year[1]:year[.N], by = pid]

ids <- sample(unique(xx$pid), 1)
xx[pid %in% ids, .(pid, start, stop, year, nyear, age, count)]
xx[xx[, .I[2], by = .(pid, year)][, V1], age := NA] # remove repeated age
ldat <- xx

ldat[, oyear := year]
ldat[, year := nyear]

ldat[pid %in% ids, .(pid, start, stop, year, oyear, age, count)]
table(ldat$year, useNA = "ifany")
ldat[pid == 1988005, .(pid, year, oyear, age, rth, whynr, count)]
table(ldat[year == 2013, whynr], useNA = "ifany") # right

# variables of interest
# age, ok
# race, ok
# two parents # no
# parents education # no
# gov. assistance # I don't know, for now I wouldn't include it
# college degree ok
# poverty # income
# employment, ok
# marital status, ok, but not asked to all
# drug use # I have to check
# violence # no
# health problems # ok
# health insurance # from where? I have to check

# create sequence number
# ldat[, s := seq(.N), by = pid]
setkey(ldat,  pid, year)

# remove rows after death
if ("died" %in% names(ldat)) { ldat[, died := NULL] }
ldat[pid == 686032, ydeath]

ldat[, died := ifelse(year >= ydeath, 1, 0)]
ldat[is.na(died), died := 0]
table(ldat$died, useNA = "ifany")
table(ldat$death, useNA = "ifany")

ldat[death == 1 & is.na(ydeath), ydeath := 9]

table(ldat$died, useNA = "ifany")
summary(ldat$ydeath) # there are some cases

# remove records after death
ldat[, cdeath := cumsum(died), by = pid]
ldat <- ldat[cdeath < 2, ][, cdeath := NULL]
nrow(ldat)

table(ldat[year == 2013, whynr], useNA = "ifany")
table(ldat[year == 2013, whynr], useNA = "ifany")

table(ldat$died) # 6863, right!

ldat[pid == 1988005, .(pid, year, oyear, age, rth, whynr, count, died)]

# explore some examples
ids <- unique(ldat[death == 1, pid])
ldat[pid == sample(ids, 1), .(pid, year, death, ydeath, age, died)]

# track variable
if ("itrack" %in% names(ldat)) { ldat[, itrack := NULL] }
ldat[sn %in% 1:20, itrack := 1] # present in FU
ldat[sn %in% 51:59, itrack := 2] # institution
ldat[sn %in% 71:80, itrack := 3] # move out, not interviewed
ldat[sn %in% 81:89, itrack := 4] # died since last interview
ldat[sn == 0, itrack := 0] # not yet born, non-response, not followed

table(ldat$itrack, useNA = "ifany")

# 1968
ldat[year == 1968 & rth > 0  & rth < 9 & whynr == 0, itrack := 1]
ldat[year == 1968 & rth > 0  & rth < 9  &
    whynr %in% c(11, 12, 13, 14 ,19),
    itrack := 2]

ldat[year == 1968 & rth == 0 & whynr == 97, itrack := 0]
ldat[year == 1968 & rth %in% c(8,9), itrack := 3]

table(ldat$itrack, useNA = "ifany") # no missing data
table(ldat[year == 2013, itrack], useNA = "ifany")

# track variable vs why non-response
table(ldat[whynr %in% c(11:14, 19), .(year, itrack)], useNA = "ifany")
ldat[whynr %in% c(11:13, 14, 19), itrack := 2] # force them to be 2
table(ldat[whynr %in% c(11:14, 19), .(year, itrack)], useNA = "ifany")
table(ldat[whynr == 0, itrack], useNA = "ifany")

table(ldat[, .(year, itrack)])

# create response indicator
setkey(ldat, year, pid)
ldat[, response := 0]
ldat[itrack %in% c(1,2), response := 1]
ldat[whynr == 0, response := 1]
# ldat[year %in% missed_years, response := 0] # do not include cases that are not part of the PSID sample

table(ldat[, .(year, response)])
ldat[died == 1, response := 1] # this is problematic for 686032

ldat[pid == 686032, .(pid, ydeath, death, died, year)]

table(ldat$response, useNA = "ifany")
ids <- unique(ldat$pid)
ldat[pid == sample(ids, 1), .(pid, year, gender, age, rth, whynr,
  itrack, died, response)]

table(ldat[year == 2013 , whynr], useNA = "ifany")
table(ldat[year == 2013 , itrack], useNA = "ifany")

# remove records before first observation
setkey(ldat, year, pid)
ldat[, cresp := cumsum(response), by = pid]
table(ldat$cresp, useNA = "ifany") # 48, that is right

table(ldat[cresp == 0, .(whynr)])
table(ldat[cresp == 0, .(itrack)])

nrow(ldat)
ldat <- ldat[cresp > 0]
nrow(ldat)
summary(ldat$cresp, useNA = "ifany") # max 48, right
ldat[, cresp := NULL]

# remove records from last observation (dropout)
ldat <- ldat[order(pid, year, decreasing = TRUE),]
ldat[, cresp := cumsum(response), by = pid]
ldat[pid == sample(unique(ldat$pid), 1), .(pid, year, response, cresp)]

summary(ldat$cresp)
ldat <- ldat[cresp > 0]
nrow(ldat)

ldat[pid == 1988005, .(pid, year, oyear, gender, age, rth, whynr,
  itrack, died, response, cresp, count, inc, iwt)]

table(ldat$died) # same number, 6825

# remove last observation in non-sample years
ldat[, select := TRUE]
ldat[, max_year := max(year), pid]
ldat[year == max_year & max_year %in% missed_years & died != 1, select := FALSE, pid]
table(ldat$select)

ids <- unique(ldat$pid)
ldat[pid == sample(ids, 1), .(pid, max_year, year, gender, age, rth, whynr,
  itrack, died, response, select)]

ldat <- ldat[select == TRUE]
table(ldat[, max(year), pid]$V1) # only people who died!

# ldat <- ldat[select == TRUE]

# TODO: to explore case 1988005
ldat[pid == 1988005, .(pid, year, oyear, gender, age, rth, whynr,
  itrack, died, response, cresp, count, inc)]

table(ldat$died, useNA = "ifany") # right! 6825

# explore weights
setkey(ldat, pid, year)

###################
# impute age
###################

setkey(ldat, pid, year)
ldat[age == 0 | age == 999, age := NA] # set missing data
summary(ldat$age)

impute.age <- function(age, year) {
  if (any(is.na(age))) {
  min.age <- Min(age)
  position <- which(age == min.age)[1] # ties
  if (!is.na(position)) {
   if (position > 1) { # initial values
    for (i in 1:(position-1)) {
      age[position - i] <- age[position] - i
    }
    }
  missing <- which(is.na(age)) # missing data position
  for (i in missing) {
    age[i] = age[i-1] + (year[i] - year[i-1])
  }
  } else { age = as.numeric(NA) }
}
return(age)
}

setkey(ldat, pid, year)
ldat[, agei := impute.age(age, year), by = pid]
summary(ldat$agei)
length(unique(ldat[agei < 0, pid])) # 5 cases with values < 0

ldat[pid %in% sample(unique(ldat[agei < 0, pid]),1), .(pid, year, age, agei)]
ldat[agei < 1, agei := 1]

# fixing some ages manually
ldat[pid == 6357176 & year == 1973, agei := 22]
ldat[pid == 6272006 & year == 1971, agei := 9]
ldat[pid == 8595004 & year == 1991, agei := 13]
ldat[pid == 5128182 & year == 1992, agei := 13]
ldat[pid == 5664030 & year == 1972, agei := 2]
ldat[pid == 6859173 & year == 1979, agei := 19]
ldat[pid == 1540031 & year == 1993, agei := 19]
ldat[pid == 2731002 & year == 1974, agei := 15]
ldat[pid == 5748005 & year == 1977, agei := 19]

summary(ldat$agei)
setkey(ldat, pid, year)
ldat <- ldat[!is.na(agei)] # remove cases with missing ages
ldat[, any18 := cumsum(agei >= 18), pid]
table(ldat$any18, useNA = "ifany")

############################
# prison variable
############################

ldat[, nrprison := ifelse(whynr == 14, 1, 0)]
table(ldat$nrprison, useNA = "ifany")
table(ldat[, .(anyprison = any(nrprison == 1, na.rm = TRUE)), pid][,
  anyprison])

table(ldat[, .(anyprison = any(prison1995 == 1, na.rm = TRUE)), pid][,
    anyprison])

prop.table(table(ldat[, .(anyprison = any(pmax(nrprison, prison1995, na.rm = TRUE) == 1, na.rm = TRUE)), pid][,anyprison]))

table(ldat[, .(anyprison = any(pmax(nrprison, prison1995, na.rm = TRUE) == 1, na.rm = TRUE)), pid][,
                anyprison])

table(ldat$response)

ldat[response == 0, nrprison := NA]
table(ldat$nrprison, useNA = "ifany")
table(ldat[, .(nrprison, year)], useNA = "ifany")
table(ldat[, .(died, year)], useNA = "ifany") # only 71 according to PSID


# based on non-response
# temp <- ldat[any18 > 0 & whynr == 14, .(pid, year)][, .(mpryr = min(year),
  # years_prison = .N), by = pid]

# table(duplicated(temp, by = "pid")) # no duplicates, 612 cases
# setkey(temp, pid)
# setkey(ldat, pid)
# ldat <- temp[ldat]
# nrow(ldat)

# ldat[, prisonwhynr_first := ifelse(year >= mpryr, 1, 0)]
# ldat[is.na(prisonwhynr_first), prisonwhynr_first := 0]
# table(ldat$prisonwhynr_first, useNA = "ifany")

# ids <- unique(ldat[died == 1 & prisonwhynr_first == 1, pid])
# ldat[pid == sample(ids, 1), .(pid, year, agei, whynr, itrack, response,
  # died, prisonwhynr_first, inc)] # right

# based on 1995 question
# temp <- ldat[, .(anyprison95 = as.numeric(any(prison1995 == 1))), by = pid]
# table(ldat$prison1995, useNA = "always")
# setkey(temp, pid)

# ldat <- temp[ldat]

ldat[, prison95 := 0] # different from prison1995 (original variable)
summary(ldat$yprison1995)
ldat[agei >= 18 & yprison1995 <= year, prison95 := 1] # I am not considering missing records here
table(ldat$prison95, useNA = "ifany")

# ids <- unique(ldat[anyprison95 == 1, pid])

# ldat[pid == sample(ids, 1), .(pid, year, whynr, itrack, response,
#   died, prison1995, yprison1995, prisonwhynr_first, prison95 )] # right

# # prison: combination of records
# table(ldat$prison95, useNA = "ifany")

# ldat[, prison := pmax(prisonwhynr_first, prison95)] # aggregate
# table(ldat$prison, useNA = "ifany")

# dropout
ldat <- ldat[order(pid, year, decreasing = TRUE),]
ldat[, s := seq(.N), by = pid]
ldat <- ldat[order(pid, year, decreasing = FALSE),]

ldat[, dropout := 0]
ldat[s == 1 & year != 2013, dropout := 1]
ldat[s == 1 & died == 1, dropout := 0]

table(ldat$dropout, useNA = "ifany")
table(ldat[, .(dropout, year)])

ids <- unique(ldat[dropout == 1, pid])
ldat[pid == sample(ids, 1), .(pid, year, agei, whynr, itrack, response,
  died, nrprison, inc, dropout)] # right

########################
# covariates
#######################

# create head-wife indicator
ldat[year < 1983, head := ifelse(sn == 1 & rth == 1, 1, 0)]
ldat[year >= 1983, head := ifelse(sn == 1 & rth == 10, 1, 0)]
table(ldat$head)
table(ldat[, .(head, year)])

ldat[year < 1983, wife := ifelse(sn == 2 & rth == 2, 1, 0)]
ldat[year >= 1983, wife := ifelse(sn == 2 & rth == 20, 1, 0)]
table(ldat$wife)
table(ldat[, .(wife, year)])

#############
# gender
#############

ldat[, male := ifelse(gender == 1, 1, 0)]
table(ldat$male)

###################
# race
###################

ldat[hh_race %in% c(0,9), hh_race := NA]
ldat[ww_race %in% c(0,9), ww_race := NA]
table(ldat$hh_race, useNA = "ifany")
table(ldat$ww_race, useNA = "ifany")

# race based on head and wife data
if ("race" %in% names(ldat)) { ldat[, race := NULL] }
ldat[head == 1, race := hh_race]
ldat[wife == 1, race := ww_race]

ldat[head == 1 & !is.na(ww_race), race := ww_race]
ldat[wife == 1 & !is.na(hh_race), race := hh_race]

ldat[head == 0 & wife == 0, race := hh_race]
ldat[head == 0 & wife == 0 & is.na(hh_race), race := ww_race]

# from hispanic to other races
ldat[race %in% 3:7, race := 3]

# else get first household race observed
ldat[, racei := head(na.omit(race), 1L), by = pid]
table(ldat$racei, useNA = "ifany") # 800 missing cases

# ldat[is.na(racei), racei := 4] # unknown

# ids <- sample(unique(ldat[racei == 4, pid]), 1)
# ldat[pid %in% ids, .(pid, sn, year, race, hh_race, ww_race, racei)]

if ("frace" %in% names(ldat)) { ldat[, frace := NULL] }
ldat[racei == 1, frace := "white"]
ldat[racei == 2, frace := "black"]
ldat[racei %in% c(3), frace := "other"]
table(ldat$frace, useNA = "ifany")

ldat[, frace := factor(frace, levels = c("white", "black", "other"))]
table(ldat$frace, useNA = "ifany")

table(ldat[, .(year, frace)])

################
# education
################

# check codes
ldat[, edu := ifelse(edu %in% c(0, 98, 99), NA, edu)]
table(ldat$edu, useNA = "ifany")
table(ldat[agei > 30, edu], useNA = "ifany")
setkey(ldat, pid, year)

if ("edui" %in% names(ldat)) { ldat[, edui := NULL] }
# impute forward only for ages 30 or higher
ldat[agei > 30, edui := na.locf(edu, na.rm = FALSE), pid] # last value carried forward
ldat[agei <= 30, edui := edu]
table(ldat$edui, useNA = "ifany")

table(ldat[is.na(edui), head]) # this looks ok!
table(ldat[is.na(edui), wife])

table(ldat[, .(year, edui)])

# create last forward imputation for education

setkey(ldat, pid, year)

# create index prison variable

table(ldat$nrprison, useNA = "ifany")
table(ldat$prison95, useNA = "ifany")

ldat[, iprison := na.locf(nrprison, na.rm = FALSE), pid] # imputing
ldat[, anrprison := cumsum(iprison), pid][, anrprison := ifelse(anrprison > 0, 1, 0)]
table(ldat$iprison, useNA = "ifany")
ldat[, iprison := pmax(prison95, anrprison)]
ldat[, iprison := cumsum(iprison), pid][, iprison := ifelse(iprison > 0, 1, 0)]
table(ldat$iprison, useNA = "ifany")

# records of education before prison?
ldat[, index_edu := cumsum(!is.na(edu)), pid]
ldat[, index_prison := cumsum(iprison), pid]

# create variable 'before'
ldat[, before := as.numeric(any(index_prison <= index_edu)), pid] # ok, most of cases
table(ldat$before, useNA = "ifany")

# define selection variable
ldat[, select := NULL]
ldat[, select := 1]
ldat[before == 0, select:= 0]

# check
nrow(ldat[, max(select), .(select, pid)]) == length(unique(ldat$pid))
table(ldat$select, useNA = "ifany")
ldat[select == 0, .(pid, year, agei, iprison, edu)]

# impute education forward, and then backward
ldat[, iedu := na.locf(edu, na.rm = FALSE), pid]
ldat[select == 1, iedu := na.locf(iedu, fromLast=TRUE), pid] # all cases, it is not relevant

table(ldat$iedu, useNA = "ifany")

##################
# income (not to impute forward)
##################

# summary(ldat[year == 1968, inc])
# summary(ldat[year == 1970, inc])
# summary(ldat[year == 1980, inc])
# summary(ldat[year == 1990, inc])
# summary(ldat[year == 2001, inc])

ldat[inc == 9999999, inc := NA] # latino sample or residual category

# remove data for non-existent waves
# ldat[year %in% missed_years | response == 0, inc := NA]
ldat[, .(mean(inc, na.rm = TRUE)), by = .(year)]

# number of people at home
ldat[response == 1, fam_size := .N, .(year, fn)][,
    fam_size := Min(fam_size), .(year, fn)]

summary(ldat$fam_size)
cor(ldat[, .(fam_size, inc)], use="complete") # very low correlation
setkey(ldat, pid, year)

## impute forward and backward
summary(ldat$inc)
setkey(ldat, year, pid)

# income before imprisonment
ldat[, index_inc := cumsum(!is.na(inc)), pid]
ldat[, before := NULL]
ldat[, before := as.numeric(any(index_prison <= index_inc)), pid]
table(ldat$before, useNA = "ifany")

# define selection variable
ldat[before == 0, select:= 0]

# check
nrow(ldat[, max(select), .(select, pid)]) == length(unique(ldat$pid))
table(ldat$select, useNA = "ifany")
ldat[select == 0, .(pid, year, agei, iprison, edu)]

# impute education forward, and then backward
ldat[, iinc := na.locf(inc, na.rm = FALSE), pid]
ldat[select == 1, iinc := na.locf(iinc, fromLast=TRUE), pid] # all cases, it is not relevant

summary(ldat$iinc, useNA = "ifany")

# inflation adjustment
cpi <- fread("data/psid_cpi.csv")
cpi[, cpi := value / 100]
setkey(cpi, year)
cpi[, value := NULL]
setkey(ldat, year)

ldat <- cpi[ldat]
nrow(ldat)

ldat[, iinc := iinc * cpi]
ldat[, liinc := log(iinc)]
ldat[, liinc := scale(liinc, center = TRUE, scale = FALSE)]

hist(ldat$liinc)

ldat[, inca := inc * cpi]
ldat[, linca := log(inca)]
ldat[, linca := scale(linca, center = TRUE, scale = FALSE)]
hist(ldat$linca)

##################
# health
###################

ldat[, .(pid, year, agei, ww_health, hh_health)]

ldat[hh_health %in% c(0,8,9), hh_health := NA]
ldat[ww_health %in% c(0,8,9), ww_health := NA]

if ("ghealth" %in% names(ldat)) { ldat[, ghealth := NULL] }
if ("type_health" %in% names(ldat)) { ldat[, type_health := NULL] }
ldat[head == 1, ghealth := hh_health]
ldat[wife == 1, ghealth := ww_health]
ldat[!is.na(ghealth), type_health := 1]

table(ldat$ghealth, useNA = "ifany") # 1 = excelent, 5 = poor
table(ldat[, .(year, ghealth)])

# other family members
table(ldat$ofu_dhealth, useNA = "ifany")
ldat[ofu_dhealth %in% c(0,9), ofu_dhealth := NA]
ldat[ofu_dhealth == 1, new_ofu_dhealth := 5] # recoded, change direction general health
ldat[ofu_dhealth == 5, new_ofu_dhealth := 1]
ldat[is.na(ghealth) & !is.na(new_ofu_dhealth), `:=` (ghealth = new_ofu_dhealth,
                                                 type_health = 2)]
table(ldat$ghealth, useNA = "ifany")
table(ldat[type_health == 2, .(year, ghealth)])

table(ldat$shealth)
table(ldat$ofu_shealth1986)
ldat[shealth %in% c(0,8,9), shealth := NA]
ldat[ofu_shealth1986 %in% c(0,8,9), ofu_shealth1986 := NA]
ldat[year != 1986, ofu_shealth1986 := NA]

# other health reports
ldat[(head == 0 | wife == 0) & !is.na(shealth), `:=` (ghealth = shealth, type_health = 1)]
ldat[(head == 0 | wife == 0) & !is.na(ofu_shealth1986), `:=` (ghealth = ofu_shealth1986, type_health = 1)]

# explore
ids <- sample(unique(ldat[, pid]), 1)
ldat[pid %in% ids, .(pid, sn, rth, gened,wife, head, agei, year, type_health,
                     ghealth, hh_health, ww_health, ofu_dhealth,
                     shealth, ofu_shealth1986)]

# recode variable
ldat[type_health == 1 & ghealth %in% c(1:3), dghealth := 0] # good or regular health
ldat[type_health == 1 & ghealth %in% c(4:5), dghealth := 1] # poor health
ldat[type_health == 2 & ghealth == 1 , dghealth := 0] # good health (I changed the code above)
ldat[type_health == 2 & ghealth == 5 , dghealth := 1] # poor health

table(ldat$dghealth, useNA = "ifany")
table(ldat$ghealth, useNA = "ifany")
table(ldat[, .(dghealth, ghealth)])

# missing data
table(ldat$dghealth, useNA = "ifany")
# ldat[year %in% missed_years | response == 0, dghealth := NA]
table(ldat$dghealth, useNA = "ifany")
table(ldat[, .(dghealth, year)], useNA = "ifany")

# imputing health variable

# impute health last observed value

ldat[, index_health := cumsum(!is.na(dghealth)), pid]
ldat[, before := NULL]
ldat[, before := as.numeric(any(index_prison <= index_health)), pid]
table(ldat$before, useNA = "ifany")

# update selection variable
ldat[before == 0, select:= 0]

# check
nrow(ldat[, max(select), .(select, pid)]) == length(unique(ldat$pid))
table(ldat$select, useNA = "ifany")
ldat[select == 0, .(pid, year, agei, iprison, dghealth)]

# impute education forward, and then backward
ldat[, idghealth := na.locf(dghealth, na.rm = FALSE), pid]
ldat[select == 1, idghealth := na.locf(idghealth, fromLast=TRUE), pid] # all cases, it is not relevant

summary(ldat$idghealth, useNA = "ifany")

# # couples

# # this variable links pairs of individuals who were married or
# # permanently cohabiting at the time of the 1968 interview.

# table(ldat$pairs, useNA = "ifany")

# ldat[, marital := ifelse(pairs %in% 1:4, 1, 0)]

# ids <- sample(unique(ldat[, pid]), 1)
# ldat[pid %in% ids, .(pid, sn, rth, gened,wife, head, agei, year, marital)]

# table(ldat[, .(marital, prisonwhynr_first)])
# table(ldat$marital, useNA = "ifany")

# # employment
# table(ldat$empl, useNA = "ifany")

# # marital status (export marital status)
# table(ldat$marital_status_ln, useNA = "ifany")

# ldat[marital_status_ln == 1, marital_ln := 1]
# ldat[marital_status_ln %in% c(2:5), marital_ln := 0]

# table(ldat$marital_ln, useNA = "ifany")
# table(ldat[head == 1 | wife == 1, marital_status_ln], useNA = "ifany")

# # employment (from 1979)
# table(ldat$empl, useNA = "ifany")
# ldat[empl %in% c(0,8,9), empl := NA]
# ldat[, fempl := ifelse(empl == 1, 1, 0)]
# ldat[, disable := ifelse(empl == 5, 1, 0)]
# table(ldat$fempl, useNA = "ifany")
# table(ldat$disable, useNA = "ifany")

# table(ldat[, .(year, fempl)])
# save data

# assign weights, 18 year old or above
setkey(ldat, pid, year)
summary(ldat$iwt)
ldat[, fwt := head(iwt, 1L), pid] # it seems right!
summary(ldat$fwt)
length(unique(ldat[is.na(fwt), pid])) # 459, ok!
length(unique(ldat[is.na(iwt), pid])) # 459, ok!

# test first observation (18 years old or more)
ldat[, s := 1:.N, pid]
dim(ldat)

nrow(ldat[s == 1 & fwt == iwt,]) # all cases
nrow(ldat[s == 1 & fwt != iwt,]) # no cases

setkey(ldat, pid, year)
hist(ldat$fwt)

# select people 18 year old or above, important!
ldat <- ldat[any18 > 0] # 15 in some cases, ok!
summary(ldat$agei)

######################
# some plots
######################

# deaths
temp1 <- ldat[!is.na(iwt), .(deaths = mean(died)), year]
temp1[, type := "Unweighted"]
temp2 <- ldat[!is.na(iwt), .(deaths = weighted.mean(died, iwt, na.rm = TRUE)), year]
temp2[, type := "Weighted"]
temp <- rbind(temp1, temp2)
ggplot(temp, aes(x = year, y = deaths, color = type)) + geom_line() +
  labs(title = "Proportion respondents who died, PSID, sample and non-sample respondents",
       subtitle = "Longitudinal varying weights")


# plot prison rate
temp1 <- ldat[!is.na(iwt), .(prison = mean(nrprison, na.rm = TRUE)), year]
temp1[, type := "Unweighted"]
temp2 <- ldat[!is.na(iwt), .(prison = weighted.mean(nrprison, iwt, na.rm = TRUE)), year]
temp2[, type := "Weighted"]
temp2
temp <- rbind(temp1, temp2)

g <- ggplot(temp, aes(x = year, y = prison, color = type)) +
 geom_line(aes(linetype = type))  + theme_minimal() + theme(legend.position = "top", legend.title=element_blank()) +
  labs(x = "\nYear", y = "Proportion\n") +
  scale_color_manual(name = "type",
                     values = c('black', 'black')) +
  scale_linetype_manual(name = "type",
                        values = c(2,1))

 # labs(title = "Proportion Respondents in prison, PSID 1968-2013",
      # subtitle = "sample and non-sample respondents",
      # x = "\nYear", y = "Proportion\n")
savepdf("output/imprisonment_psid")
print(g)
dev.off()

ggplot(ldat[death == 1], aes(age)) + geom_density() + theme_minimal()

 # number of cases
length(unique(ldat$pid))
anyDuplicated(ldat[, family_id])

# save data
saveRDS(ldat, file = "output/psid_long_format_covariates_year_13.rds")

#############################
# end script
#############################
