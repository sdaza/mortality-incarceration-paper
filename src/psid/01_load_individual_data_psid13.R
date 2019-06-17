##############################
# health and incarceration
# load individual data (public release)
# author: sebastian daza
##############################

# libraries
library(sdazar)
library(haven)

# load original file selection
dat <- readRDS("data/psid15selection.rds")
dim(dat)

# create identification variables

# unique id
dat[, pid := er30001 * 1000 + er30002]

anyDuplicated(dat[, pid])
setnames(dat, "er32006", "sample")

table(dat$sample)
dat[, smember := 0]
dat[sample %in% c(1:3), smember := 1]
table(dat$smember, useNA = "ifany")

# family id
dat[, family_id := er30001]

# gened (sample members)
dat[, gened := ifelse(er30002 %in% c(1:19,20:26, 30:169), 1, 0)]
table(dat$gened, useNA = "ifany")

nrow(dat[er30002 >= 900])
nrow(dat[er30002 >= 229 & er30002 <= 399])

# using original variables

# id
# relationship to head
# age
# why not response
# type of respondent

# rename variables

# sex
setnames(dat, "er32000", "sex")
table(dat$sex, useNA = "ifany")
dat[, gender := ifelse(sex == 1, 1, 0)][sex == 9, gender := NA]
table(dat$gender, useNA = "ifany")

# family interview number
ovars <- paste0("er", c(30020, 30043, 30067, 30091, 30117, 30138, 30160, 30188,
                30217, 30246, 30283, 30313, 30343, 30373, 30399, 30429, 30463, 30498, 30535,
                30570, 30606, 30642, 30689, 30733, 30806, 33101, 33201, 33301, 33401, 33501,
                33601, 33701, 33801, 33901, 34001, 34101, 34201))

length(ovars)

nvars <- paste0("fn", c(1969:1997, seq(1999, 2013, 2)))
length(nvars)
setnames(dat, ovars, nvars)

# sequence number
n <- c(30020, 30043, 30067, 30091, 30117, 30138, 30160, 30188, 30217, 30246,
       30283, 30313, 30343, 30373, 30399, 30429, 30463, 30498, 30535, 30570, 30606,
       30642, 30689, 30733, 30806, 33101, 33201, 33301, 33401, 33501, 33601, 33701,
       33801, 33901, 34001, 34101, 34201)

ovars <- paste0("er", n + 1)
length(ovars)
nvars <- paste0("sn", c(1969:1997, seq(1999, 2013, 2)))
length(nvars)

dat[, sn1968 := er30002] # person number 1968
setnames(dat, ovars, nvars)
table(dat$sn1968, useNA = "ifany")
table(dat$sn1969, useNA = "ifany")

# relationship to the head
n <- c(30001, 30020, 30043, 30067, 30091, 30117, 30138, 30160, 30188, 30217,
       30246, 30283, 30313, 30343, 30373, 30399, 30429, 30463, 30498, 30535, 30570,
       30606, 30642, 30689, 30733, 30806, 33101, 33201, 33301, 33401, 33501, 33601,
       33701, 33801, 33901, 34001, 34101, 34201)

ovars <- paste0("er", n + 2)
nvars <- paste0("rth", c(1968:1997, seq(1999, 2013, 2)))
nvars
ovars[grep("1968|2005|2007|2009|2011", nvars)]
length(ovars) == length(nvars)

setnames(dat, ovars, nvars)
table(dat$rth1968, useNA = "ifany")

table(dat[sn1992 == 1, rth1992])
table(dat[sn1992 == 2, rth1992])
table(dat[sn1969 == 1, rth1969])
table(dat[sn1969 == 2, rth1969])

# define family number 1968
dat[, fn1968 := ifelse(rth1968 != 0, er30001, 0)]
summary(dat$fn1968)
summary(dat$fn1969)

# using family level data
ovars <- c("v3","v442","v1102","v1802","v2402","v3002","v3402","v3802","v4302",
           "v5202","v5702","v6302","v6902","v7502","v8202","v8802","v10002","v11102",
           "v12502","v13702","v14802","v16302","v17702","v19002","v20302","v21602",
           "er2002","er5002","er7002","er10002","er13002","er17002","er21002",
           "er25002","er36002","er42002","er47302","er53002")

nvars <- paste0("fu", c(1968:1997, seq(1999, 2013,2)))
setnames(dat, ovars, nvars)
lookvar(dat, "fu")

summary(dat$fu1968)

# same number
length(unique(na.omit(dat[, fu1968])))

# age
n <- c(30001, 30020, 30043, 30067, 30091, 30117, 30138, 30160, 30188, 30217,
       30246, 30283, 30313, 30343, 30373, 30399, 30429, 30463, 30498, 30535, 30570,
       30606, 30642, 30689, 30733, 30806, 33101, 33201, 33301, 33401, 33501, 33601,
       33701, 33801, 33901, 34001, 34101, 34201, 34301)

ovars <- paste0(paste0("er", n[-length(n)] + 3))
# ovars[length(ovars)+1] <- "er34305"
nvars <- paste0("age", c(1968:1997, seq(1999, 2013, 2)))
ovars[grep("1968|2005|2007|2009|2011", nvars)]
setnames(dat, ovars, nvars)

table(dat$age1968, useNA = "ifany")
table(dat$age1970, useNA = "ifany")
table(dat$age1980)
table(dat$age1981)


# type of individual record
n <- c(30017, 30040, 30064, 30088, 30114, 30135, 30157, 30185, 30214, 30243,
       30280, 30310, 30340, 30370, 30396, 30426, 30460, 30495, 30532, 30567, 30603,
       30639, 30684, 30728, 30801, 30862, 33126, 33282, 33324, 33436, 33544, 33635,
       33738, 33846, 33948, 34043, 34152, 34266)

ovars <- paste0("er", n)
nvars <- paste0("typr", c(1968:1997, seq(1999, 2013, 2)))
ovars[grep("1968|2005|2007|2009|2011", nvars)]
setnames(dat, ovars, nvars)

table(dat$typr1968, useNA = "ifany")
table(dat$typr1976, useNA = "ifany")

# why nonresponse
n <- c(30017, 30040, 30064, 30088, 30114, 30135, 30157, 30185, 30214, 30243,
       30280, 30310, 30340, 30370, 30396, 30426, 30460, 30495, 30532, 30567, 30603,
       30639, 30684, 30728, 30801, 30862, 33126, 33282, 33324, 33436, 33544, 33635,
       33738, 33846, 33948, 34043, 34152, 34266)

ovars <- paste0("er", n + 1)
nvars <- paste0("whynr", c(1968:1997, seq(1999, 2013, 2)))
ovars[grep("1968|2005|2007|2009|2011", nvars)]
setnames(dat, ovars, nvars)

table(dat$whynr1968, useNA = "ifany")
table(dat$whynr1980, useNA = "ifany")

# type of sample
dat[er30001 %in% 1:2930, tysample := "src"]
dat[er30001 %in% 5001:6872, tysample := "seo"]
dat[er30001 %in% 3001:3511, tysample := "immigrant refresher"]
dat[er30001 >= 7000, tysample := "latino"]

table(dat$tysample, useNA = "ifany")

# r died between surveys
# based on sequence number (81-89), not consistent
nrow(dat[sn1969 %in% 81:89])
nrow(dat[whynr1969 == 41])
nrow(dat[whynr1969 == 41 & sn1969 == 0])

dat[sn1969 %in% 81:89  & whynr1969 == 41, .(sn1969, typr1969)]
dat[sn1969 == 0 & whynr1969 == 41, .(sn1969, typr1969)]

# dat[sn1969 %in% 81:89]
# nrow(dat[sn1970 %in% 81:89])
# nrow(dat[whynr1970 == 41])
# nrow(dat[whynr1970 == 41 & sn1970 == 0])

# check number if cases
nrow(dat[rth1968 != 0])
nrow(dat[rth1969 != 0])
nrow(dat[rth1970 != 0])

# it was in the FU typr == 0, or weight equal to 0
# how to deal with the weight of people at an institution?
# R in a given survey regardless whether in current FU
# observed in 1968 and 1970, but not necessary in current FU
# rth == 0

nrow(dat[rth1968 != 0 & rth1970 != 0])

# observed in 1968 and 1970 but not 1969
nrow(dat[rth1968 != 0 & rth1970 != 0 & rth1969 == 0])

# in 1968 and 1970 panels but not in 1969 panesls (current FU)
nrow(dat[typr1968 == 0 & typr1970 & typr1969 != 0])

# observed in 1969 or 1970 were not observed in 1968
nrow(dat[rth1968 == 0 & (rth1969!=0 | rth1970!=0), ])
nrow(dat[typr1968!=0 & (typr1969 == 0| typr1970 == 0), ])

# incarceration 1995
# question only for 13-49 years old respondents
if ("prison1995" %in% names(dat)) { dat[, prison1995 := NULL] }
dat[er33267 %in% c(1, 5, 8, 9), prison1995 := 0]
dat[er33267 == 1, prison1995 := 1]
dat[er33267 %in% c(8,9), prison1995 := 9]
table(dat$prison1995, useNA = "ifany")

# people who attended school, booked and charged (er33266)
dat[er33266 %in% c(1, 5) & is.na(prison1995), prison1995 := 0]
dat[er33266 %in% c(8, 9) & is.na(prison1995), prison1995 := 9]
table(dat[, .(prison1995)], useNA="ifany")
table(dat[, .(er33266, prison1995)], useNA="ifany")

table(dat[age1995 < 13 | age1995 > 49, prison1995], useNA = "ifany")
table(dat[, .(age1995, er33266)], useNA="ifany")
table(dat[, .(age1995, er33267)], useNA="ifany")
table(dat[, .(age1995, prison1995)], useNA="ifany")

# er33225 == 9996, those who never were at school
table(dat$er33225, useNA = "ifany")
dat[er33225  %in% c(1943, 1950:1995, 9997:9999)
    & age1995 %in% c(13:49)
    & is.na(prison1995),
    prison1995 := 0]

table(dat$prison1995, useNA = "ifany")

# year last released from prison
table(dat[, er33271], useNA = "ifany")

# at an institution now = 1995
dat[er33271 == 9997, yprison1995 := 1995]
# year reported
dat[er33271 %in% 1960:1995, yprison1995 := er33271]
# if I don't know, I impute 1995
dat[er33271 %in% c(9998, 9999), yprison1995 := 1995]

# I do not remember why I did this!
dat[gened == 1 & (age1979 == 1 | age1980 == 1 | age1981 == 1),]
vars <- paste0("age", 1979:1986)
dat[, test := as.numeric(apply(dat[, vars, with = FALSE], 1,
                         function (x) any(x == 1)))]

temp <- dat[gened == 1 & test == 1, ]
table(duplicated(temp, by=c("pid")))
table(dat[gened == 1, test], useNA = "ifany")

dat[gened == 1 & apply(dat[, vars, with = FALSE], 1,
                       function (x) any(x == 1)) ]

ncol(temp)

nrow(dat[gened == 1 & age1979 == 1]) + nrow(dat[gened == 1 & age1980 == 1])
    + nrow(dat[gened == 1 & age1981 == 1]) + nrow(dat[gened == 1 & age1982 == 1])
    + nrow(dat[gened == 1 & age1983 == 1]) + nrow(dat[gened == 1 & age1984 == 1])

# remove records out of the range 13:49
dat[age1995 %in% c(0:12, 50:999) & !is.na(prison1995), prison1995 := NA]
dat[age1995 %in% c(0:12, 50:999) & !is.na(yprison1995), prison1995 := NA]

table(dat[, .(age1995, prison1995)])
table(dat$prison1995, useNA = "ifany")
table(dat$yprison1995, useNA = "ifany")
table(dat[, .(yprison1995, prison1995)], useNA="ifany")

############################
# other covariates!
############################

# further adjustments will be done using a long format (it is easier)

# education
# individual, not ask annually to head of households I will need family data
ovars <- c("er30010","er30052","er30076","er30100","er30126","er30147",
           "er30169","er30197","er30226","er30255","er30296","er30326","er30356",
           "er30384","er30413","er30443","er30478","er30513","er30549","er30584",
           "er30620","er30657","er30703","er30748","er30820","er33115","er33215",
           "er33315","er33415","er33516","er33616","er33716","er33817","er33917",
           "er34020","er34119","er34230")

length(ovars)

nvars <- paste0("edu", c(1968, 1970:1997, seq(1999, 2013, 2)))
length(nvars)
edu <- nvars
setnames(dat, ovars, nvars)
lookvar(dat, "edu") # 1969 not available!

# employment status
ovars <- c("er30293","er30323","er30353","er30382","er30411",
           "er30441","er30474","er30509","er30545","er30580","er30616",
           "er30653","er30699","er30744","er30816","er33111","er33211",
           "er33311","er33411","er33512","er33612","er33712","er33813",
           "er33913","er34016","er34116","er34216")

# asked to those who were 16 years old or older
year <- c(1979:1997, seq(1999, 2013, 2))
nvars <- paste0("empl", year)
employment <- nvars

setnames(dat, ovars, nvars)

table(dat[, empl1979], useNA = "ifany")
table(dat[, empl1995], useNA = "ifany")
table(dat[, empl2013], useNA = "ifany")
dat[, nvars, with = FALSE]

# last known marital status
setnames(dat, "er32049", "marital_status_ln")
# 8 missing
# 9 not ask his/her marital history
table(dat$marital_status_ln, useNA = "ifany") # all cases with valid values

# load pairs (I don't include these data)
# marital <- data.table(read_dta("data/marital.dta"))
# setnames(marital, names(marital), tolower(names(marital)))
# ovars <- c("er30005","er30024","er30047","er30071","er30095","er30121",
#            "er30142","er30164","er30192","er30221","er30250","er30287",
#            "er30317","er30347","er30377","er30405","er30435","er30469",
#            "er30504","er30541","er30576","er30612","er30648","er30695",
#            "er30739","er30812","er33107","er33207","er33307","er33407",
#            "er33507","er33607","er33707","er33807","er33907","er34007",
#            "er34107","er34207")

# nvars <- paste0("pairs", c(1968:1997, seq(1999, 2013, 2)))
# pairs <- nvars

# setnames(marital, ovars, nvars)

# marital[, pid := er30001 * 1000 + er30002]
# table(duplicated(marital, by=c("pid")))

# setkey(marital, pid)
# marital <- marital[, c("pid", nvars), with = FALSE]
# marital

# setkey(dat, pid)
# dat <- marital[dat]
# nrow(dat)

# health (different reports, I should combine then in some way)

# individual health status
# for everyone, 1994 to 1996, just before prison question, baseline?
ovars <- c("er33128", "er33284", "er33326")
nvars <- paste0("shealth", 1994:1996)
shealth <- nvars
setnames(dat, ovars, nvars)
setnames(dat, "er30527", "ofu_shealth1986")

# individuals (all)
table(dat$ofu_shealth1986, useNA = "ifany") # other FU members
table(dat$shealth1994, useNA = "ifany")
table(dat$shealth1995, useNA = "ifany")
table(dat$shealth1996, useNA = "ifany")
dat[, paste0("shealth", 1994:1996), with = FALSE]

# health of head of household (1984-2013)
# FU level variable
ovars <- c("v10877","v11991","v13417","v14513","v15993","v17390","v18721",
           "v20021","v21321","v23180","er3853","er6723","er8969","er11723","er15447",
           "er19612","er23009","er26990","er38202","er44175","er49494","er55244")

nvars <- paste0("hh_health", c(1984:1997, seq(1999, 2013,2)))
hh_health <- nvars
setnames(dat, ovars, nvars)

# responses repeat per FU
summary(dat$sn1994) # current hh sn == 1
dat[sn1994 == 1, .(pid, rth1994, hh_health1994, shealth1994)]

# not always consistent, why?
table(dat[sn1994 == 1, .(hh_health1994, shealth1994)])
dat[sn1994 == 1 & shealth1994 == 2 & hh_health1994 == 4,
    .(rth1994, sn1994, pid, age1994, gender, hh_health1994, shealth1994)]

# only wife
ovars <- c("v10884","v12344","v13452","v14524","v15999","v17396","v18727",
           "v20027","v21328","v23187","er3858","er6728","er8974","er11727","er15555",
           "er19720","er23136","er27113","er39299","er45272","er50612","er56360")

nvars <- paste0("ww_health", c(1984:1997, seq(1999, 2013,2)))
ww_health <- nvars
setnames(dat, ovars, nvars)

dat[sn1994 == 2, .(pid, rth1994, ww_health1994, shealth1994)]

# not always consistent, why?
table(dat[sn1994 == 2 & rth1994 == 20, .(ww_health1994, shealth1994)])
dat[sn1994 == 2 & shealth1994 == 3 & hh_health1994 == 4, .(rth1994, fu1994, sn1994,
                                                           pid, age1994, gender, hh_health1994,
                                                           shealth1994)]

# other FU health (good or poor) 1988 to 2013
ovars <- c("er30598","er30634","er30671","er30719","er30764",
           "er30827","er33117","er33217","er33317","er33417","er33517","er33617",
           "er33717","er33818","er33918","er34021","er34120","er34231")

nvars <- paste0("ofu_dhealth", c(1988:1997, seq(1999, 2013, 2)))
ofu_dhealth <- nvars
setnames(dat, ovars, nvars)

table(dat$ofu_dhealth1988, useNA = "ifany")
table(dat$ofu_dhealth2013, useNA = "ifany")

###########################
# death (psid)
###########################

table(dat[, er32050])

dat[, death := ifelse(er32050 > 0, 1, 0)] # including 9999

table(dat$death, useNA = "ifany") # 6863

dat[pid == 1167001, er32050]

# dat[pid == 6156001, c("death", "er32050") := list(1, 2005)]
dat[pid == 6156001, .(death, er32050)]
dat[, ydeath := ifelse(er32050 %in% 1967:2015, er32050, NA)]
summary(dat$ydeath) # correct

# impute range of years
values <- sort(unique(dat[!er32050 %in% 1967:2013, er32050]))
values
(values <- values[values != 0 & values != 9999])

values1 <- floor(values / 100)
values1
(values1 <- ifelse(values1 < 16, values1 + 2000, values1 + 1900)) # floor
values2 <- values %% 100
(values2 <- ifelse(values2 < 16, values2 + 2000, values2 + 1900)) # ceiling

length(values1) == length(values2)
values3 <- round((values1 + values2)/2) # mid-year

# impute year of death using mid-year
for (i in seq_along(values)) {
    dat[er32050 == values[i], ydeath := values3[i]]
}

table(dat$ydeath, useNA = "ifany")

table(dat[, .(ydeath, death)], useNA = "ifany") # only 39 cases missing
dat[pid == 1167001, ydeath]

# check computation
dat[er32050 == values[49], .(ydeath, er32050)]

# correct some years of death
tab <- fread("data/psid_mortality_records.csv")
anyDuplicated(tab)

for (i in 1:nrow(tab)) {
    dat[pid == tab[i, pid], ydeath := tab[i, dyear]]
}

table(dat$death, useNA = "ifany")

# check
dat[pid == 1167001, .(death, ydeath)] # 2000
summary(dat$ydeath)
table(dat[, .(ydeath, death)], useNA = "ifany") # only 38 cases missing

# individual sampling weights

# define years
years <- c(1968:1997, seq(1999, 2013, 2))

setnames(dat, c("er31996", "er31997"), c("stratum", "cluster"))

# latino + core
ovars <- c("er30688","er30732","er30805","er30866","er33121","er33277")
nvars <- paste0("wtl", 1990:1995)
setnames(dat, ovars, nvars)

# core + immigrants
wvars <- c("er30019","er30042","er30066","er30090","er30116","er30137","er30159",
           "er30187","er30216","er30245","er30282","er30312","er30342","er30372",
           "er30398","er30428","er30462","er30497","er30534","er30569","er30605","er30641",
           "er30686","er30730","er30803","er30864","er33119","er33275","er33318","er33430",
           "er33546","er33637","er33740","er33848","er33950","er34045","er34154",
           "er34268")

nwtvars <- paste0("wt", years)
setnames(dat, wvars, nwtvars)

# complete variables
cvars <- paste0("wtl", years)
vars <- cvars[which(!cvars %in% lookvar(dat, "^wtl"))]
dat[, (vars) := NA]


###############################################
# family level variables (income and race)
###############################################

# race
# individual for head of the house household and wife

# head, first mention
ovars <- c("v181","v801","v1490","v2202","v2828","v3300","v3720","v4204",
           "v5096", "v5662","v6209","v6802","v7447","v8099","v8723","v9408","v11055",
           "v11938", "v13565","v14612","v16086","v17483","v18814","v20114","v21420",
           "v23276", "er3944","er6814","er9060","er11848","er15928","er19989",
           "er23426","er27393","er40565","er46543","er51904","er57659")

nvars <- paste0("hh_race", c(1968:1997, seq(1999, 2013, 2)))
hh_race <- nvars
setnames(dat, ovars, nvars)

table(dat[sn1989 == 1, hh_race1989], useNA = "ifany")
table(dat$hh_race2001, useNA = "ifany")

# wife, first mention, 1985-2013
ovars <- c("v12293","v13500","v14547","v16021","v17418","v18749","v20049",
           "v21355","v23212","er3883","er6753","er8999","er11760","er15836","er19897",
           "er23334","er27297","er40472","er46449","er51810","er57549")

nvars <- paste0("ww_race", c(1985:1997, seq(1999, 2013, 2)))
ww_race <- nvars
setnames(dat, ovars, nvars)
table(dat[sn1985 == 2 & rth1985 %in% c(20, 22), ww_race1985], useNA = "ifany")
table(dat$ww_race2013, useNA = "ifany")

# region
ovars <- c("v361","v876","v1572","v2284","v2911","v3279","v3699","v4178",
           "v5054","v5633","v6180","v6773","v7419","v8071","v8695","v9381",
           "v11028","v12379","v13631","v14678","v16152","v17538","v18889","v20189",
           "v21495","v23327","er4157e","er6997e","er9248e","er12221e","er16430",
           "er20376","er24143","er28042","er41032","er46974","er52398","er58215")

nvars <- paste0("region", c(1968:1997, seq(1999, 2013, 2)))
region <- nvars
setnames(dat, ovars, nvars)

table(dat$region1968, useNA = "ifany")

# total income
ovars <- c("v81","v529","v1514","v2226","v2852","v3256","v3676","v4154","v5029",
           "v5626","v6173","v6766","v7412","v8065","v8689","v9375","v11022","v12371",
           "v13623","v14670","v16144","v17533","v18875","v20175","v21481","v23322",
           "er4153","er6993","er9244","er12079","er16462","er20456","er24099",
           "er28037","er41027","er46935","er52343","er58152")

nvars <- paste0("inc", c(1968:1997, seq(1999, 2013, 2)))
inc <- nvars
setnames(dat, ovars, nvars)

# recode negative and 0 values as 1
dat[, (nvars) := lapply(.SD,
    function(x) ifelse(x <= 1, 1, x)), .SDcols = nvars]

# select variables
years <- c(1968:1997, seq(1999, 2013, 2))

rth <- paste0("rth", years)
sn <- paste0("sn", years)
typr <- paste0("typr", years)
fn <- paste0("fn", years)
fu <- paste0("fu", years)
whynr <- paste0("whynr", years)
age <- paste0("age", years)
wt <- paste0("wt", years)
wtl <- paste0("wtl", years)

vars <-  c("pid", "family_id", fn, fu, sn, typr, rth, "stratum", "cluster", wt, wtl,
           "smember", "tysample", "gened", whynr, "gender", age,
           "marital_status_ln", edu, employment, "prison1995", "yprison1995",
           "ofu_shealth1986", shealth, hh_health, ww_health, ofu_dhealth,
           "death", "ydeath",  hh_race, ww_race, region, inc)

# long format
wdat <- copy(dat[, vars, with = FALSE])

# create null variables when variables were not included
# length(fn)
# length(fu)
# length(sn)
# length(typr)
# length(rth)
# length(whynr)
# length(age)
# length(wt)
# length(wtl)
# length(edu)

cvars <- paste0("edu", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^edu"))]
wdat[, (vars) := NA]

length(employment)
cvars <- paste0("empl", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^empl"))]
wdat[, (vars) := NA]

length(shealth)
cvars <- paste0("shealth", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^shealth"))]
wdat[, (vars) := NA]

length(hh_health)
cvars <- paste0("hh_health", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^hh_health"))]
wdat[, (vars) := NA]

length(ww_health)
cvars <- paste0("ww_health", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^ww_health"))]
wdat[, (vars) := NA]

length(ofu_dhealth)
cvars <- paste0("ofu_dhealth", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^ofu_dhealth"))]
wdat[, (vars) := NA]

length(hh_race)

length(ww_race)
cvars <- paste0("ww_race", years)
vars <- cvars[which(!cvars %in% lookvar(wdat, "^ww_race"))]
wdat[, (vars) := NA]

length(region)
length(inc)

wdat <- wdat[, sort(names(wdat)), with = FALSE]

# patterns and variables
patt <- c("^fn", "^fu", "^sn", "^wtl[0-9]", "^wt[0-9]","^typr", "^rth", "^whynr", "^age", "^edu",
          "^empl", "^shealth", "^hh_health", "^ww_health", "^ofu_dhealth", "^hh_race",
          "^ww_race", "^region", "^inc")

vnames <- c("fn", "fu", "sn", "wtl", "wt", "typr", "rth",
            "whynr", "age", "edu", "empl", "shealth", "hh_health", "ww_health",
            "ofu_dhealth", "hh_race", "ww_race", "region", "inc")

# to avoid melting warnings
wdat[, (paste0("edu", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("edu", years)]

wdat[, (paste0("empl", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("empl", years)]

wdat[, (paste0("shealth", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("shealth", years)]

wdat[, (paste0("hh_health", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("hh_health", years)]

wdat[, (paste0("ww_health", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("ww_health", years)]

wdat[, (paste0("ofu_dhealth", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("ofu_dhealth", years)]

wdat[, (paste0("ww_race", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("ww_race", years)]

wdat[, (paste0("wtl", years)) := lapply(.SD, as.numeric),
    .SDcols = paste0("wtl", years)]

length(patt) == length(vnames)

ldat <- melt(wdat, measure.vars = patterns(patt),
    value.name = vnames,
    variable.name = "time")

# create year variable
for (i in seq_along(years)) {
    ldat[time == i, year := years[i]]
}

# explore
ids <- unique(ldat$pid)
id <- sample(ids, 1)
ldat[pid == id, .(pid, year, time, whynr)]
wdat[pid == id, lookvar(wdat, "whynr"), with = FALSE]


# save data
vars <-  c("pid", "family_id", "year", "fn", "fu", "sn", "typr", "rth", "stratum", "cluster", "wt", "wtl",
           "smember", "tysample", "gened",
           "whynr", "gender", "age",  "marital_status_ln", "edu", "empl",
           "prison1995", "yprison1995", "ofu_shealth1986", "shealth",
           "hh_health", "ww_health", "ofu_dhealth", "death", "ydeath",
           "hh_race", "ww_race", "region", "inc")

ldat <- ldat[, vars, with = FALSE]
setkey(ldat, pid, year)
ldat[pid == sample(ids, 1)]

# define weights
ldat[wt == 0, wt := NA]
ldat[wtl == 0, wtl := NA]

ldat[!is.na(fu), awt := mean(wt, na.rm = TRUE), by = .(year, fu)]
ldat[!is.na(fu), awtl := mean(wtl, na.rm = TRUE), by = .(year, fu)]
countmis(ldat[, .(wt, awt, wtl, awtl)])

ldat[, iwt := wt]
ldat[is.na(iwt), iwt := wtl]
ldat[is.na(wt), iwt := awt]
ldat[is.na(iwt), iwt := awtl]

summary(ldat$iwt)
# hist(ldat$iwt)

saveRDS(wdat, file = "output/psid_ind_wide_selection_13.rds")
saveRDS(ldat, file = "output/psid_ind_long_selection_13.rds")

#############################
# end script
#############################
