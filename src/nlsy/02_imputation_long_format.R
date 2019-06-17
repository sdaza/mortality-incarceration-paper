##########################
# imputation long format NLSY79
##########################

rm(list=ls(all=TRUE))

#+ library
library(sdazar)
library(ggplot2)
library(zoo)
library(colorout)

#+ load data
# setwd("/Users/sdaza/Google Drive/01Projects/01IncarcerationHealth/")
# load("05Research/sdaza/output/rdata/nlsy/nlsy79_longformat.Rdata")
setwd("00projects/incarceration")
load("nlsy/nlsy79_longformat.Rdata")
source("nlsy/functions.R")

#+ load data
# load("output/rdata/long_selection_covariates_year_unit.Rdata")

#+ define data set for imputation
vars <- c("id", "year", "start", "stop", "agei", "male", "race", "edui", "pedu",
          "married", "job", "healthw", "rprison", "died", "lincome_adj", "deltot",
          "dropout", "stratum", "cluster", "wt")

ex <- ldat[, vars, with = FALSE]

#+ death variable
ex[, death := max(died), id]
ex[, adropout := max(dropout), id]
table(ex$death, useNA = "ifany")
table(ex$died, useNA = "ifany")

#+ center some variables
summary(ex$edui)
ex[, cedui := edui - 13]
ex[, cpedu := pedu - 13]

ex[, cage := scale(agei, center = TRUE, scale = FALSE)][, cage2 := cage^2]
ex[, cyear := scale(year, center = TRUE, scale = FALSE)][, cyear2 := cyear^2]
ex[, clincome_adj := scale(lincome_adj, center = TRUE, scale = FALSE)]
ex[, cdeltot := scale(deltot, center = TRUE, scale = FALSE)]

countmis(ex)

md.pattern(ex[, .(clincome_adj, job,healthw,deltot, married,rprison, pedu)])

vars <- c("id", "year", "cyear", "cyear2", "start", "stop", "agei", "cage", "cage2",
         "male", "race", "cedui", "cpedu",
          "married", "job", "healthw", "rprison", "died", "death",
          "clincome_adj", "cdeltot", "dropout", "adropout",
          "stratum", "cluster", "wt")

ex <- ex[, vars, with = FALSE]
countmis(ex)

#+ impute using mice
imp <- mice(ex, maxit = 0)
mat <- imp$predictorMatrix

#+ define matrix using fixed effects and specific methods
mat[,] <- 0

mat[c("rprison", "cpedu", "job",
      "healthw", "cdeltot", "clincome_adj", "cedui", "married"), "id"] <- -2 # cluster variable

mat["cpedu", c("cedui", "male", "race", "cage", "job", "rprison",
               "clincome_adj", "healthw", "cdeltot",
               "death", "adropout", "married")] <- 1

mat["cedui", c("cyear", "cpedu", "male", "race", "cage", "job", "rprison",
               "clincome_adj", "healthw", "cdeltot",
               "death", "adropout", "married")] <- 1

mat["rprison", c("cyear", "cpedu", "male", "race", "cage", "job", "cedui",
               "clincome_adj", "healthw", "cdeltot",
               "death", "adropout", "married")] <- 1

mat["job", c("cyear", "cpedu", "male", "race", "cage", "rprison", "cedui",
               "clincome_adj", "healthw", "cdeltot",
               "death", "adropout", "married")] <- 1

mat["married", c("cyear", "cpedu", "male", "race", "cage", "rprison", "cedui",
               "clincome_adj", "healthw", "cdeltot",
               "death", "adropout", "job")] <- 1

mat["clincome_adj", c("cyear", "cpedu", "male", "race", "cage", "rprison", "cedui",
              "married", "healthw", "cdeltot",
               "death", "adropout", "job")] <- 1

mat["cdeltot", c("cpedu", "male", "race", "cage", "rprison", "cedui",
              "married", "healthw", "clincome_adj",
               "death", "adropout", "job")] <- 1

mat["healthw", c("cyear", "cpedu", "male", "race", "cage", "rprison", "cedui",
              "married", "cdeltot", "clincome_adj",
               "death", "adropout", "job")] <- 1

# define mehtos
meth <- imp$meth
meth["clincome_adj"] <- "2l.pmm"
meth["cedui"] <- "2l.pmm"
meth["healthw"] <- "2l.pmm"
meth["married"] <- "2l.pmm"
meth["job"] <- "2l.pmm"
meth["rprison"] <- "2l.pmm"
meth["cpedu"] <- "2lonly.pmm"
meth["cdeltot"] <- "2lonly.pmm"

meth

#+ imputation

# mice
# imp <- mice(data = ex, seed = 123456, predictorMatrix = mat , method = meth ,
#               maxit = 10, m = 1)

# parallel
imp1 <- parmice(data = ex, seed = 123456,  predictorMatrix = mat , method = meth ,
              maxit = 10, n.core = 10, n.imp.core = 1)

save(imp1, file = "nlsy/imp1.Rdata")

# load("nlsy/imp1.Rdata")

# summary(imp1)
# plot(imp1)

###########################
#+ process imputation
###########################

# rm(list=ls(all=TRUE))

# library(sdazar)
# library(lattice)
# library(ggplot2)
# library(survival)
# library(ipw)

# setwd("/Users/sdaza/Google Drive/01Projects/01IncarcerationHealth/")
# load("05Research/sdaza/output/rdata/nlsy/imp1.Rdata")

# summary(imp1)

# plot(imp1)
# # densityplot(imp1, data = ~ cedui)

# # get data
# long <- data.table(complete(imp1, "long", inc = TRUE))
# exi <- data.table(complete(imp1, 1))
# org <- data.table(complete(imp1, 0))


# org[, income_missing := ifelse(is.na(clincome_adj), 1, 0)]
# org[, mage := min(agei), id]
# names(org)
# screenreg(glm(income_missing ~ male + mage + race + rprison + job +
#   married + cpedu + adropout + cdeltot + year, dat = org))

# # explore individual imputations
# ids <- unique(exi$id)
# sid <- sample(ids, 1)
# exi[id == sid , .(id, start, stop, year, agei, male, race, rprison, clincome_adj)]
# org[id == sid , .(id, start, stop, year, agei, male, race, rprison, clincome_adj)]

# names(exi)

# table(long$agei, useNA = "ifany")
# table(long$year, useNA = "ifany")

# years <- c(1980:2014)

# # income
# temp <- long[, list(myvar = mean(clincome_adj, na.rm = TRUE)), by = .(agei, .imp)]
# ggplot(temp[.imp != 0], aes(x = agei, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0 & agei >=18 & agei < 90], aes(x = agei, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "income by age (10 imputations)", x = "\nage", y = "ln income centered\n")

# temp <- long[, list(myvar = mean(clincome_adj, na.rm = TRUE)), by = .(year, .imp)]
# ggplot(temp[.imp != 0], aes(x = year, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0], aes(x = year, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "income by year (10 imputations)", x = "\nyear", y = "ln income centered\n")

# # job
# temp <- long[, list(myvar = mean(job, na.rm = TRUE)), by = .(agei, .imp)]
# ggplot(temp[.imp != 0], aes(x = agei, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0], aes(x = agei, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "job by age (10 imputations)", x = "\nage", y = "prop job\n")

# temp <- long[, list(myvar = mean(job, na.rm = TRUE)), by = .(year, .imp)]
# ggplot(temp[.imp != 0], aes(x = year, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0 & year %in% years], aes(x = year, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "job by age (10 imputations)", x = "\nyear", y = "years of education centered\n")

# # prison
# temp <- long[, list(myvar = mean(rprison, na.rm = TRUE)), by = .(agei, .imp)]
# ggplot(temp[.imp != 0], aes(x = agei, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0], aes(x = agei, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "proportion in prison by age (10 imputations)", x = "\nage", y = "proportion in prison \n")


# temp <- long[, list(myvar = mean(rprison, na.rm = TRUE)), by = .(year, .imp)]
# ggplot(temp[.imp != 0], aes(x = year, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0], aes(x = year, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "proportion in prison by year (10 imputations)", x = "\nyear", y = "proportion in prison \n")

# # this look more problematic
# temp <- long[, list(myvar = mean(healthw, na.rm = TRUE)), by = .(agei, .imp)]
# ggplot(temp[.imp != 0], aes(x = agei, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0 & agei >=18 & agei < 60], aes(x = agei, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "proportion poor health by age (10 imputations)", x = "\nage", y = "proportion poor health \n")

# temp <- long[, list(myvar = mean(healthw, na.rm = TRUE)), by = .(year, .imp)]
# ggplot(temp[.imp != 0], aes(x = year, y = myvar, group = .imp)) +
#  geom_line(colour = "gray") +
#  geom_line(data = temp[.imp == 0], aes(x = year, y = myvar), colour = "red") +
#   theme_bw() + labs(title = "proportion poor health by year (60 imputations)", x = "\nyear", y = "proportion poor health \n")

# #+ create models per imputation

# # redefine some variables
# limp <- long[.imp != 0]
# setkey(limp, .imp, id, start)

# limp[, cprison := cumsum(rprison), by = .(.imp, id)][, prison := ifelse(cprison > 0, 1, 0)]
# table(limp$prison, useNA = "ifany")
# limp[, chealth := cumsum(healthw), by =.(id, .imp)][, poorhealth := ifelse(chealth > 0, 1, 0)]
# table(limp$poorhealth, useNA = "ifany")

# table(limp$race, useNA = "ifany")

# # education
# limp[, edu := cedui + 13]
# table(limp$edu)
# limp[, pedu := cpedu + 13]
# table(limp$pedu)

# limp[, mage := min(agei), id]
# limp[, magef := factor(mage)]
# table(limp$magef)

# # limp[edu %in% 1:11, educ := "less high school"]
# # limp[edu == 12, educ := "high school"]
# # limp[edu %in% 13:15, educ := "some college"]
# # limp[edu > 15, educ := "college"]
# # limp[, educ := factor(educ, levels = c("less high school", "high school",
# #                                        "some college", "college"))]
# # table(limp$educ)
# # run models and then pooled results
# # table(limp$frace, useNA = "ifany")

# names(long)

# models <- list()
# for (i in 1:10) { # number of imputation
#       print(paste0(":::::::: running model for imputation ", i))
#       t <- limp[.imp == i]
#       models[[i]] <- coxph( Surv(start, stop, died) ~ prison * male + magef + race +
#                            edu + pedu + clincome_adj + poorhealth +
#                            job + married + cdeltot, data = t)
# }

# summary(mitools::MIcombine(models))

# i <- 1
# modelsmsm <- list()
# for (i in 1:10) { # number of imputation
#       print(paste0(":::::::: running model for imputation ", i))
#       t <- limp[.imp == i]
#       w1 <- ipwtm(exposure = dropout, family = "survival",
#               numerator = ~ male + magef + race + cdeltot + pedu,
#               denominator = ~ male + magef + race + cdeltot + pedu +
#                             prison +  edu + clincome_adj + healthw +
#                            job + married,
#               id = id,
#               tstart = start, timevar = stop,
#               type = "first",
#               data = t)
#       w2 <- ipwtm(exposure = prison, family = "survival",
#               numerator = ~ male + magef + race + cdeltot + pedu,
#               denominator = ~ male + magef + race + cdeltot + pedu +
#                               edu + clincome_adj + healthw +
#                            job + married, id = id,
#               tstart = start, timevar = stop,
#               type = "first",
#               data = t)
#       t[, wt := w1$ipw.weights * w2$ipw.weights]
#       modelsmsm[[i]] <- coxph( Surv(start, stop, died) ~ prison + magef + male + race +
#                            edu + pedu + clincome_adj + healthw +
#                            job + married + cdeltot + cluster(id),
#                            weights = t$wt,
#                            data = t)
# }

# summary(mitools::MIcombine(modelsmsm))

#######################
#######################
# m1 <- coxph( Surv(start, stop, dropout) ~ magei + male + cprison + race
#             + lincome_adj + edu, data = ldat)
# summary(m1)

# xx <- ldat[agei >=24 & age <= 47]
# table(xx[, .(cprison, died)]) # 87, small sample any ways
# table(xx[male == 1, .(cprison, died)]) # 87, small sample any ways
# table(xx[male == 0, .(cprison, died)]) # 87, small sample any ways

# library(survival)

# library(coxphf)
# m1 <- coxphf(formula = Surv(start, stop, died) ~ magei + male
#                 + cprison + race , data = ldat, pl=FALSE)


# # imputation
# ex <- ldat[, .(id, start, stop, died, agei, male, rprison, race, lincome_adj)]
# countmis(ex)

# imp <- mice(ex, maxit = 0)
# mat <- imp$predictorMatrix
# meth <- imp$meth

# mat
# mat[, c("start", "stop", "died")] <- 0

# mat[c("rprison", "lincome_adj"), "id"] <- -2

# meth$rprison <- "2l.pmm"
# meth$lincome_adj <- "2l.pmm"

# str(ex)
# meth <- imp$meth
# mat <- imp$predictionMatrix

# imp <- mice(data = ex, seed = 123456, predictorMatrix = mat , method = meth)
# fit <- survfit(Surv(start, stop, died) ~ magei + male, data = ldat)
# ggsurvplot(fit, data = ldat, risk.table = TRUE)

#  # + gender + fracei + prisonwhynr_first  + cluster(family_id),
#               # data = m1dat)



