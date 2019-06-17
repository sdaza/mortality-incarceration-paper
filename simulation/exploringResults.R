
##########################################
# exploring check censoring and truncation effects
##########################################

rm(list=ls(all=TRUE) )

# the ABM is a multi-state process


library(sdazar)
library(survival)
library(texreg)
library(eha)

dat <- fread("~/Desktop/incarcerationABM/output.csv")
length(table(dat$rep))

# 500 replicates

coefb <- NULL
coefvar <- NULL
coefnvar <- NULL

maxrep <- 500

for (i in 1:maxrep) {
  
 sdat <- dat[rep == i]
 sdat[ageprison < agedeath, prison := ifelse(ageprison > 0, 1, 0)]
 table(sdat$prison)
 sdat[, stop := agedeath]
 sdat[, start := 0]
 
 sdat[, event := 1]
 sdat[, count := ifelse(prison > 0, 2, 1)]
 sdat <- sdat[rep(seq(1, nrow(sdat)), sdat$count)] # expand data
 sdat[, s := 1:.N, id]
 
 sdat[count == 1, vprison := 0]
 sdat[count == 2 & s == 1, stop := ageprison]
 sdat[count == 2 & s == 2, start := ageprison]
 sdat[count == 2 & s == 2, vprison := 1]
 sdat[count == 2 & s == 1, vprison := 0]
 sdat[event == 1 & count == 2 & s == 1, event := 0]
 sdat[count == 2]
 
 m0 <- coxph(Surv(start, stop, event) ~ vprison, data = sdat) 
 coefb <- c(coefb, exp(coef(m0)[1]))
 
 tdat <- dat[rep == i]  
 tdat[ageprison < censored | ageprison < agedeath, anyprison := ifelse(ageprison > 0, 1,0)]
 tdat[, exit := ifelse(agedeath > censored, censored, agedeath)]
 tdat[ageprison > exit, ageprison := 0]
 tdat[ageprison < enter, ageprison := 0] # under estimation
 tdat[, select := ifelse(exit > enter, 1, 0)] # only if death occured before death
 tdat[, event := ifelse(agedeath == exit, 1, 0)] # deaths
 tdat <- tdat[select == 1]
 tdat[, prison := ifelse(ageprison > 0, 1, 0)] # people with prison experiences

 tdat[, age := enter]
 tdat[, start := 0]
 tdat[, stop := exit - enter] # observation time
 tdat[ageprison > 0, tprison := ageprison - enter] # observation time for prison
 
 # split dataset
 tdat[, count := ifelse(ageprison > 0, 2, 1)]
 tdat <- tdat[rep(seq(1, nrow(tdat)), tdat$count)] # expand data
 tdat[, s := 1:.N, id]
 
 tdat[count == 1, vprison := 0]
 tdat[count == 2 & s == 1, stop := tprison]
 tdat[count == 2 & s == 2, start := tprison]
 tdat[count == 2 & s == 2, vprison := 1]
 tdat[count == 2 & s == 1, vprison := 0]
 tdat[event == 1 & count == 2 & s == 1, event := 0]
 
 m1 <- coxph(Surv(start, stop, event) ~ vprison + age, data = tdat) 
 coefvar <- c(coefvar,exp(coef(m1)[1]))
 m2 <- coxph(Surv(start, stop, event) ~ prison + age, data = tdat) 
 coefnvar <- c(coefnvar, exp(coef(m2)[1]))

}

hist(coefb)
median(coefb)

hist(coefvar)
median(coefvar)

hist(coefnvar)
median(coefnvar)

