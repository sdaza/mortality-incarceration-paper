##############################
# health and incarceration
# decomposition of mortality gap
# author: sebastian daza
##############################

library(sdazar)
library(stringr)
library(xtable)

# function decomposition
getAB <- function(mort1, mort2, incar1, incar2, p1, rr) {

  d = mort1 - mort2

  # get r
  r = p1/incar1
  p2 = r * incar2
  h1 = p1 *(rr-1) + 1
  h2 = p2 *(rr-1) + 1
  mu1 = mort1/h1
  mu2 = mort2/h2

  a =  ((h1 + h2) / 2 ) * (mu1 - mu2)
  b =  ((mu1 + mu2) / 2 ) * (h1 - h2)

  return( data.table("$\\beta_{prison}$" = round(log(rr), 2),
          "Hazard Ratio" = rr,
          "$P_{(x)}$" = p1,
          "$A_{(x)}$" = a/d,
          "$B_{(x)}$" = b/d) )

}

# read mortality data
ukmx <- fread("data/mortalityUK2011.csv")
ukmx[, nage := as.numeric(str_extract(age, "^[0-9]+"))]
table(ukmx$nage, useNA = "ifany")
str(ukmx)

usmx <- fread("data/mortalityUS2011.csv")
usmx[, nage := as.numeric(str_extract(age, "^[0-9]+"))]
table(usmx$nage, useNA = "ifany")
str(usmx)

# selection relevant rows to extract data
(uk_mort_20 <- sum(ukmx[nage >= 20, all]))
(us_mort_20 <- sum(usmx[nage >= 20, all]))

(uk_mort_20_70 <- sum(ukmx[nage >= 20 & nage < 70, all]))
(us_mort_20_70 <- sum(usmx[nage >= 20 & nage < 70, all]))

(uk_mort_20_45 <- sum(ukmx[nage >= 20 & nage < 45, all]))
(us_mort_20_45 <- sum(usmx[nage >= 20 & nage < 45, all]))

uk_mort_20_45 * 1000
us_mort_20_45 * 1000

uk_mort_20_70 * 1000
us_mort_20_70 * 1000

# define survival coefficients to assess
survival_coef <- sort(c(0.69, 0.63, 0.74, 0.70, 0.99, 0.88,
                        0.91, 0.92, 0.57))

# decomposition
results <- list()
for( i in seq_along(survival_coef)) {
    results[[i]] <- getAB(mort1 = us_mort_20_45,
                          mort2 = uk_mort_20_45,
                          incar1 = 707,
                          incar2 = 148,
                          p1 = .0209,
                          rr = exp(survival_coef[i]))
}

(tab_20_45 <-  rbindlist(results))
rm(results)


results <- list()
for( i in seq_along(survival_coef)) {
    results[[i]] <- getAB(mort1 = us_mort_20_70,
                          mort2 = uk_mort_20_70,
                          incar1 = 707,
                          incar2 = 148,
                          p1 = .0209, rr = exp(survival_coef[i]))
}

(tab_20_70 <-  rbindlist(results))
rm(results)

tab_20_45
tab_20_70

tab <- rbind(tab_20_45, tab_20_70)

# create table
print(xtable(tab,
             caption = "Imprisonment contribution to Mortality gap, U.S. and U.K."),
             caption.placement = "top", include.rownames = FALSE,
             table.placement = "htp",
             file = here("output/tables", "gap.tex"),
             sanitize.text.function=function(x){x}
)
rm(tab)
