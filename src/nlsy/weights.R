
# get wegihts


new_data <- read.table("/Users/sdaza/Google Drive/01Projects/01IncarcerationHealth/04Data/NLSY79/nlsy79/weights/weights.dat",
                       sep='')

names(new_data) <- c('A0002600', 'R0000100', 'R0173600', 'R0214700', 'R0214800', 'R0216100')

# varlabels <- c("VERSION_R26_1 2014",
#   "ID# (1-12686) 79",
#   "SAMPLE ID  79 INT",
#   "RACL/ETHNIC COHORT /SCRNR 79",
#   "SEX OF R 79",
#   "SAMPLING WEIGHT 79"

new_data <- data.table(new_data)
setnames(new_data, names(new_data), tolower(names(new_data)))

setnames(new_data, c("r0000100", "r0216100"), c("id", "wt"))
wt <- new_data[, .(id, wt)]
setkey(wt, id)

# get location
loc <- fread("/Users/sdaza/Documents/Workplace/Data/NLSY/NLSY79 data/location_123014.csv")
setnames(loc, names(loc), tolower(names(loc)))

loc <- loc[, .(r0000100, r0219145, r0219146)]
setnames(loc, names(loc), c("id", "cluster", "stratum"))
summary(loc)
setkey(loc, id)

# merge
nrow(loc)
nrow(wt)

wt <- loc[wt]
# hist(wt$wt)

save(wt, file = "/Users/sdaza/Google Drive/01Projects/01IncarcerationHealth/05Research/sdaza/data/nlsy79/weights.Rdata")

