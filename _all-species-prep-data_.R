#######################################################################
# various data prep steps for mixed models 
# Bryan S. McLean
# 27 April 2021, last updated 6 September 2023
#######################################################################

############
## DATA   ##
############

# read the data
dat <- read.csv("_all-species-v6_.csv")

# replace native Ochotona head-body length measurements with total lengths (none had tail lengths and tails are very short)
dat[which(dat$binomial == 'Ochotona princeps'),'headbodylength'] <- dat[which(dat$binomial == 'Ochotona princeps'),'totallength'] 

############
## CHECKS ##
## ON     ##
## CLIMATE##
## DATA   ##
############

## remove litter size records missing (only) vars of interest
missing.clims <- which(is.na(dat$MAT.10yrmean) & is.na(dat$MAP.10yrmean) & is.na(dat$NFFD.10yrmean))
dat <- dat[-missing.clims, ]

## omit MAR variable, which also has many missing values
dat <- dat[, -grep('MAR.10yrmean',colnames(dat))]
clim.fields <- grep('10yrmean', colnames(dat))

## calculate species mean headbodylength directly from individual-level data
mean.hb <- aggregate(headbodylength ~ binomial, dat = dat, mean)
headbodylength.sp <- rep(NA,nrow(dat))
for(i in mean.hb$binomial) {
	headbodylength.sp[which(dat$binomial == i)] <- mean.hb$headbodylength[which(mean.hb$binomial == i)]
	}
dat <- data.frame(dat, headbodylength.sp)
dat$binomial <- as.factor(dat$binomial)

## write the final dataset to file
write.csv(dat, "_all-species-v6-reducedForGLMMs_.csv", row.names = F)