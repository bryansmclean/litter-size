#######################################################################
# PHYLOGENETIC LINEAR MIXED MODELS (PLMMs) 
# used for testing drivers of inter- and intraspecific
# litter size variation in North American small mammals
# Bryan S. McLean
# 22 February 2023, last updated 6 September 2023
#######################################################################

library(data.table)
library(lme4)
library(lmerTest)
library(standardize)
library(phyr)
library(ape)

###########################################
### 	prepare data for GLMMs			###
###########################################

############
## DATA   ##
############

dat <- read.csv("_all-species-v6-reducedForGLMMs_.csv", row.names = F)
tre <- read.tree("littertree_newtips.tre")
tre$tip.label <- gsub("_", " ", tre$tip.label)
unique(dat$binomial) %in% tre$tip.label # check that tree contains required tips

############
## MODELS ##
############

# construct a model based on raw, minimally correlated (r<=0.70) long-term ClimateNA variables
# including inter- and intraspecific body size and accounting for phylogenetic structure

# standardize raw data

m0 <- standardize::standardize(

	# response variable
	log10(embryocount) ~

	# fixed effects - climate
	log10(MAP.10yrmean) + 
	RH.10yrmean +
	TD.10yrmean +
	sqrt(Eref.10yrmean) + 

	# fixed effects - climate interactions
	log10(MAP.10yrmean):RH.10yrmean +
	log10(MAP.10yrmean):TD.10yrmean +
	log10(MAP.10yrmean):sqrt(Eref.10yrmean) +
	RH.10yrmean:TD.10yrmean +
	RH.10yrmean:sqrt(Eref.10yrmean) +
	TD.10yrmean:sqrt(Eref.10yrmean) +

	# fixed effects - species average body size
	log10(headbodylength.sp) +

    # random effects - species identity, climate and body size
	(1 | binomial) +
	(0 + log10(MAP.10yrmean) | binomial) +
	(0 + RH.10yrmean | binomial) + 
	(0 + TD.10yrmean | binomial) +
	(0 + sqrt(Eref.10yrmean) | binomial) +     
	(0 + log10(headbodylength) | binomial),

	data = dat,
	family = gaussian()
)

# run the model (specified in full, as it was run remotely on longleaf supercomputer)

pg.m0.mod <- pglmm(
	
	# response variable
	log10_embryocount ~

	# fixed effects - climate
	log10_MAP.10yrmean + 
	RH.10yrmean + 
	TD.10yrmean + 
	sqrt_Eref.10yrmean + 

	# fixed effects - climate interactions
	log10_MAP.10yrmean:RH.10yrmean +
	log10_MAP.10yrmean:TD.10yrmean +  
	log10_MAP.10yrmean:sqrt_Eref.10yrmean + 
	RH.10yrmean:TD.10yrmean +
	RH.10yrmean:sqrt_Eref.10yrmean +
	RH.10yrmean:sqrt_Eref.10yrmean +
	TD.10yrmean:sqrt_Eref.10yrmean +

	# fixed effects - species average body size
	log10_headbodylength.sp +

    # random effects - species identity, climate and body size
    # modeling both the phylogenetic and non-phylogenetic components
	(1 | binomial__) +
	(0 + RH.10yrmean | binomial__) + 
	(0 + log10_MAP.10yrmean | binomial__) +
	(0 + sqrt_Eref.10yrmean | binomial__) +
	(0 + TD.10yrmean | binomial__) +
	(0 + log10_headbodylength | binomial),

	data = na.omit(m0$data),
	family = "gaussian",
	cov_ranef = list(binomial = tre)
	
	)

############
## OUTPUT ##
############

saveRDS(pg.m0.mod, "pg.m0.mod.rds")
