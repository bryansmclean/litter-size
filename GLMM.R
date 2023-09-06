#######################################################################
# LINEAR MIXED MODELS (LMMs) 
# used for testing drivers of inter- and intraspecific
# litter size variation in North American small mammals
# Bryan S. McLean
# 27 April 2021, last updated 6 September 2023
#######################################################################

library(data.table)
library(lme4)
library(lmerTest)
library(standardize)

###########################################
### 	prepare data for GLMMs			###
###########################################

############
## DATA   ##
############

dat <- read.csv("_all-species-v6-reducedForGLMMs_.csv", row.names = F)

############
## MODELS ##
############

# construct a model based on raw, minimally correlated (r<=0.70) long-term ClimateNA variables
# including inter- and intraspecific body size

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

# run the model

m0.mod <- lmer(
  formula = m0$formula,
  data = na.omit(m0$data),
  control = lmerControl(optimizer = "bobyqa")
  )

# backwards model selection

m0.step <- step(m0.mod)
m0.mod.step <- get_model(m0.step)

############
## OUTPUT ##
############

saveRDS(m0.mod, "m0.mod.fullv2.rds")
capture.output(summary(as_lmerModLmerTest(m0.mod)), file = "m0.mod.fullv2.summary.txt")
saveRDS(m0.mod.step, "m0.mod.stepv2.rds")
capture.output(summary(as_lmerModLmerTest(m0.mod.step)), file = "m0.mod.stepv2.summary.txt")

# write model selection summary to file too
capture.output(m0.step, file = "m0.mod.stepwisev2.summary.txt")

