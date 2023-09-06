##########################################
## plotting routine for
## average littersize comparisons
## Bryan S. McLean
## 22 February 2023, last mod. 6 Sept 2023
##########################################

library('ggplot2')
library('sjPlot')

############
## DATA   ##
############

# read in raw data and model objects
dat <-  read.csv("_all-species-v6-reducedForGLMMs_.csv")
top <- readRDS("m0.mod.stepv2.rds")

# get standardized model object in the workspace again
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

# create clade-specific color palette and link to taxonomy

cols <- c(
	'#FCBA65', #blbr
	'#40663F', #cala
	'#497381', #caca
	'firebrick4', #digr
	'#497381', #dime
	'#497381', #dior
	'firebrick4', #letr
	'#8C9D57', #leam
	'firebrick4', #microtus
	'firebrick4', #microtus	
	'firebrick4', #microtus
	'firebrick4', #microtus
	'firebrick4', #microtus
	'firebrick4', #microtus
	'firebrick4', #microtus
	'#8D7F99', #mus
	'firebrick4', #myga
	'firebrick4', #myru
	'#163343', #nain
	'#EC8FA3', #nealb
	'#8C9D57', #ochotona	
	'#EC8FA3', #onle
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#EC8FA3', #peros
	'#FCBA65', #sorex
	'#FCBA65', #sorex
	'firebrick4', #sybo
	'firebrick4', #syco
	'#40663F', #tahu
	'#163343', #zapus	
	'#163343' #zapus
	)
spp <- c(
	'Blarina brevicauda', #blbr
	'Callospermophilus lateralis', #cala
	'Castor canadensis', #caca
	'Dicrostonyx groenlandicus', #digr
	'Dipodomys merriami', #dime
	'Dipodomys ordii', #dior
	'Lemmus trimucronatus', #letr
	'Lepus americanus', #leam
	'Microtus californicus', #microtus
	'Microtus longicaudus', #microtus	
	'Microtus miurus', #microtus
	'Microtus montanus', #microtus
	'Microtus ochrogaster', #microtus
	'Microtus oeconomus', #microtus
	'Microtus pennsylvanicus', #microtus
	'Mus musculus', #mus
	'Myodes gapperi', #myga
	'Myodes rutilus', #myru
	'Napaeozapus insignis', #nain
	'Neotoma albigula', #nealb
	'Ochotona princeps', #ochotona	
	'Onychomys leucogaster', #onle
	'Peromyscus boylii', #peros
	'Peromyscus crinitus', #peros
	'Peromyscus eremicus', #peros
	'Peromyscus keeni', #peros
	'Peromyscus leucopus', #peros
	'Peromyscus maniculatus', #peros
	'Peromyscus truei', #peros
	'Reithrodontomys fulvescens', #peros
	'Reithrodontomys megalotis', #peros
	'Sigmodon hispidus', #peros
	'Sorex cinereus', #sorex
	'Sorex monticolus', #sorex
	'Synaptomys borealis', #sybo
	'Synaptomys cooperi', #syco
	'Tamiasciurus hudsonicus', #tahu
	'Zapus hudsonius', #zapus	
	'Zapus princeps' #zapus
	)
cols <- data.frame(binomial = spp, cols = factor(cols))

############
## MODELS ##
############

# function to place model predictions back on scale of original data
## some help here: https://stats.stackexchange.com/questions/209784/rescale-predictions-of-regression-model-fitted-on-scaled-predictors

# m0$pred contains the center/scale parameters, so harvest this manually for vars of interest
standardized.sd.EMB <- 0.177413026423131
standardized.EMB <- 0.663681397466708
standardized.sd.HBL <- 0.121367050777162
standardized.HBL <- 2.01322902544076

# checks to make sure we're still sane
embs.raw <- dat$embryocount
embs.raw.unstandardize <- as.vector(10^(m0$data$log10_embryocount * standardized.sd.EMB + standardized.EMB))
# all.equal(embs.raw, embs.raw.unstandardize)
hbl.raw <- dat$headbodylength
hbl.raw.unstandardize <- as.vector(10^(m0$data$log10_headbodylength * standardized.sd.HBL + standardized.HBL))
# all.equal(hbl.raw, hbl.raw.unstandardize)

############
## PLOTS  ##
############

# embryo count predictions from top-ranked GLMM
top.predict <- predict(top, na.omit(m0$data))
top.predict.unstd <- as.vector(10^(top.predict * standardized.sd.EMB + standardized.EMB))
headbodylength.unstd <- as.vector(10^(na.omit(m0$data)$log10_headbodylength * standardized.sd.HBL + standardized.HBL))
dat.raw.predict <- data.frame(na.omit(m0$data), top.predict, top.predict.unstd, headbodylength.unstd)

# add clade-specific colors for plotting
dat.raw.predict <- dplyr::left_join(dat.raw.predict, cols, by = "binomial")
# also create named vector for scale_color_manual
cols.named <- as.vector(cols[,2]); names(cols.named) <- cols[,1]

# plotting predicted inter- and intra-specific allometries
g <- ggplot(
	data = dat.raw.predict,
	aes(
		x = log10(headbodylength.unstd), 
		y = top.predict.unstd
			)
		) +
	geom_point(
		colour = dat.raw.predict$cols,
		alpha = 0.25,
		size = .5
		) + 
	theme_bw()
	
g + geom_smooth(
		method = "lm",
		se = FALSE,
		aes(
			group = binomial,
			col = binomial
			),
		#alpha = 0.25,
		size = .5
		) + 
	scale_color_manual(values = cols.named, guide = "none")


# plotting random slope estimates for intraspecific litter size allometries
re.all <- as.data.frame(ranef(top, condVar = T))
re.slopeHBL <- re.all[re.all$term == "log10_headbodylength",][,-c(1:2)]
re.intercept <- re.all[re.all$term == "(Intercept)",][,-c(1:2)]
re.HBL <- dplyr::inner_join(re.intercept, re.slopeHBL, by = 'grp', suffix = c('.intercept', '.slopeHBL'))
colnames(re.HBL)[1] <- 'binomial'
re.HBL <- re.HBL[order(re.HBL$condval.slopeHBL, decreasing = T), ]

# add clade-specific colors for plotting
re.HBL <- dplyr::left_join(re.HBL, cols, by = "binomial")
re.HBL$cols <- factor(re.HBL$cols, levels = c("#8D7F99", "#163343", "#497381", "#FCBA65", "#8C9D57", "#40663F", "firebrick4", "#EC8FA3"))
# also create named vector for scale_color_manual
cols.named <- as.vector(cols[,2]); names(cols.named) <- cols[,1]

g. <- ggplot(
	data = re.HBL,
	aes(
		x = condval.slopeHBL,
		y = cols
			)
		) +
	geom_vline(
		xintercept = 0,
		linetype = "dashed"
		) +
	geom_point(
		aes(
			group = binomial,
			col = binomial
			),
		size = 3,
		shape = 16
		) + 
	scale_color_manual(values = cols.named, guide = "none") +
	geom_point(
		aes(
			group = binomial,
			col = 'black'
			),
		size = 3,
		shape = 1
		) +
	scale_y_discrete(
		position = "right"
		) + 
	theme_bw() 

