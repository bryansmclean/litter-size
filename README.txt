## README

# This repository contains data and code from a study of litter size variation in North American small mammals.

# REFERENCES
Weller AK, Chapman OS, Gora SL, Guralnick RP, McLean BS. 2023. New insight into drivers of mammalian litter size from individual-level traits. Ecography in press.

# DATA SETS
_all-species-v6-reducedForGLMMs_.csv - curated litter size and body size data for individual small mammal records, plus institutional codes and numbers, geocoordinates, and raw + processed ClimateNA data. ClimateNA data are provided for the year of litter size observation plus the previous 9 years (labeled .00, .01, etc) as well as the decadal averages. input for mixed models
_all-species-v6_.csv - curated litter size and body size data for individual small mammal records, plus institutional codes and numbers, geocoordinates, and raw ClimateNA data. ClimateNA data are provided for the year of litter size observation plus the previous 9 years (labeled .00, .01, etc) as well as the decadal averages. this is the raw data prior to several steps for modeling.
species-littersize-comparisons.csv - average reported litter sizes for all species in our dataset from 3 different sources (plus averages for the present study), and comparisons of the former to the latter

# R CODE
R code used for phylogenetic and non-phylogenetic generalized linear mixed models (PGLMMs and GLMMs), model comparisons, and various plotting functions.
