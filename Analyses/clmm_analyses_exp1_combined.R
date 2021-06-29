rm(list = ls(all = TRUE)) #clear workspace
## set working directory
# setwd("/Users/aennebrielmann/Desktop/characterizing_beauty-master/Data files for analyses/")
setwd("D:/Nextcloud/Submitted/beauty_the_feeling/analyses/data for analyses/")

## load necessary packages
require("readxl")
require('ordinal')
require("MuMIn")
require("performance")
require("ggeffects")

# read the complete data set
dat = read_xlsx('rating_data_merged_all_psych_beauty_experiments.xlsx', na = 'NA')
dat = dat[dat$excluded==0,]
#distinguish the subject IDs for the 2 different experiments for lm analyses
dat$subjID[dat$experiment=="imagesAlone"] = dat$subjID[dat$experiment=="imagesAlone"] + 101
dat$subjID[dat$experiment=="memory"] = dat$subjID[dat$experiment=="memory"] + 201
dat$subjID = droplevels.factor(dat$subjID)

# we also exclude cases of incomplete ratings
full_n = nrow(dat)
dat = dat[complete.cases(dat[,16:26]),]
included_n = nrow(dat)
full_n - included_n

# make sure that all variables are in the correct format
dat$subjID = as.factor(dat$subjID)
dat$stim_cat = as.factor(dat$stim_cat)
dat$exist_universal = as.factor(dat$exist_universal)

dat$gender_binary = as.factor(dat$gender)
dat$gender_binary[dat$gender_binary=="4"] = NA
dat$gender_binary[dat$gender_binary=="3"] = NA
dat$gender_binary = droplevels(dat$gender_binary)

# and set all Likert ratings to ordinal
dat$beauty = factor(dat$beauty, ordered=TRUE)
dat$pleasure = factor(dat$pleasure, ordered=TRUE)
dat$complex = factor(dat$complex, ordered=TRUE)
dat$exciting = factor(dat$exciting, ordered=TRUE)
dat$learn = factor(dat$learn, ordered=TRUE)
dat$understandable = factor(dat$understandable, ordered=TRUE)
dat$harmony_variety = factor(dat$harmony_variety, ordered=TRUE)
dat$meaningful = factor(dat$meaningful, ordered=TRUE)
dat$exceeded_expected = factor(dat$exceeded_expected, ordered=TRUE)
dat$interesting = factor(dat$interesting, ordered=TRUE)
dat$moved = factor(dat$moved, ordered=TRUE)

# RE-ANALYSIS of the data using ordinal models: 
model.random = clmm(data=dat, beauty ~ 1 + (1|subjID) + (1|stimulus))
summary(model.random)

model.fixed = clmm(data=dat, beauty ~ 1 + (1|subjID) + (1|stimulus) + pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)
summary(model.fixed)

model.stimType = clmm(data=dat, beauty ~ 1 + (1|subjID) + (1|stimulus) + stim_cat * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))
summary(model.stimType)

model.gender = clmm(data=dat, beauty ~ 1 + (1|subjID) + (1|stimulus) + gender_binary * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))
summary(model.gender)

model.age = clmm(data=dat, beauty ~ 1 + (1|subjID) + (1|stimulus) + age * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))
summary(model.age)

BIC(model.random)
BIC(model.fixed)
BIC(model.stimType)
BIC(model.gender)
BIC(model.age)