rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
#setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/")

## load necessary packages
require("readxl")
require('lme4')
require("lmerTest")
require("MuMIn")
require("performance")

# read the complete data set
dat = read_xlsx('imagesAlone_imagesMusic_merged_data.xlsx', na = 'NA')
excluded_subjs = c(12,15,28,83,85,88) # based on the question about the stimulus type for image and music data
#distinguish the subject IDs for the 2 different experiments for lmer analyses
dat$subjID[dat$experiment=="images_only"] = dat$subjID[dat$experiment=="images_only"] + 100
dat = dat[!(is.element(dat$subjID,excluded_subjs) & dat$experiment=="images_and_music"),]
dat$subjID = droplevels.factor(dat$subjID)
dat_singleQuestions = dat[dat$stimulus=="Lake14",]

# make sure that all variables are in the correct format
dat$subjID = as.factor(dat$subjID)
dat$stimulus_category = as.factor(dat$stimulus_category)
dat$exist_universal_beauty = as.factor(dat$exist_universal_beauty)
dat$gender_binary = as.factor(dat$gender)
dat$gender_binary[dat$gender_binary=="4"] = NA
# we also exclude cases of incomplete ratings
full_n = nrow(dat)
dat = dat[complete.cases(dat[,10:25]),]
included_n = nrow(dat)
full_n - included_n

# MAIN ANALYSIS: 
model.random = lmer(data=dat, beauty ~ (1|subjID) + (1|stimulus))
summary(model.random)

model.fixed = lmer(data=dat, beauty ~ (1|subjID) + (1|stimulus) + pleasure + surprise + want_to_look + desire_free + alive + understand_more + mind_wander + connected + story + universal_beauty + longing)
summary(model.fixed)

model.stimType = lmer(data=dat, beauty ~ (1|subjID) + (1|stimulus) + stimulus_category * (pleasure + surprise + want_to_look + desire_free + alive + understand_more + mind_wander + connected + story + universal_beauty + longing))
summary(model.stimType)

model.gender = lmer(data=dat, beauty ~ (1|subjID) + (1|stimulus) + gender_binary * (pleasure + surprise + want_to_look + desire_free + alive + understand_more + mind_wander + connected + story + universal_beauty + longing))
summary(model.gender)

model.age = lmer(data=dat, beauty ~ (1|subjID) + (1|stimulus) + age * (pleasure + surprise + want_to_look + desire_free + alive + understand_more + mind_wander + connected + story + universal_beauty + longing))
summary(model.age)

compare_performance(model.random, model.fixed, model.stimType, model.gender, model.age)
