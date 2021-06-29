rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
#setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/")

## load necessary packages
require("readxl")
require('lme4')
require("lmTest")
require("MuMIn")
require("performance")
require("sjPlot")
require("cvms")
library(groupdata2) # fold()
library(dplyr) # %>% arrange()

# read the complete data set
dat = read_xlsx('rating_data_merged_all_psych_beauty_experiments.xlsx', na = 'NA')
dat = dat[dat$excluded==0,]
#distinguish the subject IDs for the 2 different experiments for lm analyses
dat$subjID[dat$experiment=="imagesAlone"] = dat$subjID[dat$experiment=="imagesAlone"] + 101
dat$subjID[dat$experiment=="memory"] = dat$subjID[dat$experiment=="memory"] + 201
dat$subjID = droplevels.factor(dat$subjID)

# make sure that all variables are in the correct format
dat$subjID = as.factor(dat$subjID)
dat$stim_cat = as.factor(dat$stim_cat)
dat$exist_universal = as.factor(dat$exist_universal)

dat$gender_binary = as.factor(dat$gender)
dat$gender_binary[dat$gender_binary=="4"] = NA
dat$gender_binary[dat$gender_binary=="3"] = NA
dat$gender_binary = drop.levels(dat$gender_binary)

# add quadratic complexity term
dat$complexSq = (dat$complex-4)^2

# we also exclude cases of incomplete ratings
full_n = nrow(dat)
dat = dat[complete.cases(dat[,16:26]),]
included_n = nrow(dat)
full_n - included_n

# MAIN ANALYSIS
# in this case, due to 1-off measures in memory, we cannot include subjID or stimulus as random intercepts

model.fixed = lm(data=dat, beauty ~ pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)

model.stimType_broad = lm(data=dat, beauty ~ stim_cat * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))

model.stimType_narrow = lm(data=dat, beauty ~ stim_type * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))

model.gender = lm(data=dat, beauty ~ gender_binary * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))

model.age = lm(data=dat, beauty ~ age * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))

model.complexSq = lm(data=dat, beauty ~ pleasure + complexSq + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)

model.complexSq_stimType_broad = lm(data=dat, beauty ~ stim_cat * (pleasure + complexSq + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))

model.complexSq_stimType_narrow = lm(data=dat, beauty ~ stim_type * (pleasure + complexSq + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved))

compare_performance(model.fixed, model.stimType_broad, model.stimType_narrow, model.gender, model.age, model.complexSq, model.complexSq_stimType_broad, model.complexSq_stimType_narrow, rank=TRUE)

# crossvalidation
# Set seed for reproducibility
set.seed(7)

# Fold data
dat <- fold(
  dat,
  k = 10,
  id_col = "subjID",
  handle_existing_fold_cols = 'remove'
) %>%
  arrange(.folds)

models <- c("beauty ~ pleasure + complexSq + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved", 
            "beauty ~ pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved", 
            "beauty ~ stim_cat * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)",
            "beauty ~ stim_type * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)",
            "beauty ~ stim_cat * (pleasure + complexSq + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)",
            "beauty ~ stim_type * (pleasure + complexSq + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)",
            "beauty ~ gender_binary * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)",
            "beauty ~ age * (pleasure + complex + exciting + learn + understandable + harmony_variety + meaningful + exceeded_expected + interesting + moved)")

CV_result <- cross_validate(
  dat,
  formulas = models,
  family = "gaussian"
)

# taking everythign together, the model with stimulus type (broad) and a squared complexity term seems to have just about an edge over all other models
summary(model.complexSq_stimType_broad)

