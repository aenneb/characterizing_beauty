rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
# setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/analyses/")

## load necessary packages
require("readxl")
require("ggplot2")

# read the complete data of all experiments
dat = read_xlsx('merged_general_beauty_questions_ALL.xlsx', na = 'NA')
dat$gender[dat$gender==4] = NA
dat$gender[dat$gender==3] = NA

# get N per experiment
table(dat$experiment)

# get medians where appropriate for reporting
median(dat$rel_beaut_pleas, na.rm=T)
median(dat$art_vs_nature, na.rm=T)
median(dat$image_vs_story, na.rm=T)
median(dat$communication, na.rm=T)
median(dat$mood_effect, na.rm=T)

table(dat$universal_beauty)/nrow(dat)
table(dat$all_beaut_same)

# are those the same people?
table(dat$all_beaut_same, dat$universal_beauty)
prop.table(table(dat$all_beaut_same, dat$universal_beauty),2)

# density plots for each question
# I think the best thing we can do is provide the overall distributions as plot and then add those individually for each sample to the Supplement
ggplot(dat, aes(x=rel_beaut_pleas)) +
   geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=7, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=7, fill=NA, color='black')

ggplot(dat, aes(x=art_vs_nature)) +
  geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=7, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=7, fill=NA, color='black')

ggplot(dat, aes(x=image_vs_story)) +
  geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=7, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=7, fill=NA, color='black')

ggplot(dat, aes(x=communication)) +
  geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=7, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=7, fill=NA, color='black')

ggplot(dat, aes(x=mood_effect)) +
  geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=7, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=7, fill=NA, color='black')

ggplot(dat, aes(x=universal_beauty)) +
  geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=2, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=2, fill=NA, color='black')

ggplot(dat, aes(x=all_beaut_same)) +
  geom_histogram(aes(y=..density.., group=experiment, fill=experiment), bins=2, color=NA, position='dodge') +
  geom_histogram(aes(y=..density.., group=1), bins=2, fill=NA, color='black')
