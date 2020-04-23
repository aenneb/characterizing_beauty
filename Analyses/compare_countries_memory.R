rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
# setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/analyses")

## load necessary packages
require("readxl")
require('ggplot2')
require('effsize')

# we load the merged data file and take it as data for analyses
dat = read_xlsx('beauty_memory_data_merged_with_coding.xlsx', na = 'NA')
dat = dat[dat$excluded==0,]
# because we do get overall differences in beauty ratings, let us look at max beauty only, again
dat = dat[dat$beauty==7,]

# get Ns
table(dat$experiment)

# re-code and look at differences between countries, not samples
dat$country = 'us'
dat$country[dat$experiment=='in'] = 'in'
dat$country[dat$experiment=='uk'] = 'uk'

## manova for memory ratings
memory_ratings = cbind(dat$beauty, dat$pleasure, dat$surprise, dat$`want longer`, dat$`desire free`, dat$alive, dat$`understand more`, dat$`mind wander`, dat$connections, dat$`tells story`, dat$`beautiful to everyone`, dat$longing, dat$peaceful, dat$perfect)
aov = manova(memory_ratings ~ dat$country + dat$gender + dat$age)
summary(aov, test="Pillai")

# follow up with univariate ANOVAs
aov.beauty = aov(data=dat, beauty ~ country)
summary(aov.beauty)

# variables associated with high beauty ratings based on stimulus studies
aov.pleasure = aov(data=dat, pleasure ~ country)
summary(aov.pleasure)

aov.longer = aov(data=dat, `want longer` ~ country)
summary(aov.longer)

aov.desire = aov(data=dat, `desire free` ~ country)
summary(aov.desire)

aov.connection = aov(data=dat, connections ~ country)
summary(aov.connection)

aov.everyone = aov(data=dat, `beautiful to everyone` ~ country)
summary(aov.everyone)
TukeyHSD(aov.everyone)
aggregate(data=dat, `beautiful to everyone` ~ country, mean, na.rm=T)
cohen.d(dat$`beautiful to everyone`[dat$country=='in'], dat$`beautiful to everyone`[dat$country=='us'], na.rm=T)

# not associated with high beauty
aov.surprise = aov(data=dat, surprise ~ country)
summary(aov.surprise)
TukeyHSD(aov.surprise)

aov.alive = aov(data=dat, alive ~ country)
summary(aov.alive)
TukeyHSD(aov.alive)

aov.understand = aov(data=dat, `understand more` ~ country)
summary(aov.understand)
TukeyHSD(aov.understand)

aov.mind = aov(data=dat, `mind wander` ~ country)
summary(aov.mind)
TukeyHSD(aov.mind)

aov.story = aov(data=dat, `tells story` ~ country)
summary(aov.story)
TukeyHSD(aov.story)
aggregate(data=dat, `tells story` ~ country, mean, na.rm=T)

aov.longing = aov(data=dat, longing ~ country)
summary(aov.longing)
TukeyHSD(aov.longing)
aggregate(data=dat, longing ~ country, mean, na.rm=T)

aov.peaceful = aov(data=dat, peaceful ~ country)
summary(aov.peaceful)
TukeyHSD(aov.peaceful)

aov.perfect = aov(data=dat, perfect ~ country)
summary(aov.perfect)
TukeyHSD(aov.perfect)

## repeat procedure with the end of survey questions
## manova for memory ratings
general_qs = cbind(dat$rel_beaut_pleas, dat$art_vs_nature, dat$image_vs_story, dat$communication, dat$mood_effect)
aov = manova(general_qs ~ dat$experiment + dat$gender + dat$age)
summary(aov, test="Pillai")

aov.rel = aov(data=dat, rel_beaut_pleas ~ experiment)
summary(aov.rel)
TukeyHSD(aov.rel)

aov.artNature = aov(data=dat, art_vs_nature ~ experiment)
summary(aov.artNature)
TukeyHSD(aov.artNature)

aov.imageStory = aov(data=dat, image_vs_story ~ experiment)
summary(aov.imageStory)
TukeyHSD(aov.imageStory)

aov.comm = aov(data=dat, communication ~ experiment)
summary(aov.comm)
TukeyHSD(aov.comm)

aov.mood = aov(data=dat, mood_effect ~ experiment)
summary(aov.mood)

