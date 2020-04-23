rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
#setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/analyses/")

## load necessary packages
require("readxl")
require("effsize")

# read in the data from both image and image-music study
dat = read_xlsx('imagesAlone_imagesMusic_merged_data.xlsx', na = 'NA')
excluded_subjs = c(12,15,28,83,85,88) # based on the question about the stimulus type for image and music data
#distinguish the subject IDs for the 2 different experiments for lmer analyses
dat$subjID[dat$experiment=="images_only"] = dat$subjID[dat$experiment=="images_only"] + 100
dat = dat[!(is.element(dat$subjID,excluded_subjs) & dat$experiment=="images_and_music"),]
dat$subjID = droplevels.factor(dat$subjID)
dat_stimuli = dat[dat$beauty==7,]

# read in memory data from both US studies
dat_memory = read_xlsx('beauty_memory_data_merged_with_coding.xlsx', na = 'NA')
dat_memory = dat_memory[dat_memory$excluded==0 & dat_memory$beauty==7 & dat_memory$experiment!="uk" & dat_memory$experiment != 'in',]

# The question is not so much whether the means are the same but whether the ratings stem from one distribution, therefore use Smirnov test
# let us first look at those variables that are associated with high beauty ratings
ks.test(dat_stimuli$pleasure, dat_memory$pleasure)
ks.test(dat_stimuli$want_to_look, dat_memory$`want longer`)
ks.test(dat_stimuli$desire_free, dat_memory$`desire free`)
ks.test(dat_stimuli$connected, dat_memory$connections)
ks.test(dat_stimuli$universal_beauty, dat_memory$`beautiful to everyone`)
# check effect size for those ratings with differences
cohen.d(dat_stimuli$desire_free, dat_memory$`desire free`, na.rm=T)
cohen.d(dat_stimuli$connected, dat_memory$connections, na.rm=T)
cohen.d(dat_stimuli$universal_beauty, dat_memory$`beautiful to everyone`, na.rm=T)
#show at least that these ratings are high
mean(dat_memory$pleasure)
mean(dat_memory$`want longer`)
mean(dat_memory$`desire free`)
mean(dat_memory$connections)
mean(dat_memory$`beautiful to everyone`)


# then look at the partial associated ones
t.test(dat_stimuli$alive, dat_memory$alive)
t.test(dat_stimuli$longing, dat_memory$longing)

# and then, last, we also look at the non-beauty-associated variables
t.test(dat_stimuli$surprise, dat_memory$surprise)
t.test(dat_stimuli$understand_more, dat_memory$`understand more`)
t.test(dat_stimuli$mind_wander, dat_memory$`mind wander`)
t.test(dat_stimuli$story, dat_memory$`tells story`)
