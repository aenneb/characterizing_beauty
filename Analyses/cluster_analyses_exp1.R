rm(list = ls(all = TRUE)) #clear workspace
## set working directory
# setwd("/Users/aennebrielmann/Desktop/characterizing_beauty-master/Data files for analyses/")
setwd("D:/Nextcloud/Submitted/beauty_the_feeling/analyses/data for analyses/")

## load necessary packages
require("readxl")
require('stats')
require("NbClust")
require('factoextra')

# read the complete data set
dat = read_xlsx('imagesAlone_imagesMusic_merged_data.xlsx', na = 'NA')
excluded_subjs = c(12,15,28,83,85,88) # based on the question about the stimulus type for image and music data
#distinguish the subject IDs for the 2 different experiments for lmer analyses
dat$subjID[dat$experiment=="images_only"] = dat$subjID[dat$experiment=="images_only"] + 100

# make sure that all variables are in the correct format
dat$exist_universal_beauty = as.factor(dat$exist_universal_beauty)
dat['gender_binary'] = as.factor(dat$gender)
dat$gender_binary[dat$gender_binary=="4"] = NA
dat$gender_binary = droplevels(dat$gender_binary)

# for each participant, we need to get the correlation between beauty and each other rating
df = data.frame()
ii = 1
for (subj in unique(dat$subjID)) {
  
  this_dat = dat[dat$subjID==subj,]
  
  df[ii,1] = cor(this_dat$beauty, this_dat$pleasure, use='pairwise.complete.obs')
  df[ii,2] = cor(this_dat$beauty, this_dat$surprise, use='pairwise.complete.obs')
  df[ii,3] = cor(this_dat$beauty, this_dat$want_to_look, use='pairwise.complete.obs')
  df[ii,4] = cor(this_dat$beauty, this_dat$desire_free, use='pairwise.complete.obs')
  df[ii,5] = cor(this_dat$beauty, this_dat$alive, use='pairwise.complete.obs')
  df[ii,6] = cor(this_dat$beauty, this_dat$understand_more, use='pairwise.complete.obs')
  df[ii,7] = cor(this_dat$beauty, this_dat$mind_wander, use='pairwise.complete.obs')
  df[ii,8] = cor(this_dat$beauty, this_dat$connected, use='pairwise.complete.obs')
  df[ii,9] = cor(this_dat$beauty, this_dat$story, use='pairwise.complete.obs')
  df[ii,10] = cor(this_dat$beauty, this_dat$universal_beauty, use='pairwise.complete.obs')
  df[ii,11] = cor(this_dat$beauty, this_dat$longing, use='pairwise.complete.obs')
  df[ii,12] = subj
  
  ii = ii+1
}

complete_df = na.omit(df)
# perform cluster analysis
nb <- NbClust(complete_df[,1:11], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
km.res <- kmeans(complete_df[,1:11], 2)
complete_df$cluster <- km.res$cluster
r_means_cluster1 <- apply(complete_df[complete_df$cluster==1,1:11],2,mean)  
r_means_cluster2 <- apply(complete_df[complete_df$cluster==2,1:11],2,mean)  

dat['cluster'] <- NA
ii = 1
for (subj in unique(complete_df[,12])) {

  dat$cluster[dat$subjID==subj] <- km.res$cluster[ii]
  ii = ii+1

}

# look at possible differences in demographics; answers to general questions
single_obs = dat[dat$stimulus=='Lake9',]
chisq.test( table(single_obs$cluster, single_obs$gender_binary))
ks.test(single_obs$age[single_obs$cluster==1], single_obs$age[single_obs$cluster==2])
ks.test(single_obs$rel_beaut_pleas[single_obs$cluster==1], single_obs$rel_beaut_pleas[single_obs$cluster==2])
ks.test(single_obs$art_or_nature[single_obs$cluster==1], single_obs$art_or_nature[single_obs$cluster==2])
ks.test(single_obs$surface_or_story[single_obs$cluster==1], single_obs$surface_or_story[single_obs$cluster==2])
ks.test(single_obs$communication[single_obs$cluster==1], single_obs$communication[single_obs$cluster==2])
ks.test(single_obs$mood_effect[single_obs$cluster==1], single_obs$mood_effect[single_obs$cluster==2])
chisq.test(table(single_obs$exist_universal_beauty, single_obs$cluster))

