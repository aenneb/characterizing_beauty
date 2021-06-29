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
dat = read_xlsx('rating_data_merged_all_psych_beauty_experiments.xlsx', na = 'NA')
dat = dat[dat$excluded==0,]
#distinguish the subject IDs for the 2 different experiments for lm analyses
dat$subjID[dat$experiment=="imagesAlone"] = dat$subjID[dat$experiment=="imagesAlone"] + 101
dat$subjID[dat$experiment=="memory"] = dat$subjID[dat$experiment=="memory"] + 201

# exclude one-off ratings in memory experiment
dat = dat[dat$subjID<200,]

#modify gender variable
dat['gender_binary'] = as.factor(dat$gender)
dat$gender_binary[dat$gender_binary=="3"] = NA
dat$gender_binary[dat$gender_binary=="4"] = NA
dat$gender_binary = droplevels(dat$gender_binary)

# for each participant, we need to get the correlation between beauty and each other rating
df = data.frame()
ii = 1
for (subj in unique(dat$subjID)) {
  
  this_dat = dat[dat$subjID==subj,]
  
  df[ii,1] = cor(this_dat$beauty, this_dat$pleasure, use='pairwise.complete.obs')
  df[ii,2] = cor(this_dat$beauty, this_dat$complex, use='pairwise.complete.obs')
  df[ii,3] = cor(this_dat$beauty, this_dat$exceeded_expected, use='pairwise.complete.obs')
  df[ii,4] = cor(this_dat$beauty, this_dat$exciting, use='pairwise.complete.obs')
  df[ii,5] = cor(this_dat$beauty, this_dat$harmony_variety, use='pairwise.complete.obs')
  df[ii,6] = cor(this_dat$beauty, this_dat$interesting, use='pairwise.complete.obs')
  df[ii,7] = cor(this_dat$beauty, this_dat$learn, use='pairwise.complete.obs')
  df[ii,8] = cor(this_dat$beauty, this_dat$meaningful, use='pairwise.complete.obs')
  df[ii,9] = cor(this_dat$beauty, this_dat$moved, use='pairwise.complete.obs')
  df[ii,10] = cor(this_dat$beauty, this_dat$understandable, use='pairwise.complete.obs')
  df[ii,11] = subj
  
  ii = ii+1
}

complete_df = na.omit(df)
# perform cluster analysis
nb <- NbClust(complete_df[,1:10], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
km.res <- kmeans(complete_df[,1:10], 2)
complete_df$cluster <- km.res$cluster
r_means_cluster1 <- apply(complete_df[complete_df$cluster==1,1:10],2,mean)  
r_means_cluster2 <- apply(complete_df[complete_df$cluster==2,1:10],2,mean)  

dat['cluster'] <- NA
ii = 1
for (subj in unique(complete_df[,11])) {
  
  dat$cluster[dat$subjID==subj] <- km.res$cluster[ii]
  ii = ii+1
  
}

# look at possible differences in demographics; answers to general questions
single_obs = dat[dat$stimulus=='Lake9',]
chisq.test(table(single_obs$cluster, single_obs$gender_binary))
ks.test(single_obs$age[single_obs$cluster==1], single_obs$age[single_obs$cluster==2])
aggregate(age~cluster, data=single_obs, median)
ks.test(single_obs$rel_beaut_pleas[single_obs$cluster==1], single_obs$rel_beaut_pleas[single_obs$cluster==2])
ks.test(single_obs$art_or_nature[single_obs$cluster==1], single_obs$art_or_nature[single_obs$cluster==2])
ks.test(single_obs$image_or_Story[single_obs$cluster==1], single_obs$image_or_Story[single_obs$cluster==2])
aggregate(image_or_Story~cluster, data=single_obs, median)
ks.test(single_obs$communication[single_obs$cluster==1], single_obs$communication[single_obs$cluster==2])
ks.test(single_obs$mood[single_obs$cluster==1], single_obs$mood[single_obs$cluster==2])
chisq.test(table(single_obs$all_beaut_same, single_obs$cluster))
table(single_obs$all_beaut_same, single_obs$cluster)
