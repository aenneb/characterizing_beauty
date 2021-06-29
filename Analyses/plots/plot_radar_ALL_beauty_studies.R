rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
#setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/")

## load necessary packages
require("readxl")
require('ggplot2')
require('reshape2')
require('plotrix')

# read the complete data set
dat = read_xlsx('rating_data_merged_all_beauty_experiments.xlsx', na = 'NA')
 #subject exclusion has already taken place
# only make sure that subjIDs are unique
dat$subjID[dat$experiment=="images_only"] = dat$subjID[dat$experiment=="images_only"] + 400
dat$subjID[dat$experiment=="images_and_music"] = dat$subjID[dat$experiment=="images_and_music"] + 500
dat$subjID = droplevels.factor(dat$subjID)

# do not iclude peaceful and perfect here, since we do not have data from all studies on these
# IMPORTANT: Order the ratings in the same way as they should appear on the radar plot
# 1. Pleasure
# 2. Wish to continue
# 3. Feeling alive
# 4. Universality
# 5. Number of felt connections
# 6. Longing
# 7. Feeling free of desire
# 8. Mind-wandering
# 9. Surprise
# 10. Wanting to understand more
# 11. Telling a story
# this needs to be reflected in the naming of the platting matrix
varNames = c('beauty', 'pleasure', 'wish continue', 'alive', 'universality', 'number connections', 'longing', 'desire free', 'mind-wandering', 'surprise', 'want understand', 'tells story')

ratings = cbind(dat$beauty, dat$pleasure, dat$`want longer`, dat$alive, dat$`beautiful to everyone`, dat$connections, dat$longing, dat$`desire free`, dat$`mind wander`, dat$surprise, dat$`understand more`, dat$`tells story`)

means_highBeauty = colMeans(ratings[dat$beauty==7,], na.rm=T)
means_lowBeauty = colMeans(ratings[dat$beauty<7,], na.rm=T)

se_highBeauty = apply(ratings[dat$beauty==7,], 2, std.error, na.rm=T)
se_lowBeauty = apply(ratings[dat$beauty<7,], 2, std.error, na.rm=T)

plot_dat = as.data.frame(rbind(means_highBeauty, means_lowBeauty))
names(plot_dat) = varNames
plot_dat = melt(plot_dat)
plot_dat$cat = rep(c('high_beauty','low_beauty'),12)

ses = as.data.frame(rbind(se_highBeauty, se_lowBeauty))
ses = melt(ses)

plot_dat$upper = plot_dat$value + ses$value
plot_dat$lower = plot_dat$value - ses$value

ggplot(data= plot_dat, aes(x = variable, y = value, group = cat)) +
  geom_polygon(aes(y = upper), fill = "grey50", alpha = 0.5) +
  geom_polygon(aes(y = lower), fill = "grey99", alpha = 0.7) +
  geom_polygon(aes(colour=cat), fill = NA) +
  theme_light() +
  theme(panel.grid.minor = element_blank()) + 
  coord_polar() +
  labs(x = "", y = "")

# Get the Ns
nrow(ratings[dat$beauty==7,])
nrow(ratings[dat$beauty<7,])
