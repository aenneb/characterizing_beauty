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
dat = read_xlsx('imagesAlone_imagesMusic_merged_data.xlsx', na = 'NA')
excluded_subjs = c(12,15,28,83,85,88) # based on the question about the stimulus type for image and music data
#distinguish the subject IDs for the 2 different experiments for lmer analyses
dat$subjID[dat$experiment=="images_only"] = dat$subjID[dat$experiment=="images_only"] + 100
dat = dat[!(is.element(dat$subjID,excluded_subjs) & dat$experiment=="images_and_music"),]
dat$subjID = droplevels.factor(dat$subjID)
dat_singleQuestions = dat[dat$stimulus=="Lake14",]

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

ratings = cbind(dat$beauty, dat$pleasure, dat$want_to_look, dat$alive, dat$universal_beauty, dat$connected, dat$longing, dat$desire_free, dat$mind_wander, dat$surprise, dat$understand_more, dat$story)

means_art = colMeans(ratings[dat$stimulus_category=="art_beaut" & dat$beauty==7,], na.rm=T)
means_OASISbeaut = colMeans(ratings[dat$stimulus_category=="OASIS_beaut" & dat$beauty==7,], na.rm=T)
means_OASISneut = colMeans(ratings[dat$stimulus_category=="OASIS_neut" & dat$beauty==7,], na.rm=T)
means_music = colMeans(ratings[dat$stimulus_category=="song" & dat$beauty==7,], na.rm=T)

se_art = apply(ratings[dat$stimulus_category=="art_beaut" & dat$beauty==7,], 2, std.error, na.rm=T)
se_OASISbeaut = apply(ratings[dat$stimulus_category=="OASIS_beaut" & dat$beauty==7,], 2, std.error, na.rm=T)
se_OASISneut = apply(ratings[dat$stimulus_category=="OASIS_neut" & dat$beauty==7,], 2, std.error, na.rm=T)
se_music = apply(ratings[dat$stimulus_category=="song" & dat$beauty==7,], 2, std.error, na.rm=T)

plot_dat = as.data.frame(rbind(means_art, means_OASISbeaut, means_OASISneut, means_music))
names(plot_dat) = varNames
plot_dat = melt(plot_dat)
plot_dat$stim = rep(c('art','stock_beaut','stock_neut','music'),12)

ses = as.data.frame(rbind(se_art, se_OASISbeaut, se_OASISneut, se_music))
ses = melt(ses)

plot_dat$upper = plot_dat$value + ses$value
plot_dat$lower = plot_dat$value - ses$value

# Define a new coordinate system 
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(data= plot_dat, aes(x = variable, y = value, group = stim, ymin=3, ymax=7)) +
  facet_grid(. ~ stim) +
  geom_polygon(aes(y = upper), fill = "grey50", alpha = 0.5) +
  geom_polygon(aes(y = lower), fill = "grey99", alpha = 0.7) +
  geom_polygon(fill = NA, colour='black') +
  theme_light() +
  theme(panel.grid.minor = element_blank()) + 
  coord_radar() +
  labs(x = "", y = "")

# Get Ns
nrow(ratings[dat$stimulus_category=="art_beaut" & dat$beauty==7,])
nrow(ratings[dat$stimulus_category=="OASIS_beaut" & dat$beauty==7,])
nrow(ratings[dat$stimulus_category=="OASIS_neut" & dat$beauty==7,])
nrow(ratings[dat$stimulus_category=="song" & dat$beauty==7,])
