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
dat = read_xlsx('rating_data_merged_all_psych_beauty_experiments.xlsx', na = 'NA')
dat = dat[dat$excluded==0 & dat$beauty==7,]

# taking care to have same order here as in Table
varNames = c('beauty', 'pleasure', 'moved', 'exceeded_expected', 'harmony_variety', 'meaningful', 'complex', 'interesting', 'exciting', 'learn', 'understandable')

ratings = cbind(dat$beauty, dat$pleasure, dat$moved, dat$exceeded_expected, dat$harmony_variety, dat$meaningful, dat$complex, dat$interesting, dat$exciting, dat$learn, dat$understandable)

means_art = colMeans(ratings[dat$stim_type=="art_beaut",], na.rm=T)
means_OASISbeaut = colMeans(ratings[dat$stim_type=="OASIS_beaut",], na.rm=T)
means_OASISneut = colMeans(ratings[dat$stim_type=="OASIS_neut",], na.rm=T)
means_music = colMeans(ratings[dat$stim_type=="song",], na.rm=T)
means_memory = colMeans(ratings[dat$stim_type=="memory",], na.rm=T)

se_art = apply(ratings[dat$stim_type=="art_beaut",], 2, std.error, na.rm=T)
se_OASISbeaut = apply(ratings[dat$stim_type=="OASIS_beaut",], 2, std.error, na.rm=T)
se_OASISneut = apply(ratings[dat$stim_type=="OASIS_neut",], 2, std.error, na.rm=T)
se_music = apply(ratings[dat$stim_type=="song",], 2, std.error, na.rm=T)
se_memory = apply(ratings[dat$stim_type=="memory",], 2, std.error, na.rm=T)

plot_dat = as.data.frame(rbind(means_art, means_OASISbeaut, means_OASISneut, means_music, means_memory))
names(plot_dat) = varNames
plot_dat = melt(plot_dat)
plot_dat$stim = rep(c('art','stock_beaut','stock_neut','music', 'memory'),11)

ses = as.data.frame(rbind(se_art, se_OASISbeaut, se_OASISneut, se_music, se_memory))
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
table(dat$stim_type, dat$beauty)
