rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
# setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/analyses/")

## load necessary packages
require("readxl")
require('ggplot2')
require('reshape2')
require('plotrix')

# we load the merged data file and take it as data for analyses
dat = read_xlsx('beauty_memory_data_US_psych.xlsx', na = 'NA')
table(dat$excluded)
dat = dat[dat$excluded==0,]


varNames = c('beauty', 'pleasure', 'complex', 'exciting', 'learn', 'understandable', 'harmony_variety', 'meaningful', 'exceeded_expected', 'interesting', 'moved')

ratings = cbind(dat$beauty, dat$pleasure,  dat$complex, dat$exciting,  dat$learn, dat$understandable, dat$harmony_variety, dat$meaningful, dat$exceeded_expected, dat$interesting, dat$moved)

means_highBeauty = colMeans(ratings[dat$beauty==7,], na.rm=T)
means_lowBeauty = colMeans(ratings[dat$beauty<7,], na.rm=T)

se_highBeauty = apply(ratings[dat$beauty==7,], 2, std.error, na.rm=T)
se_lowBeauty = apply(ratings[dat$beauty<7,], 2, std.error, na.rm=T)

plot_dat = as.data.frame(rbind(means_highBeauty, means_lowBeauty))
names(plot_dat) = varNames
plot_dat = melt(plot_dat)
plot_dat$cat = rep(c('high_beauty','low_beauty'),11)

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

# odd numbers, check
table(dat$beauty)
