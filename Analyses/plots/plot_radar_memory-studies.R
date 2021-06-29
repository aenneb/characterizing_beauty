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
dat = read_xlsx('beauty_memory_data_merged_with_coding.xlsx', na = 'NA')
dat = dat[dat$excluded==0,]

## manova for memory ratings
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

memory_ratings = cbind(dat$beauty, dat$pleasure,  dat$`want longer`, dat$alive,  dat$`beautiful to everyone`, dat$connections, dat$longing, dat$`desire free`, dat$`mind wander`, dat$surprise, dat$`understand more`, dat$`tells story`)


means_in = colMeans(memory_ratings[dat$experiment=="in" & dat$beauty==7,], na.rm=T)
means_uk = colMeans(memory_ratings[dat$experiment=="uk" & dat$beauty==7,], na.rm=T)
means_us_1 = colMeans(memory_ratings[dat$experiment=="us_1" & dat$beauty==7,], na.rm=T)
means_us_2 = colMeans(memory_ratings[dat$experiment=="us_2" & dat$beauty==7,], na.rm=T)

se_in = apply(memory_ratings[dat$experiment=="in" & dat$beauty==7,], 2, std.error, na.rm=T)
se_uk = apply(memory_ratings[dat$experiment=="uk" & dat$beauty==7,], 2, std.error, na.rm=T)
se_us_1 = apply(memory_ratings[dat$experiment=="us_1" & dat$beauty==7,], 2, std.error, na.rm=T)
se_us_2 = apply(memory_ratings[dat$experiment=="us_2" & dat$beauty==7,], 2, std.error, na.rm=T)

plot_dat = as.data.frame(rbind(means_in, means_uk, means_us_1, means_us_2))
names(plot_dat) = varNames
plot_dat = melt(plot_dat)
plot_dat$country = rep(c('in','uk','us_1','us_2'),12)

ses = as.data.frame(rbind(se_in, se_uk, se_us_1, se_us_2))
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

ggplot(data= plot_dat, aes(x = variable, y = value, group = country)) +
  facet_grid(. ~ country) +
  geom_polygon(aes(y = upper), fill = "grey50", alpha = 0.5) +
  geom_polygon(aes(y = lower), fill = "grey99", alpha = 0.7) +
  geom_polygon(fill = NA, colour='black') +
  theme_light() +
  theme(panel.grid.minor = element_blank()) + 
  coord_radar() +
  labs(x = "", y = "")

# Get Ns
nrow(memory_ratings[dat$experiment=="in" & dat$beauty==7,])
nrow(memory_ratings[dat$experiment=="uk" & dat$beauty==7,])
nrow(memory_ratings[dat$experiment=="us_1" & dat$beauty==7,])
nrow(memory_ratings[dat$experiment=="us_2" & dat$beauty==7,])