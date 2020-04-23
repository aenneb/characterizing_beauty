rm(list = ls(all = TRUE)) #clear workspace
## set working directory
setwd("/Volumes/GoogleDrive/My Drive/PhD/studies/Beauty characterization/analyses/")
# setwd("/Users/aennebrielmann/Desktop/Beauty characeterization/analyses/")

## load necessary packages
require("readxl")
require('effsize')
require("ggplot2")
require('plotrix')
require('reshape2')

# get the overall beauty memory rating data for comparison
combined_dat = read_xlsx('relief_beauty_merged_data.xlsx', na = 'NA')
combined_dat = combined_dat[combined_dat$excluded==0,]
combined_dat$gender = as.factor(combined_dat$gender)

# Get Ns
nrow(combined_dat[combined_dat$concept=='beauty',])
nrow(combined_dat[combined_dat$concept=='relief',])

ratings= cbind(combined_dat$beauty, combined_dat$pleasure, combined_dat$surprise, combined_dat$`want longer`, combined_dat$`desire free`, combined_dat$alive, combined_dat$`understand more`, combined_dat$`mind wander`, combined_dat$connections, combined_dat$`tells story`, combined_dat$`beautiful to everyone`, combined_dat$peaceful, combined_dat$perfect)

aov = manova(ratings ~ combined_dat$concept + combined_dat$gender + combined_dat$age)
summary(aov, test="Pillai")

# single t-tests
t.test(beauty ~ concept, combined_dat)
t.test(pleasure ~ concept, combined_dat)
t.test(surprise ~ concept, combined_dat)
t.test(`want longer` ~ concept, combined_dat)
t.test(`desire free` ~ concept, combined_dat)
t.test(alive ~ concept, combined_dat)
t.test(`understand more` ~ concept, combined_dat)
t.test(`mind wander` ~ concept, combined_dat)
t.test(connections ~ concept, combined_dat)
t.test(`tells story` ~ concept, combined_dat)
t.test(`beautiful to everyone` ~ concept, combined_dat)
t.test(longing ~ concept, combined_dat)
t.test(peaceful ~ concept, combined_dat)
t.test(perfect ~ concept, combined_dat)

#effect size: cohen's d
cohen.d(beauty ~ concept, combined_dat)
cohen.d(pleasure ~ concept, combined_dat)
# cohen.d(surprise ~ concept, combined_dat)
cohen.d(`want longer` ~ concept, combined_dat)
cohen.d(`desire free` ~ concept, combined_dat)
cohen.d(alive ~ concept, combined_dat)
cohen.d(`understand more` ~ concept, combined_dat)
cohen.d(`mind wander` ~ concept, combined_dat)
cohen.d(connections ~ concept, combined_dat)
cohen.d(`tells story` ~ concept, combined_dat)
# cohen.d(`beautiful to everyone` ~ concept, combined_dat)
cohen.d(longing ~ concept, combined_dat)


# # plot
means = aggregate(data=combined_dat, beauty ~ concept, mean, na.rm=T)
means$pleasure = aggregate(data=combined_dat, pleasure ~ concept, mean, na.rm=T)[,2]
means$surprise = aggregate(data=combined_dat, surprise ~ concept, mean, na.rm=T)[,2]
means$wantLonger = aggregate(data=combined_dat, `want longer` ~ concept, mean, na.rm=T)[,2]
means$desireFree = aggregate(data=combined_dat, `desire free` ~ concept, mean, na.rm=T)[,2]
means$alive = aggregate(data=combined_dat, alive ~ concept, mean, na.rm=T)[,2]
means$understandMore = aggregate(data=combined_dat, `understand more` ~ concept, mean, na.rm=T)[,2]
means$mindWander = aggregate(data=combined_dat, `mind wander` ~ concept, mean, na.rm=T)[,2]
means$connections = aggregate(data=combined_dat, connections ~ concept, mean, na.rm=T)[,2]
means$tellsStory = aggregate(data=combined_dat, `tells story` ~ concept, mean, na.rm=T)[,2]
means$universal = aggregate(data=combined_dat, `beautiful to everyone` ~ concept, mean, na.rm=T)[,2]
means$longing = aggregate(data=combined_dat, longing ~ concept, mean, na.rm=T)[,2]

SEs = aggregate(data=combined_dat, beauty ~ concept, std.error, na.rm=T)
SEs$pleasure = aggregate(data=combined_dat, pleasure ~ concept, std.error, na.rm=T)[,2]
SEs$surprise = aggregate(data=combined_dat, surprise ~ concept, std.error, na.rm=T)[,2]
SEs$wantLonger = aggregate(data=combined_dat, `want longer` ~ concept, std.error, na.rm=T)[,2]
SEs$desireFree = aggregate(data=combined_dat, `desire free` ~ concept, std.error, na.rm=T)[,2]
SEs$alive = aggregate(data=combined_dat, alive ~ concept, std.error, na.rm=T)[,2]
SEs$understandMore = aggregate(data=combined_dat, `understand more` ~ concept, std.error, na.rm=T)[,2]
SEs$mindWander = aggregate(data=combined_dat, `mind wander` ~ concept, std.error, na.rm=T)[,2]
SEs$connections = aggregate(data=combined_dat, connections ~ concept, std.error, na.rm=T)[,2]
SEs$tellsStory = aggregate(data=combined_dat, `tells story` ~ concept, std.error, na.rm=T)[,2]
SEs$universal = aggregate(data=combined_dat, `beautiful to everyone` ~ concept, std.error, na.rm=T)[,2]
SEs$longing = aggregate(data=combined_dat, longing ~ concept, std.error, na.rm=T)[,2]

plot_data = cbind(melt(means, value.name="mean"), melt(SEs, value.name="SE")[,3])
colnames(plot_data)[4] <- "SE"
plot_data$lb = plot_data$mean - plot_data$SE
plot_data$ub = plot_data$mean + plot_data$SE
plot_data=as.data.frame(plot_data)

g1 <- ggplot(data=plot_data, aes(x=variable, y=mean, color=concept)) + 
  geom_pointrange(data=plot_data, aes(ymin=lb, ymax=ub)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g1

ggplot(data= plot_data, aes(x = variable, y = mean, group = concept, ymin=3, ymax=7)) +
  facet_grid(. ~ concept) +
  geom_polygon(aes(y = ub), fill = "grey50", alpha = 0.5) +
  geom_polygon(aes(y = lb), fill = "grey99", alpha = 0.7) +
  geom_polygon(fill = NA, colour='black') +
  theme_light() +
  theme(panel.grid.minor = element_blank()) + 
  coord_polar() +
  labs(x = "", y = "")

