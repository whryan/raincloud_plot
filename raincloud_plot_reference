# Raincloud plot reference
# This is from https://micahallen.org/2018/03/15/introducing-raincloud-plots/amp/?__twitter_impression=true
# Micah Allen made a cool plot, and I wanted to be able to use it later on, so I put it in a Github


library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)

#define ggplot2 theme
raincloud_theme = theme(
text = element_text(size = 10),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16),
axis.text = element_text(size = 14),
axis.text.x = element_text(angle = 45, vjust = 0.5),
legend.title=element_text(size=16),
legend.text=element_text(size=16),
legend.position = "right",
plot.title = element_text(lineheight=.8, face="bold", size = 16),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


#read data
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

my_data<-read.csv(url("https://data.bris.ac.uk/datasets/112g2vkxomjoo1l26vjmvnlexj/2016.08.14_AnxietyPaper_Data%20Sheet.csv"))

#wide to long
my_datal <- melt(my_data, id.vars = c("Participant"), measure.vars = c("AngerUH", "DisgustUH", "FearUH", "HappyUH"), variable.name = "EmotionCondition", value.name = "Sensitivity")

#summary stats
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld<- ddply(my_datal, ~EmotionCondition, summarise, mean = mean(Sensitivity), median = median(Sensitivity), lower = lb(Sensitivity), upper = ub(Sensitivity))


#Raincloud plot
g <- ggplot(data = my_datal, aes(y = Sensitivity, x = EmotionCondition, fill = EmotionCondition)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
geom_point(aes(y = Sensitivity, color = EmotionCondition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 5.25) +
guides(fill = FALSE) +
guides(color = FALSE) +
scale_color_brewer(palette = "Spectral") +
scale_fill_brewer(palette = "Spectral") +
coord_flip() +
theme_bw() +
raincloud_theme

#Elephant plot
g <- ggplot(data = my_datal, aes(y = Sensitivity, x = EmotionCondition, fill = EmotionCondition)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
geom_point(aes(y = Sensitivity, color = EmotionCondition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 5.25) +
guides(fill = FALSE) +
guides(color = FALSE) +
scale_color_brewer(palette = "Spectral") +
scale_fill_brewer(palette = "Spectral") +
# coord_flip() +
theme_bw() +
raincloud_theme