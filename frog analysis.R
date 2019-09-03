# Author: Nicholas Wu (nicholas.wu@uq.edu.au or nicholas.wu/nz@gmail.com)
# Date: 03/09/2019
# R version: 3.5.1 -- "Feather Spray"
# Paper ID: Drought plasticity of Fejervarya tadpoles
# Plotting data and statistical analysis
library(ggplot2)
library(cowplot)
mytheme <- theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                              axis.text = element_text(size = 10, colour = "black"))

## graphing #---------------------------------------------------
# load larvae data
larvae<-read.csv("C:/Users/nicho/Dropbox (Personal)/Frog drought paper/larvae_data.csv")
head(larvae)
str(larvae) # check string of dataframe
larvae$date <- as.Date(larvae$date,"%d/%m/%Y") # convert "date" factor as date
library(RColorBrewer) # manually modify colour palette
my_blue = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I exluded the two lighter hues

## larvae growth over expsoure time
fig1a <- ggplot(larvae, aes(x = days, y = length, colour = water_depth)) + mytheme + 
  geom_point(size = 3, alpha = 0.8) + geom_smooth(method = "lm", fill = NA, size=2) +
  scale_colour_manual(values = my_blue) +
  ylab("Total length (cm)") + xlab("Days of exposure")

# load metamorph data
metamorph<-read.csv("C:/Users/nicho/Dropbox (Personal)/Frog drought paper/metamorph_data.csv")
head(metamorph)
str(metamorph)

## growth rate
fig1b <- ggplot(metamorph, aes(x = water_depth, y = growth_rate, colour = water_depth)) + 
  geom_boxplot(fill=NA) + mytheme + 
  geom_jitter(position=position_jitter(0.05), size = 3) +
  scale_colour_manual(values = my_blue) +
  ylab(expression("Growth rate (cm d"^"-1"*")")) + xlab("Water depth (cm)")

leg <- get_legend(fig1a + theme(legend.box.margin = margin(0, 0, 12))) # create some space to the left of the legend
prow <- plot_grid(fig1a + theme(legend.position="none"),
                  fig1b + theme(legend.position="none"), 
                  ncol = 2, align = "h", axis = "bt", labels=c("A", "B"))
plot_grid(prow, leg, rel_widths = c(2, .4)) # group as 3 columns (2 figs + 1 legend)

## larval period
fig2a <- ggplot(metamorph, aes(x = water_depth, y = larval_period, colour = water_depth)) + 
  geom_boxplot(fill=NA) + mytheme + 
  geom_jitter(position=position_jitter(0.05), size = 3) +
  scale_colour_manual(values = my_blue) +
  ylab("Larval period (days)") + xlab("Water depth (cm)")

## metamorph size
fig2b <- ggplot(metamorph, aes(x = water_depth, y = metamorph_size, colour = water_depth)) + 
  geom_boxplot(fill=NA) + mytheme + 
  geom_jitter(position=position_jitter(0.05), size = 3) +
  scale_colour_manual(values = my_blue) +
  ylab("Metamorph size (cm)") + xlab("Water depth (cm)")

# jumping distance
fig2c <- ggplot(metamorph, aes(x = water_depth, y = jump_dist, colour = water_depth)) + 
  geom_boxplot(fill=NA) + mytheme + 
  geom_jitter(position=position_jitter(0.05), size = 3) +
  scale_colour_manual(values = my_blue) +
  ylab("Maximum distance jumped (cm)") + xlab("Water depth (cm)")

#prow <- plot_grid(
#  fig2a + theme(legend.position="none"),
#  fig2b + theme(legend.position="none"),
#  fig2c + theme(legend.position="none"),
#  align = 'vh', labels = c("A", "B", "C"),
#  hjust = -1,nrow = 1) 

#legend <- get_legend(graph1 + theme(legend.box.margin = margin(0, 0, 0, 12))) # create some space to the left of the legend
legend_b <- get_legend(fig2a + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
#plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1)) # group as 3 fig columns + legend bottom

# equation 
library(ggpmisc) # for stat-poly_eq
fig2d <- ggplot(metamorph, aes(x = metamorph_size, y = jump_dist)) + 
  geom_point(size = 3, aes(colour = water_depth)) + mytheme + 
  geom_smooth(method = "lm", colour = "#09519C", fill = "#C6DBEF", linetype = "dashed") +
  scale_colour_manual(values = my_blue) +
  ylab("Maximum distance jumped (cm)") + xlab("Metamorph size (cm)") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

fig2e <- ggplot(metamorph, aes(x = larval_period, y = metamorph_size)) + 
  geom_point(size = 3, aes(colour = water_depth)) + mytheme + 
  geom_smooth(method = "lm", colour = "#09519C", fill = "#C6DBEF", linetype = "dashed") +
  scale_colour_manual(values = my_blue) +
  ylab("Metamorph size (cm)") + xlab("Larval period (days)") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

#prow <- plot_grid(fig3a + theme(legend.position="none"),
                  #fig3b + theme(legend.position="none"), 
                  #ncol = 2, align = "h", axis = "bt", labels=c("A", "B"))
#plot_grid(prow, leg, rel_widths = c(2, .4)) # group as 3 columns (2 graphs + 1 legend)

plot_grid(fig2a + theme(legend.position="none"),
                  fig2b + theme(legend.position="none"),
                  fig2c + theme(legend.position="none"),
                  fig2d + theme(legend.position="none"),
                  fig2e + theme(legend.position="none"),
                  NULL,
                  ncol = 3, align = "h", axis = "bt", 
                  labels=c("A","B","C","D","E",""))

## Statistical Analysis #-----------------------------------
## length days
head(larvae)
hist(larvae$length) # check normality of data distribution
larvae$days <- as.numeric(larvae$days)

library(nlme)
library(lme4)
lm1<- lme(length~days*water_depth, random=~1|ID, data=larvae) # mixed effects model with interactive effect
summary(lm1)
intervals(lm1, which = "fixed")
qqnorm(lm1) #plot normal probability plot of standardised resisduals
plot(lm1) # plot model

library(lsmeans)
# obtain slopes
lm1$coefficients
m.lst <- lstrends(lm1, "water_depth", var="days")
pairs(m.lst) # compare slopes

## metamorph data
head(metamorph)
hist(metamorph$growth_rate) #checked data distribution- all good!

lm2 <- lme(larval_period~water_depth, random=~1|ID, data=metamorph) # construct model for larval period
lm2 <- lme(metamorph_size~water_depth, random=~1|ID, data=metamorph) # construct model for metamorph size
lm2 <- lme(jump_dist~water_depth, random=~1|ID, data=metamorph) # construct model for distance jumped
lm2 <- lme(growth_rate~water_depth, random=~1|ID, data=metamorph) # construct model for growth rate
summary(lm2) # summary of model
intervals(lm2, which = "fixed") # extract CI95%
qqnorm(lm2) # #plot normal probability plot of standardised resisduals
plot(lm2) # plot standardised residuals
library(phia)
testInteractions(lm2, pairwise="water_depth") # poshoc across treatments

## for continuous variables
lm3 <- lm(jump_dist~metamorph_size, data=metamorph) # construct lm 
summary(lm3)
lm4 <- lm(metamorph_size~larval_period, data=metamorph) # construct lm 
summary(lm4)

# output stats
install.packages("sjlabelled") 
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(lm3, lm4, show.se = TRUE, show.df = TRUE, show.stat = TRUE)
# didnt work for nlme models with random effects (not sure why)
