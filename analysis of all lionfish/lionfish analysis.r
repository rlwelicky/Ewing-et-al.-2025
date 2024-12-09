library(tidyverse)
library(dplyr)
library(broom)
library(DHARMa)
library(spdep)
library(glmmTMB)
library(multcomp)
library(lmtest)
library(car)
library(ggplot2)
library(cowplot)
library(lme4)

#read in data
lion<-read.csv("0705-lionfishdata.csv", header=TRUE)

#For our analyses we don't want to include salmon standards or duplicates, so we are going to remove them from the dataframe, and then proceed for the mean and sd for M, S, H, G for C and N
liondata<-lion %>% 
  filter(calibration == "single" |calibration == "average")

descriptivestats<-liondata %>% 
  group_by(depth_categorical)%>% 
  summarize(
    meanmuscleC = mean(muscle_c,na.rm=TRUE),
    sdmuscleC = sd(muscle_c, na.rm = TRUE),
    meanscaleC = mean(scale_c,na.rm=TRUE),
    sdscaleC = sd(scale_c, na.rm = TRUE),
    meanheartC = mean(heart_c,na.rm=TRUE),
    sdheartC = sd(heart_c, na.rm = TRUE),
    meanmuscleN = mean(muscle_n,na.rm=TRUE),
    sdmuscleN = sd(muscle_n, na.rm = TRUE),
    meanscaleN = mean(scale_n,na.rm=TRUE),
    sdscaleN = sd(scale_n, na.rm = TRUE),
    meanheartN = mean(heart_n,na.rm=TRUE),
    sdheartN = sd(heart_n, na.rm = TRUE))
write.csv(descriptivestats, "descriptivestats.csv")

#read in relevant stats for making ggplot figures
figurevalues<-read.csv("figurevalues.csv", header=TRUE)


#check if our data are normally distributed. 


#CARBON

hist(liondata$muscle_c)
#lets check out the distribution of muscle carbon
normalitymuscle_c<-glm(muscle_c~depth_categorical, data = liondata)
resmuscle_c<-simulateResiduals(fittedModel = normalitymuscle_c, n = 250)
plot(resmuscle_c)
resmuscle_c$scaledResiduals
testUniformity(resmuscle_c)

hist(liondata$scale_c)
#lets check out the distribution of scale carbon
normalityscale_c<-glm(scale_c~depth_categorical, data = liondata)
resscale_c<-simulateResiduals(fittedModel = normalityscale_c, n = 250)
plot(resscale_c)
resscale_c$scaledResiduals
testUniformity(resscale_c) #not normal

hist(liondata$heart_c)
#lets check out the distribution of heart carbon
normalityheart_c<-glm(heart_c~depth_categorical, data = liondata)
resheart_c<-simulateResiduals(fittedModel = normalityheart_c, n = 250)
plot(resheart_c)
resheart_c$scaledResiduals
testUniformity(resheart_c)

#NITROGEN

hist(liondata$muscle_n)
#lets check out the distribution of muscle nitrogen
normalitymuscle_n<-glm(muscle_n~depth_categorical, data = liondata)
resmuscle_n<-simulateResiduals(fittedModel = normalitymuscle_n, n = 250)
plot(resmuscle_n)
resmuscle_n$scaledResiduals
testUniformity(resmuscle_n)

hist(liondata$scale_n)
#lets check out the distribution of scale nitrogen
normalityscale_n<-glm(scale_n~depth_categorical, data = liondata)
resscale_n<-simulateResiduals(fittedModel = normalityscale_n, n = 250)
plot(resscale_n)
resscale_n$scaledResiduals
testUniformity(resscale_n)

hist(liondata$heart_n)
#lets check out the distribution of heart nitrogen
normalityheart_n<-glm(heart_n~depth_categorical, data = liondata)
resheart_n<-simulateResiduals(fittedModel = normalityheart_n, n = 250)
plot(resheart_n)
resheart_n$scaledResiduals
testUniformity(resheart_n)

# #How does depth influence the diet of lionfish (using muscle tissue)? 


######## ANCOVA -- SL as a covariate, depth as categorical
anc_heart_c<-aov(heart_c ~ depth_categorical + SL_mm, data=liondata)
Anova(anc_heart_c, type="III")

anc_heart_n<-aov(heart_n ~ depth_categorical + SL_mm, data=liondata)
Anova(anc_heart_n, type="III")

anc_muscle_c<-aov(muscle_c ~ depth_categorical + SL_mm, data=liondata)
Anova(anc_muscle_c, type="III")

anc_muscle_n<-aov(muscle_n ~ depth_categorical + SL_mm, data=liondata)
Anova(anc_muscle_n, type="III")

anc_scale_c<-aov(scale_c ~ depth_categorical + SL_mm, data=liondata)
Anova(anc_scale_c, type="III")

anc_scale_n<-aov(scale_n ~ depth_categorical + SL_mm, data=liondata)
Anova(anc_scale_n, type="III")

######  TROPHIC POSITION

# making a variable called TP for trophic position. I will ammend the descriptive stats below. 
# We are using a value of 4 for the producer/prey from Zhu et al. 
# We will stick with the common 3.4 for the Trophic discrimination factor (TDF), which is the stepwise change in per mil for one trophic level to the next. 
# we'll use muscle tissue for calaculating TP

liondata$tp<-((liondata$muscle_n - 4)/3.4) + 1

#first check distribution

normalitytp<-glm(tp~depth_categorical, data = liondata)
resscale_tp<-simulateResiduals(fittedModel = normalitytp, n = 250)
plot(resscale_tp)
resscale_tp$scaledResiduals
testUniformity(resscale_tp) #data are normally dist

#ANCOVA for trophic position, with SL as covariate, depth as categorical

tpresults_cat<-aov(tp ~ depth_categorical + SL_mm, data = liondata)
Anova(tpresults_cat, type="III")




##########


######## FIGURES

#I created a column in figurevalues to be each tissue/depth combination
figurevalues$combo<-c("deep_muscle", "shallow_muscle", "deep_scale", "shallow_scale", "deep_heart", "shallow_heart")


m<-ggplot(data = figurevalues) +
  aes(x = meanC, y = meanN, color = depth, shape = tissue) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 0.1) + 
  geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)))  +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values=c("deep" = "#2815d4", "shallow" = "#15c4d4"))
plot(m)
ggsave("isoplot_alone.jpg")

# making plot without legends
m2<-ggplot(data = figurevalues) +
  aes(x = meanC, y = meanN, color = depth, shape = tissue) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 0.1) + 
  geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)))  +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab("") + theme_classic() + theme(legend.position = "none", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) 
plot(m2)

# read in longform data for plotting ellipse figures
lionlong3<-read.csv("lionlong3.csv", header=TRUE)



EllipseFig<- ggplot(lionlong3, aes(x = c, y = n, color = tissuedepth)) +
  stat_ellipse(geom = "polygon", aes(fill = tissuedepth),  alpha = 0.7, position = "identity", level = 0.95, linewidth = 1, linetype=1) + 
  scale_color_manual(values = c("heart_deep" = "black", "heart_shallow" = "black", "scale_deep" = "black", "scale_shallow" = "black", "muscle_deep" = "black", "muscle_shallow" = "black")) +
  scale_fill_manual(values = c("scale_deep" = "#FFC900", "scale_shallow" = "#FDEDA3", "heart_deep" = "#FF0000", "heart_shallow" = "#FFA6A6", "muscle_deep" = "#0080FF", "muscle_shallow" = "#A3D1FD")) +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) +
  scale_x_continuous(breaks=c(-20, -18, -16, -14, -12), limits=c(-20, -12))+
  scale_y_continuous(breaks=c(7, 8, 9, 10, 11, 12), limits=c(7, 12))+
  theme(text = element_text(size = 30))

plot(EllipseFig)
ggsave("ellipses_fulldata_2024.jpg")




