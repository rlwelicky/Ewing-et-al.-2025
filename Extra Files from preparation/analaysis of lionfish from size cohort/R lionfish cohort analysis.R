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


lion_t<-read.csv("lionfish_size_subsection.csv", header=TRUE)

# ancova of C from heart tissue
anc_heart_c<-aov(heart_c ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_heart_c, type="III")
# heart carbon not sig

# ancova of N from heart tissue
anc_heart_n<-aov(heart_n ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_heart_n, type="III")
# heart nitrogen signficant

# ancova of C from muscle tissue
anc_muscle_c<-aov(muscle_c ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_muscle_c, type="III")
# muscle carbon not sig

# ancova of N from muscle tissue
anc_muscle_n<-aov(muscle_n ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_muscle_n, type="III")
# muscle nitrogen sig

# ancova of C from scale tissue
anc_scale_c<-aov(scale_c ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_scale_c, type="III")
# scale carbon not sig

# ancova of N from scale tissue
anc_scale_n<-aov(scale_n ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_scale_n, type="III")
# scale nitrogen sig


#Make variable called TP for trophic position.
#We are using a value of 4 for the producer/prey from Zhu et al.
#We will stick with the common 3.4 for the Trophic discrimination factor (TDF), 
#which is the stepwise change in per mil for one trophic level to the next. 
#we'll use muscle tissue for calaculating TP

lion_t$tp<-((lion_t$muscle_n - 4)/3.4) + 1

anc_TP<-aov(tp ~ depth_categorical + SL_mm, data=lion_t)
Anova(anc_TP, type="III")
# tp significant

### descriptive stats

descriptivestats_t<-lion_t %>% 
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
write.csv(descriptivestats_t, "descriptivestats_t.csv")

######## ELLIPSE FIGURES



#Read in long-form stable isotope data as "figurevalues_t.csv"
#I create a column in figurevalues_t.csv, "combo", to be each tissue/depth combination

figurevalues<-read.csv("figurevalues_t.csv", header=TRUE)
figurevalues$combo<-c("deep_muscle", "shallow_muscle", "deep_scale", "shallow_scale", "deep_heart", "shallow_heart")

#plotting Carbon and Nitrogen data
m<-ggplot(data = figurevalues) +
  aes(x = meanC, y = meanN, color = depth, shape = tissue) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 0.1) + 
  geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)))  +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values=c("deep" = "#2815d4", "shallow" = "#15c4d4"))
plot(m)
ggsave("isoplot_alone.jpg")


#plotting Nitrogen data without legends, for cleaning up in post
m2<-ggplot(data = figurevalues) +
  aes(x = meanC, y = meanN, color = depth, shape = tissue) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 0.1) + 
  geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)))  +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab("") + theme_classic() + theme(legend.position = "none", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) 
plot(m2)


# plotting C and N data as ellipses

# read in data
lionlong3<-read.csv("lionlong3_t.csv", header=TRUE)

# setting graphical parameters in ggplot
plot_lion_subsection<- ggplot(lionlong3, aes(x = c, y = n, color = tissuedepth)) +
  stat_ellipse(geom = "polygon", aes(fill = tissuedepth),  alpha = 0.7, position = "identity", level = 0.95, size = 1, linetype=1) + 
  scale_color_manual(values = c("heart_deep" = "black", "heart_shallow" = "black", "scale_deep" = "black", "scale_shallow" = "black", "muscle_deep" = "black", "muscle_shallow" = "black")) +
  scale_fill_manual(values = c("scale_deep" = "#FFC900", "scale_shallow" = "#FDEDA3", "heart_deep" = "#FF0000", "heart_shallow" = "#FFA6A6", "muscle_deep" = "#0080FF", "muscle_shallow" = "#A3D1FD")) +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) +
  scale_x_continuous(breaks=c(-20, -18, -16, -14, -12), limits=c(-20, -12))+
  scale_y_continuous(breaks=c(7, 8, 9, 10, 11, 12), limits=c(7, 12))+
  theme(text = element_text(size = 30))


plot(plot_lion_subsection)
ggsave("lionfish_subsection_ellipses")










