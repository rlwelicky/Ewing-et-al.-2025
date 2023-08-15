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

#Okay, lets read in the files and packages we will use for data analysis. If the files is stored in your R project, all you need is a read.csv command! below you will see I am calling in the csv called lionfish data and from here out I'm calling this datasheet/dataframe "lion"
lion<-read.csv("0705-lionfishdata.csv", header=TRUE)



library(plotly)
scatter<-ggplot(data = lion) +
  aes(x = depth_capture_avg_m, y = muscle_c, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{13}*"C"*" (in ‰)")) + xlab("Average capture depth (in m)") + theme_classic() + theme(legend.position = "right", legend.title="element_blank()) + legen")

library(scatterplot3d)

scatterplot3d(x = lion$depth_capture_avg_m, y = lion$muscle_n, z =lion$SL_mm, highlight.3d = FALSE)


p <- plot_ly(lion, x=~SL_mm, y=~muscle_n, 
             z=~depth_capture_avg_m)
#could you please go back to the original datasheet you just read in and remove any extra spaces inthe variable names. Then remove the extra periods thruout the sheet. Then read in the file again.

#could you please go back to the original datasheet you just read in and remove any extra spaces inthe variable names. Then remove the extra periods thruout the sheet.

#also please make sure the depth for each row is filled in or left blank if unknown.




#check that the factors are properply called numeric or charachter; if they're not correct we can fix that.
#lets just start with some descriptive statistics (mean, standard deviation, standard errror)

#The "tidyverse" is one of the most succinct and user friendly ways to summarise and organize data. You can give it instructions for things like: do this, then that, by using that pipe feature which is denoted via %>%
#This website https://r4ds.had.co.nz/index.html has amazing tutorials and references. You can also google tidy cheatsheet or your question and then tidy in r, and usually a helpful example pops up.

# So, we need to make sure that the standards are not wildly different from one another before we do anything else. This is our check that the mass spec worked correctly. So, lets first make a dataframe for just standards. The filter feature in tidy lets you filter for things you want and/or do not.

#Here I am making a dataframe just for standards, and looking at the mean and sd of C and N
standards<-lion%>%
  filter(calibration == "standard") %>% 
  summarize(
    meansalmonC = mean(muscle_c, na.rm = TRUE),
    sdsamlmonC= sd(muscle_c, na.rm = TRUE),
    meansalmonN = mean(muscle_n, na.rm = TRUE),
    sdsamlmonN= sd(muscle_n, na.rm = TRUE)
    )
  
#For our analyses we don't want to include salmon standards or duplicates, so we are going to remove them from the dataframe, and then proceed for the mean and sd for M, S, H, G for C and N
liondata<-lion %>% 
  filter(calibration == "single" |calibration == "average")

#Feb282023, I am making a variable called TP for trophic position. I will ammend the descriptive states below. We are using a value of 4 for the producer/prey from Zhu et al. We will stick with the common 3.4 for the Trophic discrimination factor (TDF), which is the stepwise change in per mil for one trophic level to the next. we'll use muscle tissue for calaculating TP because this physiologically makes the most sense, and is what tissue is most common in lit for deriving tp

liondata$tp<-((liondata$muscle_n - 4)/3.4) + 1




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
    sdheartN = sd(heart_n, na.rm = TRUE),
    meantp =mean(tp,na.rm = TRUE),
    sdtp = sd(tp, na.rm = TRUE))
write.csv(descriptivestats, "descriptivestats_lion.csv")

#lets visualize our data by making a figure using the new dataframe we just created
#Megan try followingmodifying the code below as well as taking direction from this website: https://gist.github.com/AndrewLJackson/b16fb216d8a9a96c20a4a979ec97e7b0
figurevalues<-read.csv("figurevalues.csv", header=TRUE)
#I transposed descriptive stats to make a more succinct plot code. You can transpose in R, but given how small this df was I just did it manually---this is not a good practice, but c'est le vie for now.

m<-ggplot(data = figurevalues) +
  aes(x = meanC, y = meanN, color = depth, shape = tissue) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 1) + 
  geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)), width = 1)  +
  ylab(expression(δ^{15}*"N"*" (in ‰)")) + xlab(expression(δ^{13}*"C"*" (in ‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank())






ggsave("figure.jpg")

m2<-ggplot(data = descriptivestats) +
  aes(x = meanmuscleC, y = meanmuscleN, color = depth_categorical) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanmuscleN-sdmuscleN),ymax = (meanmuscleN + sdmuscleN)), width = 1) + 
  geom_errorbarh(aes(xmin = (meanmuscleC-sdmuscleC),xmax = (meanmuscleC +sdmuscleC)), width = 1)  +
  ylab(expression(δ^{15}*"N"*" (in ‰)")) + xlab(expression(δ^{13}*"C"*" (in ‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank())

s<-ggplot(data = descriptivestats) +
  aes(x = meanscaleC, y = meanscaleN, color = depth_categorical) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanscaleN-sdscaleN),ymax = (meanscaleN + sdscaleN)), width = 1) + 
  geom_errorbarh(aes(xmin = (meanscaleC-sdscaleC),xmax = (meanscaleC +sdscaleC)), width = 1)  +
  ylab(expression(δ^{15}*"N"*" (in ‰)")) + xlab(expression(δ^{13}*"C"*" (in ‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank())

h<-ggplot(data = descriptivestats) +
  aes(x = meanheartC, y = meanheartN, color = depth_categorical) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanheartN-sdheartN),ymax = (meanheartN + sdheartN)), width = 1) + 
  geom_errorbarh(aes(xmin = (meanheartC-sdheartC),xmax = (meanheartC +sdheartC)), width = 1)  +
  ylab(expression(δ^{15}*"N"*" (in ‰)")) + xlab(expression(δ^{13}*"C"*" (in ‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank())




  #you will copy and paste all the above code and do it with scale and heart, too, and change the color. you can choose colors by searching hex color pallete on google and entering the numeric hex code in "". 

#The most critical thing to do before any modeling is to check if our data are normally distributed. We can visualiz the raw data, and for best practices, we then visualize the residuals of the data

#CARBON

hist(liondata$muscle_c)
#lets check out the distribution of muscle carbon--categorical
normalitymuscle_c<-glm(muscle_c~depth_categorical, data = liondata)
resmuscle_c<-simulateResiduals(fittedModel = normalitymuscle_c, n = 250)
plot(resmuscle_c)
resmuscle_c$scaledResiduals
testUniformity(resmuscle_c)
#if we just do a shapiro wilk test, not enough data
shapiro.test(liondata$muscle_c)
#lets check out the distribution of muscle carbon--categorical
normalitymuscle_c<-glm(muscle_c~depth_capture_avg_m, data = liondata)
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
shapiro.test(liondata$scale_c)

#lets check out the distribution of scale carbon
normalityscale_c<-glm(scale_c~depth_capture_avg_m, data = liondata)
resscale_c<-simulateResiduals(fittedModel = normalityscale_c, n = 250)
plot(resscale_c)
resscale_c$scaledResiduals
testUniformity(resscale_c) #not normal
shapiro.test(liondata$scale_c)




hist(liondata$heart_c)
#lets check out the distribution of heart carbon
normalityheart_c<-glm(heart_c~depth_categorical, data = liondata)
resheart_c<-simulateResiduals(fittedModel = normalityheart_c, n = 250)
plot(resheart_c)
resheart_c$scaledResiduals
testUniformity(resheart_c)
testUniformity(normalityheart_c)
shapiro.test(liondata$heart_c)


#lets check out the distribution of heart carbon  quant
normalityheart_c<-glm(heart_c~depth_capture_avg_m, data = liondata)
resheart_c<-simulateResiduals(fittedModel = normalityheart_c, n = 250)
plot(resheart_c)
resheart_c$scaledResiduals
testUniformity(resheart_c)
testUniformity(normalityheart_c) #too few data for quick test without residuals


#NITROGEN




hist(liondata$muscle_n)
#lets check out the distribution of muscle nitrogen
normalitymuscle_n<-glm(muscle_n~depth_categorical, data = liondata)
resmuscle_n<-simulateResiduals(fittedModel = normalitymuscle_n, n = 250)
plot(resmuscle_n)
resmuscle_n$scaledResiduals
testUniformity(resmuscle_n)
shapiro.test(liondata$muscle_n)

hist(liondata$scale_n)

#lets check out the distribution of muscle nitrogen quant
normalitymuscle_n<-glm(muscle_n~depth_capture_avg_m data = liondata)
resmuscle_n<-simulateResiduals(fittedModel = normalitymuscle_n, n = 250)
plot(resmuscle_n)
resmuscle_n$scaledResiduals
testUniformity(resmuscle_n)
shapiro.test(liondata$muscle_n) #this is also not right bc it doesn't factor in depth. just running it to check it anyways




#lets check out the distribution of scale nitrogen
normalityscale_n<-glm(scale_n~depth_categorical, data = liondata)
resscale_n<-simulateResiduals(fittedModel = normalityscale_n, n = 250)
plot(resscale_n)
resscale_n$scaledResiduals
testUniformity(resscale_n)
shapiro.test(liondata$scale_n)

hist(liondata$heart_n)


#lets check out the distribution of scale nitrogen quant
normalityscale_n<-glm(scale_n~depth_capture_avg_m, data = liondata)
resscale_n<-simulateResiduals(fittedModel = normalityscale_n, n = 250)
plot(resscale_n)
resscale_n$scaledResiduals
testUniformity(resscale_n)
shapiro.test(liondata$scale_n)



#lets check out the distribution of heart nitrogen
normalityheart_n<-glm(heart_n~depth_categorical, data = liondata)
resheart_n<-simulateResiduals(fittedModel = normalityheart_n, n = 250)
plot(resheart_n)
resheart_n$scaledResiduals
testUniformity(resheart_n)
shapiro.test(liondata$heart_n)

#lets check out the distribution of heart nitrogen quant
normalityheart_n<-glm(heart_n~depth_capture_avg_m, data = liondata)
resheart_n<-simulateResiduals(fittedModel = normalityheart_n, n = 250)
plot(resheart_n)
resheart_n$scaledResiduals
testUniformity(resheart_n)
shapiro.test(liondata$heart_n)

#How does depth influence the diet of lionfish (using muscle tissue)? This format is response variable ~ indp factor + indp factor + indfactor interacts with other indp factor. We are using SL.mm*depth.categorical bc its likely at shallow depths there is more culling and so we might only find smaller/younger lionfish.  Repeat this formatting for scale and heart. 

#muscle #analyses for paper as of 2.28.2023
mcresults<-glm(muscle_c ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)


mcresults2<-glm(muscle_c ~ SL_mm + depth_capture_avg_m + SL_mm*depth_capture_avg_m, data = liondata)

summary(mcresults) #not sig
summary(mcresults2)

summary(mcresults) 
mnresults<-glm(muscle_n ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)

summary(mnresults) #not sig


#scale #analyses for paper as of 2.28.2023
scresults<-glm(scale_c ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(scresults) #not sig

#lets check normality of the model since raw data weren't  normal
residuals<-simulateResiduals(fittedModel = scresults, n = 250)
testUniformity(residuals) #residuals are normal...

snresults<-glm(scale_n ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(snresults) #sig!


#heart #analyses for paper as of 2.28.2023
hcresults<-glm(heart_c ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(hcresults) #not sig
hnresults<-glm(heart_n ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(hnresults) #sig!

#new analysis with trophic position  Does depth, size, etc influence TP?

#first check distribution
#lets check out the distribution of scale nitrogen
normalitytp<-glm(tp~depth_categorical, data = liondata)
resscale_tp<-simulateResiduals(fittedModel = normalitytp, n = 250)
plot(resscale_tp)
resscale_tp$scaledResiduals
testUniformity(resscale_tp) #data are normally dist

tpresults<-glm(tp ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(tpresults)
#TP results not sig

#One reviewer asked if maybe we don't see differences because the 13C values are the same in our deep and shallow environments. Lets ask this exact question...However, I don't think we can use this because SIA values will be influenced by size. Our best bet is to just look at the range in our carbon values and compare this qualitatively to ranges in carbon values in the lit.

sitediffmuscle_c<-glm(muscle_c~depth_categorical, data = liondata)
summary(sitediffmuscle_c)

sitediffheart_c<-glm(heart_c~depth_categorical, data = liondata)
summary(sitediffheart_c)

sitediffscale_c<-glm(scale_c~depth_categorical, data = liondata)
summary(sitediffscale_c)

plot<-ggplot(data = descriptivestats) +
  aes(x = meanscaleC, y = meanscaleN, color = depth_categorical) +
  geom_point(size=4)  +
  geom_errorbar(aes(ymin = (meanscaleN-sdscaleN),ymax = (meanscaleN + sdscaleN)), width = 1) + 
  geom_errorbarh(aes(xmin = (meanscaleC-sdscaleC),xmax = (meanscaleC +sdscaleC)), width = 1)  +
  ylab(expression(δ^{15}*"N"*" (in ‰)")) + xlab(expression(δ^{13}*"C"*" (in ‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank())



#figures for paper

scattercarbon_muscle<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = muscle_c, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{13}*"C"*" of tissue (‰)")) + xlab("Depth (m)") + theme_classic() + labs(color = "Standard length (mm)") + scale_colour_gradient(low="#FFC20A", high="#994F00")  +  theme(text = element_text(size = )) + ylab("") + theme(legend.position="none") + ggtitle("Muscle")+ theme(axis.text = element_text(size = 36)) + theme(title  = element_text(size = 36))

ggsave("muscle.depth.carbon.jpg")

scatternitrogen_muscle<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = muscle_n, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{15}*"N"*" of tissue (‰)")) + xlab("Depth (m)") + theme_classic() + labs(color = "Standard length (mm)") + scale_colour_gradient(low="#FFC20A", high="#994F00")  +  theme(text = element_text(size = )) + ylab("") + theme(legend.position="none") + ggtitle("Muscle")+ theme(axis.text = element_text(size = 36)) + theme(title  = element_text(size = 36))
ggsave("muscle.depth.nitrogen.jpg")

#figures for supp

scattercarbon_heart<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = heart_c, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{13}*"C"*" of tissue (‰)"))  + xlab("")+ theme_classic()  + scale_colour_gradient(low="#FFC20A", high="#994F00") +  theme(text = element_text(size = 36)) + theme(legend.position="none") + ggtitle("Heart")+ theme(axis.text = element_text(size = 36))


ggsave("heart.depth.carbon.jpg")

scatternitrogen_heart<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = heart_n, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{15}*"N"*" oftissue (‰)"))  + xlab("")+ theme_classic()  + scale_colour_gradient(low="#FFC20A", high="#994F00") +  theme(text = element_text(size = 36)) + theme(legend.position="none") + ggtitle("Heart")+ theme(axis.text = element_text(size = 36))
ggsave("heart.depth.nitrogen.jpg")


scattercarbon_scale<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y =scale_c, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{13}*"C"*" of scale tissue (in ‰)"))  + xlab("") + theme_classic() + labs(color = "Standard length (mm)") + scale_colour_gradient(low="#FFC20A", high="#994F00")  + ggtitle("Scale") +theme(text = element_text(size = 36)) + theme(axis.text = element_text(size = 36)) + ylab("") + theme(legend.key.size = unit(2, 'cm'))

ggsave("scale.depth.carbon.jpg")


scatternitrogen_scale<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = scale_n, color=SL_mm) +
  geom_point(size = 2)  +
  ylab(expression(δ^{15}*"N"*" of scale tissue (in ‰)"))  + xlab("") + theme_classic() + labs(color = "Standard length (mm)") + scale_colour_gradient(low="#FFC20A", high="#994F00")  + ggtitle("Scale") +theme(text = element_text(size = 36)) + theme(axis.text = element_text(size = 36)) + ylab("") + theme(legend.key.size = unit(2, 'cm'))



#+ theme(legend.text=element_text(size=rel(0.55)))
ggsave("scale.depth.nitrogen.jpg")

cor.test(liondata$SL_mm, liondata$depth_capture_avg_m)



#nitrogen versus depth panel figure
library(patchwork)
library(ggpubr)
library(gridExtra)
library(plotly)

scatternitogenpanel<-(scatternitrogen_heart | scatternitrogen_muscle) | scatternitrogen_scale
ggsave(scatternitogenpanel, file = "scatternitogenpanel.jpg", width=24, height =8)
cor.test(liondata$scale_n, liondata$depth_capture_avg_m)           
cor.test(liondata$muscle_n, liondata$depth_capture_avg_m)     
cor.test(liondata$heart_n, liondata$depth_capture_avg_m)  


scattercarbonpanel<-(scattercarbon_heart | scattercarbon_muscle) | scattercarbon_scale
ggsave(scattercarbonpanel, file = "scattercarbonpanel.jpg", width=24, height =8)
cor.test(liondata$scale_c, liondata$depth_capture_avg_m)           
cor.test(liondata$muscle_c, liondata$depth_capture_avg_m)     
cor.test(liondata$heart_c, liondata$depth_capture_avg_m)  



corr<-glm(SL_mm ~ depth_capture_avg_m, data = liondata)


reg.sl.depth<-lm(formula = SL_mm ~ depth_capture_avg_m, data=liondata)
summary(reg.sl.depth)
new.dat<-data.frame(depth_capture_avg_m = 100)
predict(reg.sl.depth, newdata = new.dat, interval = "confidence")
predict(reg.sl.depth, newdata = new.dat, interval = "prediction")
confint(reg.sl.depth)


#get intercept and slope value
coeff<-coefficients(reg.sl.depth)          
intercept<-coeff[1]
slope<- coeff[2]



sl.depth<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = SL_mm) +
  geom_point(size = 2, color = "#6C0BA9")  +
  ylab("Standard length (in mm)") + xlab("Depth (m)") + theme_classic() + geom_smooth(data = reg.sl.depth, method = lm, color = "black", fullrange = TRUE) #t = 5.3225, df = 74, p-value = 1.053e-06, r2 = 0.526

# add the regression line
sl.depth.fig<-sl.depth+geom_abline(intercept = intercept, slope = slope, color="#6C0BA9", size=1) +  theme(text = element_text(size = 18)) 

ggsave("sl.depth.jpg")

sl.depth<-ggplot(data = liondata) +
  aes(x = depth_capture_avg_m, y = SL_mm) +
  geom_point(size = 2, color = "#6C0BA9")  +
  ylab("Standard length (in mm)") + xlab("Depth (m)") + theme_classic() 


#if I make a scatter plot with the ellipses

lionlong1<-liondata%>%
  pivot_longer((cols = c("muscle_c", "muscle_n", "heart_c", "heart_n", "scale_c", "scale_n")) , names_to = c("tissue", "isotope"),
               names_sep = "_")
lionlong2<-lionlong1%>%
  pivot_wider(names_from = isotope, values_from = value)

write.csv(lionlong2, "lionlong2.csv")

  ellipsefig.depthcat<-ggplot(data = lionlong2) +
  aes(x = c, y = n, color = depth_categorical, shape = tissue) +
  #geom_point(size=2)  +
    stat_ellipse((aes( x = c, y = n, group =  depth_categorical)),position = "identity", level = 0.95, size = 1) +
    ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4"))

  ellipsefig.tissuecat<-ggplot(data = lionlong2) +
    aes(x = c, y = n, color = depth_categorical, shape = tissue) +
    geom_point(size=2)  +
    stat_ellipse((aes( x = c, y = n, group =  tissue)),position = "identity", level = 0.95, size = 1) +
    ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4"))

  scatter<-ggplot(data = lionlong2) +
    aes(x = c, y = n, color = depth_categorical, shape = tissue) +
    geom_point(size=2)  +
    ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4"))

ggsave("scatter.jpg")

scatter2<-ggplot(data = lionlong2) +
  aes(x = c, y = n, color = depth_categorical, shape = tissue) +
  geom_point(size=2)  +
  ylab("") + xlab("") + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4"))
  
  
  #on 4/23/23, editted this figure to meet reviewer requests
  m<-ggplot(data = figurevalues) +
    aes(x = meanC, y = meanN, color = depth, shape = tissue) +
    geom_point(size=4)  +
    geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 0.1) + 
    geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)))  +
    ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) 
  
  ggsave("isoplot_alone.jpg")
  
  m2<-ggplot(data = figurevalues) +
    aes(x = meanC, y = meanN, color = depth, shape = tissue) +
    geom_point(size=4)  +
    geom_errorbar(aes(ymin = (meanN-sdN),ymax = (meanN + sdN)), width = 0.1) + 
    geom_errorbarh(aes(xmin = (meanC-sdC),xmax = (meanC +sdC)))  +
    ylab(expression(δ^{15}*"N"*" (‰)")) + xlab("") + theme_classic() + theme(legend.position = "none", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) 
    
#combine isoplot and ellipse figure  
  combinedfig<- m + ellipsefig.depthcat
    
try2<- ggplot() +
   
  stat_ellipse((aes( x = lionlong2$c, y = lionlong2$n, group =  lionlong2$depth_categorical, color =lionlong2$depth_categorical)),position = "identity", level = 0.95, size = 1, linetype = 2) +  

    scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) +
  
   
    geom_point(aes(x = figurevalues$meanC, y = figurevalues$meanN, shape = figurevalues$tissue, color = figurevalues$depth), size = 4) +
   
   geom_errorbar(aes(x=figurevalues$meanC,  ymin = (figurevalues$meanN-figurevalues$sdN),ymax = (figurevalues$meanN + figurevalues$sdN), color = figurevalues$depth), width = 0.1) + 
   
   geom_errorbarh(aes(y =figurevalues$meanN, xmin = (figurevalues$meanC-figurevalues$sdC),xmax = (figurevalues$meanC +figurevalues$sdC), color=figurevalues$depth))  +
   
   ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) 

ggsave("isoplot_ellipse.jpg")



try3<- ggplot() +
  
  stat_ellipse((aes( x = lionlong2$c, y = lionlong2$n, group =  lionlong2$depth_categorical, color =lionlong2$depth_categorical)),position = "identity", level = 0.95, size = 1, linetype = 2) +  
  
  scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) +
  
  
  geom_point(aes(x = figurevalues$meanC, y = figurevalues$meanN, shape = figurevalues$tissue, color = figurevalues$depth), size = 4) +
  
  geom_errorbar(aes(x=figurevalues$meanC,  ymin = (figurevalues$meanN-figurevalues$sdN),ymax = (figurevalues$meanN + figurevalues$sdN), color = figurevalues$depth), width = 0.1) + 
  
  geom_errorbarh(aes(y =figurevalues$meanN, xmin = (figurevalues$meanC-figurevalues$sdC),xmax = (figurevalues$meanC +figurevalues$sdC), color=figurevalues$depth))  +
  
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) + scale_color_manual(values = c("deep" = "#2815d4", "shallow" = "#15c4d4")) 

ggsave("isoplot_ellipse.jpg")


library("gridExtra")
combinedfig<-ggarrange(m2, scatter2, nrow = 1, common.legend = TRUE, legend = "right")
combinedfig2<-annotate_figure(combinedfig,
                              bottom = text_grob((expression(δ^{13}*"C"*" (‰)"))))
ggsave("iso_scatter.jpg")




lionlong3<-read.csv("lionlong3.csv", header=TRUE)

try9<- ggplot() +
  
  stat_ellipse((aes( x = lionlong3$c, y = lionlong3$n, group =  lionlong3$tissuedepth)),position = "identity", level = 0.95, size = 1, linetype = 2, fill = lionlong3$tissuedepth) +  
  
  #scale_color_manual(values = c("deep_heart" = "#2815d4", "shallow_heart" = "#15c4d4"))   +
  
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) 






try10<- ggplot(lionlong3, aes(x = c, y = n, color = tissuedepth)) +
  stat_ellipse(geom = "polygon", aes(fill = tissuedepth),  alpha = 0.7, position = "identity", level = 0.95, size = 1) + 
  scale_color_manual(values = c("heart_deep" = "#2815d4", "heart_shallow" = "#15c4d4", "scale_deep" = "#2815d4", "scale_shallow" = "#15c4d4", "muscle_deep" = "#2815d4", "muscle_shallow" = "#15c4d4")) +
  scale_fill_manual(values = c("scale_deep" = "#B90A0A", "scale_shallow" = "#B90A0A", "heart_deep" = "#FFE000", "heart_shallow" = "#FFE000", "muscle_deep" = "#08B835", "muscle_shallow" = "#08B835")) +
  ylab(expression(δ^{15}*"N"*" (‰)")) + xlab(expression(δ^{13}*"C"*" (‰)")) + theme_classic() + theme(legend.position = "right", legend.title=element_blank()) 

#are the C13 vs depth data outliers_scale?
boxplot(liondata$scale_c)
quantile(liondata$scale_c)
IQR(liondata$scale_c)
#A data point on the high end of the distribution is an outlier if its value is > Q3 + 1.5 * IQR
# so, -13.985 + (1.5*0.7325) = -12.88625; the upper value is >, so it is a true outlier, value we have is -9.6
-13.985 + (1.5*0.7325) 
#A data point on the lower end of the distribution is an outlier if its value is < Q1 - 1.5 * IQR
# so, -14.7175 - (1.5*0.7325) = -15.81625; no outliers

#are the C13 vs depth data outliers_muscle?
boxplot(liondata$muscle_c)
quantile(liondata$muscle_c)
IQR(liondata$muscle_c)
#A data point on the high end of the distribution is an outlier if its value is > Q3 + 1.5 * IQR
#so, -16.4975 + (1.5*0.88875) = -15.16437; values we have are -12.140
-16.4975 + (1.5*0.88875)
#A data point on the lower end of the distribution is an outlier if its value is < Q1 - 1.5 * IQR
#so, -17.38625 - (1.5*0.88875) = -18.71937
-17.38625 - (1.5*0.88875)


#are the C13 vs depth data outliers_heart?
boxplot(liondata$heart_c)
quantile(liondata$heart_c)
IQR(liondata$heart_c)
#A data point on the high end of the distribution is an outlier if its value is > Q3 + 1.5 * IQR
#so, -17.005 + (1.5*0.915) = -15.6325; -13.46 is an outlier
-17.005 + (1.5*0.915)
#A data point on the lower end of the distribution is an outlier if its value is < Q1 - 1.5 * IQR
#so, -17.920 - (1.5*0.915) = -19.2925; -20.81 and -19.8 are outliers
-17.920 - (1.5*0.915)


var.test(liondata$heart_c, liondata$muscle_c, alternative = "two.sided")
bartlett.test(heart_c ~ depth_categorical, data = liondata)
bartlett.test(muscle_c ~ depth_categorical, data = liondata)
bartlett.test(scale_c ~ depth_categorical, data = liondata)



