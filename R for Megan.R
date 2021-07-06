#Welcome to your R Script, Megan!

#First we will make a project on Github and link it your Git

#R is finicky and can be annoying, but gets easier with time, particulary easier when you know what terms to google... #Nevertheless, she persisted...
#R is finicky and can be annoying, but gets easier with time, particulary easier when you know what terms too google... #Nevertheless, she persisted...

#In R, you'll need to download packages to use certain functions. Once the packages are downloaded you call them in with the library function. Look below for the names of packages <library(package)> and search for them using the install button on the right corner packages tab.

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


#lets visualize our data by making a figure using the new dataframe we just created
#Megan try followingmodifying the code below as well as taking direction from this website: https://gist.github.com/AndrewLJackson/b16fb216d8a9a96c20a4a979ec97e7b0
m<-ggplot(data = descriptivestats, aes(x = meanmuscleC, y = meanmuscleN, color = depth_categorical)) +
  geom_point(size=4, shape = 0)  +
  geom_errorbar(aes(ymin = (meanmuscleN-sdmuscleN),ymax = (meanmuscleN + sdmuscleN))) + 
  geom_errorbarh(aes(xmin = (meanmuscleC-sdmuscleC),xmax = (meanmuscleC +sdmuscleC)))  +theme_classic()

s<- ggplot(data = descriptivestats, aes(x = meanscaleC, y = meanscaleN, color = depth_categorical)) +
  geom_point(size=4, shape = 1)  +
  geom_errorbar(aes(ymin = (meanscaleN-sdscaleN),ymax = (meanscaleN + sdscaleN))) + 
  geom_errorbarh(aes(xmin = (meanscaleC-sdscaleC),xmax = (meanscaleC +sdscaleC)))  +theme_classic()

h <-  

plot_grid(m, s)
print(msh)  
  
  #you will copy and paste all the above code and do it with scale and heart, too, and change the color. you can choose colors by searching hex color pallete on google and entering the numeric hex code in "". 

#The most critical thing to do before any modeling is to check if our data are normally distributed. We can visualiz the raw data, and for best practices, we then visualize the residuals of the data

#CARBON

hist(liondata$muscle_c)
plotResiduals(liondata$muscle_c)
#lets check out the distribution of muscle carbon
normalitymuscle_c<-glm(muscle_c~depth_categorical, data = liondata)
resmuscle_c<-simulateResiduals(fittedModel = normalitymuscle_c, n = 250)
plot(resmuscle_c)
resmuscle_c$scaledResiduals
testUniformity(resmuscle_c)

hist(liondata$scale_c)
plotResiduals(liondata$scale_c)
#lets check out the distribution of scale carbon
normalityscale_c<-glm(scale_c~depth_categorical, data = liondata)
resscale_c<-simulateResiduals(fittedModel = normalityscale_c, n = 250)
plot(resscale_c)
resscale_c$scaledResiduals
testUniformity(resscale_c)

hist(liondata$heart_c)
plotResiduals(liondata$heart_c)
#lets check out the distribution of heart carbon
normalityheart_c<-glm(heart_c~depth_categorical, data = liondata)
resheart_c<-simulateResiduals(fittedModel = normalityheart_c, n = 250)
plot(resheart_c)
resheart_c$scaledResiduals
testUniformity(resheart_c)

#NITROGEN

hist(liondata$muscle_n)
plotResiduals(liondata$muscle_n)
#lets check out the distribution of muscle nitrogen
normalitymuscle_n<-glm(muscle_n~depth_categorical, data = liondata)
resmuscle_n<-simulateResiduals(fittedModel = normalitymuscle_n, n = 250)
plot(resmuscle_n)
resmuscle_n$scaledResiduals
testUniformity(resmuscle_n)

hist(liondata$scale_n)
plotResiduals(liondata$scale_n)
#lets check out the distribution of scale nitrogen
normalityscale_n<-glm(scale_n~depth_categorical, data = liondata)
resscale_n<-simulateResiduals(fittedModel = normalityscale_n, n = 250)
plot(resscale_n)
resscale_n$scaledResiduals
testUniformity(resscale_n)

hist(liondata$heart_n)
plotResiduals(liondata$heart_n)
#lets check out the distribution of heart nitrogen
normalityheart_n<-glm(heart_n~depth_categorical, data = liondata)
resheart_n<-simulateResiduals(fittedModel = normalityheart_n, n = 250)
plot(resheart_n)
resheart_n$scaledResiduals
testUniformity(resheart_n)

#How does depth influence the diet of lionfish (using muscle tissue)? This format is response variable ~ indp factor + indp factor + indfactor interacts with other indp factor. We are using SL.mm*depth.categorical bc its likely at shallow depths there is more culling and so we might only find smaller/younger lionfish.  Repeat this formatting for scale and heart. 

mcresults<-lm(muscle_c ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(mcresults)
mnresults<-lm(muscle_n ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(mnresults)

scresults<-lm(scale_c ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(scresults)
snresults<-lm(scale_n ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(snresults)

hcresults<-lm(heart_c ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(hcresults)
hnresults<-lm(heart_n ~ SL_mm + depth_categorical + SL_mm*depth_categorical, data = liondata)
summary(hnresults)
