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
lion<-read.csv("0702-lionfishdata.csv", header=TRUE)
<<<<<<< HEAD
#could you please go back to the original datasheet you just read in and remove any extra spaces inthe variable names. Then remove the extra periods thruout the sheet. Then read in the file again.
=======
#could you please go back to the original datasheet you just read in and remove any extra spaces inthe variable names. Then remove the extra periods thruout the sheet.
>>>>>>> 356da277b9c35867a5d8c57ae9acd0abb520cd20
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
    sdsamlmonN= sd(muscle_n, na.rm = TRUE))
  
#For our analyses we don't want to include salmon standards or duplicates, so we are going to remove them from the dataframe, and then proceed for the mean and sd for M, S, H, G for C and N
liondata<-lion %>% 
  filter(calibration == "single" |calibration == "average")

descriptivestats<-liondata %>% 
  group_by(depth.categorical.)%>% 
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
    sdheartB = sd(heart_n, na.rm = TRUE))


#lets visualize our data by making a figure using the new dataframe we just created
#Megan try followingmodifying the code below as well as taking direction from this website: https://gist.github.com/AndrewLJackson/b16fb216d8a9a96c20a4a979ec97e7b0
m<-ggplot(data = descriptivestats, aes(x = meanmuscleC,y = meanmuscleN), color = depth.categorical.) + 
  #note x is carbon and y = nitrogen, just like isoscapes, which we are in the process of making here
  geom_point(color = "#00529c") + 
  geom_errorbar(aes(ymin = (meanmuscleN-sdmuscleN),ymax = (meanmuscleN + sdmuscleN))) + 
  geom_errorbarh(aes(xmin = (meanmuscleC-sdmuscleC),xmax = (meanmuscleC +sdmuscleC))) 

ms<- musclefig +
  
  
  
msh<- ms +   
  
  #you will copy and paste all the above code and do it with scale and heart, too, and change the color. you can choose colors by searching hex color pallete on google and entering the numeric hex code in "". 

#The most critical thing to do before any modeling is to check if our data are normally distributed. We can visualiz the raw data, and for best practices, we then visualize the residuals of the data

hist(liondata$muscle_c)
plotResiduals(liondata$muscle_c)
#lets check out the distribution of muscle carbon
normalitymuscle_c<-glm(muscle_c~depth.categorical., data = liondata)
resmuscle_c<-simulateResiduals(fittedModel = normalitymuscle_c, n = 250)
plot(resmuscle_c)
resmuscle_c$scaledResiduals
testUniformity(resmuscle_c)

#muscle_c is normal (Kolmogorov-Smirnov, p=0.1368)

#How does depth influence the diet of lionfish (using muscle tissue)? This format is response variable ~ indp factor + indp factor + indfactor interacts with other indp factor. We are using SL.mm*depth.categorical bc its likely at shallow depths there is more culling and so we might only find smaller/younger lionfish.  Repeat this formatting for scale and heart. 

mcresults<-lm(muscle_c ~ SL..mm. + depth.categorical. + SL..mm.*depth.categorical., data = liondata)
summary(mcresults)
mnresults<-lm(muscle_n ~ SL..mm. + depth.categorical. + SL..mm.*depth.categorical., data = liondata)
summary(mnresults)


