#Welcome to your R Script, Megan!

#First we will make a project on Github and link it your Git

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

#Okay, lets read in the files and packages we will use for data analysis. If the files is stored in your R project, all you need is a read.csv command! below you will see I am calling in the csv called lionfish data and from here out I'm calling this datasheet/dataframe "lion"
lion<-read.csv("lionfishdata.csv", header=TRUE)

#check that the factors are properply called numeric or charachter; if they're not correct we can fix that.
#lets just start with some descriptive statistics (mean, standard deviation, standard errror)

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

#The "tidyverse" is one of the most succinct and user friendly ways to summarise and organize data. You can give if instruction do this, then that, by using that pipe feature which is denoted via %>%  


lionstats<-lion%>%
  group_by(depth, na.rm = TRUE)%>% 
  summarize(
    meanmuscleC = mean(muscle_c,na.rm=TRUE),
    sdmuscleC = sd(muscle_c, na.rm = TRUE),
    
    #repeat this structure for the other tissue types and nitrogen and end with a closed parenthesis
    
  )

#lets visualize our data by making a figure using the new dataframe we just created
trophicfig<-ggplot(data = lionstats, aes(x = muscle_c,y = muscle_n)) + 
  #note x is carbon and y = nitrogen, just like isoscapes, which we are in the process of making here
  geom_point(color = "#00529c") + 
  geom_errorbar(aes(ymin = sdmuscleN,ymax = sdmuscleN)) + 
  geom_errorbarh(aes(xmin = sdmuscleC,xmax = sdmuscleC)) + 
  #you will copy and paste all the above code and do it with scale and heart, too, and change the color. you can choose colors by searching hex color pallete on google and entering the numeric hex code in "". 

#The most critical thing to do before any modeling is to check if our data are normally distributed. We can visualiz the raw data, and for best practices, we then visualize the residuals of the data

histogram(lion$muscle_c)
plotResiduals(lion$muscle_c)
#lets check out the distribution of muscle carbon
normalitymuscle_c<-glm(muscle_c~depth, data = lion)
resmuscle_c<-simulateResiduals(fittedModel = normalitymuscle_c, n = 250)
plot(resmuscle_c)
resmuscle_c$scaledResiduals
testUniformity(resmuscle_c)



#Are the data spatially autocorrelated?
#spatial checks for glutamic acid, this would be the same as phe or tp, since all these values are from 1 fish from the same site; One fish didn't have a lat/long, so I removed it to run the check using a unique dataframe for the check
#the Moran's I test cannot handle NAs, so first I'm removing all NAs from this 'mock' dataset
allsppspatial<- allspp %>%
  filter(!is.na(latjitt)) %>%
  filter(!is.na(longjitt))%>%
  filter(!is.na(tp)) %>%
  filter(!is.na(year))

spatial.allspp<-glm(tp~year, data=allsppspatial)
simspatial.allspp<-simulateResiduals(fittedModel = spatial.allspp)
spatialtest.allspp<-testSpatialAutocorrelation(simulationOutput = simspatial.allspp,  x = allsppspatial$longjitt, y = allsppspatial$latjitt)
spatialtest.allspp #Yay, no spatial corr


#temporal checks for glutamic acid, his would be the same as phe or tp, since all these values are from 1 fish from the same site

allspp$timegroup<-allspp$year
temporal.glu<-glm(glu~year, data= allspp)
temporaltest.glu<-dwtest(temporal.glu, order.by = NULL, alternative = "two.sided", exact = FALSE, tol = 1e-10) 
temporaltest.glu #yay, no temporal corr
dwtest(temporal.glu)

#Analyses with all species in one model, no lag time of temp
#does time influence glu?
library(lmerTest)
gluint<-glmmTMB(glu~ year  + hostsp +  (1|site)  , family = "gaussian", data= allspp)
summary(gluint) #AIC 781.5; 749
glu<-glmmTMB(glu~ year + sl + (1|hostsp)  + (1|site) , family = "gaussian", data= allspp)
summary(glu) #AIC 748
glulmer<-lmer(glu~ year  + hostsp +  (1|site), data= allspp)
summary(glulmer)
#does time influence phe?
pheint<-glmmTMB(phe~ year + sl + hostsp  + (1|site) + lag3_temp, family = "gaussian", data= allspp)
summary(pheint) #AIC 811;779
phe<-glmmTMB(phe~ year  + (1|hostid)  + hostsp , family = "gaussian", data= allspp)
summary(phe) #806;776


#does time influence diff?
diffint<-glmmTMB(diff~ year  + sl + (1|site) , family = "gaussian", data= allspp)
summary(diffint) #AIC 857
diff<-glmmTMB(diff~ year + sl + hostsp  + (1|site), family = "gaussian", data= allspp)
summary(diff) #853

#does time influence tp?
tpint<-glmmTMB(tp~ year + hostsp + sl   +(1|site) , family = "gaussian", data= allspp)
summary(tpint) #AIC  18
tp<-glmmTMB(diff~ year + sl + hostsp  + (1|site), family = "gaussian", data= allspp)
summary(tp) 

ab<-glht(tpint, linfct = mcp(hostsp = "Tukey"))
summary(ab)

#Analyses for only Sole

sole_only<-allspp%>%
  filter(hostsp == "sole")
library(predictmeans)
normalityglu<-glm(glu~year, data = sole_only)
ressole<-simulateResiduals(fittedModel = normalityglu, n = 250)
ressole$scaledResiduals
plot(ressole)
testUniformity(ressole) #glu is normally distributed
glusole<-glmmTMB(glu~ scale(year) + scale(sl) +(1|site)  , family = "gaussian", data= sole_only)
summary(glusole)
glusolelmer<-lmer(glu~ year + sl +(1|site), data= sole_only)
summary(glusolelmer)
library(visreg)
a<-visreg(glusolelmer, "year")
view (a)
normalityphe<-glm(phe~year, data = sole_only)
ressole<-simulateResiduals(fittedModel = normalityphe, n = 250)
ressole$scaledResiduals
plot(ressole)
testUniformity(ressole)#phe is normally distrbuted
phesole<-glmmTMB(phe~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= sole_only)
summary(phesole)


normalitytp<-glm(tp~year, data = sole_only)
ressole<-simulateResiduals(fittedModel = normalitytp, n = 250)
ressole$scaledResiduals
plot(ressole)
testUniformity(ressole) #tp is normally distributed
tpsole<-glmmTMB(tp~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= sole_only)
summary(tpsole)

diffsole<-glmmTMB(diff~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= sole_only)
summary(diffsole)


#Analyses for only hake
hake_only<-allspp%>%
  filter(hostsp == "hake")

normalityglu<-glm(glu~year, data = hake_only)
reshake<-simulateResiduals(fittedModel = normalityglu, n = 250)
reshake$scaledResiduals
plot(reshake)
testUniformity(reshake) #normal :)
gluhake<-glmmTMB(glu~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= hake_only)
summary(gluhake)


normalityphe<-glm(phe~year, data = hake_only)
reshake<-simulateResiduals(fittedModel = normalityphe, n = 250)
reshake$scaledResiduals
plot(reshake)
testUniformity(reshake) #normal :) 
phehake<-glmmTMB(phe~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= hake_only)
summary(phehake)


normalitytp<-glm(tp~year, data = hake_only)
reshake<-simulateResiduals(fittedModel = normalitytp, n = 250)
reshake$scaledResiduals
plot(reshake)
testUniformity(reshake) #normal
tphake<-glmmTMB(tp~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= hake_only)
summary(tphake)

diffhake<-glmmTMB(diff~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= hake_only)
summary(diffhake)

#Analyses for only pollock
pollock_only<-allspp%>%
  filter(hostsp == "pollock")

normalityglu<-glm(glu~year, data = pollock_only)
respollock<-simulateResiduals(fittedModel = normalityglu, n = 250)
respollock$scaledResiduals
plot(respollock)
testUniformity(respollock) #normal
glupollock<-glmmTMB(glu~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= pollock_only)
summary(glupollock)


normalityphe<-glm(phe~year, data = pollock_only)
respollock<-simulateResiduals(fittedModel = normalityphe, n = 250)
respollock$scaledResiduals
plot(respollock)
testUniformity(respollock)#normal
phepollock<-glmmTMB(phe~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= pollock_only)
summary(phepollock)


normalitytp<-glm(tp~year, data = pollock_only)
respollock<-simulateResiduals(fittedModel = normalitytp, n = 250)
respollock$scaledResiduals
plot(respollock)
testUniformity(respollock)
tppollock<-glmmTMB(tp~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= pollock_only)
summary(tppollock)

diffpollock<-glmmTMB(diff~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= pollock_only)
summary(diffpollock)

#Analyses for only herring
herring_only<-allspp%>%
  filter(hostsp == "herring")

normalityglu<-glm(glu~year, data = herring_only)
resherring<-simulateResiduals(fittedModel = normalityglu, n = 250)
resherring$scaledResiduals
plot(resherring)
testUniformity(resherring) #normal
gluherring<-glmmTMB(glu~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= herring_only)
summary(gluherring)


normalityphe<-glm(phe~year, data = herring_only)
resherring<-simulateResiduals(fittedModel = normalityphe, n = 250)
resherring$scaledResiduals
plot(resherring)
testUniformity(resherring) #normal
pheherring<-glmmTMB(phe~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= herring_only)
summary(pheherring)


normalitytp<-glm(phe~year, data = herring_only)
resherring<-simulateResiduals(fittedModel = normalitytp, n = 250)
resherring$scaledResiduals
plot(resherring)
testUniformity(resherring) #normal
tpherring<-glmmTMB(tp~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= herring_only)
summary(tpherring)

diffherring<-glmmTMB(diff~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= herring_only)
summary(diffherring)

#Analyses for only rock
rock_only<-allspp%>%
  filter(hostsp == "rock")

normalityglu<-glm(glu~year, data = rock_only)
resrock<-simulateResiduals(fittedModel = normalityglu, n = 250)
resrock$scaledResiduals
plot(resrock)
testUniformity(resrock) #normal
glurock<-glmmTMB(glu~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= rock_only)
summary(glurock)


normalityphe<-glm(phe~year, data = rock_only)
resrock<-simulateResiduals(fittedModel = normalityphe, n = 250)
resrock$scaledResiduals
plot(resrock)
testUniformity(resrock) #normal
pherock<-glmmTMB(phe~ scale(year) + scale(sl) +(1|site) , family = "gaussian", data= rock_only)
summary(pherock)



normalitytp<-glm(tp~year, data = rock_only)
resrock<-simulateResiduals(fittedModel = normalitytp, n = 250)
resrock$scaledResiduals
plot(resrock)
testUniformity(resrock) 
tprock<-glmmTMB(tp~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= rock_only)
summary(tprock)
#plot residuals for year to see if there are non-linear trends; look at GAMS
diffrock<-glmmTMB(diff~ scale(year) + scale(sl) +(1|site), family = "gaussian", data= rock_only)
summary(diffrock)

#make the tpfigure for publication
predict(tprock, rock_only, allow.new.levels=TRUE)
ndtp<-rock_only[1,]
ndtp$year<-"new"
ndtp_pop<-data.frame(year=rock_only$year, site=NA, sl = 180) #median = sl
tppredict<-predict(tprock, newdata=ndtp_pop, se.fit=TRUE)
as.data.frame(tppredict)
tppredict$year<-rock_only$year
tppredict<-as.data.frame(tppredict)

tplot1rock<- ggplot() + geom_line(data =tppredict, aes(x = year, y = fit)) +
  geom_ribbon(data = tppredict, aes(x = year, ymin = fit-se.fit, ymax = fit+se.fit), fill="#00529c", alpha=0.3) + geom_point(data = rock_only, aes(x = year, y = tp), color="#00529c") +  xlab("Year collected") + ylab("tp") +theme_classic()

#make figure for glu
predict(glurock, rock_only, allow.new.levels=TRUE)
ndglu<-rock_only[1,]
ndglu$year<-"new"
ndglu_pop<-data.frame(year=rock_only$year, site=NA, sl = 180) #random effects are set to NA, other effects I chose median of climate and mean for sl
glupredict<-predict(glurock, newdata=ndglu_pop, se.fit=TRUE)
as.data.frame(glupredict)
glupredict$year<-rock_only$year
glupredict<-as.data.frame(glupredict)

gluplot1rock<- ggplot() + geom_line(data =glupredict, aes(x = year, y = fit)) +
  geom_ribbon(data = glupredict, aes(x = year, ymin = fit-se.fit, ymax = fit+se.fit), fill="#00529c", alpha=0.3) + geom_point(data = rock_only, aes(x = year, y = glu), color="#00529c") +  xlab("Year collected") + ylab("glu") +theme_classic()

#make figure for phe
predict(pherock, rock_only, allow.new.levels=TRUE)
ndphe<-rock_only[1,]
ndphe$year<-"new"
ndphe_pop<-data.frame(year=rock_only$year, site=NA, sl = 180) #random effects are set to NA, other effects I chose median of climate and mean for sl
phepredict<-predict(pherock, newdata=ndphe_pop, se.fit=TRUE)
as.data.frame(phepredict)
phepredict$year<-rock_only$year
phepredict<-as.data.frame(phepredict)

pheplot1rock<- ggplot() + geom_line(data =phepredict, aes(x = year, y = fit)) +
  geom_ribbon(data = phepredict, aes(x = year, ymin = fit-se.fit, ymax = fit+se.fit), fill="#00529c", alpha=0.3) + geom_point(data = rock_only, aes(x = year, y = phe), color="#00529c") +  xlab("Year collected") + ylab("phe") +theme_classic()ppppppp

#make figure for diff rock
predict(diffrock, rock_only, allow.new.levels=TRUE)
nddiff<-rock_only[1,]
nddiff$year<-"new"
nddiff_pop<-data.frame(year=rock_only$year, site=NA, sl = 180) #random effects are set to NA, other effects I chose median of climate and mean for sl
diffpredict<-predict(diffrock, newdata=nddiff_pop, se.fit=TRUE)
as.data.frame(diffpredict)
diffpredict$year<-rock_only$year
diffpredict<-as.data.frame(diffpredict)

diffplot1rock<- ggplot() + geom_line(data =diffpredict, aes(x = year, y = fit)) +
  geom_ribbon(data = diffpredict, aes(x = year, ymin = fit-se.fit, ymax = fit+se.fit), fill="#00529c", alpha=0.3) + geom_point(data = rock_only, aes(x = year, y = diff), color="#00529c") +  xlab("Year collected") + ylab("diff") +theme_classic()

fig_rock_predictandraw<-plot_grid(gluplot1rock, pheplot1rock, diffplot1rock, tplot1rock)
ggsave("rockfig.jpg")
