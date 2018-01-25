#Negative grouped independent variables test
#load data
care=read.table(file="caredata.csv", header=TRUE, sep=",")
names(care)
str(care)

#Installed Packages
install.packages("lme4")
install.packages("geepack")
install.packages("lsmeans")
install.packages("dplyr")
install.packages("MuMIn")
install.packages("ggplot2")
install.packages("nortest")
install.packages("multcomp")
install.packages("Matrix")

#Library to run 
library(nlme)
library(dplyr)
#packages gotten from last years project
library(lattice)
library(lme4)
library(geepack)
library(lsmeans)
library(dplyr)
library(MuMIn)
library(ggplot2)
library(nortest)
library(multcomp)

#Putting the 4 condition in as a factor
care$cond.f=as.factor(care$cond)
#Putting the 3 focals in as a factor
care$focal.f=as.factor(care$focal)
#Putting totalday in as condition
care$tday.f=as.factor(care$tday)
#Putting the day of condition in as a factor
care$dayofcond.f=as.factor(care$dayofcond)
#Putting the number of caretakers in as a factor
care$numct.f=as.factor(care$numct)
str(care)

#Form the negative count
care$negativect=rowSums(care[,c("iproxct", "iproxout","threatg", "bark", "headshake", "yawn", "aggb", "pace", "aggdisp", "teeth", "rubgen", "sway", "turn", "scream")], na.rm=T)
care$negativect
################################################################################################
#Playing with models. 
m2=lme(negativect~cond.f+dayofcond.f+observer+focal.f+weather+ctpos+ctenrich+enrich+timetofeed+clean+xtraint+pxtraint+sep, random=~1|id,data=care, na.action=na.omit)
summary(m2)
m2=lme(negativect~tday.f+numct.f, random=~1|id,data=care, na.action=na.omit)
#so far following independent factor does not wanna work:
#tday.f,numct.f,sexct,ctclean,ctint,timetoenrich,timetoclean
#Dropping factors- Not really sure were to go from here. 
drop1(m2, test="Chi")
m2=lme(negativect~cond.f+observer+focal.f+weather+ctpos+ctenrich+enrich+timetofeed+clean+xtraint+pxtraint+sep, random=~1|id,data=care, na.action=na.omit)
summary(m2)




###########################################################################################
install.packages("leaps")
library(leaps)
m2=regsubsets(negativect~cond.f+dayofcond.f+observer+focal.f+weather+ctpos+ctenrich+enrich+timetofeed+clean+xtraint+pxtraint+sep,data=care, na.action=na.omit,nbest=4, really.big = T)
plot(m2,scale="adjr2")
plot(m2,scale="bic")
