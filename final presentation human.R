#Final present to humans
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
str(care)

#Presentations to humans modelling
m1=gls(phuman~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(phuman~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a significant lower IAC score we use that. 
#Model M2 AIC = 575.12 P=0.001

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(phuman)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(phuman)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#This ad.test is also significant P=2.2*10^-16
summary(m2)
#Now we normalize the model
#Normalizing
care$l.phuman=log(care$phuman+1)

#Now we run same models with the normalised data
m1=gls(l.phuman~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.phuman~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2n)
anova(m1,m2n)#Model 2 is significant 
#Plotting residuals for m2n to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.phuman)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.phuman)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))#This ad.test is also significant P=2.2*10^-16
summary(m2n)
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.phuman~id, data=care)
#P is significant so alternate variance structure is used.
#Now we run the alternate variance structures on the normalized model
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
m2n=lme(l.phuman~cond.f, random=~1|id,data=care, na.action=na.omit)

M2.1<-lme(l.phuman~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.phuman~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
#Checking with one is better only M2.1 works.
anova(m2n,M2.2)
#M2.2 is better with lower AIC score. 
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(M2.2, add.smooth=FALSE, which=1)
E=resid(M2.2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.phuman)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.phuman)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))#This ad.test is also significant P=2.2*10^-16
summary(M2.2)
#Now we look for significance in results for model M2.2
odel.matrix.gls <- function(M2.2, ...){
  
  model.matrix(terms(M2.2), data = getData(M2.2), ...) 
  
}

model.frame.gls <- function(M2.2, ...){
  
  model.frame(formula(M2.2), data = getData(M2.2), ...) 
  
}

terms.gls <- function(M2.2, ...){
  
  terms(model.frame(M2.2),...) 
  
}



multCompTukey <- glht(M2.2, linfct = mcp(cond.f = "Tukey"))

summary(multCompTukey)
#There are no significant difference with this model M2.2 
#Highest amount in cond 2,3,4 than 1.  
lsmeans(M2.2,pairwise~cond.f)
#Also no significane here.Therefore we do not move forward with autocorrelation.  