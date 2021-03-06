#Final pacing
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

#pace modelling
m1=gls(pace~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(pace~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant but we use m2.  
#M2 AIC = 1315,91

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(pace)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(pace)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))
summary(m2)
#Normalizing the model 
care$l.pace=log(care$pace+1)

#Now we run same models with the normalised data
m1n=gls(l.pace~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1n)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.pace~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2n)
anova(m1n,m2n)
#Model 2 is significant better with a lower AIC score
#Model M2 AIC=375,21 P=0.0441
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.pace)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.pace)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))
summary(m2)
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.pace~id, data=care)
#P is significant so alternate variance structure is used.
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
#Now we run the alternate variance structures for the normalised model
m2n=lme(l.pace~cond.f, random=~1|id,data=care, na.action=na.omit)

M2.1<-lme(l.pace~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.pace~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
anova(m2n,M2.1, M2.2)
#M2.1 has the lowest AIC score so we continue with that. 
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(M2.1, add.smooth=FALSE, which=1)
E=resid(M2.1)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.pace)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.pace)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(M2.1))
qqline(residuals(M2.1))
ad.test(residuals(M2.1))
summary(M2.1)
#Now we test for differences between condition
odel.matrix.gls <- function(M2.1, ...){
  
  model.matrix(terms(M2.1), data = getData(M2.1), ...) 
  
}

model.frame.gls <- function(M2.1, ...){
  
  model.frame(formula(M2.1), data = getData(M2.1), ...) 
  
}

terms.gls <- function(M2.1, ...){
  
  terms(model.frame(M2.1),...) 
  
}



multCompTukey <- glht(M2.1, linfct = mcp(cond.f = "Tukey"))

summary(multCompTukey)
#There are significant differences between cond 1-2,1-3, 1-4. So there are most pacing in cond 1. 
lsmeans(M2.1,pairwise~cond.f)

#Plotting x with the model
x<-care$l.pace[!is.na(care$l.pace)]#removes na values from column
E2<-residuals(M2.1,type="normalized")
plot(x, E2)
#residuals v observation 

#check for autocorrelation
acf(E2, na.action=na.pass,
    main="Auto-correlation plot for residuals")
#There are no autocorrelation.
