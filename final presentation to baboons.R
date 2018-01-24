#Final present to baboons

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

#Presentations to baboons modelling
m1=gls(pbaboon~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(pbaboon~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a significant lower IAC score we wanna use that. 
#m2 AIC = 837.75 P=8*10^-4
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(pbaboon)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(pbaboon)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))
summary(m2)
#We now normalize the model 
#Normalizing
care$l.pbaboon=log(care$pbaboon+1)
#Now we run same models with the normalised data
m1=gls(l.pbaboon~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.pbaboon~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2n)
anova(m1,m2n)
#Normalised model 2 is still better
#plotting the normalised models residuals
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.pbaboon)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.pbaboon)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.pbaboon~id, data=care)
#P is significant so alternate variance structure is used.
#Now we run the alternate variance structures for the normalised model
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
#Now using these on the normalized model
#alternate models
M2n=lme(l.pbaboon~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")

M2.1<-lme(l.pbaboon~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.pbaboon~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
#checking which model is best
anova(M2.2,M2.1)
summary(M2n)
#The model M2.1 has the lower AIC score So we move forward with that one.
#plotting the normalised models M2.1 residuals
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(M2.1, add.smooth=FALSE, which=1)
E=resid(M2.1)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.pbaboon)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.pbaboon)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(M2.1))
qqline(residuals(M2.1))
ad.test(residuals(M2.1))
#Now we test if there are differences between conditions. 
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
#There are no significant difference with this model M2.1 
#Least amount in cond 2,3,4 than 1.  
lsmeans(M2.1,pairwise~cond.f) #This does not work.
#Since there is no significant result we do not move forward with autocorrelation. 
