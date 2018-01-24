#Final embrace baboon
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

#Embrace baboons modelling
m1=gls(embraceb~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(embraceb~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant but M2 has a lower AIC score so we use that. 

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(embraceb)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(embraceb)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#This ad.test is also significant P=2.2*10^-16
summary(m2)
#Now we normalize the model
care$l.embraceb=log(care$embraceb+1)

#Now we run models models with the normalised data
m1n=gls(l.embraceb~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.embraceb~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Plotting residuals for normalized model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.embraceb)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.embraceb)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))#This ad.test is also significant P=2.2*10^-16
summary(m2n)
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.embraceb~id, data=care)
#P is significant so we run alternate variance structure
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
#Now used on the normalized model
m2n=lme(l.embraceb~cond.f, random=~1|id,data=care, na.action=na.omit)

M2.1<-lme(l.embraceb~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.embraceb~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
#Now checking which model is best
anova(m2n,M2.1,M2.2)
#M2.2 is significantly better to use.
#Plotting residuals for normalized model M2.2
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(M2.2, add.smooth=FALSE, which=1)
E=resid(M2.2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(embraceb)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(embraceb)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))
summary(M2.2)
#Now we check for significant results. 
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
#There are significant differences between cond 1-3, and 1-4 there being more embraces in cond 3 and 4 than 1. 
lsmeans(M2.2,pairwise~cond.f)
#Now we check for autocorrelation 
#Plotting x with the model
x<-care$l.embraceb[!is.na(care$embraceb)]#removes na values from column
E2<-residuals(M2.2,type="normalized")
plot(x, E2)
#residuals v observation 

#check for autocorrelation
acf(E2, na.action=na.pass,
    main="Auto-correlation plot for residuals")
#There are no autocorrelation. 