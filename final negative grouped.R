#Final negative groups
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

#Form the negative count
care$negativect=rowSums(care[,c("iproxct", "iproxout","threatg", "bark", "headshake", "yawn", "aggb", "pace", "aggdisp", "teeth", "rubgen", "sway", "turn", "scream")], na.rm=T)
care$negativect

#Negative  count behaviors modelling
m1=gls(negativect~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(negativect~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a significant lower IAC score we wanna use that. 
#M2 AIC = 1731.47 It is significant P = 0,0141

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(negativect)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(negativect)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#This ad.test is also significant P=2.2*10^-16
summary(m2)

#Now we test for differences between condition

odel.matrix.gls <- function(m2, ...){
  
  model.matrix(terms(m2), data = getData(m2), ...) 
  
}

model.frame.gls <- function(m2, ...){
  
  model.frame(formula(m2), data = getData(m2), ...) 
  
}

terms.gls <- function(m2, ...){
  
  terms(model.frame(m2),...) 
  
}



multCompTukey <- glht(m2, linfct = mcp(cond.f = "Tukey"))

summary(multCompTukey)
#There are significante between 2-1, 3-1, 4-1. 
lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are significant difference between 1-2 P=0,0058, 1-3 P=0.0002, 1-4 P=0.0017.  

#We now normalize the data
care$l.negativect=log(care$negativect+1)
#Now we run same models with the normalised data
m1=gls(l.negativect~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.negativect~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2n)
anova(m1,m2n)
# Model 2 is still significant different. 
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.negativect)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.negativect)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))#
summary(m2n)
#Now we test for differences between condition

odel.matrix.gls <- function(m2n, ...){
  
  model.matrix(terms(m2n), data = getData(m2n), ...) 
  
}

model.frame.gls <- function(m2n, ...){
  
  model.frame(formula(m2n), data = getData(m2n), ...) 
  
}

terms.gls <- function(m2n, ...){
  
  terms(model.frame(m2n),...) 
  
}



multCompTukey <- glht(m2n, linfct = mcp(cond.f = "Tukey"))

summary(multCompTukey)
#There are significant results between 1-2, 1-3, 1-4,  
lsmeans(m2n,pairwise~cond.f)
#With this one there are 3 significant results.Same as before. 
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.negativect~id, data=care)
#P is not significant so alternate variance structure is not needed. 

#Now we see if alternate variant structures help
#Now we run the alternate variance structures for the normalised model
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
#Now using these on the normalized model
#alternate models
M2n=lme(l.negativect~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")

M2.1<-lme(l.negativect~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.negativect~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
#checking which model is best
anova(M2.2,M2.1)
#Since the normalised model without the alternate structure is better we just finally check for autocorrelation.

#Plotting x with the model
x<-care$l.negativect[!is.na(care$l.negativect)]#removes na values from column
E2<-residuals(M2n,type="normalized")
plot(x, E2)
#residuals v observation 

#check for autocorrelation
acf(E2, na.action=na.pass,
    main="Auto-correlation plot for residuals")
#There are no autocorrelation. 