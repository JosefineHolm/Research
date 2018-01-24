#Final Bark 
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

#Bark modelling
m1=gls(bark~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(bark~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant but we use m2.  
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(bark)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(bark)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#This ad.test is also significant P=2.2*10^-16
summary(m2)
#Now we normalize the model 
care$l.bark=log(care$bark+1)
#Now we run same models with the normalised data
m1n=gls(l.bark~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.bark~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2n)
anova(m1n,m2n)
#Still no difference so we continue with m2n. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.bark)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.bark)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))#This ad.test is also significant P=2.2*10^-16
summary(m2n)
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.bark~id, data=care)
#P is significant so alternate variance structure is used.
#Now we run the alternate variance structures for the normalised model
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
#Now using these on the normalized model
m2n=lme(l.bark~cond.f, random=~1|id,data=care, na.action=na.omit)

M2.1<-lme(l.bark~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.bark~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
#None of these alternate variant structures work so we continue with the normalized model.

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
#Plotting x with the model
x<-care$l.bark[!is.na(care$l.bark)]#removes na values from column
E2<-residuals(m2n,type="normalized")
plot(x, E2)
#residuals v observation 

#check for autocorrelation
acf(E2, na.action=na.pass,
    main="Auto-correlation plot for residuals")
#There are no autocorrelation. 
