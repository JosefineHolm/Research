#Final decrease proximity caretaker
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

#Decrease proximity to caretaker modelling
m1=gls(dproxct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(dproxct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant but we use m2.  

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(dproxct)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(dproxct)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))
summary(m2)
#Now we normalize the model
#Normalizing
care$l.dproxct=log(care$dproxct+1)

#Now we run same models with the normalised data
m1n=gls(l.dproxct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1n)

#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.dproxct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2n)
anova(m1n,m2n)
#The models are not significant different so we chose m2n. 
#Plotting residuals for m2n to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2n, add.smooth=FALSE, which=1)
E=resid(m2n)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.dproxct)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.dproxct)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2n))
qqline(residuals(m2n))
ad.test(residuals(m2n))
summary(m2n)

#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.dproxct~id, data=care)
#P is significant so we run alternate variance structure
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
#We now use these on the model
m2n=lme(l.dproxct~cond.f, random=~1|id,data=care, na.action=na.omit)

M2.1<-lme(l.dproxct~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.dproxct~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
anova(m2n,M2.1,M2.2)
#Model 2 has a lower AIC score so we continue with that
#Plotting residuals for M2.2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(M2.2, add.smooth=FALSE, which=1)
E=resid(M2.2)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.dproxct)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.dproxct)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))
summary(M2.2)
#Now checking for significant results
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
#There are significant differences between cond 1-4, and 2-4, 3-4 - so there are more in cond 4 than 1,2,3.
lsmeans(M2.2,pairwise~cond.f)
#Now we check for autocorrelation 
#Plotting x with the model
x<-care$l.dproxct[!is.na(care$l.dproxct)]#removes na values from column
E2<-residuals(M2.2,type="normalized")
plot(x, E2)
#residuals v observation 

#check for autocorrelation
acf(E2, na.action=na.pass,
    main="Auto-correlation plot for residuals")
#There are no autocorrelation. 