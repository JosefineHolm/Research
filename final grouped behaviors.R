#FInal grouped behaviors 
#Results putting all the behaviors in blocks Neutral/Negative/Positive

#load data
care=read.table(file="caredata.csv", header=TRUE, sep=",")
names(care)
str(care)

#Installed packages
install.packages("lme4")
install.packages("geepack")
install.packages("lsmeans")
install.packages("dplyr")
install.packages("MuMIn")
install.packages("ggplot2")
install.packages("nortest")
install.packages("multcomp")
install.packages("Matrix")
#Libraries to run 
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

#Adding counts and duration of the 3 behavioral type (positive, negative and neutral)
care$positivect=rowSums(care[,c("foodgrunt", "grunt","lipsmack","pbaboon", "phuman","hgroomct","bgroomct","ghgroomct","gbgroomct","penisd", "playb", "plays","manenrichct","embraceh", "embraceb", "dproxct","dproxout", "movetoobs","playvocal")], na.rm=T)
care$positivect
care$positived=rowSums(care[,c("hgroomd","bgroomd","ghgroomd","gbgroomd","manenrichd")], na.rm=T)
care$positived
care$negativect=rowSums(care[,c("iproxct", "iproxout","threatg", "bark", "headshake", "yawn", "aggb", "pace", "aggdisp", "teeth", "rubgen", "sway", "turn", "scream")], na.rm=T)
care$negativect
care$neutralct=rowSums(care[,c("sgroomct", "eatct","drink","foragect", "restct","schratch", "other")], na.rm=T) 
care$neutralct
care$neutrald=rowSums(care[,c("sgroomd","eatd","foraged","restd")], na.rm=T )
care$neutrald

#Now running the statitics for the different blocks

#######################################################################
#Positive count behaviors
#model one without random factor
m1=gls(positivect~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)
#model2 - try put individual in as a main factor so cond.f+id
m2=lme(positivect~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
lsmeans(m2,pairwise~cond.f)
#Normalizing
care$l.positivect=log(care$positivect+1)

#Now we run same models with the normalised data
m1n=gls(l.positivect~cond.f, data=care, na.action=na.omit, method="ML")
#model2 - try put individual in as a main factor so cond.f+id
m2n=lme(l.positivect~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
anova(m1n,m2n)
#Test variance equality if the p is sign run alternate variance structure
bartlett.test(l.positivect~id, data=care)
#P is significant so we run alternate variance structure
vf1=varIdent(form = ~1|id)
vf2=varIdent(form = ~1|cond.f)
m2n=lme(l.positivect~cond.f, random=~1|id,data=care, na.action=na.omit)

M2.1<-lme(l.positivect~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf1)

M2.2<-lme(l.positivect~cond.f, random=~1|id,data=care, na.action=na.omit, weights=vf2)
anova(m2n,M2.1,M2.2)
#M2.1 is the better model 
#Now checking for significant results
model.matrix.gls <- function(M2.1, ...){
  
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
#There are significant difference between cond 1-2, 1-3. There are less counted positive behaviors in cond 2 and 3 than 1. 
#Looking at residuals
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(M2.1, add.smooth=FALSE, which=1)
E=resid(M2.1)
hist(E,xlab="residuals", main="")
plot(filter(care, !is.na(l.positivect)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(care, !is.na(l.positivect)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(M2.1))
qqline(residuals(M2.1))
ad.test(residuals(M2.1))
summary(M2.1)
#Plotting my data 
#CPA added-->I think the problem with the code below is that there's no object called x1.
#x1 should be a summary table of what you're trying to plot
ggplot(data=x1,
       aes(x=cond.f, y=m.positivect, fill=id, label=m.positivect)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.positivect, ymax=m.positivect+se.positivect), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of all positive behaviors") +
  ylim(0,15) +
  labs(fill="id") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=6),
        legend.key=element_blank(),
        legend.position=c(0.5,0.95),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#####################################################################################

