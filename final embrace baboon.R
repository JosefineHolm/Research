#Final embrace baboon
#load data
care=read.table(file="caredata.csv", header=TRUE, sep=",")
names(care)
str(care)

#Installed Packages
install.packages("nlme")
install.packages("lme4")
install.packages("geepack")
install.packages("lsmeans")
install.packages("dplyr")
install.packages("MuMIn")
install.packages("ggplot2")
install.packages("nortest")
install.packages("multcomp")

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
model.matrix.gls <- function(M2.2, ...){
  
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
ggplot(data=x1,
       aes(x=cond.f, y=m.embraceb, fill=id, label=m.embraceb)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.embraceb, ymax=m.embraceb+se.embraceb), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Embraces") +
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
#Now looking at independent variables
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
##DOES NOT WANT TO RUN THE MODEL WITH THE WEIGHTS
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctpos+ctenrich+enrich+timetofeed+clean+xtraint+sep+pxtraint+(1|id),data=care, na.action=na.omit,weights=vf2)
#So continuing without the "weight" factor here
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctpos+ctenrich+enrich+timetofeed+clean+xtraint+sep+pxtraint+(1|id),data=care, na.action=na.omit)
summary(M2.2)
drop1(M2.2, test="Chi")#drop ctpos.
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctenrich+enrich+timetofeed+clean+xtraint+sep+pxtraint+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop extrainteraction
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctenrich+enrich+timetofeed+clean+sep+pxtraint+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop timetofeed
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctenrich+enrich+clean+sep+pxtraint+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop clean
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctenrich+enrich+sep+pxtraint+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop pxtraint
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+weather+ctenrich+enrich+sep+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop weather
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+ctenrich+enrich+sep+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop enrich
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+ctenrich+sep+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop ctenrich
M2.2<-lmer(l.embraceb~cond.f+focal.f+dayofcond.f+observer+sep+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop focal.f
M2.2<-lmer(l.embraceb~cond.f+dayofcond.f+observer+sep+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop observer
M2.2<-lmer(l.embraceb~cond.f+dayofcond.f+sep+(1|id),data=care, na.action=na.omit)
drop1(M2.2, test="Chi")#drop dayofcond.f
M2.2<-lmer(l.embraceb~cond.f+sep+(1|id),data=care, na.action=na.omit)
# So condition and if the individuals were seperated had an affect on the results. 
#Checking the rest of the independent variables (NOT SURE WHAT TO DO HERE)
M2.2<-lm(l.embraceb~tday.f,data=care, na.action=na.omit)
summary(M2.2)





############################################################
#Non-parametric tests just in case you need them
kruskal.test(embraceb~cond.f+sep, data=care)
install.packages("sm")
library(sm)
with(care, ancova.np<-sm.ancova(embraceb, sep, cond.f, model="equal"))#this is the non-parametric ancova

############################################################
#Non-parametric tests just in case you need them
kruskal.test(embraceb~cond.f+sep, data=care)
install.packages("sm")
library(sm)
with(care, ancova.np<-sm.ancova(embraceb, sep, cond.f, model="equal"))#this is the non-parametric ancova




###########################################################
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
#this example for "grunt"...change this variable for each
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(grunt.m = mean(grunt, na.rm = TRUE)) # na.rm = TRUE to remove missing values
            
#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(grunt.m = mean(grunt.m, na.rm = TRUE))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental space saved after her name so this needs to be
                                          #typed to include the space, or remove the spaces and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it grunt.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$grunt.brr=ayana.resp$grunt.m/base.ayana$grunt.m

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$grunt.brr=stevie.resp$grunt.m/base.stevie$grunt.m

base.hope<-subset(baseline, id=="hope ")
hope.resp$grunt.brr=hope.resp$grunt.m/base.hope$grunt.m

base.patats<-subset(baseline, id=="patats")
patats.resp$grunt.brr=patats.resp$grunt.m/base.patats$grunt.m

#create a summary table for plotting (for each individual)
#this example for "ayana"...change this variable for each
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(grunt.brr.m = mean(grunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            grunt.brr.sd=sd(grunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt.brr)), # of observations, excluding NAs. 
            grunt.brr.se=grunt.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=grunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=grunt.brr.m, ymax=grunt.brr.m + grunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#josefine....this is probably way clunkier than it needs to be, but this will show the average behavior
#response per individual.  We can group it so all the individuals are shown on a given graph by doing
#summaries for all the individuals and then binding them together with rbind
#a value where the error bars are below 1 shows a significant decrease relative to condition 1, 
#and a value where the error bars are greater than one shows a significant increase