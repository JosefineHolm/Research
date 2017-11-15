
#####################Preliminary statistics to find results for individual behaviors###############################

#load data
care=read.table(file="caredata.csv", header=TRUE, sep=",")
names(care)
str(care)

#Installes Packages 
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

#Putting the 4 condition in as a factor
care$cond.f=as.factor(care$cond)
str(care)

#Making a subset only consistent of integers/numbers
xx=care[,sapply(care,is.integer)] #this is to make a subset only of the integers (numbers)
xx

#Doing what I have learned one behavior at the time 

#Foodgrunt
#model one without random factor
m1=gls(foodgrunt~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(foodgrunt~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#There is not significant difference in the model P = 0.9996 so we will keep model 2 because it makes most sence.  
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))# Is significant 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant changes in the overal count of neutral behaviors between the 4 condition. 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.foodgrunt = mean(foodgrunt, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.foodgrunt=sd(foodgrunt, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foodgrunt)), # of observations, excluding NAs.
            se.foodgrunt=s.foodgrunt/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.foodgrunt, fill=id, label=m.foodgrunt)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.foodgrunt, ymax=m.foodgrunt+se.foodgrunt), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of foodgrunt") +
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

#Grunt 
#model one without random factor
m1=gls(grunt~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(grunt~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and model 2 is significant P=1*10^-4
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#Ad.test is significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant changes in the overal count of neutral behaviors between the 4 condition. 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.grunt = mean(grunt, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.grunt=sd(grunt, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt)), # of observations, excluding NAs.
            se.grunt=s.grunt/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.grunt, fill=id, label=m.grunt)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.grunt, ymax=m.grunt+se.grunt), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of grunt") +
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

#lipsmack
#model one without random factor
m1=gls(lipsmack~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(lipsmack~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=0.0001
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant change between condition 1 and 2. P=0.0398

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.lipsmack = mean(lipsmack, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.lipsmack=sd(lipsmack, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(lipsmack)), # of observations, excluding NAs.
            se.lipsmack=s.lipsmack/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.lipsmack, fill=id, label=m.lipsmack)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.lipsmack, ymax=m.lipsmack+se.lipsmack), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of lipsmacks") +
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

###Present to baboons
#model one without random factor
m1=gls(pbaboon~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(pbaboon~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=8*10^-4 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P=2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant change between condition 1 and 2 P=0.0042, 1 and 3 P=0.0022and 1 and 4 P=0.0026. 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.pbaboon = mean(pbaboon, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.pbaboon=sd(pbaboon, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pbaboon)), # of observations, excluding NAs.
            se.pbaboon=s.pbaboon/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.pbaboon, fill=id, label=m.pbaboon)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.pbaboon, ymax=m.pbaboon+se.pbaboon), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of presentations to baboons") +
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

#### Present to human 
m1=gls(phuman~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(phuman~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=0.001
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant change between condition 2 and 4 P=0.0458 and almost 1-4.

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.phuman = mean(phuman, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.phuman=sd(phuman, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(phuman)), # of observations, excluding NAs.
            se.phuman=s.phuman/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.phuman, fill=id, label=m.phuman)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.phuman, ymax=m.phuman+se.phuman), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of presentations to humans") +
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

#### Human groom count
#Since stevie can not groom a human we have to take him out by making a subset
females=subset(care,id!="stevie")#exclude the individuals you dont need
females=droplevels(females)# deletes the individual from the sets 
females$id #shows the specific columns and the structure
#Model 1 without individuals with random factors
m1=gls(hgroomct~cond.f, data=females, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(hgroomct~cond.f, random=~1|id,data=females, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and it is significant P=0.0322
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(females, !is.na(hgroomct)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(females, !is.na(hgroomct)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for human groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(females, cond.f, id) %>%
  summarize(m.hgroomct = mean(hgroomct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.hgroomct=sd(hgroomct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomct)), # of observations, excluding NAs.
            se.hgroomct=s.hgroomct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.hgroomct, fill=id, label=m.hgroomct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.hgroomct, ymax=m.hgroomct+se.hgroomct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of times a human groomed the baboon") +
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

#Still shows 4 id in the graph

#### Human groom duration 
#Before we calculate this we have to take stevie out
females=subset(care,id!="stevie")#exclude the individuals you dont need
females=droplevels(females)# deletes the individual from the sets 
females$id #shows the specific columns and the structure

m1=gls(hgroomd~cond.f, data=females, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(hgroomd~cond.f, random=~1|id,data=females, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=0.0322
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(females, !is.na(hgroomd)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(females, !is.na(hgroomd)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for human groom duration

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(females, cond.f, id) %>%
  summarize(m.hgroomd = mean(hgroomd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.hgroomd=sd(hgroomd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomd)), # of observations, excluding NAs.
            se.hgroomd=s.hgroomd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.hgroomd, fill=id, label=m.hgroomd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.hgroomd, ymax=m.hgroomd+se.hgroomd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Duration of a human grooming the baboon") +
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

######## Baboon groom count, a baboon is grooming the individual
m1=gls(bgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(bgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other and M1 does have a lower score, but since it is not significant M2 still makes more sense 
#Plotting residuals for m1 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant p=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for human groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.bgroomct = mean(bgroomct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.bgroomct=sd(bgroomct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomct)), # of observations, excluding NAs.
            se.bgroomct=s.bgroomct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.bgroomct, fill=id, label=m.bgroomct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.bgroomct, ymax=m.bgroomct+se.bgroomct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of times a baboon groomed the individual") +
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

## Baboon groom duration
m1=gls(bgroomd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(bgroomd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=0.0013 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for baboon groom duration

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.bgroomd = mean(bgroomd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.bgroomd=sd(bgroomd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomd)), # of observations, excluding NAs.
            se.bgroomd=s.bgroomd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.bgroomd, fill=id, label=m.bgroomd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.bgroomd, ymax=m.bgroomd+se.bgroomd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Duration of a baboon grooming the individual") +
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

#### Give human groom count 
#Before we calculate this we have to take stevie out
females=subset(care,id!="stevie")#exclude the individuals you dont need
females=droplevels(females)# deletes the individual from the sets 
females$id #shows the specific columns and the structure
m1=gls(ghgroomct~cond.f, data=females, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(ghgroomct~cond.f, random=~1|id,data=females, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#There is no significant difference between the models P=0.1719 so we use M2 because it makes more sense  
#Plotting residuals for m1 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(females, !is.na(ghgroomct)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(females, !is.na(ghgroomct)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant P=2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for give human groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(females, cond.f, id) %>%
  summarize(m.ghgroomct = mean(ghgroomct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.ghgroomct=sd(ghgroomct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ghgroomct)), # of observations, excluding NAs.
            se.ghgroomct=s.ghgroomct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.ghgroomct, fill=id, label=m.ghgroomct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.ghgroomct, ymax=m.ghgroomct+se.ghgroomct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of times a baboon groomed a human") +
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

#### Give human groom duration
females=subset(care,id!="stevie")#exclude the individuals you dont need
females=droplevels(females)# deletes the individual from the sets 
females$id #shows the specific columns and the structure

m1=gls(ghgroomd~cond.f, data=females, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(ghgroomd~cond.f, random=~1|id,data=females, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant different P=0.2095 so I choose M2 cause it makes more sense  
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(filter(females, !is.na(ghgroomd)) %>% dplyr::select(id), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
plot(filter(females, !is.na(ghgroomd)) %>% dplyr::select(cond.f), #this code will filter NAs out.  it's best to use this nested in the plot command all the time
     E, xlab="Treatment", ylab="residuals")
qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for give human groom duration

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(females, cond.f, id) %>%
  summarize(m.ghgroomd = mean(ghgroomd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.ghgroomd=sd(ghgroomd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ghgroomd)), # of observations, excluding NAs.
            se.ghgroomd=s.ghgroomd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.ghgroomd, fill=id, label=m.ghgroomd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.ghgroomd, ymax=m.ghgroomd+se.ghgroomd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Duration of the baboon grooming a human") +
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

##### Give baboon groom count
m1=gls(gbgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(gbgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=1*10^-4 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for give baboon groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.gbgroomct = mean(gbgroomct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.gbgroomct=sd(gbgroomct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomct)), # of observations, excluding NAs.
            se.gbgroomct=s.gbgroomct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.gbgroomct, fill=id, label=m.gbgroomct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.gbgroomct, ymax=m.gbgroomct+se.gbgroomct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of times a the individual baboon groomed another baboon") +
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

#### Give baboon groom duration 
m1=gls(gbgroomd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(gbgroomd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=0.0001
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for give baboon groom duration

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.gbgroomd = mean(gbgroomd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.gbgroomd=sd(gbgroomd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomd)), # of observations, excluding NAs.
            se.gbgroomd=s.gbgroomd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.gbgroomd, fill=id, label=m.gbgroomd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.gbgroomd, ymax=m.gbgroomd+se.gbgroomd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Duration of the individual baboon grooming another baboon") +
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

###### Penisdisplay 
#Before we calculate this we have to take the girls out
male=subset(care,penisd>=0)
male=droplevels(male)# deletes the individual from the sets 
male$id #shows the specific columns and the structure which shows all the individuals

m1=gls(penisd~cond.f, data=male, na.action=na.omit, method="ML")
summary(m1)

#model2 - since it is only one individual we will not use model 2
m2=lme(penisd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(male$cond.f, E, xlab="Treatment", ylab="residuals")
plot(male$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#ad.test significant p=3.111*10^-15 
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between condition 1-3 for penisdisplay

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(male, cond.f, id) %>%
  summarize(m.penisd = mean(penisd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.penisd=sd(penisd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(penisd)), # of observations, excluding NAs.
            se.penisd=s.penisd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.penisd, fill=id, label=m.penisd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.penisd, ymax=m.penisd+se.penisd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Penis display counts") +
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

### Play baboon 
m1=gls(playb~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(playb~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model the random factor had a lower IAC score we wanna use that it is not significant though P=1 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant 2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant difference for play baboon behavior 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.playb = mean(playb, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.playb=sd(playb, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb)), # of observations, excluding NAs.
            se.playb=s.playb/sqrt(n))
x1
#Trying to plot per individual
ggplot(data=x1,
       aes(x=cond.f, y=m.playb, fill=id, label=m.playb)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.playb, ymax=m.playb+se.playb), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Play whith baboons counts") +
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

##### Play self 
m1=gls(plays~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(plays~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that and its significant P=0.0001
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant 2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant difference for play self

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.plays = mean(plays, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.plays=sd(plays, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays)), # of observations, excluding NAs.
            se.plays=s.plays/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.plays, fill=id, label=m.plays)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.plays, ymax=m.plays+se.plays), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Play with itself") +
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

#### Manipulate enrichment count
m1=gls(manenrichct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(manenrichct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significantly different so I choose the one with random factor cause it makes most sense
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))# Ad.test is significant 2.2*10^-16
summary(m2)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant difference for manipulating enrichment count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.manenrichct = mean(manenrichct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.manenrichct=sd(manenrichct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct)), # of observations, excluding NAs.
            se.manenrichct=s.manenrichct/sqrt(n))
x1
#Trying to plot for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.manenrichct, fill=id, label=m.manenrichct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.manenrichct, ymax=m.manenrichct+se.manenrichct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Manipulate enrichment count") +
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

#### Manipulate enrich duration
m1=gls(manenrichd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(manenrichd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that its not significant: 0.0914 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant 2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant difference for manipulate enrichment duration 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.manenrichd = mean(manenrichd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.manenrichd=sd(manenrichd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd)), # of observations, excluding NAs.
            se.manenrichd=s.manenrichd/sqrt(n))
x1
#Trying to plot for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.manenrichd, fill=id, label=m.manenrichd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.manenrichd, ymax=m.manenrichd+se.manenrichd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Manipulate enrichment duration") +
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

#### Embrace human (not done yet)
#First we have to exclude Stevie
females=subset(care,id!="stevie")#exclude the individuals you dont need
females=droplevels(females)# deletes the individual from the sets 
females$id #shows the specific columns and the structure

m1=gls(embraceh~cond.f, data=females, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(embraceh~cond.f, random=~1|id,data=females, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The model is significant P = 0.0247 But M1 have a better AIC score 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#this one says error
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant difference for embrace human

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.embraceh = mean(embraceh, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.embraceh=sd(embraceh, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceh)), # of observations, excluding NAs.
            se.embraceh=s.embraceh/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.embraceh, fill=id, label=m.embraceh)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.embraceh, ymax=m.embraceh+se.embraceh), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Embrace human") +
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

##### Embrace baboon
m1=gls(embraceb~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(embraceb~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that but its not significant p=0.1007 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant 2.2*10^-16
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between 1-3 P = 0.0024 and almost 2-3 P=0.0527.

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.embraceb = mean(embraceb, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.embraceb=sd(embraceb, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceb)), # of observations, excluding NAs.
            se.embraceb=s.embraceb/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.embraceb, fill=id, label=m.embraceb)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.embraceb, ymax=m.embraceb+se.embraceb), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Embrace baboon") +
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

#### Decrease proximity caretaker
m1=gls(dproxct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(dproxct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P = 0.4771 so we choose M2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between 1-4 P=0.0002, 2-4 P=0.0001 and 3 and 4 P=0.0001. 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.dproxct = mean(dproxct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.dproxct=sd(dproxct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct)), # of observations, excluding NAs.
            se.dproxct=s.dproxct/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.dproxct, fill=id, label=m.dproxct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.dproxct, ymax=m.dproxct+se.dproxct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Decrease proximity caretaker") +
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

###Decrease proximity outside
m1=gls(dproxout~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(dproxout~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are significant from each other P = 0.0001 and we choose model 2 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant differences

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.dproxout = mean(dproxout, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.dproxout=sd(dproxout, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxout)), # of observations, excluding NAs.
            se.dproxout=s.dproxout/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.dproxout, fill=id, label=m.dproxout)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.dproxout, ymax=m.dproxout+se.dproxout), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Decrease proximity outside") +
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

####Move towards observer
m1=gls(movetoobs~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(movetoobs~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are significant from each other P = 0.0001 and we choose model 2 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant differences

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.movetoobs = mean(movetoobs, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.movetoobs=sd(movetoobs, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(movetoobs)), # of observations, excluding NAs.
            se.movetoobs=s.movetoobs/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.movetoobs, fill=id, label=m.movetoobs)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.movetoobs, ymax=m.movetoobs+se.movetoobs), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Move towards observer") +
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

####Play vocalisation
m1=gls(playvocal~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(playvocal~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P = 0.279 so we chose m2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant differences

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.playvocal = mean(playvocal, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.playvocal=sd(playvocal, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playvocal)), # of observations, excluding NAs.
            se.playvocal=s.playvocal/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.playvocal, fill=id, label=m.playvocal)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.playvocal, ymax=m.playvocal+se.playvocal), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Play vocalisations") +
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

##########################################Negative behaviors########################################
#Increase proximity caretaker
m1=gls(iproxct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(iproxct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P = 0.9997 so we chose m2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant differences

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.iproxct = mean(iproxct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.iproxct=sd(iproxct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxct)), # of observations, excluding NAs.
            se.iproxct=s.iproxct/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.iproxct, fill=id, label=m.iproxct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.iproxct, ymax=m.iproxct+se.iproxct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Increase proximity caretaker") +
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

#Increase proximity outside
m1=gls(iproxout~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(iproxout~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P = 0.9997 so we chose m2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant differences

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.iproxout = mean(iproxout, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.iproxout=sd(iproxout, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxout)), # of observations, excluding NAs.
            se.iproxout=s.iproxout/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.iproxout, fill=id, label=m.iproxout)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.iproxout, ymax=m.iproxout+se.iproxout), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Increase proximity outside") +
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

#Threatgrunt
m1=gls(threatg~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(threatg~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P = 0.3173 so we chose m2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant differences

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.threatg = mean(threatg, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.threatg=sd(threatg, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(threatg)), # of observations, excluding NAs.
            se.threatg=s.threatg/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.threatg, fill=id, label=m.threatg)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.threatg, ymax=m.threatg+se.threatg), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Threat grunts") +
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

##Bark
m1=gls(bark~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(bark~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P = 0.9997 so we chose m2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between condition 1-2 P=0.0082 and 1-3 P=0.0392 and 1-4 P=0.0184.

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.bark = mean(bark, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.bark=sd(bark, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bark)), # of observations, excluding NAs.
            se.bark=s.bark/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.bark, fill=id, label=m.bark)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.bark, ymax=m.bark+se.bark), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Bark") +
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

# Headshake 
m1=gls(headshake~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(headshake~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Model 2 is significant P = 0.0031 so we use that one  
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between condition 1-2 P=0.0082 and 1-3 P=0.0392 and 1-4 P=0.0184.

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.headshake = mean(headshake, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.headshake=sd(headshake, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(headshake)), # of observations, excluding NAs.
            se.headshake=s.headshake/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.headshake, fill=id, label=m.headshake)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.headshake, ymax=m.headshake+se.headshake), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("headshake") +
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

#yawn
m1=gls(yawn~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(yawn~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Model 2 is significant P = 0.0001 so we use that one  
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between condition 1-2 P=0.0082 and 1-3 P=0.0392 and 1-4 P=0.0184.

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.yawn = mean(yawn, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.yawn=sd(yawn, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(yawn)), # of observations, excluding NAs.
            se.yawn=s.yawn/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.yawn, fill=id, label=m.yawn)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.yawn, ymax=m.yawn+se.yawn), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("yawn") +
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

#Agressive baboon 
m1=gls(aggb~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(aggb~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P= 0.5757 so we choose M2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between condition 1-2 P=0.0082 and 1-3 P=0.0392 and 1-4 P=0.0184.

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.aggb = mean(aggb, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.aggb=sd(aggb, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggb)), # of observations, excluding NAs.
            se.aggb=s.aggb/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.aggb, fill=id, label=m.aggb)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.aggb, ymax=m.aggb+se.aggb), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("aggressive baboon") +
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

#Pacing 
m1=gls(pace~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(pace~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#The models are not significant from each other P= 0.0692 so we choose M2. 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There is a significant difference between condition 1-3 P = 0.0357

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.pace = mean(pace, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.pace=sd(pace, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pace)), # of observations, excluding NAs.
            se.pace=s.pace/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.pace, fill=id, label=m.pace)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.pace, ymax=m.pace+se.pace), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Pacing") +
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

#Agressive display 
m1=gls(aggdisp~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(aggdisp~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Model 2 is significant P = 0.0222 so we use that 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There is no significant difference

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.aggdisp = mean(aggdisp, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.aggdisp=sd(aggdisp, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggdisp)), # of observations, excluding NAs.
            se.aggdisp=s.aggdisp/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.aggdisp, fill=id, label=m.aggdisp)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.aggdisp, ymax=m.aggdisp+se.aggdisp), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Aggressive display") +
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

#showing teeth
m1=gls(teeth~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(teeth~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Model 2 is significant P = 0.0147 so we use that 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is significant P = 2.2*10^-16 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There is no significant difference

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.teeth = mean(teeth, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.teeth=sd(teeth, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(teeth)), # of observations, excluding NAs.
            se.teeth=s.teeth/sqrt(n))
x1
#Trying to plot the amount of decrease prox to caretaker behavior are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.teeth, fill=id, label=m.teeth)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.teeth, ymax=m.teeth+se.teeth), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Showing teeth") +
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

#Rub genitals
#Before we calculate this we have to take the girls out
male=subset(care,rubgen>=0)
male=droplevels(male)# deletes the individual from the sets 
male$id #shows the specific columns and the structure which shows all the individuals

m1=gls(rubgen~cond.f, data=male, na.action=na.omit, method="ML")
summary(m1)

#model2 - since it is only one individual we will not use model 2
m2=lme(rubgen~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(male$cond.f, E, xlab="Treatment", ylab="residuals")
plot(male$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#ad.test significant p=3.111*10^-15 
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant result

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(male, cond.f, id) %>%
  summarize(m.rubgen = mean(rubgen, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.rubgen=sd(rubgen, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(rubgen)), # of observations, excluding NAs.
            se.rubgen=s.rubgen/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.rubgen, fill=id, label=m.rubgen)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.rubgen, ymax=m.rubgen+se.rubgen), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Rub genitals") +
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

#Turn 360 (Not done the subsetting does not work)
#Before we calculate this we have to take the girls out
male=subset(care,turn>=0)
male=droplevels(male)# deletes the individual from the sets 
male$id #shows the specific columns and the structure which shows all the individuals

m1=gls(turn~cond.f, data=male, na.action=na.omit, method="ML")
summary(m1)

#model2 - since it is only one individual we will not use model 2
m2=lme(turn~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(male$cond.f, E, xlab="Treatment", ylab="residuals")
plot(male$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#ad.test significant p=3.111*10^-15 
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant result

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(male, cond.f, id) %>%
  summarize(m.turn = mean(turn, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.turn=sd(turn, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(turn)), # of observations, excluding NAs.
            se.turn=s.turn/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.turn, fill=id, label=m.turn)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.turn, ymax=m.turn+se.turn), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Turn 360") +
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

#scream 
m1=gls(scream~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - since it is only one individual we will not use model 2
m2=lme(scream~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test is not significant P = 0.06758
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant result

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(male, cond.f, id) %>%
  summarize(m.scream = mean(scream, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.scream=sd(scream, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(scream)), # of observations, excluding NAs.
            se.scream=s.scream/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.scream, fill=id, label=m.scream)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.scream, ymax=m.scream+se.scream), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("scream") +
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

#Self groom count 
m1=gls(sgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - takin id in as a random factor
m2=lme(sgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)#They are not significant from each other p=0.1165 so we chose M2

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant p=3.111*10^-15 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant result

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.sgroomct = mean(sgroomct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.sgroomct=sd(sgroomct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomct)), # of observations, excluding NAs.
            se.sgroomct=s.sgroomct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.sgroomct, fill=id, label=m.sgroomct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.sgroomct, ymax=m.sgroomct+se.sgroomct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("self groom count") +
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

#Selfgroom durarion
m1=gls(sgroomd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - since it is only one individual we will not use model 2
m2=lme(sgroomd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)#They are not significant from each other p=0.1165 so we chose M2

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#ad.test significant p=3.111*10^-15 
summary(m2)

lsmeans(m2,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant result

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.sgroomd = mean(sgroomd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.sgroomd=sd(sgroomd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomd)), # of observations, excluding NAs.
            se.sgroomd=s.sgroomd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.sgroomd, fill=id, label=m.sgroomd)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.sgroomd, ymax=m.sgroomd+se.sgroomd), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("self groom duration") +
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
########################################################################################################
#Extra notes 
patats<-subset(care, id=="patats")#code for subsetting
hope<-subset(care, id=="hope")
ayana<-subset(care, id=="ayana")
females=rbind(patats, hope, ayana)
females=droplevels(females)

plot(females$cond.f, E, xlab="Treatment", ylab="residuals")
plot(females$id, E, xlab="id", ylab="residuals")