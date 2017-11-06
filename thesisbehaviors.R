#Hi from Clay

#load data
care=read.table(file="caredata.csv", header=TRUE, sep=",")
names(care)
str(care)

#Packages used
library(nlme)
library(dplyr)
#packages gotten from last years project
library(lattice)
install.packages("lme4")
library(lme4)
library(geepack)
install.packages("lsmeans")
library(lsmeans)
install.packages("dplyr")
library(dplyr)
install.packages("MuMIn")
library(MuMIn)
install.packages("ggplot2")
library(ggplot2)

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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are a significant change between condition 1 and 2. 

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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are a significant change between condition 1 and 2, 1 and 3 and 1 and 4. 

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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are a significant change between condition 1 and 4 and 2 and 4.

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
m1=gls(hgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(hgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are no significant values for human groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
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
#### Human groom duration
m1=gls(hgroomd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(hgroomd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are no significant values for human groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
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

######## Baboon groom count
m1=gls(bgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(bgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model without the random factor had a lower IAC score we wanna use that 
#Plotting residuals for m1 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
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
  ylab("Number of times a baboon groomed the baboon") +
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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are no significant values for human groom count

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
  ylab("Duration of a baboon grooming the baboon") +
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
m1=gls(ghgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(ghgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model without the random factor had a lower IAC score we wanna use that 
#Plotting residuals for m1 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for human groom count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
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
m1=gls(ghgroomd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(ghgroomd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model without the random factor had a lower IAC score we wanna use that 
#Plotting residuals for m1 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant values for give human groom duration

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
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

##### Give baboon groom count
m1=gls(gbgroomct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(gbgroomct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are no significant values for human groom count

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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
m1=gls(penisd~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(penisd~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model without the random factor had a lower IAC score we wanna use that 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between condition 1-3 for penisdisplay

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
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
#Since the model the random factor had a lower IAC score we wanna use that 
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
#There are no significant difference for play baboon behavior 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.playb = mean(playb, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.playb=sd(playb, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb)), # of observations, excluding NAs.
            se.playb=s.playb/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#Since the model without the random factor had a lower IAC score we wanna use that 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant difference for manipulating enrichment count

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.manenrichct = mean(manenrichct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.manenrichct=sd(manenrichct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct)), # of observations, excluding NAs.
            se.manenrichct=s.manenrichct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are no significant difference for manipulate enrichment duration 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.manenrichd = mean(manenrichd, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.manenrichd=sd(manenrichd, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd)), # of observations, excluding NAs.
            se.manenrichd=s.manenrichd/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
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

#### Embrace human 
m1=gls(embraceh~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#model2 - try put individual in as a main factor so cond.f+id
m2=lme(embraceh~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(m2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#Since the model with the random factor had a lower IAC score we wanna use that 
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
#There are a significant difference between 1-3 and almost 2-3

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
#Since the model without the random factor had a lower IAC score we wanna use that 
#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m1, add.smooth=FALSE, which=1)
E=resid(m1)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

qqnorm(residuals(m1))
qqline(residuals(m1))
ad.test(residuals(m1))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are a significant difference between 1-4, 2-4 and 3 and 4. 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.dproxct = mean(dproxct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.dproxct=sd(dproxct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct)), # of observations, excluding NAs.
            se.dproxct=s.dproxct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
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

