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
 
#### lipsmack
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

#####Present to baboons
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

####### Present to human 
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