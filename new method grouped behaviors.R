#New method grouped behaviors
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

library(nlme)
library(dplyr)
library(lattice)
library(lme4)
library(geepack)
library(lsmeans)
library(dplyr)
library(MuMIn)
library(ggplot2)
library(nortest)
library(multcomp)

#Insert the grouped behaviors
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
#####################################################################################
#Positive behaviors count
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positivect.m = mean(positivect, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(positivect.mm = mean(positivect.m, na.rm = TRUE),
            positivect.m.sd=sd(positivect.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positivect.m)), # of observations, excluding NAs. 
            positivect.m.se=positivect.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$positivect.m.se/baseline$positivect.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental space saved after her name so this needs to be
#typed to include the space, or remove the spaces and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it positivect.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$positivect.brr=ayana.resp$positivect.m/base.ayana$positivect.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$positivect.brr=stevie.resp$positivect.m/base.stevie$positivect.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$positivect.brr=hope.resp$positivect.m/base.hope$positivect.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$positivect.brr=patats.resp$positivect.m/base.patats$positivect.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positivect.brr.m = mean(positivect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positivect.brr.sd=sd(positivect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positivect.brr)), # of observations, excluding NAs. 
            positivect.brr.se=positivect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positivect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positivect.brr.m - positivect.brr.se, ymax=positivect.brr.m + positivect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positivect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.242, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.242, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positivect.brr.m = mean(positivect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positivect.brr.sd=sd(positivect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positivect.brr)), # of observations, excluding NAs. 
            positivect.brr.se=positivect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positivect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positivect.brr.m - positivect.brr.se, ymax=positivect.brr.m + positivect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positivect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0899, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0899, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positivect.brr.m = mean(positivect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positivect.brr.sd=sd(positivect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positivect.brr)), # of observations, excluding NAs. 
            positivect.brr.se=positivect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positivect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positivect.brr.m - positivect.brr.se, ymax=positivect.brr.m + positivect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positivect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.143, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.143, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positivect.brr.m = mean(positivect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positivect.brr.sd=sd(positivect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positivect.brr)), # of observations, excluding NAs. 
            positivect.brr.se=positivect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positivect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positivect.brr.m - positivect.brr.se, ymax=positivect.brr.m + positivect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positivect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.202, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.202, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Positive duration 
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positived.m = mean(positived, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(positived.mm = mean(positived.m, na.rm = TRUE),
            positived.m.sd=sd(positived.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positived.m)), # of observations, excluding NAs. 
            positived.m.se=positived.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$positived.m.se/baseline$positived.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental space saved after her name so this needs to be
#typed to include the space, or remove the spaces and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it positived.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$positived.brr=ayana.resp$positived.m/base.ayana$positived.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$positived.brr=stevie.resp$positived.m/base.stevie$positived.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$positived.brr=hope.resp$positived.m/base.hope$positived.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$positived.brr=patats.resp$positived.m/base.patats$positived.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positived.brr.m = mean(positived.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positived.brr.sd=sd(positived.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positived.brr)), # of observations, excluding NAs. 
            positived.brr.se=positived.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positived.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positived.brr.m - positived.brr.se, ymax=positived.brr.m + positived.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positived") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.273, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.273, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positived.brr.m = mean(positived.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positived.brr.sd=sd(positived.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positived.brr)), # of observations, excluding NAs. 
            positived.brr.se=positived.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positived.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positived.brr.m - positived.brr.se, ymax=positived.brr.m + positived.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positived") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.217, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.217, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positived.brr.m = mean(positived.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positived.brr.sd=sd(positived.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positived.brr)), # of observations, excluding NAs. 
            positived.brr.se=positived.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positived.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positived.brr.m - positived.brr.se, ymax=positived.brr.m + positived.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positived") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.763, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.763, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(positived.brr.m = mean(positived.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            positived.brr.sd=sd(positived.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(positived.brr)), # of observations, excluding NAs. 
            positived.brr.se=positived.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=positived.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=positived.brr.m - positived.brr.se, ymax=positived.brr.m + positived.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for positived") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.579, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.579, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#################################################################################
#Negative behaviors count only
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(negativect.m = mean(negativect, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(negativect.mm = mean(negativect.m, na.rm = TRUE),
            negativect.m.sd=sd(negativect.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(negativect.m)), # of observations, excluding NAs. 
            negativect.m.se=negativect.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$negativect.m.se/baseline$negativect.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental space saved after her name so this needs to be
#typed to include the space, or remove the spaces and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it negativect.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$negativect.brr=ayana.resp$negativect.m/base.ayana$negativect.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$negativect.brr=stevie.resp$negativect.m/base.stevie$negativect.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$negativect.brr=hope.resp$negativect.m/base.hope$negativect.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$negativect.brr=patats.resp$negativect.m/base.patats$negativect.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(negativect.brr.m = mean(negativect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            negativect.brr.sd=sd(negativect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(negativect.brr)), # of observations, excluding NAs. 
            negativect.brr.se=negativect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=negativect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=negativect.brr.m - negativect.brr.se, ymax=negativect.brr.m + negativect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for negativect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.556, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.556, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(negativect.brr.m = mean(negativect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            negativect.brr.sd=sd(negativect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(negativect.brr)), # of observations, excluding NAs. 
            negativect.brr.se=negativect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=negativect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=negativect.brr.m - negativect.brr.se, ymax=negativect.brr.m + negativect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for negativect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.256, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.256, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(negativect.brr.m = mean(negativect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            negativect.brr.sd=sd(negativect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(negativect.brr)), # of observations, excluding NAs. 
            negativect.brr.se=negativect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=negativect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=negativect.brr.m - negativect.brr.se, ymax=negativect.brr.m + negativect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for negativect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.332, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.332, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(negativect.brr.m = mean(negativect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            negativect.brr.sd=sd(negativect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(negativect.brr)), # of observations, excluding NAs. 
            negativect.brr.se=negativect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=negativect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=negativect.brr.m - negativect.brr.se, ymax=negativect.brr.m + negativect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for negativect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.292, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.292, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
####################################################################################
#Neutral count 
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutralct.m = mean(neutralct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(neutralct.mm = mean(neutralct.m, na.rm = TRUE),
            neutralct.m.sd=sd(neutralct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutralct.m)), # of observations, excluding NAs. 
            neutralct.m.se=neutralct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$neutralct.m.se/baseline$neutralct.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental space saved after her name so this needs to be
#typed to include the space, or remove the spaces and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it neutralct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$neutralct.brr=ayana.resp$neutralct.m/base.ayana$neutralct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$neutralct.brr=stevie.resp$neutralct.m/base.stevie$neutralct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$neutralct.brr=hope.resp$neutralct.m/base.hope$neutralct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$neutralct.brr=patats.resp$neutralct.m/base.patats$neutralct.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutralct.brr.m = mean(neutralct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutralct.brr.sd=sd(neutralct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutralct.brr)), # of observations, excluding NAs. 
            neutralct.brr.se=neutralct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutralct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutralct.brr.m - neutralct.brr.se, ymax=neutralct.brr.m + neutralct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutralct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0733, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.0733, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutralct.brr.m = mean(neutralct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutralct.brr.sd=sd(neutralct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutralct.brr)), # of observations, excluding NAs. 
            neutralct.brr.se=neutralct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutralct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutralct.brr.m - neutralct.brr.se, ymax=neutralct.brr.m + neutralct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutralct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0519, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0519, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutralct.brr.m = mean(neutralct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutralct.brr.sd=sd(neutralct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutralct.brr)), # of observations, excluding NAs. 
            neutralct.brr.se=neutralct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutralct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutralct.brr.m - neutralct.brr.se, ymax=neutralct.brr.m + neutralct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutralct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.231, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.231, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutralct.brr.m = mean(neutralct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutralct.brr.sd=sd(neutralct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutralct.brr)), # of observations, excluding NAs. 
            neutralct.brr.se=neutralct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutralct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutralct.brr.m - neutralct.brr.se, ymax=neutralct.brr.m + neutralct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutralct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0366, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.0366, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###########################################################################
#Neutral durations 
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutrald.m = mean(neutrald, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(neutrald.mm = mean(neutrald.m, na.rm = TRUE),
            neutrald.m.sd=sd(neutrald.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutrald.m)), # of observations, excluding NAs. 
            neutrald.m.se=neutrald.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$neutrald.m.se/baseline$neutrald.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental space saved after her name so this needs to be
#typed to include the space, or remove the spaces and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it neutrald.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$neutrald.brr=ayana.resp$neutrald.m/base.ayana$neutrald.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$neutrald.brr=stevie.resp$neutrald.m/base.stevie$neutrald.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$neutrald.brr=hope.resp$neutrald.m/base.hope$neutrald.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$neutrald.brr=patats.resp$neutrald.m/base.patats$neutrald.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutrald.brr.m = mean(neutrald.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutrald.brr.sd=sd(neutrald.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutrald.brr)), # of observations, excluding NAs. 
            neutrald.brr.se=neutrald.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutrald.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutrald.brr.m - neutrald.brr.se, ymax=neutrald.brr.m + neutrald.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutrald") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0737, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.0737, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutrald.brr.m = mean(neutrald.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutrald.brr.sd=sd(neutrald.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutrald.brr)), # of observations, excluding NAs. 
            neutrald.brr.se=neutrald.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutrald.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutrald.brr.m - neutrald.brr.se, ymax=neutrald.brr.m + neutrald.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutrald") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0478, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0478, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutrald.brr.m = mean(neutrald.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutrald.brr.sd=sd(neutrald.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutrald.brr)), # of observations, excluding NAs. 
            neutrald.brr.se=neutrald.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutrald.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutrald.brr.m - neutrald.brr.se, ymax=neutrald.brr.m + neutrald.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutrald") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0990, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.0990, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(neutrald.brr.m = mean(neutrald.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            neutrald.brr.sd=sd(neutrald.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutrald.brr)), # of observations, excluding NAs. 
            neutrald.brr.se=neutrald.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=neutrald.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=neutrald.brr.m - neutrald.brr.se, ymax=neutrald.brr.m + neutrald.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for neutrald") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0423, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.0423, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))