#New method negative behaviors
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
#####################################################################################
#Increse proximity caretaker
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxct.m = mean(iproxct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(iproxct.mm = mean(iproxct.m, na.rm = TRUE),
            iproxct.m.sd=sd(iproxct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxct.m)), # of observations, excluding NAs. 
            iproxct.m.se=iproxct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$iproxct.m.se/baseline$iproxct.mm
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
#call it iproxct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$iproxct.brr=ayana.resp$iproxct.m/base.ayana$iproxct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$iproxct.brr=stevie.resp$iproxct.m/base.stevie$iproxct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$iproxct.brr=hope.resp$iproxct.m/base.hope$iproxct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$iproxct.brr=patats.resp$iproxct.m/base.patats$iproxct.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxct.brr.m = mean(iproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxct.brr.sd=sd(iproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxct.brr)), # of observations, excluding NAs. 
            iproxct.brr.se=iproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxct.brr.m - iproxct.brr.se, ymax=iproxct.brr.m + iproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxct.brr.m = mean(iproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxct.brr.sd=sd(iproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxct.brr)), # of observations, excluding NAs. 
            iproxct.brr.se=iproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxct.brr.m - iproxct.brr.se, ymax=iproxct.brr.m + iproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxct.brr.m = mean(iproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxct.brr.sd=sd(iproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxct.brr)), # of observations, excluding NAs. 
            iproxct.brr.se=iproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxct.brr.m - iproxct.brr.se, ymax=iproxct.brr.m + iproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxct.brr.m = mean(iproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxct.brr.sd=sd(iproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxct.brr)), # of observations, excluding NAs. 
            iproxct.brr.se=iproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxct.brr.m - iproxct.brr.se, ymax=iproxct.brr.m + iproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#################################################################################
#Increase proximity outside
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxout.m = mean(iproxout, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(iproxout.mm = mean(iproxout.m, na.rm = TRUE),
            iproxout.m.sd=sd(iproxout.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxout.m)), # of observations, excluding NAs. 
            iproxout.m.se=iproxout.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$iproxout.m.se/baseline$iproxout.mm
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
#call it iproxout.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$iproxout.brr=ayana.resp$iproxout.m/base.ayana$iproxout.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$iproxout.brr=stevie.resp$iproxout.m/base.stevie$iproxout.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$iproxout.brr=hope.resp$iproxout.m/base.hope$iproxout.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$iproxout.brr=patats.resp$iproxout.m/base.patats$iproxout.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxout.brr.m = mean(iproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxout.brr.sd=sd(iproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxout.brr)), # of observations, excluding NAs. 
            iproxout.brr.se=iproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxout.brr.m - iproxout.brr.se, ymax=iproxout.brr.m + iproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxout") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxout.brr.m = mean(iproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxout.brr.sd=sd(iproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxout.brr)), # of observations, excluding NAs. 
            iproxout.brr.se=iproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxout.brr.m - iproxout.brr.se, ymax=iproxout.brr.m + iproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxout") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxout.brr.m = mean(iproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxout.brr.sd=sd(iproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxout.brr)), # of observations, excluding NAs. 
            iproxout.brr.se=iproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxout.brr.m - iproxout.brr.se, ymax=iproxout.brr.m + iproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxout") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(iproxout.brr.m = mean(iproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            iproxout.brr.sd=sd(iproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(iproxout.brr)), # of observations, excluding NAs. 
            iproxout.brr.se=iproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=iproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=iproxout.brr.m - iproxout.brr.se, ymax=iproxout.brr.m + iproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for iproxout") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Threat grunt 
#making a table for Behavior Response Ratio
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(threatg.m = mean(threatg, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(threatg.mm = mean(threatg.m, na.rm = TRUE),
            threatg.m.sd=sd(threatg.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(threatg.m)), # of observations, excluding NAs. 
            threatg.m.se=threatg.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$threatg.m.se/baseline$threatg.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
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
#call it threatg.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$threatg.brr=ayana.resp$threatg.m/base.ayana$threatg.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$threatg.brr=stevie.resp$threatg.m/base.stevie$threatg.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$threatg.brr=hope.resp$threatg.m/base.hope$threatg.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$threatg.brr=patats.resp$threatg.m/base.patats$threatg.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(threatg.brr.m = mean(threatg.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            threatg.brr.sd=sd(threatg.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(threatg.brr)), # of observations, excluding NAs. 
            threatg.brr.se=threatg.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=threatg.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=threatg.brr.m - threatg.brr.se, ymax=threatg.brr.m + threatg.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for threatg") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(threatg.brr.m = mean(threatg.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            threatg.brr.sd=sd(threatg.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(threatg.brr)), # of observations, excluding NAs. 
            threatg.brr.se=threatg.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=threatg.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=threatg.brr.m - threatg.brr.se, ymax=threatg.brr.m + threatg.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for threatg") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(threatg.brr.m = mean(threatg.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            threatg.brr.sd=sd(threatg.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(threatg.brr)), # of observations, excluding NAs. 
            threatg.brr.se=threatg.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=threatg.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=threatg.brr.m - threatg.brr.se, ymax=threatg.brr.m + threatg.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for threatg") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.684, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.684, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(threatg.brr.m = mean(threatg.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            threatg.brr.sd=sd(threatg.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(threatg.brr)), # of observations, excluding NAs. 
            threatg.brr.se=threatg.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=threatg.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=threatg.brr.m - threatg.brr.se, ymax=threatg.brr.m + threatg.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for threatg") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.728, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.728, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Bark
#making a table for Behavior Response Ratio
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bark.m = mean(bark, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(bark.mm = mean(bark.m, na.rm = TRUE),
            bark.m.sd=sd(bark.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bark.m)), # of observations, excluding NAs. 
            bark.m.se=bark.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$bark.m.se/baseline$bark.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
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
#call it bark.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$bark.brr=ayana.resp$bark.m/base.ayana$bark.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$bark.brr=stevie.resp$bark.m/base.stevie$bark.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$bark.brr=hope.resp$bark.m/base.hope$bark.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$bark.brr=patats.resp$bark.m/base.patats$bark.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bark.brr.m = mean(bark.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bark.brr.sd=sd(bark.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bark.brr)), # of observations, excluding NAs. 
            bark.brr.se=bark.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bark.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bark.brr.m - bark.brr.se, ymax=bark.brr.m + bark.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bark") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.816, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.816, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bark.brr.m = mean(bark.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bark.brr.sd=sd(bark.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bark.brr)), # of observations, excluding NAs. 
            bark.brr.se=bark.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bark.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bark.brr.m - bark.brr.se, ymax=bark.brr.m + bark.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bark") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.714, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.714, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bark.brr.m = mean(bark.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bark.brr.sd=sd(bark.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bark.brr)), # of observations, excluding NAs. 
            bark.brr.se=bark.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bark.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bark.brr.m - bark.brr.se, ymax=bark.brr.m + bark.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bark") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bark.brr.m = mean(bark.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bark.brr.sd=sd(bark.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bark.brr)), # of observations, excluding NAs. 
            bark.brr.se=bark.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bark.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bark.brr.m - bark.brr.se, ymax=bark.brr.m + bark.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bark") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
######################################################################################
#Headshake
#making a table for Behavior Response Ratio
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(headshake.m = mean(headshake, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(headshake.mm = mean(headshake.m, na.rm = TRUE),
            headshake.m.sd=sd(headshake.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(headshake.m)), # of observations, excluding NAs. 
            headshake.m.se=headshake.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$headshake.m.se/baseline$headshake.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
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
#call it headshake.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$headshake.brr=ayana.resp$headshake.m/base.ayana$headshake.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$headshake.brr=stevie.resp$headshake.m/base.stevie$headshake.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$headshake.brr=hope.resp$headshake.m/base.hope$headshake.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$headshake.brr=patats.resp$headshake.m/base.patats$headshake.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(headshake.brr.m = mean(headshake.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            headshake.brr.sd=sd(headshake.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(headshake.brr)), # of observations, excluding NAs. 
            headshake.brr.se=headshake.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=headshake.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=headshake.brr.m - headshake.brr.se, ymax=headshake.brr.m + headshake.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for headshake") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(headshake.brr.m = mean(headshake.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            headshake.brr.sd=sd(headshake.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(headshake.brr)), # of observations, excluding NAs. 
            headshake.brr.se=headshake.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=headshake.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=headshake.brr.m - headshake.brr.se, ymax=headshake.brr.m + headshake.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for headshake") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.683, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.683, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(headshake.brr.m = mean(headshake.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            headshake.brr.sd=sd(headshake.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(headshake.brr)), # of observations, excluding NAs. 
            headshake.brr.se=headshake.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=headshake.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=headshake.brr.m - headshake.brr.se, ymax=headshake.brr.m + headshake.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for headshake") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.258, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.258, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(headshake.brr.m = mean(headshake.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            headshake.brr.sd=sd(headshake.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(headshake.brr)), # of observations, excluding NAs. 
            headshake.brr.se=headshake.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=headshake.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=headshake.brr.m - headshake.brr.se, ymax=headshake.brr.m + headshake.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for headshake") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.413, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.413, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#####################################################################################
#Yawn
#making a table for Behavior Response Ratio
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(yawn.m = mean(yawn, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(yawn.mm = mean(yawn.m, na.rm = TRUE),
            yawn.m.sd=sd(yawn.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(yawn.m)), # of observations, excluding NAs. 
            yawn.m.se=yawn.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$yawn.m.se/baseline$yawn.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
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
#call it yawn.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$yawn.brr=ayana.resp$yawn.m/base.ayana$yawn.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$yawn.brr=stevie.resp$yawn.m/base.stevie$yawn.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$yawn.brr=hope.resp$yawn.m/base.hope$yawn.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$yawn.brr=patats.resp$yawn.m/base.patats$yawn.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(yawn.brr.m = mean(yawn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            yawn.brr.sd=sd(yawn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(yawn.brr)), # of observations, excluding NAs. 
            yawn.brr.se=yawn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=yawn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=yawn.brr.m - yawn.brr.se, ymax=yawn.brr.m + yawn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for yawn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.683, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.683, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(yawn.brr.m = mean(yawn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            yawn.brr.sd=sd(yawn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(yawn.brr)), # of observations, excluding NAs. 
            yawn.brr.se=yawn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=yawn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=yawn.brr.m - yawn.brr.se, ymax=yawn.brr.m + yawn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for yawn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.683, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.683, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(yawn.brr.m = mean(yawn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            yawn.brr.sd=sd(yawn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(yawn.brr)), # of observations, excluding NAs. 
            yawn.brr.se=yawn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=yawn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=yawn.brr.m - yawn.brr.se, ymax=yawn.brr.m + yawn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for yawn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.220, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.220, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(yawn.brr.m = mean(yawn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            yawn.brr.sd=sd(yawn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(yawn.brr)), # of observations, excluding NAs. 
            yawn.brr.se=yawn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=yawn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=yawn.brr.m - yawn.brr.se, ymax=yawn.brr.m + yawn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for yawn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
################################################################################
#Agressive agains baboons
#making a table for Behavior Response Ratio
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggb.m = mean(aggb, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(aggb.mm = mean(aggb.m, na.rm = TRUE),
            aggb.m.sd=sd(aggb.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggb.m)), # of observations, excluding NAs. 
            aggb.m.se=aggb.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$aggb.m.se/baseline$aggb.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
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
#call it aggb.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$aggb.brr=ayana.resp$aggb.m/base.ayana$aggb.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$aggb.brr=stevie.resp$aggb.m/base.stevie$aggb.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$aggb.brr=hope.resp$aggb.m/base.hope$aggb.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$aggb.brr=patats.resp$aggb.m/base.patats$aggb.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggb.brr.m = mean(aggb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggb.brr.sd=sd(aggb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggb.brr)), # of observations, excluding NAs. 
            aggb.brr.se=aggb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggb.brr.m - aggb.brr.se, ymax=aggb.brr.m + aggb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.515, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.515, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggb.brr.m = mean(aggb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggb.brr.sd=sd(aggb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggb.brr)), # of observations, excluding NAs. 
            aggb.brr.se=aggb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggb.brr.m - aggb.brr.se, ymax=aggb.brr.m + aggb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.316, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.316, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggb.brr.m = mean(aggb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggb.brr.sd=sd(aggb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggb.brr)), # of observations, excluding NAs. 
            aggb.brr.se=aggb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggb.brr.m - aggb.brr.se, ymax=aggb.brr.m + aggb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patas
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggb.brr.m = mean(aggb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggb.brr.sd=sd(aggb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggb.brr)), # of observations, excluding NAs. 
            aggb.brr.se=aggb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggb.brr.m - aggb.brr.se, ymax=aggb.brr.m + aggb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.632, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.632, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##############################################################################
#Pacing
#making a table for Behavior Response Ratio
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pace.m = mean(pace, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(pace.mm = mean(pace.m, na.rm = TRUE),
            pace.m.sd=sd(pace.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pace.m)), # of observations, excluding NAs. 
            pace.m.se=pace.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$pace.m.se/baseline$pace.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
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
#call it pace.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$pace.brr=ayana.resp$pace.m/base.ayana$pace.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$pace.brr=stevie.resp$pace.m/base.stevie$pace.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$pace.brr=hope.resp$pace.m/base.hope$pace.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$pace.brr=patats.resp$pace.m/base.patats$pace.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pace.brr.m = mean(pace.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pace.brr.sd=sd(pace.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pace.brr)), # of observations, excluding NAs. 
            pace.brr.se=pace.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pace.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pace.brr.m - pace.brr.se, ymax=pace.brr.m + pace.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pace") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.742, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.742, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pace.brr.m = mean(pace.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pace.brr.sd=sd(pace.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pace.brr)), # of observations, excluding NAs. 
            pace.brr.se=pace.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pace.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pace.brr.m - pace.brr.se, ymax=pace.brr.m + pace.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pace") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.5, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.5, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pace.brr.m = mean(pace.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pace.brr.sd=sd(pace.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pace.brr)), # of observations, excluding NAs. 
            pace.brr.se=pace.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pace.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pace.brr.m - pace.brr.se, ymax=pace.brr.m + pace.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pace") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.588, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.588, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pace.brr.m = mean(pace.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pace.brr.sd=sd(pace.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pace.brr)), # of observations, excluding NAs. 
            pace.brr.se=pace.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pace.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pace.brr.m - pace.brr.se, ymax=pace.brr.m + pace.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pace") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.624, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.624, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###############################################################################
#agressive display
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggdisp.m = mean(aggdisp, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(aggdisp.mm = mean(aggdisp.m, na.rm = TRUE),
            aggdisp.m.sd=sd(aggdisp.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggdisp.m)), # of observations, excluding NAs. 
            aggdisp.m.se=aggdisp.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$aggdisp.m.se/baseline$aggdisp.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental saggdisp saved after her name so this needs to be
#typed to include the saggdisp, or remove the saggdisps and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it aggdisp.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$aggdisp.brr=ayana.resp$aggdisp.m/base.ayana$aggdisp.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$aggdisp.brr=stevie.resp$aggdisp.m/base.stevie$aggdisp.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$aggdisp.brr=hope.resp$aggdisp.m/base.hope$aggdisp.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$aggdisp.brr=patats.resp$aggdisp.m/base.patats$aggdisp.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggdisp.brr.m = mean(aggdisp.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggdisp.brr.sd=sd(aggdisp.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggdisp.brr)), # of observations, excluding NAs. 
            aggdisp.brr.se=aggdisp.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggdisp.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggdisp.brr.m - aggdisp.brr.se, ymax=aggdisp.brr.m + aggdisp.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggdisp") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggdisp.brr.m = mean(aggdisp.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggdisp.brr.sd=sd(aggdisp.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggdisp.brr)), # of observations, excluding NAs. 
            aggdisp.brr.se=aggdisp.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggdisp.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggdisp.brr.m - aggdisp.brr.se, ymax=aggdisp.brr.m + aggdisp.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggdisp") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.429, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.429, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggdisp.brr.m = mean(aggdisp.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggdisp.brr.sd=sd(aggdisp.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggdisp.brr)), # of observations, excluding NAs. 
            aggdisp.brr.se=aggdisp.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggdisp.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggdisp.brr.m - aggdisp.brr.se, ymax=aggdisp.brr.m + aggdisp.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggdisp") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.561, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.561, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(aggdisp.brr.m = mean(aggdisp.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            aggdisp.brr.sd=sd(aggdisp.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(aggdisp.brr)), # of observations, excluding NAs. 
            aggdisp.brr.se=aggdisp.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=aggdisp.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=aggdisp.brr.m - aggdisp.brr.se, ymax=aggdisp.brr.m + aggdisp.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for aggdisp") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
############################################################################
#Teeth barring 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(teeth.m = mean(teeth, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(teeth.mm = mean(teeth.m, na.rm = TRUE),
            teeth.m.sd=sd(teeth.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(teeth.m)), # of observations, excluding NAs. 
            teeth.m.se=teeth.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$teeth.m.se/baseline$teeth.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental steeth saved after her name so this needs to be
#typed to include the steeth, or remove the steeths and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it teeth.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$teeth.brr=ayana.resp$teeth.m/base.ayana$teeth.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$teeth.brr=stevie.resp$teeth.m/base.stevie$teeth.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$teeth.brr=hope.resp$teeth.m/base.hope$teeth.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$teeth.brr=patats.resp$teeth.m/base.patats$teeth.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(teeth.brr.m = mean(teeth.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            teeth.brr.sd=sd(teeth.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(teeth.brr)), # of observations, excluding NAs. 
            teeth.brr.se=teeth.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=teeth.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=teeth.brr.m - teeth.brr.se, ymax=teeth.brr.m + teeth.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for teeth") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.632, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.632, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(teeth.brr.m = mean(teeth.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            teeth.brr.sd=sd(teeth.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(teeth.brr)), # of observations, excluding NAs. 
            teeth.brr.se=teeth.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=teeth.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=teeth.brr.m - teeth.brr.se, ymax=teeth.brr.m + teeth.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for teeth") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.482, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.482, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(teeth.brr.m = mean(teeth.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            teeth.brr.sd=sd(teeth.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(teeth.brr)), # of observations, excluding NAs. 
            teeth.brr.se=teeth.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=teeth.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=teeth.brr.m - teeth.brr.se, ymax=teeth.brr.m + teeth.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for teeth") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(teeth.brr.m = mean(teeth.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            teeth.brr.sd=sd(teeth.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(teeth.brr)), # of observations, excluding NAs. 
            teeth.brr.se=teeth.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=teeth.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=teeth.brr.m - teeth.brr.se, ymax=teeth.brr.m + teeth.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for teeth") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Rubbing genitalia
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(rubgen.m = mean(rubgen, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(rubgen.mm = mean(rubgen.m, na.rm = TRUE),
            rubgen.m.sd=sd(rubgen.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(rubgen.m)), # of observations, excluding NAs. 
            rubgen.m.se=rubgen.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$rubgen.m.se/baseline$rubgen.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental srubgen saved after her name so this needs to be
#typed to include the srubgen, or remove the srubgens and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it rubgen.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$rubgen.brr=ayana.resp$rubgen.m/base.ayana$rubgen.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$rubgen.brr=stevie.resp$rubgen.m/base.stevie$rubgen.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$rubgen.brr=hope.resp$rubgen.m/base.hope$rubgen.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$rubgen.brr=patats.resp$rubgen.m/base.patats$rubgen.mm

#create a summary table for plotting (for each individual)
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(rubgen.brr.m = mean(rubgen.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            rubgen.brr.sd=sd(rubgen.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(rubgen.brr)), # of observations, excluding NAs. 
            rubgen.brr.se=rubgen.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=rubgen.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=rubgen.brr.m - rubgen.brr.se, ymax=rubgen.brr.m + rubgen.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for rubgen") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.506, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.506, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##########################################################################
#Sway
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sway.m = mean(sway, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(sway.mm = mean(sway.m, na.rm = TRUE),
            sway.m.sd=sd(sway.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sway.m)), # of observations, excluding NAs. 
            sway.m.se=sway.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$sway.m.se/baseline$sway.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental ssway saved after her name so this needs to be
#typed to include the ssway, or remove the ssways and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it sway.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$sway.brr=ayana.resp$sway.m/base.ayana$sway.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$sway.brr=stevie.resp$sway.m/base.stevie$sway.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$sway.brr=hope.resp$sway.m/base.hope$sway.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$sway.brr=patats.resp$sway.m/base.patats$sway.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sway.brr.m = mean(sway.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sway.brr.sd=sd(sway.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sway.brr)), # of observations, excluding NAs. 
            sway.brr.se=sway.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sway.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sway.brr.m - sway.brr.se, ymax=sway.brr.m + sway.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sway") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sway.brr.m = mean(sway.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sway.brr.sd=sd(sway.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sway.brr)), # of observations, excluding NAs. 
            sway.brr.se=sway.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sway.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sway.brr.m - sway.brr.se, ymax=sway.brr.m + sway.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sway") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sway.brr.m = mean(sway.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sway.brr.sd=sd(sway.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sway.brr)), # of observations, excluding NAs. 
            sway.brr.se=sway.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sway.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sway.brr.m - sway.brr.se, ymax=sway.brr.m + sway.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sway") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sway.brr.m = mean(sway.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sway.brr.sd=sd(sway.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sway.brr)), # of observations, excluding NAs. 
            sway.brr.se=sway.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sway.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sway.brr.m - sway.brr.se, ymax=sway.brr.m + sway.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sway") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#turn 360 degrees
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(turn.m = mean(turn, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(turn.mm = mean(turn.m, na.rm = TRUE),
            turn.m.sd=sd(turn.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(turn.m)), # of observations, excluding NAs. 
            turn.m.se=turn.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$turn.m.se/baseline$turn.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sturn saved after her name so this needs to be
#typed to include the sturn, or remove the sturns and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it turn.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$turn.brr=ayana.resp$turn.m/base.ayana$turn.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$turn.brr=stevie.resp$turn.m/base.stevie$turn.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$turn.brr=hope.resp$turn.m/base.hope$turn.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$turn.brr=patats.resp$turn.m/base.patats$turn.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(turn.brr.m = mean(turn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            turn.brr.sd=sd(turn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(turn.brr)), # of observations, excluding NAs. 
            turn.brr.se=turn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=turn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=turn.brr.m - turn.brr.se, ymax=turn.brr.m + turn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for turn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-1, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(turn.brr.m = mean(turn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            turn.brr.sd=sd(turn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(turn.brr)), # of observations, excluding NAs. 
            turn.brr.se=turn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=turn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=turn.brr.m - turn.brr.se, ymax=turn.brr.m + turn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for turn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(turn.brr.m = mean(turn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            turn.brr.sd=sd(turn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(turn.brr)), # of observations, excluding NAs. 
            turn.brr.se=turn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=turn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=turn.brr.m - turn.brr.se, ymax=turn.brr.m + turn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for turn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.463, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.463, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(turn.brr.m = mean(turn.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            turn.brr.sd=sd(turn.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(turn.brr)), # of observations, excluding NAs. 
            turn.brr.se=turn.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=turn.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=turn.brr.m - turn.brr.se, ymax=turn.brr.m + turn.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for turn") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0., linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0., linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Scream
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(scream.m = mean(scream, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(scream.mm = mean(scream.m, na.rm = TRUE),
            scream.m.sd=sd(scream.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(scream.m)), # of observations, excluding NAs. 
            scream.m.se=scream.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$scream.m.se/baseline$scream.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sscream saved after her name so this needs to be
#typed to include the sscream, or remove the sscreams and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it scream.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$scream.brr=ayana.resp$scream.m/base.ayana$scream.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$scream.brr=stevie.resp$scream.m/base.stevie$scream.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$scream.brr=hope.resp$scream.m/base.hope$scream.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$scream.brr=patats.resp$scream.m/base.patats$scream.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(scream.brr.m = mean(scream.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            scream.brr.sd=sd(scream.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(scream.brr)), # of observations, excluding NAs. 
            scream.brr.se=scream.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=scream.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=scream.brr.m - scream.brr.se, ymax=scream.brr.m + scream.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for scream") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.714, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.714, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(scream.brr.m = mean(scream.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            scream.brr.sd=sd(scream.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(scream.brr)), # of observations, excluding NAs. 
            scream.brr.se=scream.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=scream.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=scream.brr.m - scream.brr.se, ymax=scream.brr.m + scream.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for scream") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0., linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0., linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(scream.brr.m = mean(scream.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            scream.brr.sd=sd(scream.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(scream.brr)), # of observations, excluding NAs. 
            scream.brr.se=scream.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=scream.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=scream.brr.m - scream.brr.se, ymax=scream.brr.m + scream.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for scream") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0., linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0., linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(scream.brr.m = mean(scream.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            scream.brr.sd=sd(scream.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(scream.brr)), # of observations, excluding NAs. 
            scream.brr.se=scream.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=scream.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=scream.brr.m - scream.brr.se, ymax=scream.brr.m + scream.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for scream") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0., linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0., linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))