#New method neutal behaviors
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

################################################################
#Groom self 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomct.m = mean(sgroomct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(sgroomct.mm = mean(sgroomct.m, na.rm = TRUE),
            sgroomct.m.sd=sd(sgroomct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomct.m)), # of observations, excluding NAs. 
            sgroomct.m.se=sgroomct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$sgroomct.m.se/baseline$sgroomct.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental ssgroomct saved after her name so this needs to be
#typed to include the ssgroomct, or remove the ssgroomcts and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it sgroomct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$sgroomct.brr=ayana.resp$sgroomct.m/base.ayana$sgroomct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$sgroomct.brr=stevie.resp$sgroomct.m/base.stevie$sgroomct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$sgroomct.brr=hope.resp$sgroomct.m/base.hope$sgroomct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$sgroomct.brr=patats.resp$sgroomct.m/base.patats$sgroomct.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomct.brr.m = mean(sgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomct.brr.sd=sd(sgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomct.brr)), # of observations, excluding NAs. 
            sgroomct.brr.se=sgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomct.brr.m - sgroomct.brr.se, ymax=sgroomct.brr.m + sgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.468, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.468, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomct.brr.m = mean(sgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomct.brr.sd=sd(sgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomct.brr)), # of observations, excluding NAs. 
            sgroomct.brr.se=sgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomct.brr.m - sgroomct.brr.se, ymax=sgroomct.brr.m + sgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.365, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.365, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomct.brr.m = mean(sgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomct.brr.sd=sd(sgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomct.brr)), # of observations, excluding NAs. 
            sgroomct.brr.se=sgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomct.brr.m - sgroomct.brr.se, ymax=sgroomct.brr.m + sgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.816, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.816, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomct.brr.m = mean(sgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomct.brr.sd=sd(sgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomct.brr)), # of observations, excluding NAs. 
            sgroomct.brr.se=sgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomct.brr.m - sgroomct.brr.se, ymax=sgroomct.brr.m + sgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.353, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.353, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#######################################################################
#Self groom duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomd.m = mean(sgroomd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(sgroomd.mm = mean(sgroomd.m, na.rm = TRUE),
            sgroomd.m.sd=sd(sgroomd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomd.m)), # of observations, excluding NAs. 
            sgroomd.m.se=sgroomd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$sgroomd.m.se/baseline$sgroomd.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental ssgroomd saved after her name so this needs to be
#typed to include the ssgroomd, or remove the ssgroomds and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it sgroomd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$sgroomd.brr=ayana.resp$sgroomd.m/base.ayana$sgroomd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$sgroomd.brr=stevie.resp$sgroomd.m/base.stevie$sgroomd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$sgroomd.brr=hope.resp$sgroomd.m/base.hope$sgroomd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$sgroomd.brr=patats.resp$sgroomd.m/base.patats$sgroomd.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomd.brr.m = mean(sgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomd.brr.sd=sd(sgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomd.brr)), # of observations, excluding NAs. 
            sgroomd.brr.se=sgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomd.brr.m - sgroomd.brr.se, ymax=sgroomd.brr.m + sgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomd") +
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
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomd.brr.m = mean(sgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomd.brr.sd=sd(sgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomd.brr)), # of observations, excluding NAs. 
            sgroomd.brr.se=sgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomd.brr.m - sgroomd.brr.se, ymax=sgroomd.brr.m + sgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.527, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.527, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomd.brr.m = mean(sgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomd.brr.sd=sd(sgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomd.brr)), # of observations, excluding NAs. 
            sgroomd.brr.se=sgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomd.brr.m - sgroomd.brr.se, ymax=sgroomd.brr.m + sgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.852, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.852, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(sgroomd.brr.m = mean(sgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            sgroomd.brr.sd=sd(sgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(sgroomd.brr)), # of observations, excluding NAs. 
            sgroomd.brr.se=sgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=sgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=sgroomd.brr.m - sgroomd.brr.se, ymax=sgroomd.brr.m + sgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for sgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.378, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.378, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##############################################################################
#Eat count 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatct.m = mean(eatct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(eatct.mm = mean(eatct.m, na.rm = TRUE),
            eatct.m.sd=sd(eatct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatct.m)), # of observations, excluding NAs. 
            eatct.m.se=eatct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$eatct.m.se/baseline$eatct.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental seatct saved after her name so this needs to be
#typed to include the seatct, or remove the seatcts and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it eatct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$eatct.brr=ayana.resp$eatct.m/base.ayana$eatct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$eatct.brr=stevie.resp$eatct.m/base.stevie$eatct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$eatct.brr=hope.resp$eatct.m/base.hope$eatct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$eatct.brr=patats.resp$eatct.m/base.patats$eatct.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatct.brr.m = mean(eatct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatct.brr.sd=sd(eatct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatct.brr)), # of observations, excluding NAs. 
            eatct.brr.se=eatct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatct.brr.m - eatct.brr.se, ymax=eatct.brr.m + eatct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.305, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.305, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatct.brr.m = mean(eatct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatct.brr.sd=sd(eatct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatct.brr)), # of observations, excluding NAs. 
            eatct.brr.se=eatct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatct.brr.m - eatct.brr.se, ymax=eatct.brr.m + eatct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.220, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.220, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatct.brr.m = mean(eatct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatct.brr.sd=sd(eatct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatct.brr)), # of observations, excluding NAs. 
            eatct.brr.se=eatct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatct.brr.m - eatct.brr.se, ymax=eatct.brr.m + eatct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.549, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.549, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatct.brr.m = mean(eatct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatct.brr.sd=sd(eatct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatct.brr)), # of observations, excluding NAs. 
            eatct.brr.se=eatct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatct.brr.m - eatct.brr.se, ymax=eatct.brr.m + eatct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.326, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.326, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
############################################################################
#Eat duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatd.m = mean(eatd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(eatd.mm = mean(eatd.m, na.rm = TRUE),
            eatd.m.sd=sd(eatd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatd.m)), # of observations, excluding NAs. 
            eatd.m.se=eatd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$eatd.m.se/baseline$eatd.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental seatd saved after her name so this needs to be
#typed to include the seatd, or remove the seatds and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it eatd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$eatd.brr=ayana.resp$eatd.m/base.ayana$eatd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$eatd.brr=stevie.resp$eatd.m/base.stevie$eatd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$eatd.brr=hope.resp$eatd.m/base.hope$eatd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$eatd.brr=patats.resp$eatd.m/base.patats$eatd.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatd.brr.m = mean(eatd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatd.brr.sd=sd(eatd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatd.brr)), # of observations, excluding NAs. 
            eatd.brr.se=eatd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatd.brr.m - eatd.brr.se, ymax=eatd.brr.m + eatd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.297, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.297, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatd.brr.m = mean(eatd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatd.brr.sd=sd(eatd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatd.brr)), # of observations, excluding NAs. 
            eatd.brr.se=eatd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatd.brr.m - eatd.brr.se, ymax=eatd.brr.m + eatd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatd") +
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
  summarize(eatd.brr.m = mean(eatd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatd.brr.sd=sd(eatd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatd.brr)), # of observations, excluding NAs. 
            eatd.brr.se=eatd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatd.brr.m - eatd.brr.se, ymax=eatd.brr.m + eatd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.409, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.409, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(eatd.brr.m = mean(eatd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            eatd.brr.sd=sd(eatd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(eatd.brr)), # of observations, excluding NAs. 
            eatd.brr.se=eatd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=eatd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=eatd.brr.m - eatd.brr.se, ymax=eatd.brr.m + eatd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for eatd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.291, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.291, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##############################################################################
#Drink
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(drink.m = mean(drink, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(drink.mm = mean(drink.m, na.rm = TRUE),
            drink.m.sd=sd(drink.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(drink.m)), # of observations, excluding NAs. 
            drink.m.se=drink.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$drink.m.se/baseline$drink.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sdrink saved after her name so this needs to be
#typed to include the sdrink, or remove the sdrinks and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it drink.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$drink.brr=ayana.resp$drink.m/base.ayana$drink.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$drink.brr=stevie.resp$drink.m/base.stevie$drink.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$drink.brr=hope.resp$drink.m/base.hope$drink.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$drink.brr=patats.resp$drink.m/base.patats$drink.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(drink.brr.m = mean(drink.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            drink.brr.sd=sd(drink.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(drink.brr)), # of observations, excluding NAs. 
            drink.brr.se=drink.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=drink.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=drink.brr.m - drink.brr.se, ymax=drink.brr.m + drink.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for drink") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.447, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.447, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(drink.brr.m = mean(drink.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            drink.brr.sd=sd(drink.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(drink.brr)), # of observations, excluding NAs. 
            drink.brr.se=drink.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=drink.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=drink.brr.m - drink.brr.se, ymax=drink.brr.m + drink.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for drink") +
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
  summarize(drink.brr.m = mean(drink.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            drink.brr.sd=sd(drink.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(drink.brr)), # of observations, excluding NAs. 
            drink.brr.se=drink.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=drink.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=drink.brr.m - drink.brr.se, ymax=drink.brr.m + drink.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for drink") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.632, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.632, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(drink.brr.m = mean(drink.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            drink.brr.sd=sd(drink.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(drink.brr)), # of observations, excluding NAs. 
            drink.brr.se=drink.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=drink.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=drink.brr.m - drink.brr.se, ymax=drink.brr.m + drink.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for drink") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.683, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.683, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#################################################################################
#Forage count
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foragect.m = mean(foragect, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(foragect.mm = mean(foragect.m, na.rm = TRUE),
            foragect.m.sd=sd(foragect.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foragect.m)), # of observations, excluding NAs. 
            foragect.m.se=foragect.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$foragect.m.se/baseline$foragect.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sforagect saved after her name so this needs to be
#typed to include the sforagect, or remove the sforagects and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it foragect.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$foragect.brr=ayana.resp$foragect.m/base.ayana$foragect.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$foragect.brr=stevie.resp$foragect.m/base.stevie$foragect.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$foragect.brr=hope.resp$foragect.m/base.hope$foragect.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$foragect.brr=patats.resp$foragect.m/base.patats$foragect.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foragect.brr.m = mean(foragect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foragect.brr.sd=sd(foragect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foragect.brr)), # of observations, excluding NAs. 
            foragect.brr.se=foragect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foragect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foragect.brr.m - foragect.brr.se, ymax=foragect.brr.m + foragect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foragect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.323, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.323, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foragect.brr.m = mean(foragect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foragect.brr.sd=sd(foragect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foragect.brr)), # of observations, excluding NAs. 
            foragect.brr.se=foragect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foragect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foragect.brr.m - foragect.brr.se, ymax=foragect.brr.m + foragect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foragect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.203, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.203, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foragect.brr.m = mean(foragect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foragect.brr.sd=sd(foragect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foragect.brr)), # of observations, excluding NAs. 
            foragect.brr.se=foragect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foragect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foragect.brr.m - foragect.brr.se, ymax=foragect.brr.m + foragect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foragect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.372, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.372, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foragect.brr.m = mean(foragect.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foragect.brr.sd=sd(foragect.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foragect.brr)), # of observations, excluding NAs. 
            foragect.brr.se=foragect.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foragect.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foragect.brr.m - foragect.brr.se, ymax=foragect.brr.m + foragect.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foragect") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.270, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.270, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###############################################################################
#FOrage duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foraged.m = mean(foraged, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(foraged.mm = mean(foraged.m, na.rm = TRUE),
            foraged.m.sd=sd(foraged.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foraged.m)), # of observations, excluding NAs. 
            foraged.m.se=foraged.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$foraged.m.se/baseline$foraged.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sforaged saved after her name so this needs to be
#typed to include the sforaged, or remove the sforageds and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it foraged.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$foraged.brr=ayana.resp$foraged.m/base.ayana$foraged.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$foraged.brr=stevie.resp$foraged.m/base.stevie$foraged.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$foraged.brr=hope.resp$foraged.m/base.hope$foraged.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$foraged.brr=patats.resp$foraged.m/base.patats$foraged.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foraged.brr.m = mean(foraged.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foraged.brr.sd=sd(foraged.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foraged.brr)), # of observations, excluding NAs. 
            foraged.brr.se=foraged.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foraged.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foraged.brr.m - foraged.brr.se, ymax=foraged.brr.m + foraged.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foraged") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.354, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.354, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foraged.brr.m = mean(foraged.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foraged.brr.sd=sd(foraged.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foraged.brr)), # of observations, excluding NAs. 
            foraged.brr.se=foraged.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foraged.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foraged.brr.m - foraged.brr.se, ymax=foraged.brr.m + foraged.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foraged") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.264, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.264, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foraged.brr.m = mean(foraged.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foraged.brr.sd=sd(foraged.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foraged.brr)), # of observations, excluding NAs. 
            foraged.brr.se=foraged.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foraged.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foraged.brr.m - foraged.brr.se, ymax=foraged.brr.m + foraged.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foraged") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.415, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.415, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foraged.brr.m = mean(foraged.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foraged.brr.sd=sd(foraged.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foraged.brr)), # of observations, excluding NAs. 
            foraged.brr.se=foraged.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foraged.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foraged.brr.m - foraged.brr.se, ymax=foraged.brr.m + foraged.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foraged") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.285, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.285, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###############################################################################
#Rest count
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restct.m = mean(restct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(restct.mm = mean(restct.m, na.rm = TRUE),
            restct.m.sd=sd(restct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restct.m)), # of observations, excluding NAs. 
            restct.m.se=restct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$restct.m.se/baseline$restct.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental srestct saved after her name so this needs to be
#typed to include the srestct, or remove the srestcts and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it restct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$restct.brr=ayana.resp$restct.m/base.ayana$restct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$restct.brr=stevie.resp$restct.m/base.stevie$restct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$restct.brr=hope.resp$restct.m/base.hope$restct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$restct.brr=patats.resp$restct.m/base.patats$restct.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restct.brr.m = mean(restct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restct.brr.sd=sd(restct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restct.brr)), # of observations, excluding NAs. 
            restct.brr.se=restct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restct.brr.m - restct.brr.se, ymax=restct.brr.m + restct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.140, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.140, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restct.brr.m = mean(restct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restct.brr.sd=sd(restct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restct.brr)), # of observations, excluding NAs. 
            restct.brr.se=restct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restct.brr.m - restct.brr.se, ymax=restct.brr.m + restct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0785, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0785, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restct.brr.m = mean(restct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restct.brr.sd=sd(restct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restct.brr)), # of observations, excluding NAs. 
            restct.brr.se=restct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restct.brr.m - restct.brr.se, ymax=restct.brr.m + restct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.222, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.222, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restct.brr.m = mean(restct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restct.brr.sd=sd(restct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restct.brr)), # of observations, excluding NAs. 
            restct.brr.se=restct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restct.brr.m - restct.brr.se, ymax=restct.brr.m + restct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.212, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.212, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###################################################################################
#Rest duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restd.m = mean(restd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(restd.mm = mean(restd.m, na.rm = TRUE),
            restd.m.sd=sd(restd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restd.m)), # of observations, excluding NAs. 
            restd.m.se=restd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$restd.m.se/baseline$restd.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental srestd saved after her name so this needs to be
#typed to include the srestd, or remove the srestds and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it restd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$restd.brr=ayana.resp$restd.m/base.ayana$restd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$restd.brr=stevie.resp$restd.m/base.stevie$restd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$restd.brr=hope.resp$restd.m/base.hope$restd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$restd.brr=patats.resp$restd.m/base.patats$restd.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restd.brr.m = mean(restd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restd.brr.sd=sd(restd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restd.brr)), # of observations, excluding NAs. 
            restd.brr.se=restd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restd.brr.m - restd.brr.se, ymax=restd.brr.m + restd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0989, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.0989, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restd.brr.m = mean(restd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restd.brr.sd=sd(restd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restd.brr)), # of observations, excluding NAs. 
            restd.brr.se=restd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restd.brr.m - restd.brr.se, ymax=restd.brr.m + restd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0779, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0779, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restd.brr.m = mean(restd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restd.brr.sd=sd(restd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restd.brr)), # of observations, excluding NAs. 
            restd.brr.se=restd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restd.brr.m - restd.brr.se, ymax=restd.brr.m + restd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0806, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.0806, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(restd.brr.m = mean(restd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            restd.brr.sd=sd(restd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(restd.brr)), # of observations, excluding NAs. 
            restd.brr.se=restd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=restd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=restd.brr.m - restd.brr.se, ymax=restd.brr.m + restd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for restd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.262, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.262, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
################################################################################
#Schratch
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(schratch.m = mean(schratch, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(schratch.mm = mean(schratch.m, na.rm = TRUE),
            schratch.m.sd=sd(schratch.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(schratch.m)), # of observations, excluding NAs. 
            schratch.m.se=schratch.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$schratch.m.se/baseline$schratch.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sschratch saved after her name so this needs to be
#typed to include the sschratch, or remove the sschratchs and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it schratch.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$schratch.brr=ayana.resp$schratch.m/base.ayana$schratch.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$schratch.brr=stevie.resp$schratch.m/base.stevie$schratch.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$schratch.brr=hope.resp$schratch.m/base.hope$schratch.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$schratch.brr=patats.resp$schratch.m/base.patats$schratch.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(schratch.brr.m = mean(schratch.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            schratch.brr.sd=sd(schratch.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(schratch.brr)), # of observations, excluding NAs. 
            schratch.brr.se=schratch.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=schratch.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=schratch.brr.m - schratch.brr.se, ymax=schratch.brr.m + schratch.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for schratch") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0697, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.0697, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(schratch.brr.m = mean(schratch.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            schratch.brr.sd=sd(schratch.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(schratch.brr)), # of observations, excluding NAs. 
            schratch.brr.se=schratch.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=schratch.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=schratch.brr.m - schratch.brr.se, ymax=schratch.brr.m + schratch.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for schratch") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0316, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0316, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(schratch.brr.m = mean(schratch.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            schratch.brr.sd=sd(schratch.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(schratch.brr)), # of observations, excluding NAs. 
            schratch.brr.se=schratch.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=schratch.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=schratch.brr.m - schratch.brr.se, ymax=schratch.brr.m + schratch.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for schratch") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.266, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.266, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(schratch.brr.m = mean(schratch.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            schratch.brr.sd=sd(schratch.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(schratch.brr)), # of observations, excluding NAs. 
            schratch.brr.se=schratch.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=schratch.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=schratch.brr.m - schratch.brr.se, ymax=schratch.brr.m + schratch.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for schratch") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.200, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.200, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#################################################################################
#Other
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(other.m = mean(other, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(other.mm = mean(other.m, na.rm = TRUE),
            other.m.sd=sd(other.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(other.m)), # of observations, excluding NAs. 
            other.m.se=other.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$other.m.se/baseline$other.mm
baseline#after seeing what the standard error is by pct, you'll need to manually enter this into7
#the graphic commands below

#subset table x to remove condition 1
responses<-subset(x, cond!=1)

#subset responses to get each individual
ayana.resp<-subset(responses, id=="ayana")
stevie.resp<-subset(responses, id=="stevie")
hope.resp<-subset(responses, id=="hope ")#hope has an accidental sother saved after her name so this needs to be
#typed to include the sother, or remove the sothers and update this
patats.resp<-subset(responses, id=="patats")

#for each baboon, divide the daily average response by the average response in the "baseline" table and
#call it other.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$other.brr=ayana.resp$other.m/base.ayana$other.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$other.brr=stevie.resp$other.m/base.stevie$other.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$other.brr=hope.resp$other.m/base.hope$other.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$other.brr=patats.resp$other.m/base.patats$other.mm

#create a summary table for plotting (for each individual)
#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(other.brr.m = mean(other.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            other.brr.sd=sd(other.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(other.brr)), # of observations, excluding NAs. 
            other.brr.se=other.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=other.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=other.brr.m - other.brr.se, ymax=other.brr.m + other.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for other") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0., linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0., linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(other.brr.m = mean(other.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            other.brr.sd=sd(other.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(other.brr)), # of observations, excluding NAs. 
            other.brr.se=other.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=other.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=other.brr.m - other.brr.se, ymax=other.brr.m + other.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for other") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+1., linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-1., linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(other.brr.m = mean(other.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            other.brr.sd=sd(other.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(other.brr)), # of observations, excluding NAs. 
            other.brr.se=other.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=other.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=other.brr.m - other.brr.se, ymax=other.brr.m + other.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for other") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.632, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.632, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patas
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(other.brr.m = mean(other.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            other.brr.sd=sd(other.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(other.brr)), # of observations, excluding NAs. 
            other.brr.se=other.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=other.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=other.brr.m - other.brr.se, ymax=other.brr.m + other.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for other") +
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