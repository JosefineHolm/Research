#New method Posive behaviors
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
#Food grunt
#making a table for Behavior Response Ratio

#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foodgrunt.m = mean(foodgrunt, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(foodgrunt.mm = mean(foodgrunt.m, na.rm = TRUE),
            foodgrunt.m.sd=sd(foodgrunt.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foodgrunt.m)), # of observations, excluding NAs. 
            foodgrunt.m.se=foodgrunt.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$foodgrunt.m.se/baseline$foodgrunt.mm
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
#call it foodgrunt.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$foodgrunt.brr=ayana.resp$foodgrunt.m/base.ayana$foodgrunt.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$foodgrunt.brr=stevie.resp$foodgrunt.m/base.stevie$foodgrunt.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$foodgrunt.brr=hope.resp$foodgrunt.m/base.hope$foodgrunt.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$foodgrunt.brr=patats.resp$foodgrunt.m/base.patats$foodgrunt.mm

#create a summary table for plotting (for each individual)
#this example for "ayana"...change this variable for each
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foodgrunt.brr.m = mean(foodgrunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foodgrunt.brr.sd=sd(foodgrunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foodgrunt.brr)), # of observations, excluding NAs. 
            foodgrunt.brr.se=foodgrunt.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foodgrunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foodgrunt.brr.m - foodgrunt.brr.se, ymax=foodgrunt.brr.m + foodgrunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foodgrunt") +
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
  summarize(foodgrunt.brr.m = mean(foodgrunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foodgrunt.brr.sd=sd(foodgrunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foodgrunt.brr)), # of observations, excluding NAs. 
            foodgrunt.brr.se=foodgrunt.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foodgrunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foodgrunt.brr.m - foodgrunt.brr.se, ymax=foodgrunt.brr.m + foodgrunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foodgrunt") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.944, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.944, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie 
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foodgrunt.brr.m = mean(foodgrunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foodgrunt.brr.sd=sd(foodgrunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foodgrunt.brr)), # of observations, excluding NAs. 
            foodgrunt.brr.se=foodgrunt.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foodgrunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foodgrunt.brr.m - foodgrunt.brr.se, ymax=foodgrunt.brr.m + foodgrunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foodgrunt") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.537, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.537, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(foodgrunt.brr.m = mean(foodgrunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            foodgrunt.brr.sd=sd(foodgrunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(foodgrunt.brr)), # of observations, excluding NAs. 
            foodgrunt.brr.se=foodgrunt.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=foodgrunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=foodgrunt.brr.m - foodgrunt.brr.se, ymax=foodgrunt.brr.m + foodgrunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for foodgrunt") +
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
####################################################################################
#Grunt
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(grunt.m = mean(grunt, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(grunt.mm = mean(grunt.m, na.rm = TRUE),
            grunt.m.sd=sd(grunt.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt.m)), # of observations, excluding NAs. 
            grunt.m.se=grunt.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$grunt.m.se/baseline$grunt.mm
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
#call it grunt.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$grunt.brr=ayana.resp$grunt.m/base.ayana$grunt.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$grunt.brr=stevie.resp$grunt.m/base.stevie$grunt.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$grunt.brr=hope.resp$grunt.m/base.hope$grunt.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$grunt.brr=patats.resp$grunt.m/base.patats$grunt.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(grunt.brr.m = mean(grunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            grunt.brr.sd=sd(grunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt.brr)), # of observations, excluding NAs. 
            grunt.brr.se=grunt.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=grunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=grunt.brr.m - grunt.brr.se, ymax=grunt.brr.m + grunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for grunt") +
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
  summarize(grunt.brr.m = mean(grunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            grunt.brr.sd=sd(grunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt.brr)), # of observations, excluding NAs. 
            grunt.brr.se=grunt.brr.sd/sqrt(n))

ggplot(data=x.p, aes(x=cond, y=grunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=grunt.brr.m - grunt.brr.se, ymax=grunt.brr.m + grunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for grunt") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0902, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0902, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(grunt.brr.m = mean(grunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            grunt.brr.sd=sd(grunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt.brr)), # of observations, excluding NAs. 
            grunt.brr.se=grunt.brr.sd/sqrt(n))

ggplot(data=x.p, aes(x=cond, y=grunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=grunt.brr.m - grunt.brr.se, ymax=grunt.brr.m + grunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for grunt") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0270, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.0270, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(grunt.brr.m = mean(grunt.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            grunt.brr.sd=sd(grunt.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(grunt.brr)), # of observations, excluding NAs. 
            grunt.brr.se=grunt.brr.sd/sqrt(n))

ggplot(data=x.p, aes(x=cond, y=grunt.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=grunt.brr.m - grunt.brr.se, ymax=grunt.brr.m + grunt.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for grunt") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0274, linetype=2) + #this value is 1+ patasSE from baseline table above
  geom_hline(yintercept = 1-0.0274, linetype=2) + #this value is 1- patasSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
########################################################################################
#Lipsmack 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(lipsmack.m = mean(lipsmack, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(lipsmack.mm = mean(lipsmack.m, na.rm = TRUE),
            lipsmack.m.sd=sd(lipsmack.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(lipsmack.m)), # of observations, excluding NAs. 
            lipsmack.m.se=lipsmack.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$lipsmack.m.se/baseline$lipsmack.mm
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
#call it lipsmack.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$lipsmack.brr=ayana.resp$lipsmack.m/base.ayana$lipsmack.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$lipsmack.brr=stevie.resp$lipsmack.m/base.stevie$lipsmack.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$lipsmack.brr=hope.resp$lipsmack.m/base.hope$lipsmack.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$lipsmack.brr=patats.resp$lipsmack.m/base.patats$lipsmack.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(lipsmack.brr.m = mean(lipsmack.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            lipsmack.brr.sd=sd(lipsmack.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(lipsmack.brr)), # of observations, excluding NAs. 
            lipsmack.brr.se=lipsmack.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=lipsmack.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=lipsmack.brr.m - lipsmack.brr.se, ymax=lipsmack.brr.m + lipsmack.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for lipsmack") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.258, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.258, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(lipsmack.brr.m = mean(lipsmack.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            lipsmack.brr.sd=sd(lipsmack.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(lipsmack.brr)), # of observations, excluding NAs. 
            lipsmack.brr.se=lipsmack.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=lipsmack.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=lipsmack.brr.m - lipsmack.brr.se, ymax=lipsmack.brr.m + lipsmack.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for lipsmack") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.142, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.142, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(lipsmack.brr.m = mean(lipsmack.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            lipsmack.brr.sd=sd(lipsmack.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(lipsmack.brr)), # of observations, excluding NAs. 
            lipsmack.brr.se=lipsmack.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=lipsmack.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=lipsmack.brr.m - lipsmack.brr.se, ymax=lipsmack.brr.m + lipsmack.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for lipsmack") +
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
  summarize(lipsmack.brr.m = mean(lipsmack.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            lipsmack.brr.sd=sd(lipsmack.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(lipsmack.brr)), # of observations, excluding NAs. 
            lipsmack.brr.se=lipsmack.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=lipsmack.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=lipsmack.brr.m - lipsmack.brr.se, ymax=lipsmack.brr.m + lipsmack.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for lipsmack") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.215, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.215, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Presentations to baboon
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pbaboon.m = mean(pbaboon, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(pbaboon.mm = mean(pbaboon.m, na.rm = TRUE),
            pbaboon.m.sd=sd(pbaboon.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pbaboon.m)), # of observations, excluding NAs. 
            pbaboon.m.se=pbaboon.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$pbaboon.m.se/baseline$pbaboon.mm
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
#call it pbaboon.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$pbaboon.brr=ayana.resp$pbaboon.m/base.ayana$pbaboon.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$pbaboon.brr=stevie.resp$pbaboon.m/base.stevie$pbaboon.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$pbaboon.brr=hope.resp$pbaboon.m/base.hope$pbaboon.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$pbaboon.brr=patats.resp$pbaboon.m/base.patats$pbaboon.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pbaboon.brr.m = mean(pbaboon.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pbaboon.brr.sd=sd(pbaboon.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pbaboon.brr)), # of observations, excluding NAs. 
            pbaboon.brr.se=pbaboon.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pbaboon.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pbaboon.brr.m - pbaboon.brr.se, ymax=pbaboon.brr.m + pbaboon.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pbaboon") +
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
  summarize(pbaboon.brr.m = mean(pbaboon.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pbaboon.brr.sd=sd(pbaboon.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pbaboon.brr)), # of observations, excluding NAs. 
            pbaboon.brr.se=pbaboon.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pbaboon.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pbaboon.brr.m - pbaboon.brr.se, ymax=pbaboon.brr.m + pbaboon.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pbaboon") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.231, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.231, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pbaboon.brr.m = mean(pbaboon.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pbaboon.brr.sd=sd(pbaboon.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pbaboon.brr)), # of observations, excluding NAs. 
            pbaboon.brr.se=pbaboon.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pbaboon.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pbaboon.brr.m - pbaboon.brr.se, ymax=pbaboon.brr.m + pbaboon.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pbaboon") +
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
  summarize(pbaboon.brr.m = mean(pbaboon.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pbaboon.brr.sd=sd(pbaboon.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pbaboon.brr)), # of observations, excluding NAs. 
            pbaboon.brr.se=pbaboon.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=pbaboon.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=pbaboon.brr.m - pbaboon.brr.se, ymax=pbaboon.brr.m + pbaboon.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for pbaboon") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.447, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.447, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#####################################################################################
#Presentation to humans
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(phuman.m = mean(phuman, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(phuman.mm = mean(phuman.m, na.rm = TRUE),
            phuman.m.sd=sd(phuman.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(phuman.m)), # of observations, excluding NAs. 
            phuman.m.se=phuman.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$phuman.m.se/baseline$phuman.mm
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
#call it phuman.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$phuman.brr=ayana.resp$phuman.m/base.ayana$phuman.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$phuman.brr=stevie.resp$phuman.m/base.stevie$phuman.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$phuman.brr=hope.resp$phuman.m/base.hope$phuman.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$phuman.brr=patats.resp$phuman.m/base.patats$phuman.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(phuman.brr.m = mean(phuman.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            phuman.brr.sd=sd(phuman.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(phuman.brr)), # of observations, excluding NAs. 
            phuman.brr.se=phuman.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=phuman.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=phuman.brr.m - phuman.brr.se, ymax=phuman.brr.m + phuman.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for phuman") +
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
  summarize(phuman.brr.m = mean(phuman.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            phuman.brr.sd=sd(phuman.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(phuman.brr)), # of observations, excluding NAs. 
            phuman.brr.se=phuman.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=phuman.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=phuman.brr.m - phuman.brr.se, ymax=phuman.brr.m + phuman.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for phuman") +
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
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(phuman.brr.m = mean(phuman.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            phuman.brr.sd=sd(phuman.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(phuman.brr)), # of observations, excluding NAs. 
            phuman.brr.se=phuman.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=phuman.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=phuman.brr.m - phuman.brr.se, ymax=phuman.brr.m + phuman.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for phuman") +
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
  summarize(phuman.brr.m = mean(phuman.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            phuman.brr.sd=sd(phuman.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(phuman.brr)), # of observations, excluding NAs. 
            phuman.brr.se=phuman.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=phuman.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=phuman.brr.m - phuman.brr.se, ymax=phuman.brr.m + phuman.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for phuman") +
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
####################################################################################
#human groom count 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(hgroomct.m = mean(hgroomct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(hgroomct.mm = mean(hgroomct.m, na.rm = TRUE),
            hgroomct.m.sd=sd(hgroomct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomct.m)), # of observations, excluding NAs. 
            hgroomct.m.se=hgroomct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$hgroomct.m.se/baseline$hgroomct.mm
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
#call it hgroomct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$hgroomct.brr=ayana.resp$hgroomct.m/base.ayana$hgroomct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$hgroomct.brr=stevie.resp$hgroomct.m/base.stevie$hgroomct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$hgroomct.brr=hope.resp$hgroomct.m/base.hope$hgroomct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$hgroomct.brr=patats.resp$hgroomct.m/base.patats$hgroomct.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(hgroomct.brr.m = mean(hgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            hgroomct.brr.sd=sd(hgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomct.brr)), # of observations, excluding NAs. 
            hgroomct.brr.se=hgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=hgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=hgroomct.brr.m - hgroomct.brr.se, ymax=hgroomct.brr.m + hgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for hgroomct") +
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
  summarize(hgroomct.brr.m = mean(hgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            hgroomct.brr.sd=sd(hgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomct.brr)), # of observations, excluding NAs. 
            hgroomct.brr.se=hgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=hgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=hgroomct.brr.m - hgroomct.brr.se, ymax=hgroomct.brr.m + hgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for hgroomct") +
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
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(hgroomct.brr.m = mean(hgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            hgroomct.brr.sd=sd(hgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomct.brr)), # of observations, excluding NAs. 
            hgroomct.brr.se=hgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=hgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=hgroomct.brr.m - hgroomct.brr.se, ymax=hgroomct.brr.m + hgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for hgroomct") +
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
#Human groom duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(hgroomd.m = mean(hgroomd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(hgroomd.mm = mean(hgroomd.m, na.rm = TRUE),
            hgroomd.m.sd=sd(hgroomd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomd.m)), # of observations, excluding NAs. 
            hgroomd.m.se=hgroomd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$hgroomd.m.se/baseline$hgroomd.mm
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
#call it hgroomd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$hgroomd.brr=ayana.resp$hgroomd.m/base.ayana$hgroomd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$hgroomd.brr=stevie.resp$hgroomd.m/base.stevie$hgroomd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$hgroomd.brr=hope.resp$hgroomd.m/base.hope$hgroomd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$hgroomd.brr=patats.resp$hgroomd.m/base.patats$hgroomd.mm

#patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(hgroomd.brr.m = mean(hgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            hgroomd.brr.sd=sd(hgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(hgroomd.brr)), # of observations, excluding NAs. 
            hgroomd.brr.se=hgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=hgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=hgroomd.brr.m - hgroomd.brr.se, ymax=hgroomd.brr.m + hgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for hgroomd") +
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
###############################################################################
#Baboon groom count 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomct.m = mean(bgroomct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(bgroomct.mm = mean(bgroomct.m, na.rm = TRUE),
            bgroomct.m.sd=sd(bgroomct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomct.m)), # of observations, excluding NAs. 
            bgroomct.m.se=bgroomct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$bgroomct.m.se/baseline$bgroomct.mm
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
#call it bgroomct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$bgroomct.brr=ayana.resp$bgroomct.m/base.ayana$bgroomct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$bgroomct.brr=stevie.resp$bgroomct.m/base.stevie$bgroomct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$bgroomct.brr=hope.resp$bgroomct.m/base.hope$bgroomct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$bgroomct.brr=patats.resp$bgroomct.m/base.patats$bgroomct.mm

#ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomct.brr.m = mean(bgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomct.brr.sd=sd(bgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomct.brr)), # of observations, excluding NAs. 
            bgroomct.brr.se=bgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomct.brr.m - bgroomct.brr.se, ymax=bgroomct.brr.m + bgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.302, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.302, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomct.brr.m = mean(bgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomct.brr.sd=sd(bgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomct.brr)), # of observations, excluding NAs. 
            bgroomct.brr.se=bgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomct.brr.m - bgroomct.brr.se, ymax=bgroomct.brr.m + bgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.286, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.286, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevies
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomct.brr.m = mean(bgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomct.brr.sd=sd(bgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomct.brr)), # of observations, excluding NAs. 
            bgroomct.brr.se=bgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomct.brr.m - bgroomct.brr.se, ymax=bgroomct.brr.m + bgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.344, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.344, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomct.brr.m = mean(bgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomct.brr.sd=sd(bgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomct.brr)), # of observations, excluding NAs. 
            bgroomct.brr.se=bgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomct.brr.m - bgroomct.brr.se, ymax=bgroomct.brr.m + bgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.409, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.409, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###############################################################################
#Baboon groom duration
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomd.m = mean(bgroomd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(bgroomd.mm = mean(bgroomd.m, na.rm = TRUE),
            bgroomd.m.sd=sd(bgroomd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomd.m)), # of observations, excluding NAs. 
            bgroomd.m.se=bgroomd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$bgroomd.m.se/baseline$bgroomd.mm
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
#call it bgroomd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$bgroomd.brr=ayana.resp$bgroomd.m/base.ayana$bgroomd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$bgroomd.brr=stevie.resp$bgroomd.m/base.stevie$bgroomd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$bgroomd.brr=hope.resp$bgroomd.m/base.hope$bgroomd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$bgroomd.brr=patats.resp$bgroomd.m/base.patats$bgroomd.mm

#ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomd.brr.m = mean(bgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomd.brr.sd=sd(bgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomd.brr)), # of observations, excluding NAs. 
            bgroomd.brr.se=bgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomd.brr.m - bgroomd.brr.se, ymax=bgroomd.brr.m + bgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.332, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.332, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomd.brr.m = mean(bgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomd.brr.sd=sd(bgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomd.brr)), # of observations, excluding NAs. 
            bgroomd.brr.se=bgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomd.brr.m - bgroomd.brr.se, ymax=bgroomd.brr.m + bgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.536, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.536, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie 
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomd.brr.m = mean(bgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomd.brr.sd=sd(bgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomd.brr)), # of observations, excluding NAs. 
            bgroomd.brr.se=bgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomd.brr.m - bgroomd.brr.se, ymax=bgroomd.brr.m + bgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.514, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.514, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(bgroomd.brr.m = mean(bgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            bgroomd.brr.sd=sd(bgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(bgroomd.brr)), # of observations, excluding NAs. 
            bgroomd.brr.se=bgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=bgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=bgroomd.brr.m - bgroomd.brr.se, ymax=bgroomd.brr.m + bgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for bgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.762, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.762, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###############################################################################
#Give human groom count
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(ghgroomct.m = mean(ghgroomct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(ghgroomct.mm = mean(ghgroomct.m, na.rm = TRUE),
            ghgroomct.m.sd=sd(ghgroomct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ghgroomct.m)), # of observations, excluding NAs. 
            ghgroomct.m.se=ghgroomct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$ghgroomct.m.se/baseline$ghgroomct.mm
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
#call it ghgroomct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$ghgroomct.brr=ayana.resp$ghgroomct.m/base.ayana$ghgroomct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$ghgroomct.brr=stevie.resp$ghgroomct.m/base.stevie$ghgroomct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$ghgroomct.brr=hope.resp$ghgroomct.m/base.hope$ghgroomct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$ghgroomct.brr=patats.resp$ghgroomct.m/base.patats$ghgroomct.mm

#only patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(ghgroomct.brr.m = mean(ghgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            ghgroomct.brr.sd=sd(ghgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ghgroomct.brr)), # of observations, excluding NAs. 
            ghgroomct.brr.se=ghgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=ghgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=ghgroomct.brr.m - ghgroomct.brr.se, ymax=ghgroomct.brr.m + ghgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for ghgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
###################################################################################
#Give human groom duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(ghgroomd.m = mean(ghgroomd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(ghgroomd.mm = mean(ghgroomd.m, na.rm = TRUE),
            ghgroomd.m.sd=sd(ghgroomd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ghgroomd.m)), # of observations, excluding NAs. 
            ghgroomd.m.se=ghgroomd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$ghgroomd.m.se/baseline$ghgroomd.mm
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
#call it ghgroomd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$ghgroomd.brr=ayana.resp$ghgroomd.m/base.ayana$ghgroomd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$ghgroomd.brr=stevie.resp$ghgroomd.m/base.stevie$ghgroomd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$ghgroomd.brr=hope.resp$ghgroomd.m/base.hope$ghgroomd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$ghgroomd.brr=patats.resp$ghgroomd.m/base.patats$ghgroomd.mm

#only patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(ghgroomd.brr.m = mean(ghgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            ghgroomd.brr.sd=sd(ghgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ghgroomd.brr)), # of observations, excluding NAs. 
            ghgroomd.brr.se=ghgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=ghgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=ghgroomd.brr.m - ghgroomd.brr.se, ymax=ghgroomd.brr.m + ghgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for ghgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
################################################################################
#Give baboon groom count
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomct.m = mean(gbgroomct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(gbgroomct.mm = mean(gbgroomct.m, na.rm = TRUE),
            gbgroomct.m.sd=sd(gbgroomct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomct.m)), # of observations, excluding NAs. 
            gbgroomct.m.se=gbgroomct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$gbgroomct.m.se/baseline$gbgroomct.mm
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
#call it gbgroomct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$gbgroomct.brr=ayana.resp$gbgroomct.m/base.ayana$gbgroomct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$gbgroomct.brr=stevie.resp$gbgroomct.m/base.stevie$gbgroomct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$gbgroomct.brr=hope.resp$gbgroomct.m/base.hope$gbgroomct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$gbgroomct.brr=patats.resp$gbgroomct.m/base.patats$gbgroomct.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomct.brr.m = mean(gbgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomct.brr.sd=sd(gbgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomct.brr)), # of observations, excluding NAs. 
            gbgroomct.brr.se=gbgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomct.brr.m - gbgroomct.brr.se, ymax=gbgroomct.brr.m + gbgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.473, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.473, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomct.brr.m = mean(gbgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomct.brr.sd=sd(gbgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomct.brr)), # of observations, excluding NAs. 
            gbgroomct.brr.se=gbgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomct.brr.m - gbgroomct.brr.se, ymax=gbgroomct.brr.m + gbgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.212, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.212, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie 
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomct.brr.m = mean(gbgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomct.brr.sd=sd(gbgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomct.brr)), # of observations, excluding NAs. 
            gbgroomct.brr.se=gbgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomct.brr.m - gbgroomct.brr.se, ymax=gbgroomct.brr.m + gbgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomct") +
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
  summarize(gbgroomct.brr.m = mean(gbgroomct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomct.brr.sd=sd(gbgroomct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomct.brr)), # of observations, excluding NAs. 
            gbgroomct.brr.se=gbgroomct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomct.brr.m - gbgroomct.brr.se, ymax=gbgroomct.brr.m + gbgroomct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.335, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.335, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##############################################################################
#Give baboon groom duration 
#calculate mean
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomd.m = mean(gbgroomd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(gbgroomd.mm = mean(gbgroomd.m, na.rm = TRUE),
            gbgroomd.m.sd=sd(gbgroomd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomd.m)), # of observations, excluding NAs. 
            gbgroomd.m.se=gbgroomd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$gbgroomd.m.se/baseline$gbgroomd.mm
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
#call it gbgroomd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$gbgroomd.brr=ayana.resp$gbgroomd.m/base.ayana$gbgroomd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$gbgroomd.brr=stevie.resp$gbgroomd.m/base.stevie$gbgroomd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$gbgroomd.brr=hope.resp$gbgroomd.m/base.hope$gbgroomd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$gbgroomd.brr=patats.resp$gbgroomd.m/base.patats$gbgroomd.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomd.brr.m = mean(gbgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomd.brr.sd=sd(gbgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomd.brr)), # of observations, excluding NAs. 
            gbgroomd.brr.se=gbgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomd.brr.m - gbgroomd.brr.se, ymax=gbgroomd.brr.m + gbgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.400, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.400, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomd.brr.m = mean(gbgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomd.brr.sd=sd(gbgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomd.brr)), # of observations, excluding NAs. 
            gbgroomd.brr.se=gbgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomd.brr.m - gbgroomd.brr.se, ymax=gbgroomd.brr.m + gbgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.190, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.190, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#STevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomd.brr.m = mean(gbgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomd.brr.sd=sd(gbgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomd.brr)), # of observations, excluding NAs. 
            gbgroomd.brr.se=gbgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomd.brr.m - gbgroomd.brr.se, ymax=gbgroomd.brr.m + gbgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomd") +
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
#patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gbgroomd.brr.m = mean(gbgroomd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gbgroomd.brr.sd=sd(gbgroomd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gbgroomd.brr)), # of observations, excluding NAs. 
            gbgroomd.brr.se=gbgroomd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=gbgroomd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gbgroomd.brr.m - gbgroomd.brr.se, ymax=gbgroomd.brr.m + gbgroomd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for gbgroomd") +
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
##############################################################################
#Penisdisplay
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(penisd.m = mean(penisd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(penisd.mm = mean(penisd.m, na.rm = TRUE),
            penisd.m.sd=sd(penisd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(penisd.m)), # of observations, excluding NAs. 
            penisd.m.se=penisd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$penisd.m.se/baseline$penisd.mm
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
#call it penisd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$penisd.brr=ayana.resp$penisd.m/base.ayana$penisd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$penisd.brr=stevie.resp$penisd.m/base.stevie$penisd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$penisd.brr=hope.resp$penisd.m/base.hope$penisd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$penisd.brr=patats.resp$penisd.m/base.patats$penisd.mm

#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(penisd.brr.m = mean(penisd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            penisd.brr.sd=sd(penisd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(penisd.brr)), # of observations, excluding NAs. 
            penisd.brr.se=penisd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=penisd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=penisd.brr.m - penisd.brr.se, ymax=penisd.brr.m + penisd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for penisd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.182, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.182, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#########################################################################
#Play baboon
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playb.m = mean(playb, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(playb.mm = mean(playb.m, na.rm = TRUE),
            playb.m.sd=sd(playb.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb.m)), # of observations, excluding NAs. 
            playb.m.se=playb.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$playb.m.se/baseline$playb.mm
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
#call it playb.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$playb.brr=ayana.resp$playb.m/base.ayana$playb.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$playb.brr=stevie.resp$playb.m/base.stevie$playb.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$playb.brr=hope.resp$playb.m/base.hope$playb.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$playb.brr=patats.resp$playb.m/base.patats$playb.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playb.brr.m = mean(playb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playb.brr.sd=sd(playb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb.brr)), # of observations, excluding NAs. 
            playb.brr.se=playb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playb.brr.m - playb.brr.se, ymax=playb.brr.m + playb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playb.brr.m = mean(playb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playb.brr.sd=sd(playb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb.brr)), # of observations, excluding NAs. 
            playb.brr.se=playb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playb.brr.m - playb.brr.se, ymax=playb.brr.m + playb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playb.brr.m = mean(playb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playb.brr.sd=sd(playb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb.brr)), # of observations, excluding NAs. 
            playb.brr.se=playb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playb.brr.m - playb.brr.se, ymax=playb.brr.m + playb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playb.brr.m = mean(playb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playb.brr.sd=sd(playb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playb.brr)), # of observations, excluding NAs. 
            playb.brr.se=playb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playb.brr.m - playb.brr.se, ymax=playb.brr.m + playb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playb") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Play self 
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(plays.m = mean(plays, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(plays.mm = mean(plays.m, na.rm = TRUE),
            plays.m.sd=sd(plays.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays.m)), # of observations, excluding NAs. 
            plays.m.se=plays.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$plays.m.se/baseline$plays.mm
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
#call it plays.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$plays.brr=ayana.resp$plays.m/base.ayana$plays.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$plays.brr=stevie.resp$plays.m/base.stevie$plays.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$plays.brr=hope.resp$plays.m/base.hope$plays.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$plays.brr=patats.resp$plays.m/base.patats$plays.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(plays.brr.m = mean(plays.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            plays.brr.sd=sd(plays.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays.brr)), # of observations, excluding NAs. 
            plays.brr.se=plays.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=plays.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=plays.brr.m - plays.brr.se, ymax=plays.brr.m + plays.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for plays") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ ayanaSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- ayanaSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Hope
x.p <- group_by(hope.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(plays.brr.m = mean(plays.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            plays.brr.sd=sd(plays.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays.brr)), # of observations, excluding NAs. 
            plays.brr.se=plays.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=plays.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=plays.brr.m - plays.brr.se, ymax=plays.brr.m + plays.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for plays") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.0, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.0, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(plays.brr.m = mean(plays.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            plays.brr.sd=sd(plays.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays.brr)), # of observations, excluding NAs. 
            plays.brr.se=plays.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=plays.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=plays.brr.m - plays.brr.se, ymax=plays.brr.m + plays.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for plays") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.301, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.301, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(plays.brr.m = mean(plays.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            plays.brr.sd=sd(plays.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays.brr)), # of observations, excluding NAs. 
            plays.brr.se=plays.brr.sd/sqrt(n))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(plays.brr.m = mean(plays.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            plays.brr.sd=sd(plays.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(plays.brr)), # of observations, excluding NAs. 
            plays.brr.se=plays.brr.sd/sqrt(n))
#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=plays.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=plays.brr.m - plays.brr.se, ymax=plays.brr.m + plays.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for plays") +
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
#################################################################################
#Manipulate enrichment count 
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(manenrichct.m = mean(manenrichct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(manenrichct.mm = mean(manenrichct.m, na.rm = TRUE),
            manenrichct.m.sd=sd(manenrichct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct.m)), # of observations, excluding NAs. 
            manenrichct.m.se=manenrichct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$manenrichct.m.se/baseline$manenrichct.mm
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
#call it manenrichct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$manenrichct.brr=ayana.resp$manenrichct.m/base.ayana$manenrichct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$manenrichct.brr=stevie.resp$manenrichct.m/base.stevie$manenrichct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$manenrichct.brr=hope.resp$manenrichct.m/base.hope$manenrichct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$manenrichct.brr=patats.resp$manenrichct.m/base.patats$manenrichct.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(manenrichct.brr.m = mean(manenrichct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichct.brr.sd=sd(manenrichct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct.brr)), # of observations, excluding NAs. 
            manenrichct.brr.se=manenrichct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichct.brr.m - manenrichct.brr.se, ymax=manenrichct.brr.m + manenrichct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichct") +
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
  summarize(manenrichct.brr.m = mean(manenrichct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichct.brr.sd=sd(manenrichct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct.brr)), # of observations, excluding NAs. 
            manenrichct.brr.se=manenrichct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichct.brr.m - manenrichct.brr.se, ymax=manenrichct.brr.m + manenrichct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichct") +
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
  summarize(manenrichct.brr.m = mean(manenrichct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichct.brr.sd=sd(manenrichct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct.brr)), # of observations, excluding NAs. 
            manenrichct.brr.se=manenrichct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichct.brr.m - manenrichct.brr.se, ymax=manenrichct.brr.m + manenrichct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.651, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.651, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(manenrichct.brr.m = mean(manenrichct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichct.brr.sd=sd(manenrichct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichct.brr)), # of observations, excluding NAs. 
            manenrichct.brr.se=manenrichct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichct.brr.m - manenrichct.brr.se, ymax=manenrichct.brr.m + manenrichct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.714, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.714, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#############################################################################
#Manipulate enrichment duration 
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(manenrichd.m = mean(manenrichd, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(manenrichd.mm = mean(manenrichd.m, na.rm = TRUE),
            manenrichd.m.sd=sd(manenrichd.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd.m)), # of observations, excluding NAs. 
            manenrichd.m.se=manenrichd.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$manenrichd.m.se/baseline$manenrichd.mm
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
#call it manenrichd.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$manenrichd.brr=ayana.resp$manenrichd.m/base.ayana$manenrichd.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$manenrichd.brr=stevie.resp$manenrichd.m/base.stevie$manenrichd.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$manenrichd.brr=hope.resp$manenrichd.m/base.hope$manenrichd.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$manenrichd.brr=patats.resp$manenrichd.m/base.patats$manenrichd.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(manenrichd.brr.m = mean(manenrichd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichd.brr.sd=sd(manenrichd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd.brr)), # of observations, excluding NAs. 
            manenrichd.brr.se=manenrichd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichd.brr.m - manenrichd.brr.se, ymax=manenrichd.brr.m + manenrichd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichd") +
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
  summarize(manenrichd.brr.m = mean(manenrichd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichd.brr.sd=sd(manenrichd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd.brr)), # of observations, excluding NAs. 
            manenrichd.brr.se=manenrichd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichd.brr.m - manenrichd.brr.se, ymax=manenrichd.brr.m + manenrichd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichd") +
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
  summarize(manenrichd.brr.m = mean(manenrichd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichd.brr.sd=sd(manenrichd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd.brr)), # of observations, excluding NAs. 
            manenrichd.brr.se=manenrichd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichd.brr.m - manenrichd.brr.se, ymax=manenrichd.brr.m + manenrichd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.961, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.961, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(manenrichd.brr.m = mean(manenrichd.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            manenrichd.brr.sd=sd(manenrichd.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(manenrichd.brr)), # of observations, excluding NAs. 
            manenrichd.brr.se=manenrichd.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=manenrichd.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=manenrichd.brr.m - manenrichd.brr.se, ymax=manenrichd.brr.m + manenrichd.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for manenrichd") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.869, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.869, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##################################################################################
#Embrace Human
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(embraceh.m = mean(embraceh, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(embraceh.mm = mean(embraceh.m, na.rm = TRUE),
            embraceh.m.sd=sd(embraceh.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceh.m)), # of observations, excluding NAs. 
            embraceh.m.se=embraceh.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$embraceh.m.se/baseline$embraceh.mm
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
#call it embraceh.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$embraceh.brr=ayana.resp$embraceh.m/base.ayana$embraceh.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$embraceh.brr=stevie.resp$embraceh.m/base.stevie$embraceh.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$embraceh.brr=hope.resp$embraceh.m/base.hope$embraceh.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$embraceh.brr=patats.resp$embraceh.m/base.patats$embraceh.mm

#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(embraceh.brr.m = mean(embraceh.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            embraceh.brr.sd=sd(embraceh.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceh.brr)), # of observations, excluding NAs. 
            embraceh.brr.se=embraceh.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=embraceh.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=embraceh.brr.m - embraceh.brr.se, ymax=embraceh.brr.m + embraceh.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for embraceh") +
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
##############################################################################
#Embrace baboon 
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(embraceb.m = mean(embraceb, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(embraceb.mm = mean(embraceb.m, na.rm = TRUE),
            embraceb.m.sd=sd(embraceb.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceb.m)), # of observations, excluding NAs. 
            embraceb.m.se=embraceb.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$embraceb.m.se/baseline$embraceb.mm
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
#call it embraceb.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$embraceb.brr=ayana.resp$embraceb.m/base.ayana$embraceb.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$embraceb.brr=stevie.resp$embraceb.m/base.stevie$embraceb.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$embraceb.brr=hope.resp$embraceb.m/base.hope$embraceb.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$embraceb.brr=patats.resp$embraceb.m/base.patats$embraceb.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(embraceb.brr.m = mean(embraceb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            embraceb.brr.sd=sd(embraceb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceb.brr)), # of observations, excluding NAs. 
            embraceb.brr.se=embraceb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=embraceb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=embraceb.brr.m - embraceb.brr.se, ymax=embraceb.brr.m + embraceb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for embraceb") +
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
  summarize(embraceb.brr.m = mean(embraceb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            embraceb.brr.sd=sd(embraceb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceb.brr)), # of observations, excluding NAs. 
            embraceb.brr.se=embraceb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=embraceb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=embraceb.brr.m - embraceb.brr.se, ymax=embraceb.brr.m + embraceb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for embraceb") +
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
  summarize(embraceb.brr.m = mean(embraceb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            embraceb.brr.sd=sd(embraceb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceb.brr)), # of observations, excluding NAs. 
            embraceb.brr.se=embraceb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=embraceb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=embraceb.brr.m - embraceb.brr.se, ymax=embraceb.brr.m + embraceb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for embraceb") +
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
  summarize(embraceb.brr.m = mean(embraceb.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            embraceb.brr.sd=sd(embraceb.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(embraceb.brr)), # of observations, excluding NAs. 
            embraceb.brr.se=embraceb.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=embraceb.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=embraceb.brr.m - embraceb.brr.se, ymax=embraceb.brr.m + embraceb.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for embraceb") +
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
###################################################################################
#Decrease proximity caretaker 
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(dproxct.m = mean(dproxct, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(dproxct.mm = mean(dproxct.m, na.rm = TRUE),
            dproxct.m.sd=sd(dproxct.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct.m)), # of observations, excluding NAs. 
            dproxct.m.se=dproxct.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$dproxct.m.se/baseline$dproxct.mm
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
#call it dproxct.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$dproxct.brr=ayana.resp$dproxct.m/base.ayana$dproxct.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$dproxct.brr=stevie.resp$dproxct.m/base.stevie$dproxct.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$dproxct.brr=hope.resp$dproxct.m/base.hope$dproxct.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$dproxct.brr=patats.resp$dproxct.m/base.patats$dproxct.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(dproxct.brr.m = mean(dproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxct.brr.sd=sd(dproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct.brr)), # of observations, excluding NAs. 
            dproxct.brr.se=dproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxct.brr.m - dproxct.brr.se, ymax=dproxct.brr.m + dproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxct") +
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
  summarize(dproxct.brr.m = mean(dproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxct.brr.sd=sd(dproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct.brr)), # of observations, excluding NAs. 
            dproxct.brr.se=dproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxct.brr.m - dproxct.brr.se, ymax=dproxct.brr.m + dproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxct") +
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
  summarize(dproxct.brr.m = mean(dproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxct.brr.sd=sd(dproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct.brr)), # of observations, excluding NAs. 
            dproxct.brr.se=dproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxct.brr.m - dproxct.brr.se, ymax=dproxct.brr.m + dproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.745, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.745, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(dproxct.brr.m = mean(dproxct.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxct.brr.sd=sd(dproxct.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxct.brr)), # of observations, excluding NAs. 
            dproxct.brr.se=dproxct.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxct.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxct.brr.m - dproxct.brr.se, ymax=dproxct.brr.m + dproxct.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxct") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.742, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.742, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#############################################################################
#Decrease proximity outside 
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(dproxout.m = mean(dproxout, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(dproxout.mm = mean(dproxout.m, na.rm = TRUE),
            dproxout.m.sd=sd(dproxout.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxout.m)), # of observations, excluding NAs. 
            dproxout.m.se=dproxout.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$dproxout.m.se/baseline$dproxout.mm
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
#call it dproxout.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$dproxout.brr=ayana.resp$dproxout.m/base.ayana$dproxout.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$dproxout.brr=stevie.resp$dproxout.m/base.stevie$dproxout.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$dproxout.brr=hope.resp$dproxout.m/base.hope$dproxout.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$dproxout.brr=patats.resp$dproxout.m/base.patats$dproxout.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(dproxout.brr.m = mean(dproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxout.brr.sd=sd(dproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxout.brr)), # of observations, excluding NAs. 
            dproxout.brr.se=dproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxout.brr.m - dproxout.brr.se, ymax=dproxout.brr.m + dproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxout") +
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
  summarize(dproxout.brr.m = mean(dproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxout.brr.sd=sd(dproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxout.brr)), # of observations, excluding NAs. 
            dproxout.brr.se=dproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxout.brr.m - dproxout.brr.se, ymax=dproxout.brr.m + dproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxout") +
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
  summarize(dproxout.brr.m = mean(dproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxout.brr.sd=sd(dproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxout.brr)), # of observations, excluding NAs. 
            dproxout.brr.se=dproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxout.brr.m - dproxout.brr.se, ymax=dproxout.brr.m + dproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxout") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.527, linetype=2) + #this value is 1+ stevieSE from baseline table above
  geom_hline(yintercept = 1-0.527, linetype=2) + #this value is 1- stevieSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Patats
x.p <- group_by(patats.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(dproxout.brr.m = mean(dproxout.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            dproxout.brr.sd=sd(dproxout.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(dproxout.brr)), # of observations, excluding NAs. 
            dproxout.brr.se=dproxout.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=dproxout.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=dproxout.brr.m - dproxout.brr.se, ymax=dproxout.brr.m + dproxout.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for dproxout") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.527, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.527, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##############################################################################
#Move to observer
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(movetoobs.m = mean(movetoobs, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(movetoobs.mm = mean(movetoobs.m, na.rm = TRUE),
            movetoobs.m.sd=sd(movetoobs.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(movetoobs.m)), # of observations, excluding NAs. 
            movetoobs.m.se=movetoobs.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$movetoobs.m.se/baseline$movetoobs.mm
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
#call it movetoobs.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$movetoobs.brr=ayana.resp$movetoobs.m/base.ayana$movetoobs.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$movetoobs.brr=stevie.resp$movetoobs.m/base.stevie$movetoobs.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$movetoobs.brr=hope.resp$movetoobs.m/base.hope$movetoobs.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$movetoobs.brr=patats.resp$movetoobs.m/base.patats$movetoobs.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(movetoobs.brr.m = mean(movetoobs.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            movetoobs.brr.sd=sd(movetoobs.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(movetoobs.brr)), # of observations, excluding NAs. 
            movetoobs.brr.se=movetoobs.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=movetoobs.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=movetoobs.brr.m - movetoobs.brr.se, ymax=movetoobs.brr.m + movetoobs.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for movetoobs") +
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
  summarize(movetoobs.brr.m = mean(movetoobs.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            movetoobs.brr.sd=sd(movetoobs.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(movetoobs.brr)), # of observations, excluding NAs. 
            movetoobs.brr.se=movetoobs.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=movetoobs.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=movetoobs.brr.m - movetoobs.brr.se, ymax=movetoobs.brr.m + movetoobs.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for movetoobs") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.632, linetype=2) + #this value is 1+ hopeSE from baseline table above
  geom_hline(yintercept = 1-0.632, linetype=2) + #this value is 1- hopeSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
#Stevie
x.p <- group_by(stevie.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(movetoobs.brr.m = mean(movetoobs.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            movetoobs.brr.sd=sd(movetoobs.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(movetoobs.brr)), # of observations, excluding NAs. 
            movetoobs.brr.se=movetoobs.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=movetoobs.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=movetoobs.brr.m - movetoobs.brr.se, ymax=movetoobs.brr.m + movetoobs.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for movetoobs") +
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
  summarize(movetoobs.brr.m = mean(movetoobs.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            movetoobs.brr.sd=sd(movetoobs.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(movetoobs.brr)), # of observations, excluding NAs. 
            movetoobs.brr.se=movetoobs.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=movetoobs.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=movetoobs.brr.m - movetoobs.brr.se, ymax=movetoobs.brr.m + movetoobs.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for movetoobs") +
  ylim(0,2) +
  theme_bw() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1+0.209, linetype=2) + #this value is 1+ patatsSE from baseline table above
  geom_hline(yintercept = 1-0.209, linetype=2) + #this value is 1- patatsSE from baseline table above
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
##########################################################################
#Play vocalization
#create a summary table for each behavior one at a time by individual, condition, and total day
x <- group_by(care, id, cond, tday) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playvocal.m = mean(playvocal, na.rm = TRUE)) # na.rm = TRUE to remove missing values

#calculate average of baseline condition per individual from table x
avg <- group_by(x, id, cond) %>%
  summarize(playvocal.mm = mean(playvocal.m, na.rm = TRUE),
            playvocal.m.sd=sd(playvocal.m, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playvocal.m)), # of observations, excluding NAs. 
            playvocal.m.se=playvocal.m.sd/sqrt(n))

#subset avg table to remove all conditions except for 1
baseline<-subset(avg, cond==1)

#calculate standard error of condition 1 as a percentage rather than a raw number
baseline$sepct=baseline$playvocal.m.se/baseline$playvocal.mm
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
#call it playvocal.brr (behavior response ratio)
base.ayana<-subset(baseline, id=="ayana")
ayana.resp$playvocal.brr=ayana.resp$playvocal.m/base.ayana$playvocal.mm

base.stevie<-subset(baseline, id=="stevie")
stevie.resp$playvocal.brr=stevie.resp$playvocal.m/base.stevie$playvocal.mm

base.hope<-subset(baseline, id=="hope ")
hope.resp$playvocal.brr=hope.resp$playvocal.m/base.hope$playvocal.mm

base.patats<-subset(baseline, id=="patats")
patats.resp$playvocal.brr=patats.resp$playvocal.m/base.patats$playvocal.mm

#Ayana
x.p <- group_by(ayana.resp, cond) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(playvocal.brr.m = mean(playvocal.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playvocal.brr.sd=sd(playvocal.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playvocal.brr)), # of observations, excluding NAs. 
            playvocal.brr.se=playvocal.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playvocal.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playvocal.brr.m - playvocal.brr.se, ymax=playvocal.brr.m + playvocal.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playvocal") +
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
  summarize(playvocal.brr.m = mean(playvocal.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playvocal.brr.sd=sd(playvocal.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playvocal.brr)), # of observations, excluding NAs. 
            playvocal.brr.se=playvocal.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playvocal.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playvocal.brr.m - playvocal.brr.se, ymax=playvocal.brr.m + playvocal.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playvocal") +
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
  summarize(playvocal.brr.m = mean(playvocal.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playvocal.brr.sd=sd(playvocal.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playvocal.brr)), # of observations, excluding NAs. 
            playvocal.brr.se=playvocal.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playvocal.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playvocal.brr.m - playvocal.brr.se, ymax=playvocal.brr.m + playvocal.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playvocal") +
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
  summarize(playvocal.brr.m = mean(playvocal.brr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            playvocal.brr.sd=sd(playvocal.brr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(playvocal.brr)), # of observations, excluding NAs. 
            playvocal.brr.se=playvocal.brr.sd/sqrt(n))

#create plot
library(ggplot2)
ggplot(data=x.p, aes(x=cond, y=playvocal.brr.m)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=playvocal.brr.m - playvocal.brr.se, ymax=playvocal.brr.m + playvocal.brr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black")) +
  xlab("Condition") +
  ylab("Behavior Response Ratio for playvocal") +
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