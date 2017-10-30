
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

#Adding counts and duration of the 3 behavioral type (positive, negative and neutral)
care$positivect=rowSums(care[,c("foodgrunt", "grunt","lipsmack","pbaboon", "phuman","hgroomct","bgroomct","ghgroomct","gbgroomct","penisd", "playb", "plays","manenrichct","embraceh", "embraceb", "dproxct","dproxout", "movetoobs","playvocal")])
care$positivect
care$positived=rowSums(care[,c("hgroomd","bgroomd","ghgroomd","gbgroomd","manenrichd")])
care$positived
care$negativect=rowSums(care[,c("iproxct", "iproxout","threatg", "bark", "headshake", "yawn", "aggb", "pace", "aggdisp", "teeth", "rubgen", "sway", "turn", "scream")])
care$negativect
care$neutralct=rowSums(care[,c("sgroomct", "eatct","drink","foragect", "restct","schratch", "other")]) 
care$neutralct
care$neutrald=rowSums(care[,c("sgroomd","eatd","foraged","restd")] )6
care$neutrald

#The numbers are not showing for the positive and negative counts

#I will be using the neutral for the first testing since they worked

#example
m1=gls(neutralct~cond.f, data=care, na.action=na.omit, method="ML")
summary(m1)

#example2 - try put individual in as a main factor so cond.f+id
m2=lme(neutralct~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(out2)
anova(m1,m2)
#Since the model with the random factor had a lower IAC score we wanna use that 

#Plotting residuals for m2 to check if we can use this model
op=par(mfrow=c(2,2), mar=c(5,4,1,2))
plot(m2, add.smooth=FALSE, which=1)
E=resid(m2)
hist(E,xlab="residuals", main="")
plot(care$cond.f, E, xlab="Treatment", ylab="residuals")
plot(care$id, E, xlab="id", ylab="residuals")

#trying the other residuals plot clay showed
E1<-residuals(m2)

plot(filter(care, !is.na(neutralct)) %>% dplyr::select(id),
     E1, xlab="id", ylab="Residuals")
plot(filter(care, !is.na(neutralct)) %>% dplyr::select(cond.f),
     E1, xlab="id", ylab="Residuals")

qqnorm(residuals(m2))
qqline(residuals(m2))
ad.test(residuals(m2))#this one says error
summary(m1)

lsmeans(m1,pairwise~cond.f)#This one schould be able to show how the behaviors have changes
#There are no significant changes in the overal count of neutral behaviors between the 4 condition. 

#making a table that will show the means per individual that can be graphed not sure it works
x1 <- group_by(care, cond.f, id) %>%
  summarize(m.neutralct = mean(neutralct, na.rm = TRUE), # na.rm = TRUE to remove missing values
            s.neutralct=sd(neutralct, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(neutralct)), # of observations, excluding NAs.
            se.neutralct=s.neutralct/sqrt(n))
x1
#Trying to plot the amount of neutral counts there are for each individual
ggplot(data=x1,
       aes(x=cond.f, y=m.neutralct, fill=id, label=m.neutralct)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=m.neutralct, ymax=m.neutralct+se.neutralct), width=0.2,
                position=position_dodge(0.9)) +
  scale_fill_manual(values=c("black","white", "light grey", "dark grey")) +
  xlab("ID") +
  ylab("Number of neutral behaviors") +
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



###################################################################################################################################
#example
out1=gls(lipsmack~cond.f, data=care, na.action=na.omit, method="ML")

summary(out1)

#example2 - try put individual in as a main factor so cond.f+id
out2=lme(lipsmack~cond.f, random=~1|id,data=care, na.action=na.omit, method="ML")
summary(out2)
anova(out2)
names(xx)
anova(out1,out2) #we wanna use the random factor its better 

#notes from meetings with clay
#this line might now be useful
care$test
cor(care, use="pairwise.complete.obs", method="kendall") 
cor(care$eatd, y=care$foraged)
cor(care, use="everything")


#adding ct or duration total for negative positive or neutral
care$test=rowSums(care[,c("eatct", "foragect","restct")])
care$test
#looking at residuals

E1<-residuals(out2)

plot(filter(care, !is.na(lipsmack)) %>% dplyr::select(id),
     E1, xlab="id", ylab="Residuals")
plot(filter(care, !is.na(lipsmack)) %>% dplyr::select(cond.f),
     E1, xlab="id", ylab="Residuals")
#couple bad outliers


qqnorm(residuals(out2))
qqline(residuals(out2))
ad.test(residuals(out2))

plot(out2)
#residuals increase with model fit

x<-care$lipsmack[!is.na(care$lipsmack)]#removes na values from column
E1<-residuals(out2,type="normalized")
plot(x, E1)

care.integer=data.matrix(xx, rownames.force = NA)
cor(xx, use="everything")

#new code 


