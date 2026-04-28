#Lia's Full Code (using slightly outdated files), sent Jan 16 2026

##Final Code Repository

#setwd("~/Desktop/Master's dissertation students/Isabelle Apple Leavy 2024/Final dissertation files")
spec <- read.csv("spec91_NicoleCPP.csv")
beh <- read.csv("beh91_NicoleCPP.csv")
beh$species<-as.factor(beh$species)
beh.neonates.CPP<-read.csv("neonates mastersheet randomisation.csv")
beh.neonates.CPP$species<-as.factor(beh.neonates.CPP$species)


library(ape)
library(caper)
library("ggtree")
library(ggplot2)
library(phytools)
library(dplyr)
library(ggpubr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Look at available behavioural data

# Species for which we have CPP

beh.CPP<-droplevels(beh[!is.na(beh$cpp),])
beh.CPP$species<-as.factor(beh.CPP$species)

#write.csv(table(beh.CPP$species, beh.CPP$cephalic.det, useNA = "always"),"Species.CPP.cephalic.csv") # cephalic orientation
#write.csv(table(beh.CPP$species, beh.CPP$fep, useNA = "always"),"Species.CPP.OPOA.csv") # OA/OP orientation
#write.csv(table(beh.CPP$species, beh.CPP$assist, useNA = "always"),"Species.CPP.assistance.csv") # assistance
#write.csv(table(beh.CPP$species, beh.CPP$con.assist, useNA = "always"),"Species.CPP.con.assist.csv") # assistance by conspecific
#write.csv(table(beh.CPP$species, beh.CPP$con.prox, useNA = "always"),"Species.CPP.con.prox.csv") #proximity by conspecific
#write.csv(table(beh.CPP$species, beh.CPP$inf.dead, useNA = "always"),"Species.CPP.inf.death.csv") # infant death

# with categories at genus level

beh.CPP.gen.cat<-droplevels(beh[!is.na(beh$cpp.gencat),])
beh.CPP.gen.cat$species<-as.factor(beh.CPP.gen.cat$species)
beh.CPP.gen.cat$genus<-as.factor(beh.CPP.gen.cat$genus)

#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$cephalic.det, useNA = "always"),"Species.CPP.gen.cephalic.csv") # cephalic orientation
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$fep, useNA = "always"),"Species.CPP.gen.OPOA.csv") # OA/OP orientation
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$assist, useNA = "always"),"Species.CPP.gen.assistance.csv") # assistance
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$con.assist, useNA = "always"),"Species.CPP.gen.con.assist.csv") # assistance by conspecific
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$con.prox, useNA = "always"),"Species.CPP.gen.con.prox.csv") #proximity by conspecific
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$inf.dead, useNA = "always"),"Species.CPP.gen.inf.death.csv") # infant death


###### Creating new dataframe with neonatal info required to test for relations cpp vs death proportion

beh.neonates.CPP.surv<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$inf.stat),]) # cleaning data to remove entries without infant survival data

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.surv$species))) #creating new dataset at species level for species info on infant survival
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.surv$group, beh.neonates.CPP.surv$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.surv$cpp, beh.neonates.CPP.surv$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.surv$cpp.gen, beh.neonates.CPP.surv$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.surv$cpp.gencat, beh.neonates.CPP.surv$species, first) #variable: cpp.gencat
neonates.spec.CPP$inf.n<-tapply(beh.neonates.CPP.surv$inf.stat, beh.neonates.CPP.surv$species, function(x){ length(which(!is.na(x))) }) #variable: info.n (number of neonates per species with info on survival)

neonates.spec.CPP$inf.dead<-tapply(beh.neonates.CPP.surv$inf.stat, beh.neonates.CPP.surv$species, function(x){ length(which(x=="Dead")) }) # new variable with number of dead infants per species
neonates.spec.CPP$death.prop<-as.numeric(neonates.spec.CPP$inf.dead/neonates.spec.CPP$inf.n) # new variable with death proportion

CPP.death.plot<-ggplot(neonates.spec.CPP, aes(x = cpp, y = death.prop))+
  geom_smooth(method = lm, se = F, color = "hotpink")+
  geom_point(color = "black", size = 2) +
  #geom_point(aes(color = group), size = 3)+
  #scale_color_manual(labels = c("Ape", "Lemur", "Loris", "American Monkey", "Afro-Eurasian Monkey"),
  #                   values = c("#ffe76a", "#00cc33" ,"#1893f8","#febe15", "#f8766d"))+
  xlab("CPP")+
  ylab("Neonatal death")+
  theme_classic()+
  theme(legend.position = "none")

CPP.death.plot

# regression
reg.species.CPP<-summary(lm(neonates.spec.CPP$death.prop~neonates.spec.CPP$cpp))
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)            0.06324    0.12537   0.504    0.621
#neonates.spec.CPP$cpp  0.08313    0.12983   0.640    0.531

#Residual standard error: 0.2575 on 16 degrees of freedom
#(17 observations deleted due to missingness)
#Multiple R-squared:  0.02499,	Adjusted R-squared:  -0.03595
#F-statistic:  0.41 on 1 and 16 DF,  p-value: 0.531

slope.species.CPP<-reg.species.CPP$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.surv$inf.stat) # Randomise infant status (live/dead)
  random.inf.dead <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(which(x=="Dead")) }) # recount N dead infants per species
  random.inf.n <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(!is.na(x)) }) #noting N infant with survival info per species
  random.death.prop <- random.inf.dead/random.inf.n # recalculate death prop
  lm.random<-summary(lm(random.death.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.CPP)
length(which(abs(test_slope)>=abs(slope.species.CPP)))/1000 #sig difference

#0.233


## Analyses at species level but using categorical values for CPP based on genus (35 species)

#CPP as categorical (3 categories at genus level:  >=90,65-89,<65))

means.gencpp<-tapply(neonates.spec.CPP$death.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#   extreme      high      low
#0.16556588 0.17187500 0.08943615
tapply(neonates.spec.CPP$death.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#   extreme      high      low
# 0.03841908 0.12025670 0.03868885

table(neonates.spec.CPP$cpp.gencat)
# extreme    high     low
# 5           8       22

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

CPP.cat.death.plot<-ggplot(neonates.spec.CPP, aes(x = cpp.gencat, y = death.prop, fill = cpp.gencat))+
  geom_boxplot()+
  stat_summary(fun=mean, shape = 8)+
  scale_fill_manual(values = c("aquamarine2", "#ffe76a", "#f8766d"))+
  scale_x_discrete(labels=c("Low", "High", "Extreme"))+
  #labs(title = "Proportion of Birth Events that Include a Foetal Death", subtitle = "CPP Expressed Categorically at the Genus Level (n = 35)")+
  xlab("CPP category")+
  ylab("Neonatal death")+
  theme_classic()+
  theme(legend.position = "none")

CPP.cat.death.plot

##testing for significance with randomisation

random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean death prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean death prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean death prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.surv$inf.stat) # Randomise infant status (live/dead)
  random.inf.dead <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(which(x=="Dead")) }) # recount N dead infants per species
  random.inf.n <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(!is.na(x)) }) #noting N infant with survival info per species
  random.death.prop <- random.inf.dead/random.inf.n # recalculate death prop
  random.extreme.high[i]<- mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="high"]) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="high"]) - mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.995
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.455
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.348

##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#############
#Is there a relationship between CPP and foetal emergence presentation

beh.neonates.CPP.pres<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$cephalic.det),]) # cleaning data to remove entries without infant survival data

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.pres$species))) #creating new dataset at species level for species info on infant face/vertex presentation
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.pres$group, beh.neonates.CPP.pres$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.pres$cpp, beh.neonates.CPP.pres$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.pres$cpp.gen, beh.neonates.CPP.pres$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.pres$cpp.gencat, beh.neonates.CPP.pres$species, first) #variable: cpp.gencat

neonates.spec.CPP$face<-tapply(beh.neonates.CPP.pres$cephalic.det, beh.neonates.CPP.pres$species, function(x){ length(which(x %in% c("Face","Brow"))) }) #variable:face (number of neonates per species with face or brow presentationl)
neonates.spec.CPP$vertex<-tapply(beh.neonates.CPP.pres$cephalic.det, beh.neonates.CPP.pres$species, function(x){ length(which(x=="Vertex")) }) # new variable with number of infants per species in vertex presentation
neonates.spec.CPP$face.prop<-as.numeric(neonates.spec.CPP$face/(neonates.spec.CPP$face+neonates.spec.CPP$vertex)) # new variable with proportion of face presentation
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.pres.csv")


lm.model.face<-lm(neonates.spec.CPP$face.prop~neonates.spec.CPP$cpp)
summary(lm.model.face)
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
#(Intercept)             0.4389     0.2660   1.650    0.133
#neonates.spec.CPP$cpp  -0.1265     0.2367  -0.535    0.606

#Residual standard error: 0.4256 on 9 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.03079,	Adjusted R-squared:  -0.0769
#F-statistic: 0.2859 on 1 and 9 DF,  p-value: 0.6058

CPP.face.plot<-ggplot(neonates.spec.CPP, aes(x = cpp, y = face.prop))+
  geom_smooth(method = lm, se = F, color = "hotpink")+
  geom_point(color = "black", size = 2) +
  #geom_point(aes(color = group), size = 3)+
  #scale_color_manual(labels = c("Ape", "Lemur", "Loris", "American Monkey", "Afro-Eurasian Monkey"),
  #                   values = c("#ffe76a", "#00cc33" ,"#1893f8","#febe15", "#f8766d"))+
  xlab("CPP")+
  ylab("Face presentation")+
  theme_classic()+
  theme(legend.position = "none")
CPP.face.plot

# testing significance with randomisation

slope.species.face<-lm.model.face$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pres$cephalic.det) # Randomise infant presentation
  random.face <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ length(which(x %in% c("Face","Brow"))) }) # recount N face presentation
  random.n <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ length(!is.na(x)) }) #noting N infant with presentation info
  random.face.prop <- random.face/random.n # recalculate proportion for face pres
  lm.random<-summary(lm(random.face.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.face)
length(which(abs(test_slope)>=abs(slope.species.face)))/1000 #sig

#0.357



#CPP as a categorical variable

means.gencpp<-tapply(neonates.spec.CPP$face.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#   extreme      high      low
#   0.1500000 0.3928571 0.2824176
tapply(neonates.spec.CPP$face.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#   extreme      high      low
#   0.0900000 0.2363946 0.1748863

table(neonates.spec.CPP$cpp.gencat)
# extreme    high     low
# 4           4       10

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

CPP.cat.face.plot<-ggplot(neonates.spec.CPP, aes(x = cpp.gencat, y = face.prop, fill = cpp.gencat))+
  geom_boxplot()+
  stat_summary(fun=mean, shape = 8)+
  scale_fill_manual(values = c("aquamarine2", "#ffe76a", "#f8766d"))+
  scale_x_discrete(labels=c("Low", "High", "Extreme"))+
  #labs(title = "Proportion of Birth Events in face presentation", subtitle = "CPP Expressed Categorically at the Genus Level (n = 35)")+
  xlab("CPP category")+
  ylab("Face presentation")+
  theme_classic()+
  theme(legend.position = "none")

##testing for significance with randomisation

random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pres$cephalic.det) # Randomise infant presentation
  random.face <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ length(which(x %in% c("Face","Brow"))) }) # recount N face presentation
  random.n <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ length(!is.na(x)) }) #noting N infant with presentation info
  random.face.prop <- random.face/random.n # recalculate proportion for face pres
  random.extreme.high[i]<- mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="high"]) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="high"]) - mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.272
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.655
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.382

##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#############
#Is there a relationship between CPP and foetal emergence position (OA/OP)?

beh.neonates.CPP.pos<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$occ),]) # cleaning data to remove entries without infant survival data

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.pos$species))) #creating new dataset at species level for species info on infant face/vertex presentation
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.pos$group, beh.neonates.CPP.pos$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.pos$cpp, beh.neonates.CPP.pos$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.pos$cpp.gen, beh.neonates.CPP.pos$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.pos$cpp.gencat, beh.neonates.CPP.pos$species, first) #variable: cpp.gencat

neonates.spec.CPP$OA<-tapply(beh.neonates.CPP.pos$occ, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) #variable:OA (number of neonates per species with OA positionl)
neonates.spec.CPP$OP<-tapply(beh.neonates.CPP.pos$occ, beh.neonates.CPP.pos$species, function(x){ length(which(x=="OP")) }) # variable:OP (number of neonates per species with OP positionl)
neonates.spec.CPP$OA.prop<-as.numeric(neonates.spec.CPP$OA/(neonates.spec.CPP$OA+neonates.spec.CPP$OP)) # new variable with proportion of OA/(OA+OP)
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.OAOP.csv")


lm.model.oa<-lm(neonates.spec.CPP$OA.prop~neonates.spec.CPP$cpp)
summary(lm.model.oa)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)             0.6197     0.2177   2.846   0.0159 *
#  neonates.spec.CPP$cpp  -0.3547     0.2064  -1.719   0.1136

#Residual standard error: 0.3923 on 11 degrees of freedom
#(8 observations deleted due to missingness)
#Multiple R-squared:  0.2117,	Adjusted R-squared:   0.14
#F-statistic: 2.954 on 1 and 11 DF,  p-value: 0.1136

CPP.OA.plot<-ggplot(neonates.spec.CPP, aes(x = cpp, y = OA.prop))+
  geom_smooth(method = lm, se = F, color = "hotpink")+
  geom_point(color = "black", size = 2) +
  #geom_point(aes(color = group), size = 3)+
  #scale_color_manual(labels = c("Ape", "Lemur", "Loris", "American Monkey", "Afro-Eurasian Monkey"),
  #values = c("#ffe76a", "#00cc33" ,"#1893f8","#febe15", "#f8766d"))+
  xlab("CPP")+
  ylab("OA position")+
  theme_classic()+
  theme(legend.position = "none")

# testing significance with randomisation

slope.species.oa<-lm.model.oa$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pos$occ) # Randomise infant presentation
  random.oa <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) # recount N OA position
  random.n <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x %in% c("OA","OP"))) }) #noting N infant with position info
  random.oa.prop <- random.oa/random.n # recalculate proportion for oa pos
  lm.random<-summary(lm(random.oa.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.oa)
length(which(abs(test_slope)>=abs(slope.species.oa)))/1000 #sig

#0.016


#CPP as a categorical variable

means.gencpp<-tapply(neonates.spec.CPP$OA.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#   extreme      high      low
#   0.04310345 0.44285714 0.25315657
tapply(neonates.spec.CPP$OA.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#   extreme      high      low
#   0.007431629 0.266326531 0.146469429


table(neonates.spec.CPP$cpp.gencat)
# extreme    high     low
# 4           5       12

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

CPP.cat.OA.plot<-ggplot(neonates.spec.CPP, aes(x = cpp.gencat, y = OA.prop, fill = cpp.gencat))+
  geom_boxplot()+
  stat_summary(fun=mean, shape = 8)+
  scale_fill_manual(values = c("aquamarine2", "#ffe76a", "#f8766d"))+
  scale_x_discrete(labels=c("Low", "High", "Extreme"))+
  #labs(title = "Proportion of Birth Events in OA position", subtitle = "CPP Expressed Categorically at the Genus Level (n = 21)")+
  xlab("CPP category")+
  ylab("OA position")+
  theme_classic()+
  theme(legend.position = "none")

##testing for significance with randomisation

random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pos$occ) # Randomise infant presentation
  random.oa <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) # recount N OA position
  random.n <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x %in% c("OA","OP"))) }) #noting N infant with position info
  random.oa.prop <- random.oa/random.n # recalculate proportion for oa pos
  random.extreme.high[i]<- mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="high"]) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="high"]) - mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.036
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.269
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.179

##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#############
#Is there a relationship between CPP and self assistance?

beh.CPP<-droplevels(beh[!is.na(beh$cpp.gencat),])
beh.neonates.CPP.assist<-droplevels(beh.CPP[!is.na(beh.CPP$assist),]) # cleaning data to remove entries without self assistance data

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.assist$species))) #creating new dataset at species level for species info
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.assist$group, beh.neonates.CPP.assist$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.assist$cpp, beh.neonates.CPP.assist$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.assist$cpp.gen, beh.neonates.CPP.assist$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.assist$cpp.gencat, beh.neonates.CPP.assist$species, first) #variable: cpp.gencat

neonates.spec.CPP$Y<-tapply(beh.neonates.CPP.assist$assist, beh.neonates.CPP.assist$species, function(x){ length(which(x =="TRUE")) }) #variable:Y (number of births per species with self-assistance)
neonates.spec.CPP$N<-tapply(beh.neonates.CPP.assist$assist, beh.neonates.CPP.assist$species, function(x){ length(which(x=="FALSE")) }) # variable:N (number of births per species without self assistance)
neonates.spec.CPP$assist.prop<-as.numeric(neonates.spec.CPP$Y/(neonates.spec.CPP$Y+neonates.spec.CPP$N)) # new variable with proportion of Y/Y+N
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.assist.csv")

# CPP analyses at species level (16 species only)

lm.model.assist<-lm(neonates.spec.CPP$assist.prop~neonates.spec.CPP$cpp)
summary(lm.model.assist)

#Coefficients:#
#                     Estimate Std. Error t value Pr(>|t|)
#                       0.8071     0.1860   4.339 0.000584 ***
#neonates.spec.CPP$cpp  -0.3813     0.1913  -1.994 0.064700 .

#Residual standard error: 0.3793 on 15 degrees of freedom
#(13 observations deleted due to missingness)
#Multiple R-squared:  0.2095,	Adjusted R-squared:  0.1568
#F-statistic: 3.975 on 1 and 15 DF,  p-value: 0.0647


CPP.assist.plot<-ggplot(neonates.spec.CPP, aes(x = cpp, y = assist.prop))+
  geom_smooth(method = lm, se = F, color = "hotpink")+
  geom_point(color = "black", size = 2) +
  #geom_point(aes(color = group), size = 3)+
  #scale_color_manual(labels = c("Ape", "Lemur", "Loris", "American Monkey", "Afro-Eurasian Monkey"),
  #                   values = c("#ffe76a", "#00cc33" ,"#1893f8","#febe15", "#f8766d"))+
  xlab("CPP")+
  ylab("Self assistance")+
  theme_classic()+
  theme(legend.position = "none")

# testing significance with randomisation

slope.species.assist<-lm.model.assist$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.assist$assist) # Randomise self assistance variable
  random.assist <- tapply(randomised_status, beh.neonates.CPP.assist$species, function(x){ length(which(x =="TRUE")) }) # recount N births with self assistance
  random.n <- tapply(randomised_status, beh.neonates.CPP.assist$species, function(x){ length(which(x %in% c("TRUE","FALSE"))) }) #noting N births with assistance info
  random.assist.prop <- random.assist/random.n # recalculate proportion for self assistance
  lm.random<-summary(lm(random.assist.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.assist)
length(which(abs(test_slope)>=abs(slope.species.assist)))/1000 #sig

#0.005


#CPP as a categorical variable

means.gencpp<-tapply(neonates.spec.CPP$assist.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#   extreme      high      low
#   0.09003623 0.81250000 0.71542317
tapply(neonates.spec.CPP$assist.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#   extreme      high      low
#   0.01213624 0.14192708 0.13360157


table(neonates.spec.CPP$cpp.gencat)
# extreme    high     low
# 4           7       19

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

CPP.cat.assist.plot<-ggplot(neonates.spec.CPP, aes(x = cpp.gencat, y = assist.prop, fill = cpp.gencat))+
  geom_boxplot()+
  stat_summary(fun=mean, shape = 8)+
  scale_fill_manual(values = c("aquamarine2", "#ffe76a", "#f8766d"))+
  scale_x_discrete(labels=c("Low", "High", "Extreme"))+
  #labs(title = "Proportion of Birth Events with self assistance", subtitle = "CPP Expressed Categorically at the Genus Level (n = 30)")+
  xlab("CPP category")+
  ylab("Self assistance")+
  theme_classic()+
  theme(legend.position = "none")

##testing for significance with randomisation

random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.assist$assist) # Randomise self assistance variable
  random.assist <- tapply(randomised_status, beh.neonates.CPP.assist$species, function(x){ length(which(x =="TRUE")) }) # recount N births with self assistance
  random.n <- tapply(randomised_status, beh.neonates.CPP.assist$species, function(x){ length(which(x %in% c("TRUE","FALSE"))) }) #noting N births with assistance info
  random.assist.prop <- random.assist/random.n # recalculate proportion for self assistance
  random.extreme.high[i]<- mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="high"]) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="high"]) - mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="extreme"]) - mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="low"]) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.001
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.557
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.001

##### Panel plots for paper

# regressions
ggarrange(CPP.death.plot,CPP.face.plot,CPP.OA.plot,CPP.assist.plot,
          labels = c("A", "B", "C", "D"), # tags for the plots
          nrow = 2, ncol = 2)
ggsave("Regressions.pdf", width=18, height= 14,units="cm")

# boxplots

ggarrange(CPP.cat.death.plot,CPP.cat.face.plot,CPP.cat.OA.plot,CPP.cat.assist.plot,
          labels = c("A", "B", "C", "D"), # tags for the plots
          nrow = 2, ncol = 2)
ggsave("Boxplots.pdf", width=18, height= 14,units="cm")
