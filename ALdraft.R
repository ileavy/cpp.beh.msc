## AL Draft

## Setup
setwd("~/Desktop/MSc Publication")
spec <- read.csv("spec_ESM.csv")
beh <- read.csv("beh_ESM.csv") #ESM
beh$species<-as.factor(beh$species)
beh.neonates.CPP<-read.csv("neonates_ESM.csv")
beh.neonates.CPP$species<-as.factor(beh.neonates.CPP$species)

library(ape)
library(caper)
library(ggtree) #BiocManager::install("ggtree")
library(ggplot2)
library(phytools)
library(dplyr)
library(ggpubr)
library(ggrepel)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Filtering only species that we have CPP values for
beh.CPP<-droplevels(beh[!is.na(beh$cpp),])

#write.csv(table(beh.CPP$species, beh.CPP$cephalic.det, useNA = "always"),"Species.CPP.cephalic.csv") # cephalic orientation
#write.csv(table(beh.CPP$species, beh.CPP$fep, useNA = "always"),"Species.CPP.OPOA.csv") # OA/OP orientation
#write.csv(table(beh.CPP$species, beh.CPP$assist, useNA = "always"),"Species.CPP.assistance.csv") # assistance
#write.csv(table(beh.CPP$species, beh.CPP$con.assist, useNA = "always"),"Species.CPP.con.assist.csv") # assistance by conspecific
#write.csv(table(beh.CPP$species, beh.CPP$con.prox, useNA = "always"),"Species.CPP.con.prox.csv") #proximity by conspecific
#write.csv(table(beh.CPP$species, beh.CPP$inf.dead, useNA = "always"),"Species.CPP.inf.death.csv") # infant death

#Filtering only species for which we have a CPP value at the genus level
beh.CPP.gen <-droplevels(beh[!is.na(beh$cpp.gen),])

#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$cephalic.det, useNA = "always"),"Species.CPP.gen.cephalic.csv") # cephalic orientation
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$fep, useNA = "always"),"Species.CPP.gen.OPOA.csv") # OA/OP orientation
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$assist, useNA = "always"),"Species.CPP.gen.assistance.csv") # assistance
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$con.assist, useNA = "always"),"Species.CPP.gen.con.assist.csv") # assistance by conspecific
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$con.prox, useNA = "always"),"Species.CPP.gen.con.prox.csv") #proximity by conspecific
#write.csv(table(beh.CPP.gen.cat$species, beh.CPP.gen.cat$inf.dead, useNA = "always"),"Species.CPP.gen.inf.death.csv") # infant death

#Filtering only species that we have a category at the the genus level
beh.CPP.gen.cat<-droplevels(beh[!is.na(beh$cpp.gencat),])

## ~~~~ PART 1: Death Proportion vs. CPP ~~~~ ##
# only for species that we have a CPP value for
# 18 species

beh.neonates.CPP.surv<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$inf.stat),]) # cleaning data to remove entries without infant survival data
beh.neonates.CPP.surv$species <- as.factor(beh.neonates.CPP.surv$species)

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.surv$species))) #creating new dataset at species level for species info on infant survival
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.surv$group, beh.neonates.CPP.surv$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.surv$cpp, beh.neonates.CPP.surv$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.surv$cpp.gen, beh.neonates.CPP.surv$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.surv$cpp.gencat, beh.neonates.CPP.surv$species, first) #variable: cpp.gencat
neonates.spec.CPP$inf.n<-tapply(beh.neonates.CPP.surv$inf.stat, beh.neonates.CPP.surv$species, function(x){ length(which(!is.na(x))) }) #variable: info.n (number of neonates per species with info on survival)

neonates.spec.CPP$inf.dead<-tapply(beh.neonates.CPP.surv$inf.stat, beh.neonates.CPP.surv$species, function(x){ length(which(x=="dead")) }) # new variable with number of dead infants per species
neonates.spec.CPP$death.prop<-as.numeric(neonates.spec.CPP$inf.dead/neonates.spec.CPP$inf.n) # new variable with death proportion

# Regression
reg.species.CPP<-summary(lm(neonates.spec.CPP$death.prop~neonates.spec.CPP$cpp))
reg.species.CPP
#                         Estimate Std. Error t value Pr(>|t|)
#(Intercept)            0.06221    0.12466   0.499    0.625
#neonates.spec.CPP$cpp  0.08134    0.12647   0.643    0.529
#Residual standard error: 0.2574 on 16 degrees of freedom
#(39 observations deleted due to missingness)
#Multiple R-squared:  0.0252,	Adjusted R-squared:  -0.03572
#F-statistic: 0.4137 on 1 and 16 DF,  p-value: 0.5292

# y = 0.08134x + 0.6221

#with permutation
slope.species.CPP<-reg.species.CPP$coefficients[2]
random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.surv$inf.stat) # Randomise infant status (live/dead)
  random.inf.dead <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(which(x=="dead")) }) # recount N dead infants per species
  random.inf.n <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(!is.na(x)) }) #noting N infant with survival info per species
  random.death.prop <- random.inf.dead/random.inf.n # recalculate death prop
  lm.random<-summary(lm(random.death.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.CPP)
length(which(abs(test_slope)>=abs(slope.species.CPP)))/1000
#0.242

# Plot 1: Relationship betwen neonatal death proportion and CPP
CPP.death.plot <- ggplot(neonates.spec.CPP, aes(x = cpp, y = death.prop)) +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(color = "black", size = 2) +
  xlab("Cephalopelvic Proportion") +
  ylab("Neonatal Death Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.death.plot

## Plot 2: CPP.death.plot with grouping
CPP.death.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp, y = death.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2.5) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#9d02d7", "#0000ff")) +
  xlab("CPP") +
  ylab("Neonatal Death Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.death.plot.grouped

## ~~~~ PART 2: Death Proportion vs. CPP Category ~~~~ ##
# CPP category taken from the genus overall, but analyses still performed at genus level
# 35 species
# CPP as categorical (3 categories at genus level:  >=90,65-89,<65))

means.gencpp<-tapply(neonates.spec.CPP$death.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#   extreme       high        low
# 0.18187369 0.17187500 0.08943615
tapply(neonates.spec.CPP$death.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#   extreme       high        low
#0.03347116 0.12025670 0.03868885
table(neonates.spec.CPP$cpp.gencat, useNA = "always")
#extreme    high     low
#     6       8      22

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)
## Testing for significance with randomisation
beh.neonates.CPP.surv$inf.stat <- as.factor(beh.neonates.CPP.surv$inf.stat)
random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean death prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean death prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean death prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.surv$inf.stat) # randomise infant status (live/dead)
  random.inf.dead <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ sum(which(x=="dead")) }) # recount N dead infants per species
  random.inf.n <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ sum(!is.na(x)) }) #noting N infant with survival info per species
  random.death.prop <- random.inf.dead/random.inf.n # recalculate death prop
  random.extreme.high[i]<- mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) - mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.death.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.999
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.824
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.987

# Plot 3: Box Plot CPP category vs. death proportion
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.death.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = death.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_jitter(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("Neonatal Death Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.cat.death.plot

## ~~~~ PART 3: Fetal Emergence (Presentation) vs. CPP ~~~~ ##
beh.neonates.CPP.pres<- droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$presentation),]) # cleaning data to remove neonates without presentation data

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.pres$species))) #creating new dataset at species level for species info on infant face/vertex presentation
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.pres$group, beh.neonates.CPP.pres$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.pres$cpp, beh.neonates.CPP.pres$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.pres$cpp.gen, beh.neonates.CPP.pres$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.pres$cpp.gencat, beh.neonates.CPP.pres$species, first) #variable: cpp.gencat

neonates.spec.CPP$face<-tapply(beh.neonates.CPP.pres$presentation, beh.neonates.CPP.pres$species, function(x){ length(which(x == "face"))} ) #variable:face (number of neonates per species with face or brow presentationl)
neonates.spec.CPP$vault<-tapply(beh.neonates.CPP.pres$presentation, beh.neonates.CPP.pres$species, function(x){ length(which(x=="vault")) }) # new variable with number of infants per species in vertex presentation
neonates.spec.CPP$face.prop<-as.numeric(neonates.spec.CPP$face/(neonates.spec.CPP$face+neonates.spec.CPP$vault)) # new variable with proportion of face presentation
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.pres.csv")

# Regression
lm.model.face<-lm(neonates.spec.CPP$face.prop~neonates.spec.CPP$cpp)
summary(lm.model.face)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)             0.2648     0.3337   0.793    0.454
#neonates.spec.CPP$cpp   0.1358     0.3497   0.388    0.709
#Residual standard error: 0.4898 on 7 degrees of freedom
#(14 observations deleted due to missingness)
#Multiple R-squared:  0.02109,	Adjusted R-squared:  -0.1188
#F-statistic: 0.1508 on 1 and 7 DF,  p-value: 0.7093

# testing significance with randomisation
slope.species.face<-lm.model.face$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pres$presentation) # randomise infant presentation
  random.face <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ length(which(x == "face")) }) # recount N face presentation
  random.n <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ sum(!is.na(x)) }) #noting N infant with presentation info
  random.face.prop <- random.face/random.n # recalculate proportion for face pres
  lm.random<-summary(lm(random.face.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.face)
length(which(abs(test_slope)>=abs(slope.species.face)))/1000
#0.422

# Plot 4: scatter CPP vs. proportion of neonates in face presentation
CPP.face.plot <- ggplot(neonates.spec.CPP, aes(x = cpp, y = face.prop)) +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(color = "black", size = 2) +
  xlab("Cephalopelvic Proportion") +
  ylab("Face Presentation Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.face.plot

# Plot 4: scatter CPP vs. proportion of neonates in face presentation by group
CPP.face.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp, y = face.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2.5) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#9d02d7", "#0000ff")) +
  xlab("CPP") +
  ylab("Face Presentation Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.face.plot.grouped

## ~~~~ PART 4: Fetal Emergence (Presentation) vs. CPP Category ~~~~ ##
# CPP category taken from the genus overall, but analyses still performed at genus level
# CPP as categorical (3 categories at genus level:  >=90,65-89,<65))

means.gencpp<-tapply(neonates.spec.CPP$face.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#extreme      high       low
#0.5333333 0.5000000 0.2606061
tapply(neonates.spec.CPP$face.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#extreme      high       low
#0.2533333 0.3333333 0.1657851

table(neonates.spec.CPP$cpp.gencat)
#extreme    high     low
#3       4       9

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

#significance testing with permutations
random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pres$presentation) # Randomise infant presentation
  random.face <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ length(which(x == "face")) }) # recount N face presentation
  random.n <- tapply(randomised_status, beh.neonates.CPP.pres$species, function(x){ sum(!is.na(x)) }) #noting N infant with presentation info
  random.face.prop <- random.face/random.n # recalculate proportion for face pres
  random.extreme.high[i]<- mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) - mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.face.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.878
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.276
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.097

# Plot 5: Box Plot CPP category vs. face proportion
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.face.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = face.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("Face Presentation Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.cat.face.plot

## ~~~~ PART 5: Fetal Emergence (Orientation) vs. CPP  ~~~~ ##
beh.neonates.CPP.pos<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$orientation),]) # cleaning data to remove entries without orientation data

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.pos$species))) #creating new dataset at species level for species info on infant face/vertex presentation
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.pos$group, beh.neonates.CPP.pos$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.pos$cpp, beh.neonates.CPP.pos$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.pos$cpp.gen, beh.neonates.CPP.pos$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.pos$cpp.gencat, beh.neonates.CPP.pos$species, first) #variable: cpp.gencat

neonates.spec.CPP$OA<-tapply(beh.neonates.CPP.pos$orientation, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) #variable:OA (number of neonates per species with OA positionl)
neonates.spec.CPP$OP<-tapply(beh.neonates.CPP.pos$orientation, beh.neonates.CPP.pos$species, function(x){ length(which(x=="OP")) }) # variable:OP (number of neonates per species with OP positionl)
neonates.spec.CPP$OA.prop<-as.numeric(neonates.spec.CPP$OA/(neonates.spec.CPP$OA+neonates.spec.CPP$OP)) # new variable with proportion of OA/(OA+OP)
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.OAOP.csv")

lm.model.oa<-lm(neonates.spec.CPP$OA.prop~neonates.spec.CPP$cpp)
summary(lm.model.oa)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept)             0.4805     0.1697   2.832   0.0163 *
#neonates.spec.CPP$cpp  -0.2662     0.1580  -1.685   0.1202
#Residual standard error: 0.308 on 11 degrees of freedom
#(16 observations deleted due to missingness)
#Multiple R-squared:  0.2051,	Adjusted R-squared:  0.1328
#F-statistic: 2.838 on 1 and 11 DF,  p-value: 0.1202

slope.species.oa<-lm.model.oa$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pos$orientation) # randomise infant orientation
  random.oa <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) # recount N OA position
  random.n <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x %in% c("OA","OP"))) }) #noting N infant with position info
  random.oa.prop <- random.oa/random.n # recalculate proportion for oa pos
  lm.random<-summary(lm(random.oa.prop~neonates.spec.CPP$cpp))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.oa)
length(which(abs(test_slope)>=abs(slope.species.oa)))/1000 #sig
#0.051

#Plot 6: scatter OA prop vs. CPP
CPP.OA.plot <- ggplot(neonates.spec.CPP, aes(x = cpp, y = OA.prop)) +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(color = "black", size = 2) +
  xlab("Cephalopelvic Proportion") +
  ylab("Occiput Anterior Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.OA.plot

#Plot 7: scatter OA prop vs. CPP grouped
CPP.OA.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp, y = OA.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2.5) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#9d02d7", "#0000ff")) +
  xlab("CPP") +
  ylab("OA Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.OA.plot.grouped

## ~~~~ PART 6: Fetal Emergence (Orientation) vs. CPP Category ~~~~ ##

means.gencpp<-tapply(neonates.spec.CPP$OA.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#extreme      high       low
#0.04347826 0.32285714 0.25344353

tapply(neonates.spec.CPP$OA.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#extreme        high         low
#0.009451796 0.171183673 0.123615285

table(neonates.spec.CPP$cpp.gencat)
#extreme    high     low
#   5       5      11

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

# Testing for significance with randomisation

random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pos$orientation) # Randomise infant presentation
  random.oa <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) # recount N OA position
  random.n <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x %in% c("OA","OP"))) }) #noting N infant with position info
  random.oa.prop <- random.oa/random.n # recalculate proportion for oa pos
  random.extreme.high[i]<- mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) - mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.oa.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.103
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.695
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.118

#Plot 8: boxplot CPP vs. OA proportion
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.OA.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = OA.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("OA Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.cat.OA.plot

## ~~~~ PART 7: Self-Assistance vs. CPP ~~~~ ##
# 30 species

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

# CPP analyses at species level (17 species)
lm.model.assist<-lm(neonates.spec.CPP$assist.prop~neonates.spec.CPP$cpp)
summary(lm.model.assist)
#                         Estimate Std. Error t value Pr(>|t|)
#(Intercept)             0.8189     0.1825   4.486 0.000435 ***
#neonates.spec.CPP$cpp  -0.3884     0.1841  -2.110 0.052076 .
#Residual standard error: 0.3747 on 15 degrees of freedom
#(13 observations deleted due to missingness)
#Multiple R-squared:  0.2289,	Adjusted R-squared:  0.1774
#F-statistic: 4.452 on 1 and 15 DF,  p-value: 0.05208

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
#0.002

#Plot 9: scatter assistance vs CPP
CPP.assist.plot <- ggplot(neonates.spec.CPP, aes(x = cpp, y = assist.prop)) +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(color = "black", size = 2) +
  xlab("Cephalopelvic Proportion") +
  ylab("Self Assistance Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.assist.plot

#plot 10
CPP.assist.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp, y = assist.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2.5) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#9d02d7", "#0000ff")) +
  xlab("CPP") +
  ylab("Self Assistance Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.assist.plot.grouped

## ~~~~ PART 8: Self-Assistance vs. CPP category ~~~~ ##
means.gencpp<-tapply(neonates.spec.CPP$assist.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#extreme       high        low
#0.09003623 0.81250000 0.71542317
tapply(neonates.spec.CPP$assist.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#extreme       high        low
#0.01213624 0.14192708 0.13360157
table(neonates.spec.CPP$cpp.gencat)
#extreme    high     low
#4       7      19
neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

#testing for significance with randomisation
random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.assist$assist) # Randomise self assistance variable
  random.assist <- tapply(randomised_status, beh.neonates.CPP.assist$species, function(x){ length(which(x =="TRUE")) }) # recount N births with self assistance
  random.n <- tapply(randomised_status, beh.neonates.CPP.assist$species, function(x){ length(which(x %in% c("TRUE","FALSE"))) }) #noting N births with assistance info
  random.assist.prop <- random.assist/random.n # recalculate proportion for self assistance
  random.extreme.high[i]<- mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm = "TRUE") - mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm = "TRUE") ## Store the difference high-extreme
  random.high.low[i]<- mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm = "TRUE") - mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm = "TRUE") ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm = "TRUE") - mean(random.assist.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm = "TRUE") ## Store the difference high-extreme
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
#0.559
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.001

#Plot 11: boxplot assistance vs CPP category
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.assist.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = assist.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("Self Assistance Proportion") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )
CPP.cat.assist.plot

## ~~~ Panel Plots ~~~ ##

ggarrange(CPP.death.plot,CPP.face.plot,CPP.OA.plot,CPP.assist.plot,
          labels = c("A", "B", "C", "D"), # tags for the plots
          nrow = 2, ncol = 2)

ggarrange(CPP.death.plot.grouped,CPP.face.plot.grouped,CPP.OA.plot.grouped,CPP.assist.plot.grouped,
          labels = c("A", "B", "C", "D"), # tags for the plots
          nrow = 2, ncol = 2,
          common.legend = TRUE,
          legend = "bottom")

ggarrange(CPP.cat.death.plot,CPP.cat.face.plot,CPP.cat.OA.plot,CPP.cat.assist.plot,
          labels = c("A", "B", "C", "D"), # tags for the plots
          nrow = 2, ncol = 2)

##~~~~~~~~~~~##
#vector export
ggsave("CPP_assist_plot_grouped.eps", plot = CPP.assist.plot.grouped
,
       width = 84, height = 70, units = "mm",
       device = cairo_ps)

#raster export
ggsave("CPP_assist_plot_grouped.tiff", plot = CPP.assist.plot.grouped
,
       width = 84, height = 70, units = "mm",
       dpi = 300, compression = "lzw")
