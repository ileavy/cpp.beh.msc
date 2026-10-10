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
library(ggtext)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Filtering only species that we have CPP values for
beh.CPP<-droplevels(beh[!is.na(beh$cpp),])
#beh.CPP$species<-as.factor(beh.CPP$species)

#Filtering only species for which we have a CPP value at the genus level
beh.CPP.gen <-droplevels(beh[!is.na(beh$cpp.gen),])
#beh.CPP.gen.cat$species<-as.factor(beh.CPP.gen.cat$species)
#beh.CPP.gen.cat$genus<-as.factor(beh.CPP.gen.cat$genus)

#Filtering only species that we have a category at the the genus level
beh.CPP.gen.cat<-droplevels(beh[!is.na(beh$cpp.gencat),])

## ~~~~ PART 1: Death Proportion vs. CPP ~~~~ ##
# only for species that we have a CPP value for


beh.neonates.CPP.surv<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$inf.stat),]) # cleaning data to remove entries without infant survival data
beh.neonates.CPP.surv$species <- as.factor(beh.neonates.CPP.surv$species)
nrow(beh.neonates.CPP.surv) #351 cases we have survival data for

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.surv$species))) #creating new dataset at species level for species info on infant survival
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.surv$group, beh.neonates.CPP.surv$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.surv$cpp, beh.neonates.CPP.surv$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.surv$cpp.gen, beh.neonates.CPP.surv$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.surv$cpp.gencat, beh.neonates.CPP.surv$species, first) #variable: cpp.gencat
neonates.spec.CPP$inf.n<-tapply(beh.neonates.CPP.surv$inf.stat, beh.neonates.CPP.surv$species, function(x){ length(which(!is.na(x))) }) #variable: info.n (number of neonates per species with info on survival)

neonates.spec.CPP$inf.dead<-tapply(beh.neonates.CPP.surv$inf.stat, beh.neonates.CPP.surv$species, function(x){ length(which(x=="dead")) }) # new variable with number of dead infants per species
neonates.spec.CPP$death.prop<-as.numeric(neonates.spec.CPP$inf.dead/neonates.spec.CPP$inf.n) # new variable with death proportion
nrow(neonates.spec.CPP) #57 species we have death.prop for
#write.csv(neonates.spec.CPP, "neonates_spec_cpp_death.csv")

# Linear Regression
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
#0.232

# Linear Regression with genus-level CPP
reg.species.CPP<-summary(lm(neonates.spec.CPP$death.prop~neonates.spec.CPP$cpp.gen))
reg.species.CPP
#                         Estimate Std. Error t value Pr(>|t|)
#(Intercept)                0.06906    0.07839   0.881    0.384
#neonates.spec.CPP$cpp.gen  0.06774    0.08522   0.795    0.432

#Residual standard error: 0.2334 on 34 degrees of freedom
#(21 observations deleted due to missingness)
#Multiple R-squared:  0.01825,	Adjusted R-squared:  -0.01063 
#F-statistic: 0.6319 on 1 and 34 DF,  p-value: 0.4322

#with permutation
slope.species.CPP<-reg.species.CPP$coefficients[2]
random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.surv$inf.stat) # Randomise infant status (live/dead)
  random.inf.dead <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(which(x=="dead")) }) # recount N dead infants per species
  random.inf.n <- tapply(randomised_status, beh.neonates.CPP.surv$species, function(x){ length(!is.na(x)) }) #noting N infant with survival info per species
  random.death.prop <- random.inf.dead/random.inf.n # recalculate death prop
  lm.random<-summary(lm(random.death.prop~neonates.spec.CPP$cpp.gen))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.CPP)
length(which(abs(test_slope)>=abs(slope.species.CPP)))/1000
#0.284

## Plot 2: CPP.death.plot with grouping (species level)
CPP.death.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp, y = death.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#00baff", "#0000ff")) +
  xlab("CPP (species)") +
  ylab("Neonatal Death Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.death.plot.grouped

## ~~~~ PART 2: Death Proportion vs. CPP Category ~~~~ ##
# CPP category taken from the genus overall, but analyses still performed at genus level
# 36 species
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

neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
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
#0.791
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.982

# Plot 3: Box Plot CPP category vs. death proportion
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.death.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = death.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_jitter(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("Neonatal Death Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.cat.death.plot

## ~~~~ PART 3: Fetal Emergence (Presentation) vs. CPP ~~~~ ##
beh.neonates.CPP.pres<- droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$presentation),]) # cleaning data to remove neonates without presentation data
nrow(beh.neonates.CPP.pres) #87 cases

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.pres$species))) #creating new dataset at species level for species info on infant face/vertex presentation
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.pres$group, beh.neonates.CPP.pres$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.pres$cpp, beh.neonates.CPP.pres$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.pres$cpp.gen, beh.neonates.CPP.pres$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.pres$cpp.gencat, beh.neonates.CPP.pres$species, first) #variable: cpp.gencat

neonates.spec.CPP$face<-tapply(beh.neonates.CPP.pres$presentation, beh.neonates.CPP.pres$species, function(x){ length(which(x == "face"))} ) #variable:face (number of neonates per species with face or brow presentationl)
neonates.spec.CPP$vault<-tapply(beh.neonates.CPP.pres$presentation, beh.neonates.CPP.pres$species, function(x){ length(which(x=="vault")) }) # new variable with number of infants per species in vertex presentation
neonates.spec.CPP$face.prop<-as.numeric(neonates.spec.CPP$face/(neonates.spec.CPP$face+neonates.spec.CPP$vault)) # new variable with proportion of face presentation
#write.csv(neonates.spec.CPP, "neonates_spec_cpp_pres.csv")
nrow(neonates.spec.CPP) #23 species but only 16 with CPP genus data

# Regression
lm.model.face<-lm(neonates.spec.CPP$face.prop~neonates.spec.CPP$cpp.gen)
summary(lm.model.face)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)                0.09803    0.21092   0.465    0.649
#neonates.spec.CPP$cpp.gen  0.34410    0.22725   1.514    0.152

#Residual standard error: 0.4354 on 14 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.1407,	Adjusted R-squared:  0.07935 
#F-statistic: 2.293 on 1 and 14 DF,  p-value: 0.1522

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
#0.035

# Plot 4: scatter CPP vs. proportion of neonates in face presentation by group
CPP.face.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp.gen, y = face.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#00baff", "#0000ff")) +
  xlab("CPP (genus)") +
  ylab("Face Presentation Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.face.plot.grouped

## ~~~~ PART 4: Fetal Emergence (Presentation) vs. CPP Category ~~~~ ##
# CPP category taken from the genus overall, but analyses still performed at species level
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

neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
nrow(neonates.spec.CPP.plot) # n = 16
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
#0.899
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.331
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.099

# Plot 5: Box Plot CPP category vs. face proportion
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.face.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = face.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("Face Presentation Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.cat.face.plot

## ~~~~ PART 5: Fetal Emergence (Orientation) vs. CPP  ~~~~ ##
beh.neonates.CPP.pos<-droplevels(beh.neonates.CPP[!is.na(beh.neonates.CPP$orientation),]) # cleaning data to remove entries without orientation data
nrow(beh.neonates.CPP.pos) #124 cases

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.pos$species))) #creating new dataset at species level for species info on infant face/vertex presentation
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.pos$group, beh.neonates.CPP.pos$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.pos$cpp, beh.neonates.CPP.pos$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.pos$cpp.gen, beh.neonates.CPP.pos$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.pos$cpp.gencat, beh.neonates.CPP.pos$species, first) #variable: cpp.gencat

neonates.spec.CPP$OA<-tapply(beh.neonates.CPP.pos$orientation, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OA")) }) #variable:OA (number of neonates per species with OA positionl)
neonates.spec.CPP$OP<-tapply(beh.neonates.CPP.pos$orientation, beh.neonates.CPP.pos$species, function(x){ length(which(x=="OP")) }) # variable:OP (number of neonates per species with OP positionl)
neonates.spec.CPP$OP.prop<-as.numeric(neonates.spec.CPP$OP/(neonates.spec.CPP$OA+neonates.spec.CPP$OP)) # new variable with proportion of OP/(OA+OP)
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.OAOP.csv")
nrow(neonates.spec.CPP) #29 species

#Regression
lm.model.op<-lm(neonates.spec.CPP$OP.prop~neonates.spec.CPP$cpp.gen)
summary(lm.model.op)

#                           Estimate  Std. Error t value Pr(>|t|)
#(Intercept)                 0.5770     0.1349   4.278 0.000407 ***
#neonates.spec.CPP$cpp.gen   0.2341     0.1339   1.748 0.096566 .  

#Residual standard error: 0.3144 on 19 degrees of freedom
#(8 observations deleted due to missingness)
#Multiple R-squared:  0.1386,	Adjusted R-squared:  0.09323 
#F-statistic: 3.056 on 1 and 19 DF,  p-value: 0.09657

slope.species.op<-lm.model.op$coefficients[2]

random.slope <- rep(NA,times=999)

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pos$orientation) # randomise infant orientation
  random.op <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OP")) }) # recount N OP position
  random.n <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x %in% c("OA","OP"))) }) #noting N infant with position info
  random.op.prop <- random.op/random.n # recalculate proportion for oa pos
  lm.random<-summary(lm(random.op.prop~neonates.spec.CPP$cpp.gen))
  random.slope[i]<-lm.random$coefficients[2]
}

test_slope<-c(random.slope,slope.species.op)
length(which(abs(test_slope)>=abs(slope.species.op)))/1000 #sig
#0.023

#Plot 7: scatter OP prop vs. CPP grouped
CPP.OP.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp.gen, y = OP.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#00baff", "#0000ff")) +
  xlab("CPP (genus)") +
  ylab("OP Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.OP.plot.grouped

## ~~~~ PART 6: Fetal Emergence (Orientation) vs. CPP Category ~~~~ ##

means.gencpp<-tapply(neonates.spec.CPP$OP.prop, neonates.spec.CPP$cpp.gencat, mean, na.rm=T)
means.gencpp
#extreme      high       low
#0.9565217 0.6771429 0.7465565

tapply(neonates.spec.CPP$OP.prop, neonates.spec.CPP$cpp.gencat, var, na.rm=T)
#extreme        high         low
#0.009451796 0.171183673 0.123615285

table(neonates.spec.CPP$cpp.gencat)
#extreme    high     low
#   5       5      11

neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
nrow(neonates.spec.CPP.plot) # n = 21

neonates.spec.CPP$cpp.gencat <- factor(neonates.spec.CPP$cpp.gencat,
                                       levels = c("low", "high", "extreme"),ordered = TRUE)

# Testing for significance with randomisation

random.extreme.high <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and extreme CPP
random.high.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between high and low CPP
random.extreme.low <- rep(NA,times=999) # A vector to store randomised differences in mean face prop between low and extreme CPP

for(i in 1:999){
  randomised_status <- sample(beh.neonates.CPP.pos$orientation) # Randomise infant presentation
  random.op <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x =="OP")) }) # recount N OP position
  random.n <- tapply(randomised_status, beh.neonates.CPP.pos$species, function(x){ length(which(x %in% c("OA","OP"))) }) #noting N infant with position info
  random.op.prop <- random.op/random.n # recalculate proportion for oa pos
  random.extreme.high[i]<- mean(random.op.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.op.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) ## Store the difference high-extreme
  random.high.low[i]<- mean(random.op.prop[neonates.spec.CPP$cpp.gencat=="high"], na.rm=TRUE) - mean(random.op.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
  random.extreme.low[i]<- mean(random.op.prop[neonates.spec.CPP$cpp.gencat=="extreme"], na.rm=TRUE) - mean(random.op.prop[neonates.spec.CPP$cpp.gencat=="low"], na.rm=TRUE) ## Store the difference high-extreme
}

obs_dist.ex.hi<-means.gencpp[1]-means.gencpp[2]
obs_dist.hi.lo<-means.gencpp[2]-means.gencpp[3]
obs_dist.ex.lo<-means.gencpp[1]-means.gencpp[3]

test_dist.ex.hi <- c(random.extreme.high,obs_dist.ex.hi) # Add our observed difference to the series of randomisation outcomes
test_dist.hi.lo <- c(random.high.low,obs_dist.hi.lo) # Add our observed difference to the series of randomisation outcomes
test_dist.ex.lo <- c(random.extreme.low,obs_dist.ex.lo) # Add our observed difference to the series of randomisation outcomes

length(which(abs(test_dist.ex.hi )>=abs(obs_dist.ex.hi)))/1000 #sig difference extreme-high
#0.095
length(which(abs(test_dist.hi.lo )>=abs(obs_dist.hi.lo)))/1000 #sig difference low-high
#0.719
length(which(abs(test_dist.ex.lo )>=abs(obs_dist.ex.lo)))/1000 #sig difference extreme-low
#0.144

#Plot 8: boxplot CPP vs. OP proportion
neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
CPP.cat.OP.plot <- ggplot(neonates.spec.CPP.plot, aes(x = cpp.gencat, y = OP.prop, fill = cpp.gencat)) +
  geom_boxplot(outlier.shape = 16, linewidth = 0.4, alpha = 0.85) +
  #geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, shape = 8, size = 0.8, fill = "white", color = "black") +
  scale_fill_manual(values = c("#00baff", "#ffa600", "#cd34b5")) +
  scale_x_discrete(labels = c("Low", "High", "Extreme")) +
  xlab("CPP Category") +
  ylab("OP Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.cat.OP.plot

## ~~~~ PART 7: Self-Assistance vs. CPP ~~~~ ##
# 30 species

beh.CPP<-droplevels(beh[!is.na(beh$cpp.gencat),])
beh.neonates.CPP.assist<-droplevels(beh.CPP[!is.na(beh.CPP$assist),]) # cleaning data to remove entries without self assistance data
nrow(beh.neonates.CPP.assist) #174 cases

neonates.spec.CPP<- data.frame(species = as.factor(levels(beh.neonates.CPP.assist$species))) #creating new dataset at species level for species info
neonates.spec.CPP$group<-tapply(beh.neonates.CPP.assist$group, beh.neonates.CPP.assist$species, first) #variable: group
neonates.spec.CPP$cpp<-as.numeric(tapply(beh.neonates.CPP.assist$cpp, beh.neonates.CPP.assist$species, first)) #variable: cpp
neonates.spec.CPP$cpp.gen<-as.numeric(tapply(beh.neonates.CPP.assist$cpp.gen, beh.neonates.CPP.assist$species, first)) #variable: cpp.gen
neonates.spec.CPP$cpp.gencat<-tapply(beh.neonates.CPP.assist$cpp.gencat, beh.neonates.CPP.assist$species, first) #variable: cpp.gencat

neonates.spec.CPP$Y<-tapply(beh.neonates.CPP.assist$assist, beh.neonates.CPP.assist$species, function(x){ length(which(x =="TRUE")) }) #variable:Y (number of births per species with self-assistance)
neonates.spec.CPP$N<-tapply(beh.neonates.CPP.assist$assist, beh.neonates.CPP.assist$species, function(x){ length(which(x=="FALSE")) }) # variable:N (number of births per species without self assistance)
neonates.spec.CPP$assist.prop<-as.numeric(neonates.spec.CPP$Y/(neonates.spec.CPP$Y+neonates.spec.CPP$N)) # new variable with proportion of Y/Y+N
#write.csv(neonates.spec.CPP,"neonates.spec.CPP.assist.csv")
nrow(neonates.spec.CPP) #30

# CPP analyses at species level (17 species)
lm.model.assist<-lm(neonates.spec.CPP$assist.prop~neonates.spec.CPP$cpp.gen)
summary(lm.model.assist)
#                         Estimate Std. Error t value Pr(>|t|)
#(Intercept)                 1.0299     0.1403   7.338 5.45e-08 ***
#neonates.spec.CPP$cpp.gen  -0.5003     0.1654  -3.026  0.00527 ** 
#Residual standard error: 0.3599 on 28 degrees of freedom
#Multiple R-squared:  0.2464,	Adjusted R-squared:  0.2195 
#F-statistic: 9.154 on 1 and 28 DF,  p-value: 0.005273

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
#0.001

#plot 10 assistance vs CPP
CPP.assist.plot.grouped <- ggplot(neonates.spec.CPP, aes(x = cpp.gen, y = assist.prop)) +
  geom_smooth(method = lm, se = F, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(color = group), size = 2) +
  scale_color_manual(name = "Group",
                     labels = c("Cercopithecoidea", "Hominoidea", "Platyrrhini", "Strepsirrhini"),
                     values = c("#ffb14e", "#ea5f94", "#00baff", "#0000ff")) +
  xlab("CPP (genus)") +
  ylab("Self Assistance Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 8)
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

neonates.spec.CPP.plot <- droplevels(neonates.spec.CPP[!is.na(neonates.spec.CPP$cpp.gencat), ])
nrow(neonates.spec.CPP.plot) # n = 30
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
#0.584
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
  ylab("Self Assistance Prop.") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 8)
  )
CPP.cat.assist.plot

## ~~~~ PART 9: Additional Figures ~~~~ ##

## Panel Plots

library(patchwork)

( (CPP.death.plot.grouped+CPP.face.plot.grouped)/
    (CPP.OP.plot.grouped+CPP.assist.plot.grouped)+
    plot_layout(guides = "collect") + 
    plot_annotation(theme = theme(legend.position = "bottom")) )+ 
  plot_annotation(tag_levels = 'A')

ggsave("CPP_regressions_combined.png",
       width = 20, height = 15, units = "cm", dpi = 600)
  
( (CPP.cat.death.plot+CPP.cat.face.plot)/
    (CPP.cat.OP.plot+CPP.cat.assist.plot)+
    plot_layout(guides = "collect") )+ 
  plot_annotation(tag_levels = 'A')
  
ggsave("CPP_boxplots_combined.png",
       width = 20, height = 15, units = "cm", dpi = 600)


### MISSING OBJECTS! I cannot run the rest of the code.


## Lollipop Chart
spec.CPP <- spec[which(!is.na(spec$CPP)),] #drop species with no cpp values, n = 28
#adding a row for Homo sapiens
spec.CPP <- spec.CPP %>% add_row(N = NA, species.iucn = "Homo_sapiens", species = "Homo sapiens",
                           group = "Hominoidea", tribe = NA, family = NA, genus = "Homo",
                           have.beh = FALSE, have.cpp = TRUE,
                           reports.n = NA, videos.n = NA, sources.n = NA, cases.n = NA, infant.n = NA,
                           single.n = NA, twin.n = NA, triplet.n = NA, prim.n = NA, multip.n = NA,
                           mat.survived = NA, mat.died = NA, inf.info = NA, inf.live = NA, inf.still = NA, inf.comp = NA, case.death = NA,
                           death.prop = NA, ceph.n = NA, nonceph.n = NA, vault.n = NA, face.n = NA, brow.n = NA,
                           OP = NA, OA = NA, ass.T = NA, ass.F = NA, CPP = 0.82, CPP.gencat = "other",
                           .before = 14)

#set Homo sapiens to be bold
species_levels <- levels(reorder(spec.CPP$species, spec.CPP$CPP0))
my_labels <- setNames(
  ifelse(species_levels == "Homo sapiens",
         paste0("**", species_levels, "**"),
         species_levels),
  species_levels
)

#adding a variable that calculates distance from 1 for line length
spec.CPP$CPP0 <- spec.CPP$CPP-1

#plotting lollipop
CPP.species.plot <- ggplot(spec.CPP, aes(x = reorder(species, CPP0), y = CPP, color = CPP.gencat)) +
  geom_segment(aes(y = 1, x = species, yend = CPP, xend = species, color = CPP.gencat), linewidth = 0.6) +
  geom_point(size = 4) +
  geom_text(color = "black", size = 2, label = spec.CPP$CPP) +
  scale_color_manual(name = "CPP",
                     breaks = c("extreme", "high", "low", "other"),
                     labels = c("Extreme", "High", "Low", "Other"),
                     values = c("extreme" = "#cd34b5", "high" = "#ffa600", "low" = "#00baff", "other" = "gray60")) +
  scale_y_continuous(breaks = c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8)) +
  scale_x_discrete(labels = my_labels) +
  xlab("Species") +
  ylab("CPP") +
  coord_flip() +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    axis.text.y = element_markdown(face = "italic", size = 9),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )

CPP.species.plot

## CPP by group boxplot
spec.CPP.plot <- spec[which(!is.na(spec$CPP)),] #drop species with no cpp values, n = 28
homo.point <- spec.CPP[spec.CPP$species == "Homo sapiens", ]

CPP.group.boxplot <- ggplot(spec.CPP.plot, aes(x = group, y = CPP, fill = group)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.4, alpha = 0.85) +
  geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  geom_point(data = homo.point, aes(x = group, y = CPP),
             shape = 8, size = 3, fill = "white", color = "black", stroke = 1, inherit.aes = FALSE) +
  geom_text(data = homo.point, aes(x = group, y = CPP, label = species),
            inherit.aes = FALSE, size = 3, fontface = "italic",
            vjust = -1.5, hjust = 0.5) +
  scale_fill_manual(values = c("#ffb14e", "#ea5f94", "#9d02d7", "#0000ff", "gray")) +
  xlab("Group") +
  ylab("CPP") +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )

CPP.group.boxplot

## CPP by group violin plot
spec.CPP.plot <- spec[which(!is.na(spec$CPP)),] #drop species with no cpp values, n = 28
homo.point <- spec.CPP[spec.CPP$species == "Homo sapiens", ]

CPP.group.violin <- ggplot(spec.CPP.plot, aes(x = group, y = CPP, fill = group)) +
  geom_violin(outlier.shape = NA, linewidth = 0.4, alpha = 0.85) +
  geom_point(width = 0.15, size = 1.5, color = "black", alpha = 0.5) +
  geom_point(data = homo.point, aes(x = group, y = CPP),
             shape = 8, size = 3, fill = "white", color = "black", stroke = 1, inherit.aes = FALSE) +
  geom_text(data = homo.point, aes(x = group, y = CPP, label = species),
            inherit.aes = FALSE, size = 3, fontface = "italic",
            vjust = -1.5, hjust = 0.5) +
  scale_fill_manual(values = c("#ffb14e", "#ea5f94", "#9d02d7", "#0000ff", "gray")) +
  xlab("Group") +
  ylab("CPP") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )

CPP.group.violin

## ~~~~ PART 10: Descriptive Statistics ~~~~ ##
table(beh$group)
table(beh$habitat)
nrow(beh.neonates.CPP)

##~~~~~~~~~~~##
#vector export
ggsave("CPP_cat_lollipop.eps", plot = CPP.species.plot
,
       width = 84, height = 70, units = "mm",
       device = cairo_ps)

#raster export
ggsave("CPP_cat_lollipop.tiff", plot = CPP.species.plot
,
       width = 84, height = 70, units = "mm",
       dpi = 300, compression = "lzw")
