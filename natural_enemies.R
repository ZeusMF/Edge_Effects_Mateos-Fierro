

###           ###############            NATURAL ENEMIES

## Packages
library(sciplot)
library(Matrix)
library(statmod)
library(lme4)
library(mvtnorm)
library(survival)
library(TH.data)
library(sandwich)
library(multcomp)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridGraphics)
library(ggeffects)
library(effects)
library(MuMIn)
library(lmerTest)
library(afex)
library(forcats)

### NATURAL ENEMY DENSITY, RICHNESS AND DIVERSITY IN ALLEYWAYS
### 
alleyways<-read.csv(file.choose())

std.error<-function(x) sd(x)/sqrt(length(x))



###############
######################################################### ALLEYWAYS

#########


## subsets
Year2017<-subset(alleyways,Year=="Year_1")
Year2018<-subset(alleyways,Year=="Year_2")
Year2019<-subset(alleyways,Year=="Year_3")


##################################
## sum, mean and sd values in alleyways
##################################

## Anthocoridae
tapply(alleyways$Anthocoridae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Anthocoridae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Anthocoridae,list(alleyways$Treatment,alleyways$Section),std.error)

## Nabidae
tapply(alleyways$Nabidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Nabidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Nabidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Chrysopidae
tapply(alleyways$Chrysopidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Chrysopidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Chrysopidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Hemerobiidae
tapply(alleyways$Hemerobiidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Hemerobiidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Hemerobiidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Carabidae
tapply(alleyways$Carabidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Carabidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Carabidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Coccinellidae
tapply(alleyways$Coccinellidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Coccinellidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Coccinellidae,list(alleyways$Treatment,alleyways$Section),std.error)

# # Staphylinidae
tapply(alleyways$Staphylinidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Staphylinidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Staphylinidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Syrphidae
tapply(alleyways$Syrphidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Syrphidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Syrphidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Parasitoid
tapply(alleyways$Parasitoid,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Parasitoid,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Parasitoid,list(alleyways$Treatment,alleyways$Section),std.error)

## Araneidae
tapply(alleyways$Araneidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Araneidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Araneidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Clubionidae
tapply(alleyways$Clubionidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Clubionidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Clubionidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Linyphiidae
tapply(alleyways$Linyphiidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Linyphiidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Linyphiidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Lycosidae
tapply(alleyways$Lycosidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Lycosidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Lycosidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Tetragnathidae
tapply(alleyways$Tetragnathidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Tetragnathidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Tetragnathidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Theridiidae
tapply(alleyways$Theridiidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Theridiidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Theridiidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Thomisidae
tapply(alleyways$Thomisidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Thomisidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Thomisidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Leiobunidae
tapply(alleyways$Leiobunidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Leiobunidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Leiobunidae,list(alleyways$Treatment,alleyways$Section),std.error)

## Phalangiidae
tapply(alleyways$Phalangiidae,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Phalangiidae,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Phalangiidae,list(alleyways$Treatment,alleyways$Section),std.error)


tapply(alleyways$Total_NE,list(alleyways$Treatment,alleyways$Section),sum)
tapply(alleyways$Total_NE,list(alleyways$Treatment,alleyways$Section),mean)
tapply(alleyways$Total_NE,list(alleyways$Treatment,alleyways$Section),std.error)

tapply(alleyways$Total_NE, alleyways$Section,sum)
tapply(alleyways$Total_NE, alleyways$Section,mean)
tapply(alleyways$Total_NE, alleyways$Section,std.error)

sum(alleyways$Total_NE)
mean(alleyways$Total_NE)
std.error(alleyways$Total_NE)

#################

tapply(Year2017$Total_NE,list(Year2017$Treatment,Year2017$Section),mean)

tapply(Year2017$Hemiptera,list(Year2017$Treatment,Year2017$Section),mean)
tapply(Year2018$Hemiptera,list(Year2018$Treatment,Year2018$Section),mean)
tapply(Year2018$Opiliones,list(Year2018$Treatment,Year2018$Section),mean)
tapply(Year2017$Parasitoids,list(Year2017$Treatment,Year2017$Section),mean)
tapply(Year2018$Neuroptera,list(Year2018$Treatment,Year2018$Section),mean)
tapply(Year2018$Parasitoids,list(Year2018$Treatment,Year2018$Section),mean)




######################################################################
##################################
## shapiro wilk tests in alleyways
##################################
  
shapiro.test(Year2017$Total_NE)
shapiro.test(Year2017$Family_richness)
shapiro.test(Year2017$Shannon)

shapiro.test(Year2017$Coleoptera)
shapiro.test(Year2017$Neuroptera)
shapiro.test(Year2017$Syrphidae)
shapiro.test(Year2017$Hemiptera)
shapiro.test(Year2017$Araneae)
shapiro.test(Year2017$Opiliones)
shapiro.test(Year2017$Parasitoids)

shapiro.test(Year2018$Total_NE)
shapiro.test(Year2018$Family_richness)
shapiro.test(Year2018$Shannon)

shapiro.test(Year2018$Coleoptera)
shapiro.test(Year2018$Neuroptera)
shapiro.test(Year2018$Syrphidae)
shapiro.test(Year2018$Hemiptera)
shapiro.test(Year2018$Araneae)
shapiro.test(Year2018$Opiliones)
shapiro.test(Year2018$Parasitoids)

shapiro.test(Year2019$Total_NE)
shapiro.test(Year2019$Family_richness)
shapiro.test(Year2019$Shannon)

shapiro.test(Year2019$Coleoptera)
shapiro.test(Year2019$Neuroptera)
shapiro.test(Year2019$Syrphidae)
shapiro.test(Year2019$Hemiptera)
shapiro.test(Year2019$Araneae)
shapiro.test(Year2019$Opiliones)
shapiro.test(Year2019$Parasitoids)
##################################


## GRAPHS AND MODELS FOR THE THREE YEARS _ DENSITY
########################### 2017
model1.10 = glmer.nb(Total_NE ~ Distance*Treatment+Distance +(1|Site_Code/Orchard_Block), data = Year2017)
model1.10 %>% summary() 
car::Anova(model1.10, type=3)

## Re-scale and center continuous parameters
numcols2017 <- grep("^c\\.",names(Year2017))
Year2017_Scaled <- Year2017
Year2017_Scaled[,numcols2017] <- scale(Year2017_Scaled[,numcols2017])
model1.10_sc <- update(model1.10,data=Year2017_Scaled)
model1.10_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model1.10.0<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "AMWS") + Distance
                      +(1 | Site_Code/Orchard_Block), data = Year2017) 
model1.10.0_sc <- update(model1.10.0,data=Year2017_Scaled)
summary(model1.10.0_sc)

model1.10.1<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017)
model1.10.1_sc <- update(model1.10.1,data=Year2017_Scaled)
summary(model1.10.1_sc)

model1.10.2<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017)
model1.10.2_sc <- update(model1.10.2,data=Year2017_Scaled)
summary(model1.10.2_sc)

##
## Year2017_Scaled$Treatment <- factor(Year2017_Scaled$Treatment, levels = c("CS", "SWS", "AMWS"))
p10<-ggeffect(model1.10_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "A")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,18),breaks=seq(0, 18, 3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p10



########################### 2018
model1.11 = glmer.nb(Total_NE ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = Year2018)
model1.11 %>% summary() 
car::Anova(model1.11, type=3)
## re-scale
numcols2018 <- grep("^c\\.",names(Year2018))
Year2018_Scaled <- Year2018
Year2018_Scaled[,numcols2018] <- scale(Year2018_Scaled[,numcols2018])
model1.11_sc <- update(model1.11,data=Year2018_Scaled)
model1.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model1.11.1<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2018)
model1.11.1_sc <- update(model1.11.1,data=Year2018_Scaled)|> summary()
summary(model1.11.1_sc)

model1.11.2<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2018)
model1.11.2_sc <- update(model1.11.2,data=Year2018_Scaled)
summary(model1.11.2_sc)

p11<-ggeffect(model1.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "B")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,18),breaks=seq(0, 18, 3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p11


########################### 2019
model1.12 = glmer.nb(Total_NE ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = Year2019)
model1.12 %>% summary() 
car::Anova(model1.12, type=3)
## re-scale
numcols2019 <- grep("^c\\.",names(Year2019))
Year2019_Scaled <- Year2019
Year2019_Scaled[,numcols2019] <- scale(Year2019_Scaled[,numcols2019])
model1.12_sc <- update(model1.12,data=Year2019_Scaled)
model1.12_sc %>% summary()

model1.12.1<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2019)
model1.12.1_sc <- update(model1.12.1,data=Year2019_Scaled)|> summary()
summary(model1.12.1_sc)

model1.12.2<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2019)
model1.12.2_sc <- update(model1.12.2,data=Year2019_Scaled)
summary(model1.12.2_sc)

p12<-ggeffect(model1.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "C")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,18),breaks=seq(0, 18, 3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p12



## Models per year and treatment for FAMILY RICHNESS
### 2017

## GRAPH FOR THE THREE YEARS _ FAMILY RICHNESS

########################### 2017
model2.10 = glmer.nb(Family_richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = Year2017)
model2.10 %>% summary() 
car::Anova(model2.10, type=3)
model2.10_sc <- update(model2.10,data=Year2017_Scaled)
model2.10_sc %>% summary()

model2.10.1<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017)
model2.10.1_sc <- update(model2.10.1,data=Year2017_Scaled)|> summary()
summary(model2.10.1_sc)

model2.10.2<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017)
model2.10.2_sc <- update(model2.10.2,data=Year2017_Scaled)
summary(model2.10.2_sc)

p20<-ggeffect(model2.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Family richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "D")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,5),breaks=seq(0, 5, 1))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p20

## 2018
model2.11 = glmer.nb(Family_richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model2.11 %>% summary() 
car::Anova(model2.11, type=3)
model2.11_sc <- update(model2.11,data=Year2018_Scaled)
model2.11_sc %>% summary()

model2.11.1<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2018)
model2.11.1_sc <- update(model2.11.1,data=Year2018_Scaled)|> summary()
summary(model2.11.1_sc)

model2.11.2<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2018)
model2.11.2_sc <- update(model2.11.2,data=Year2018_Scaled)
summary(model2.11.2_sc)

p21<-ggeffect(model2.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Family richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "E")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,5),breaks=seq(0, 5, 1))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p21


##2019
model2.12 = glmer.nb(Family_richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model2.12 %>% summary() 
car::Anova(model2.12, type=3)
model2.12_sc <- update(model2.12,data=Year2019_Scaled)
model2.12_sc %>% summary()

model2.12.1<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2019)
model2.12.1_sc <- update(model2.12.1,data=Year2019_Scaled)|> summary()
summary(model2.12.1_sc)

model2.12.2<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2019)
model2.12.2_sc <- update(model2.12.2,data=Year2019_Scaled)
summary(model2.12.2_sc)

p22<-ggeffect(model2.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Family richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "F")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,5),breaks=seq(0, 5, 1))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p22





## GRAPHS AND MODELS FOR THE THREE YEARS _ SHANNON DIVERSITY

########################### 2017
model3.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model3.10 %>% summary() 
car::Anova(model3.10, type=3)
model3.10_sc <- update(model3.10,data=Year2017_Scaled)
model3.10_sc %>% summary()

model3.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017)
model3.10.1_sc <- update(model3.10.1,data=Year2017_Scaled)|> summary()
summary(model3.10.1_sc)

model3.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017)
model3.10.2_sc <- update(model3.10.2,data=Year2017_Scaled)
summary(model3.10.2_sc)

p30<-ggeffect(model3.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "G")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,1.3),breaks=seq(0, 1.2, 0.4))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p30


## 2018
model3.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = Year2018)
model3.11 %>% summary() 
car::Anova(model3.11, type=3)
model3.11_sc <- update(model3.11,data=Year2018_Scaled)
model3.11_sc %>% summary()

model3.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = Year2018)
model3.11.1_sc <- update(model3.11.1,data=Year2018_Scaled)|> summary()
summary(model3.11.1_sc)

model3.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = Year2018)
model3.11.2_sc <- update(model3.11.2,data=Year2018_Scaled)
summary(model3.11.2_sc)


p31<-ggeffect(model3.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "H")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,1.3),breaks=seq(0, 1.2, 0.4))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p31


## 2019
model3.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = Year2019)
model3.12 %>% summary() 
car::Anova(model3.12, type=3)
model3.12_sc <- update(model3.12,data=Year2019_Scaled)
model3.12_sc %>% summary()

model3.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = Year2019)
model3.12.1_sc <- update(model3.12.1,data=Year2019_Scaled)|> summary()
summary(model3.12.1_sc)

model3.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = Year2019)
model3.12.2_sc <- update(model3.12.2,data=Year2019_Scaled)
summary(model3.12.2_sc)


p32<-ggeffect(model3.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "I")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,1.3),breaks=seq(0, 1.2, 0.4))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p32



#############
## FIGURE S3 ##### NATURAL ENEMY DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE IN ALLEYWAYS
FigureS3<-ggarrange(p10 + rremove("xlab"), p11 + rremove("ylab") + rremove("xlab"), 
          p12 + rremove("ylab") + rremove("xlab"), p20 + rremove("xlab"),
          p21 + rremove("ylab") + rremove("xlab"), p22 + rremove("ylab") + rremove("xlab"),
          p30 + rremove("xlab"), p31 + rremove("ylab") + rremove("xlab"),
          p32 + rremove("ylab") + rremove("xlab"), align = "hv",
          ncol=3, nrow=3, common.legend = TRUE, legend = "right",
          font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
FigureS3
annotate_figure(FigureS3, bottom = text_grob("Distance from the orchard edge (m)",
                                             hjust = 1, x = 0.65, face = "bold", size = 13))
ggsave("FigureS3.png", width = 8.3, height = 5.2, bg = "white")
## 800x500 ## 8.3 x 5.2






###########################
##### individual taxa

########################### Coleoptera
## 2017
model4.1 = glmer.nb(Coleoptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model4.1_sc <- update(model4.1,data=Year2017_Scaled)
model4.1_sc %>% summary()

model4.11<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = Year2017)
model4.11_sc <- update(model4.11,data=Year2017_Scaled)
summary(model4.11_sc)

model4.12<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = Year2017)
model4.12_sc <- update(model4.12,data=Year2017_Scaled)
summary(model4.12_sc)

## 2018
model4.2 = glmer.nb(Coleoptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model4.2_sc <- update(model4.2,data=Year2018_Scaled)
model4.2_sc %>% summary()

model4.21<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model4.21_sc <- update(model4.21,data=Year2018_Scaled)
summary(model4.21_sc)

model4.22<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model4.22_sc <- update(model4.22,data=Year2018_Scaled)
summary(model4.22_sc)

## 2019
model4.3 = glmer.nb(Coleoptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model4.3_sc <- update(model4.3,data=Year2019_Scaled)
model4.3_sc %>% summary()

model4.31<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model4.31_sc <- update(model4.31,data=Year2019_Scaled)
summary(model4.31_sc)

model4.32<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model4.32_sc <- update(model4.32,data=Year2019_Scaled)
summary(model4.32_sc)




########################### Neuroptera
model5.1 = glmer.nb(Neuroptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model5.1_sc <- update(model5.1,data=Year2017_Scaled)
model5.1_sc %>% summary()

model5.11<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model5.11_sc <- update(model5.11,data=Year2017_Scaled)|> summary()
summary(model5.11_sc)

model5.12<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model5.12_sc <- update(model5.12,data=Year2017_Scaled)
summary(model5.12_sc)

## 2018
model5.2 = glmer.nb(Neuroptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model5.2_sc <- update(model5.2,data=Year2018_Scaled)
model5.2_sc %>% summary()
plot(model5.2_sc)

model5.21<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model5.21_sc <- update(model5.21,data=Year2018_Scaled)
summary(model5.21_sc)

model5.22<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model5.22_sc <- update(model5.22,data=Year2018_Scaled)
summary(model5.22_sc)

p50<-ggeffect(model5.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "A Lacewings 2018")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p50

## 2019
model5.3 = glmer.nb(Nabidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model5.3_sc <- update(model5.3,data=Year2019_Scaled)
model5.3_sc %>% summary()

model5.31<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model5.31_sc <- update(model5.31,data=Year2019_Scaled)
summary(model5.31_sc)

model5.32<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model5.32_sc <- update(model5.32,data=Year2019_Scaled)
summary(model5.32_sc)



########################### Syrphidae
## 2017
model6.1 = glmer.nb(Syrphidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model6.1_sc <- update(model6.1,data=Year2017_Scaled)
model6.1_sc %>% summary()

model6.11<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model6.11_sc <- update(model6.11,data=Year2017_Scaled)
summary(model6.11_sc)

model6.12<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model6.12_sc <- update(model6.12,data=Year2017_Scaled)
summary(model6.12_sc)


## 2018
model6.2 = glmer.nb(Syrphidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model6.2_sc <- update(model6.2,data=Year2018_Scaled)
model6.2_sc %>% summary()

model6.21<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model6.21_sc <- update(model6.21,data=Year2018_Scaled)
summary(model6.21_sc)

model6.22<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model6.22_sc <- update(model6.22,data=Year2018_Scaled)
summary(model6.22_sc)


## 2019
model6.3 = glmer.nb(Syrphidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model6.3_sc <- update(model6.3,data=Year2019_Scaled)
model6.3_sc %>% summary()

model6.31<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model6.31_sc <- update(model6.31,data=Year2019_Scaled)
summary(model6.31_sc)

model6.32<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model6.32_sc <- update(model6.32,data=Year2019_Scaled)
summary(model6.32_sc)



########################### Hemiptera
## 2017
model7.1 = glmer.nb(Hemiptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model7.1_sc <- update(model7.1,data=Year2017_Scaled)
model7.1_sc %>% summary()
plot(model7.1_sc)

model7.11<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model7.11_sc <- update(model7.11,data=Year2017_Scaled)|> summary()
summary(model7.11_sc)

model7.12<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model7.12_sc <- update(model7.12,data=Year2017_Scaled)
summary(model7.12_sc)

p51<-ggeffect(model7.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "B Bugs 2017")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,0.15),breaks=seq(0, 0.15, 0.05))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p51


## 2018
model7.2 = glmer.nb(Hemiptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model7.2_sc <- update(model7.2,data=Year2018_Scaled)
model7.2_sc %>% summary()
plot(model7.2_sc)

model7.21<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model7.21_sc <- update(model7.21,data=Year2018_Scaled)
summary(model7.21_sc)

model7.22<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model7.22_sc <- update(model7.22,data=Year2018_Scaled)
summary(model7.22_sc)

p52<-ggeffect(model7.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "C Bugs 2018")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,0.28),breaks=seq(0, 0.3, 0.07))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p52


## 2019
model7.3 = glmer.nb(Hemiptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model7.3_sc <- update(model7.3,data=Year2019_Scaled)
model7.3_sc %>% summary()

model7.31<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model7.31_sc <- update(model7.31,data=Year2019_Scaled)
summary(model7.31_sc)

model7.32<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model7.32_sc <- update(model7.32,data=Year2019_Scaled)
model7.32_sc %>% summary()


########################### Araneae
## 2017
model8.1 = glmer.nb(Araneae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model8.1_sc <- update(model8.1,data=Year2017_Scaled)
model8.1_sc %>% summary()

model8.11<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model8.11_sc <- update(model8.11,data=Year2017_Scaled)
model8.11_sc %>% summary()

model8.12<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model8.12_sc <- update(model8.12,data=Year2017_Scaled)
summary(model8.12_sc)


## 2018
model8.2 = glmer.nb(Araneae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model8.2_sc <- update(model8.2,data=Year2018_Scaled)
model8.2_sc %>% summary()

model8.21<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model8.21_sc <- update(model8.21,data=Year2018_Scaled)
summary(model8.21_sc)

model8.22<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model8.22_sc <- update(model8.22,data=Year2018_Scaled)
summary(model8.22_sc)


## 2019
model8.3 = glmer.nb(Araneae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model8.3_sc <- update(model8.3,data=Year2019_Scaled)
model8.3_sc %>% summary()

model8.31<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model8.31_sc <- update(model8.31,data=Year2019_Scaled)
summary(model8.31_sc)

model8.32<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model8.32_sc <- update(model8.32,data=Year2019_Scaled)
summary(model8.32_sc)


########################### Opiliones
## 2017
model9.1 = glmer.nb(Opiliones ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model9.1_sc <- update(model9.1,data=Year2017_Scaled)
model9.1_sc %>% summary()

model9.11<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model9.11_sc <- update(model9.11,data=Year2017_Scaled)|> summary()
summary(model9.11_sc)

model9.12<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model9.12_sc <- update(model9.12,data=Year2017_Scaled)
summary(model9.12_sc)


## 2018
model9.2 = glmer.nb(Opiliones ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model9.2_sc <- update(model9.2,data=Year2018_Scaled)
model9.2_sc %>% summary()
plot(model9.2_sc)

model9.21<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model9.21_sc <- update(model9.21,data=Year2018_Scaled)
summary(model9.21_sc)

model9.22<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model9.22_sc <- update(model9.22,data=Year2018_Scaled)
summary(model9.22_sc)

p54<-ggeffect(model9.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "D Harvestmen 2018")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p54


## 2019
model9.3 = glmer.nb(Opiliones ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model9.3_sc <- update(model9.3,data=Year2019_Scaled)
model9.3_sc %>% summary()

model9.31<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model9.31_sc <- update(model9.31,data=Year2019_Scaled)
summary(model9.31_sc)

model9.32<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model9.32_sc <- update(model9.32,data=Year2019_Scaled)
summary(model9.32_sc)


########################### Parasitoid
## 2017
model10.1 = glmer.nb(Parasitoid ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017)
model10.1_sc <- update(model10.1,data=Year2017_Scaled)
model10.1_sc %>% summary()

model10.11<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model10.11_sc <- update(model10.11,data=Year2017_Scaled)|> summary()
summary(model10.11_sc)

model10.12<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017)
model10.12_sc <- update(model10.12,data=Year2017_Scaled)
summary(model10.12_sc)

p55<-ggeffect(model10.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "E Parasitoid wasps 2017")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0.1,1.4),breaks=seq(-0.3, 1.5, 0.4))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p55

## 2018
model10.2 = glmer.nb(Parasitoid ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018)
model10.2_sc <- update(model10.2,data=Year2018_Scaled)
model10.2_sc %>% summary()

model10.21<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model10.21_sc <- update(model10.21,data=Year2018_Scaled)
summary(model10.21_sc)

model10.22<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018)
model10.22_sc <- update(model10.22,data=Year2018_Scaled)
summary(model10.22_sc)

p56<-ggeffect(model10.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Mean number of individuals") +
  theme(legend.title=element_blank()) + ggtitle(label = "F Parasitoid wasps 2018")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0.3,2.5),breaks=seq(0, 2.5, 0.6))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p56


## 2019
model10.3 = glmer.nb(Parasitoid ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019)
model10.3_sc <- update(model10.3,data=Year2019_Scaled)
model10.3_sc %>% summary()

model10.31<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model10.31_sc <- update(model10.31,data=Year2019_Scaled)
summary(model10.31_sc)

model10.32<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019)
model10.32_sc <- update(model10.32,data=Year2019_Scaled)
summary(model10.32_sc)


## FIGURE 2 ##### NATURAL ENEMY DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE IN ALLEYWAYS
Figure2<-ggarrange(p50 + rremove("ylab") + rremove("xlab"), p51 + rremove("ylab") + rremove("xlab"),
          p52 + rremove("ylab") + rremove("xlab"), p54 + rremove("ylab") + rremove("xlab"), 
          p55 + rremove("ylab") + rremove("xlab"), p56 + rremove("ylab") + rremove("xlab"), 
          ncol=3, nrow=2, common.legend = TRUE, legend = "right",align = "hv",
          font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
Figure2
annotate_figure(Figure2, bottom = text_grob("Distance from the orchard edge (m)",
                                             hjust = 1, x = 0.65, face = "bold", size = 13),
                left = text_grob("Mean number of individuals", rot = 90, face = "bold", size = 13, y = 0.55))
## 800x350 ## 8.3 x 3.6
ggsave("Figure2.png", width = 8.3, height = 3.6, bg = "white")









####################################


### NATURAL ENEMY DENSITY, RICHNESS AND DIVERSITY IN CHERRY TREES
### 
cherry_trees<-read.csv(file.choose())

###############
### CHERRY TREES

#########

Year2017_2<-subset(cherry_trees,Year=="Year_1")
Year2018_2<-subset(cherry_trees,Year=="Year_2")
Year2019_2<-subset(cherry_trees,Year=="Year_3")


##################################
## sum, mean and sd values in cherry trees
##################################

## Anthocoridae
tapply(cherry_trees$Anthocoridae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Anthocoridae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Anthocoridae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Nabidae
tapply(cherry_trees$Nabidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Nabidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Nabidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Chrysopidae
tapply(cherry_trees$Chrysopidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Chrysopidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Chrysopidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Hemerobiidae
tapply(cherry_trees$Hemerobiidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Hemerobiidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Hemerobiidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Carabidae
tapply(cherry_trees$Carabidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Carabidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Carabidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Coccinellidae
tapply(cherry_trees$Coccinellidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Coccinellidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Coccinellidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

# # Staphylinidae
tapply(cherry_trees$Staphylinidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Staphylinidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Staphylinidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Syrphidae
tapply(cherry_trees$Syrphidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Syrphidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Syrphidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Parasitoid
tapply(cherry_trees$Parasitoid,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Parasitoid,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Parasitoid,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Araneidae
tapply(cherry_trees$Araneidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Araneidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Araneidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Clubionidae
tapply(cherry_trees$Clubionidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Clubionidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Clubionidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Dictynidae
tapply(cherry_trees$Dictynidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Dictynidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Dictynidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Linyphiidae
tapply(cherry_trees$Linyphiidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Linyphiidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Linyphiidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Philodromidae
tapply(cherry_trees$Philodromidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Philodromidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Philodromidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Salticidae
tapply(cherry_trees$Salticidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Salticidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Salticidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Tetragnathidae
tapply(cherry_trees$Tetragnathidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Tetragnathidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Tetragnathidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Theridiidae
tapply(cherry_trees$Theridiidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Theridiidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Theridiidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Thomisidae
tapply(cherry_trees$Thomisidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Thomisidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Thomisidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Leiobunidae
tapply(cherry_trees$Leiobunidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Leiobunidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Leiobunidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Phalangiidae
tapply(cherry_trees$Phalangiidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Phalangiidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Phalangiidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

## Anystidae
tapply(cherry_trees$Anystidae,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Anystidae,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Anystidae,list(cherry_trees$Treatment,cherry_trees$Section),std.error)


tapply(cherry_trees$Total_NE,list(cherry_trees$Treatment,cherry_trees$Section),sum)
tapply(cherry_trees$Total_NE,list(cherry_trees$Treatment,cherry_trees$Section),mean)
tapply(cherry_trees$Total_NE,list(cherry_trees$Treatment,cherry_trees$Section),std.error)

tapply(cherry_trees$Total_NE, cherry_trees$Section,sum)
tapply(cherry_trees$Total_NE, cherry_trees$Section,mean)
tapply(cherry_trees$Total_NE, cherry_trees$Section,std.error)

sum(cherry_trees$Total_NE)
mean(cherry_trees$Total_NE)
std.error(cherry_trees$Total_NE)
################

tapply(Year2019_2$Hemiptera,list(Year2019_2$Treatment,Year2019_2$Section),mean)
tapply(Year2017_2$Hemiptera,list(Year2017_2$Treatment,Year2017_2$Section),mean)
tapply(Year2019_2$Araneae,list(Year2019_2$Treatment,Year2019_2$Section),mean)
tapply(Year2018_2$Anystidae,list(Year2018_2$Treatment,Year2018_2$Section),mean)
tapply(Year2018_2$Parasitoids,list(Year2018_2$Treatment,Year2018_2$Section),mean)
tapply(Year2019_2$Syrphidae,list(Year2019_2$Treatment,Year2019_2$Section),mean)
tapply(Year2019_2$Opiliones,list(Year2019_2$Treatment,Year2019_2$Section),mean)


######################################################################
##################################
## shapiro wilk tests in alleyways
##################################

shapiro.test(Year2017_2$Total_NE)
shapiro.test(Year2017_2$Family_richness)
shapiro.test(Year2017_2$Shannon)

shapiro.test(Year2017_2$Coleoptera)
shapiro.test(Year2017_2$Neuroptera)
shapiro.test(Year2017_2$Syrphidae)
shapiro.test(Year2017_2$Hemiptera)
shapiro.test(Year2017_2$Araneae)
shapiro.test(Year2017_2$Opiliones)
shapiro.test(Year2017_2$Anystidae)
shapiro.test(Year2017_2$Parasitoids)

shapiro.test(Year2018_2$Total_NE)
shapiro.test(Year2018_2$Family_richness)
shapiro.test(Year2018_2$Shannon)

shapiro.test(Year2018_2$Coleoptera)
shapiro.test(Year2018_2$Neuroptera)
shapiro.test(Year2018_2$Syrphidae)
shapiro.test(Year2018_2$Hemiptera)
shapiro.test(Year2018_2$Araneae)
shapiro.test(Year2018_2$Opiliones)
shapiro.test(Year2018_2$Anystidae)
shapiro.test(Year2018_2$Parasitoids)

shapiro.test(Year2019_2$Total_NE)
shapiro.test(Year2019_2$Family_richness)
shapiro.test(Year2019_2$Shannon)

shapiro.test(Year2019_2$Coleoptera)
shapiro.test(Year2019_2$Neuroptera)
shapiro.test(Year2019_2$Syrphidae)
shapiro.test(Year2019_2$Hemiptera)
shapiro.test(Year2019_2$Araneae)
shapiro.test(Year2019_2$Opiliones)
shapiro.test(Year2019_2$Anystidae)
shapiro.test(Year2019_2$Parasitoids)
##################################



#
## GRAPHS AND MODELS FOR THE THREE YEARS _ DENSITY
### 2017

model10.10 = glmer.nb(Total_NE~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2017_2)
model10.10 %>% summary() 
car::Anova(model10.10, type=3)

## Re-scale and center continuous parameters
numcols2017_2 <- grep("^c\\.",names(Year2017_2))
Year2017_2_Scaled <- Year2017_2
Year2017_2_Scaled[,numcols2017_2] <- scale(Year2017_2_Scaled[,numcols2017_2])
model10.10_sc <- update(model10.10,data=Year2017_2_Scaled)
model10.10_sc %>% summary() 

model1.10.1<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      +(1 | Site_Code/Orchard_Block), data = Year2017_2) 
model1.10.1_sc <- update(model1.10.1,data=Year2017_2_Scaled)
model1.10.1_sc  %>% summary() 

model1.10.2<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2017_2) 
model1.10.2_sc <- update(model1.10.2,data=Year2017_2_Scaled)
model1.10.2_sc  %>% summary() 


p100<-ggeffect(model10.10_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "A")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+
  scale_y_continuous(limits=c(0,8.6),breaks=seq(0, 8, 2))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p100


## 2018
model10.11 = glmer.nb(Total_NE~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2018_2)
model10.11 %>% summary() 
## Re-scale
numcols2018_2 <- grep("^c\\.",names(Year2018_2))
Year2018_2_Scaled <- Year2018_2
Year2018_2_Scaled[,numcols2018_2] <- scale(Year2018_2_Scaled[,numcols2018_2])
model10.11_sc <- update(model10.11,data=Year2018_2_Scaled)
model10.11_sc %>% summary() 

model10.11.1<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      +(1 | Site_Code/Orchard_Block), data = Year2018_2) 
model10.11.1_sc <- update(model10.11.1,data=Year2018_2_Scaled)
model10.11.1_sc  %>% summary() 

model10.11.2<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = Year2018_2) 
model10.11.2_sc <- update(model10.11.2,data=Year2018_2_Scaled)
model10.11.2_sc  %>% summary() 


p110<-ggeffect(model10.11_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "B")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,8.6),breaks=seq(0, 8, 2))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p110


## 2019
model10.12 = glmer.nb(Total_NE~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2019_2)
model10.12 %>% summary() 
## Re-scale
numcols2019_2 <- grep("^c\\.",names(Year2019_2))
Year2019_2_Scaled <- Year2019_2
Year2019_2_Scaled[,numcols2019_2] <- scale(Year2019_2_Scaled[,numcols2019_2])
model10.12_sc <- update(model10.12,data=Year2019_2_Scaled)
model10.12_sc %>% summary() 

model10.12.1<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2019_2) 
model10.12.1_sc <- update(model10.12.1,data=Year2019_2_Scaled)
model10.12.1_sc  %>% summary() 

model10.12.2<-glmer.nb(Total_NE ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2019_2) 
model10.12.2_sc <- update(model10.12.2,data=Year2019_2_Scaled)
model10.12.2_sc  %>% summary() 


p120<-ggeffect(model10.12_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "C")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,8.6),breaks=seq(0, 8, 2))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p120



## GRAPH AND MODELS FOR THE THREE YEARS _ FAMILY RICHNESS
## 2017
model20.10 = glmer.nb(Family_richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2017_2)
model20.10 %>% summary() 
model20.10_sc <- update(model20.10,data=Year2017_2_Scaled)
model20.10_sc %>% summary() 

model20.10.1<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2017_2) 
model20.10.1_sc <- update(model20.10.1,data=Year2017_2_Scaled)
model20.10.1_sc  %>% summary() 

model20.10.2<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2017_2) 
model20.10.2_sc <- update(model20.10.2,data=Year2017_2_Scaled)
model20.10.2_sc  %>% summary() 

p200<-ggeffect(model20.10_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Family richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "D")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,3.2),breaks=seq(0, 3.2, 0.8))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p200


## 2018
model20.11 = glmer.nb(Family_richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2018_2)
model20.11 %>% summary() 
model20.11_sc <- update(model20.11,data=Year2018_2_Scaled)
model20.11_sc %>% summary() 

model20.11.1<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2018_2) 
model20.11.1_sc <- update(model20.11.1,data=Year2018_2_Scaled)
model20.11.1_sc  %>% summary() 

model20.11.2<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2018_2) 
model20.11.2_sc <- update(model20.11.2,data=Year2018_2_Scaled)
model20.11.2_sc  %>% summary() 

p210<-ggeffect(model20.11_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Family richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "E")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,3.2),breaks=seq(0, 3.2, 0.8))+   
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p210


## 2019
model20.12 = glmer.nb(Family_richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2019_2)
model20.12 %>% summary() 
model20.12_sc <- update(model20.12,data=Year2019_2_Scaled)
model20.12_sc %>% summary() 

model20.12.1<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2019_2) 
model20.12.1_sc <- update(model20.12.1,data=Year2019_2_Scaled)
model20.12.1_sc  %>% summary() 

model20.12.2<-glmer.nb(Family_richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2019_2) 
model20.12.2_sc <- update(model20.12.2,data=Year2019_2_Scaled)
model20.12.2_sc  %>% summary() 

p220<-ggeffect(model20.12_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Family richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "F")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,3.2),breaks=seq(0, 3.2, 0.8))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p220



## GRAPHS AND MODELS FOR THE THREE YEARS _ SHANNON DIVERSITY
## 2017
model30.10 = lmer(Shannon~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2017_2)
model30.10 %>% summary() 
model30.10_sc <- update(model30.10,data=Year2017_2_Scaled)
model30.10 %>% summary() 

model30.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2017_2) 
model30.10.1_sc <- update(model30.10.1,data=Year2017_2_Scaled)
model30.10.1_sc  %>% summary() 

model30.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = Year2017_2) 
model30.10.2_sc <- update(model30.10.2,data=Year2017_2_Scaled)
model30.10.2_sc  %>% summary() 

p300<-ggeffect(model30.10_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "G")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,0.9),breaks=seq(0, 0.9, 0.3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p300



model30.11 = lmer(Shannon~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2018_2)
model30.11 %>% summary() 
model30.11_sc <- update(model30.11,data=Year2018_2_Scaled)
model30.11 %>% summary() 

model30.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = Year2018_2) 
model30.11.1_sc <- update(model30.11.1,data=Year2018_2_Scaled)
model30.11.1_sc  %>% summary() 

model30.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SC") + Distance
                   + (1 | Site_Code/Orchard_Block), data = Year2018_2) 
model30.11.2_sc <- update(model30.11.2,data=Year2018_2_Scaled)
model30.11.2_sc  %>% summary() 

p310<-ggeffect(model30.11_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "H")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,0.9),breaks=seq(0, 0.9, 0.3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p310


model30.12 = lmer(Shannon~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=Year2019_2)
model30.12 %>% summary() 
model30.12_sc <- update(model30.12,data=Year2019_2_Scaled)
model30.12 %>% summary() 

model30.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = Year2019_2) 
model30.12.1_sc <- update(model30.12.1,data=Year2019_2_Scaled)
model30.12.1_sc  %>% summary() 

model30.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = Year2019_2) 
model30.12.2_sc <- update(model30.12.2,data=Year2019_2_Scaled)
model30.12.2_sc  %>% summary() 

p320<-ggeffect(model30.12_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "I")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,0.9),breaks=seq(0, 0.9, 0.3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p320 


#############
## FIGURE S4 ##### NATURAL ENEMY DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE IN CHERRY TREES
FigureS4<-ggarrange(p100 + rremove("xlab") + rremove("xlab"), p110 + rremove("ylab") + rremove("xlab"),
          p120 + rremove("ylab") + rremove("xlab"), p200 + rremove("xlab") + rremove("xlab"), 
          p210 + rremove("ylab") + rremove("xlab"), p220 + rremove("ylab") + rremove("xlab"),
          p300 + rremove("xlab") + rremove("xlab"), p310 + rremove("ylab") + rremove("xlab"),
          p320 + rremove("ylab") + rremove("xlab"), align = "hv",
          ncol=3, nrow=3, common.legend = TRUE, legend = "right",
          font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
FigureS4
annotate_figure(FigureS4, bottom = text_grob("Distance from the orchard edge (m)",
                                            hjust = 1, x = 0.65, face = "bold", size = 13))
ggsave("FigureS4.png", width = 8.3, height = 5.2, bg = "white")
## 800x500




###########################
##### individual taxa in cherry trees

########################### Coleoptera
## 2017
model40.1 = glmer.nb(Coleoptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model40.1_sc <- update(model40.1,data=Year2017_2_Scaled)
model40.1_sc %>% summary()

model40.11<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model40.11_sc <- update(model40.11,data=Year2017_2_Scaled)
model40.11_sc %>% summary()

model40.12<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model40.12_sc <- update(model40.12,data=Year2017_2_Scaled)
model40.12_sc %>% summary()

## 2018
model40.2 = glmer.nb(Coleoptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model40.2_sc <- update(model40.2,data=Year2018_2_Scaled)
model40.2_sc %>% summary()

model40.21<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model40.21_sc <- update(model40.21,data=Year2018_2_Scaled)
model40.21_sc %>% summary()

model40.22<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model40.22_sc <- update(model40.22,data=Year2018_2_Scaled)
model40.22_sc %>% summary()


## 2019
model40.3 = glmer.nb(Coleoptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model40.3_sc <- update(model40.3,data=Year2019_2_Scaled)
model40.3_sc %>% summary()

model40.31<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model40.31_sc <- update(model40.31,data=Year2019_2_Scaled)
model40.31_sc %>% summary()

model40.32<-glmer.nb(Coleoptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model40.32_sc <- update(model40.32,data=Year2019_2_Scaled)
model40.32_sc %>% summary()




########################### Neuroptera
model50.1 = glmer.nb(Neuroptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model50.1_sc <- update(model50.1,data=Year2017_2_Scaled)
model50.1_sc %>% summary()

model50.11<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model50.11_sc <- update(model50.11,data=Year2017_2_Scaled)|> summary()
model50.11_sc %>% summary()

model50.12<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model50.12_sc <- update(model50.12,data=Year2017_2_Scaled)
model50.12_sc %>% summary()

## 2018
model50.2 = glmer.nb(Neuroptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model50.2_sc <- update(model50.2,data=Year2018_2_Scaled)
model50.2_sc %>% summary()

model50.21<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model50.21_sc <- update(model50.21,data=Year2018_2_Scaled)
model50.21_sc %>% summary()

model50.22<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model50.22_sc <- update(model50.22,data=Year2018_2_Scaled)
model50.22_sc %>% summary()

## 2019
model50.3 = glmer.nb(Neuroptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model50.3_sc <- update(model50.3,data=Year2019_2_Scaled)
model50.3_sc %>% summary()

model50.31<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model50.31_sc <- update(model50.31,data=Year2019_2_Scaled)
model50.31_sc %>% summary()

model50.32<-glmer.nb(Neuroptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model50.32_sc <- update(model50.32,data=Year2019_2_Scaled)
model50.32_sc %>% summary()



########################### Syrphidae
## 2017
model60.1 = glmer.nb(Syrphidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model60.1_sc <- update(model60.1,data=Year2017_2_Scaled)
model60.1_sc %>% summary()

model60.11<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model60.11_sc <- update(model60.11,data=Year2017_2_Scaled)
model60.11_sc %>% summary()

model60.12<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model60.12_sc <- update(model60.12,data=Year2017_2_Scaled)
model60.12_sc %>% summary()


## 2018
model60.2 = glmer.nb(Syrphidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model60.2_sc <- update(model60.2,data=Year2018_2_Scaled)
model60.2_sc %>% summary()

model60.21<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model60.21_sc <- update(model60.21,data=Year2018_2_Scaled)
model60.21_sc %>% summary()

model60.22<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model60.22_sc <- update(model60.22,data=Year2018_2_Scaled)
model60.22_sc %>% summary()


## 2019
model60.3 = glmer.nb(Syrphidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model60.3_sc <- update(model60.3,data=Year2019_2_Scaled)
model60.3_sc %>% summary()

model60.31<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model60.31_sc <- update(model60.31,data=Year2019_2_Scaled)
model60.31_sc %>% summary()

model60.32<-glmer.nb(Syrphidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model60.32_sc <- update(model60.32,data=Year2019_2_Scaled)
model60.32_sc %>% summary()

p61<-ggeffect(model60.3_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Mean number of individuals") +
  theme(legend.title=element_blank()) + ggtitle(label = "A Hoverflies 2019")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p61

########################### Hemiptera
## 2017
model70.1 = glmer.nb(Hemiptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model70.1_sc <- update(model70.1,data=Year2017_2_Scaled)
model70.1_sc %>% summary()

model70.11<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model70.11_sc <- update(model70.11,data=Year2017_2_Scaled)|> summary()
model70.11_sc %>% summary()

model70.12<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model70.12_sc <- update(model70.12,data=Year2017_2_Scaled)
model70.12_sc %>% summary()

p62<-ggeffect(model70.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "B Bugs 2017")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p62


## 2018
model70.2 = glmer.nb(Hemiptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model70.2_sc <- update(model70.2,data=Year2018_2_Scaled)
model70.2_sc %>% summary()

model70.21<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model70.21_sc <- update(model70.21,data=Year2018_2_Scaled)
model70.21_sc %>% summary()

model70.22<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model70.22_sc <- update(model70.22,data=Year2018_2_Scaled)
model70.22_sc %>% summary()


## 2019
model70.3 = glmer.nb(Hemiptera ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model70.3_sc <- update(model70.3,data=Year2019_2_Scaled)
model70.3_sc %>% summary()

model70.31<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model70.31_sc <- update(model70.31,data=Year2019_2_Scaled)
model70.31_sc %>% summary()

model70.32<-glmer.nb(Hemiptera ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model70.32_sc <- update(model70.32,data=Year2019_2_Scaled)
model70.32_sc %>% summary()

p63<-ggeffect(model70.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "C Bugs 2019")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p63


########################### Araneae
## 2017
model80.1 = glmer.nb(Araneae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model80.1_sc <- update(model80.1,data=Year2017_2_Scaled)
model80.1_sc %>% summary()

model80.11<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model80.11_sc <- update(model80.11,data=Year2017_2_Scaled)
model80.11_sc %>% summary()

model80.12<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model80.12_sc <- update(model80.12,data=Year2017_2_Scaled)
model80.12_sc %>% summary()


## 2018
model80.2 = glmer.nb(Araneae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model80.2_sc <- update(model80.2,data=Year2018_2_Scaled)
model80.2_sc %>% summary()

model80.21<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model80.21_sc <- update(model80.21,data=Year2018_2_Scaled)
model80.21_sc %>% summary()

model80.22<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model80.22_sc <- update(model80.22,data=Year2018_2_Scaled)
model80.22_sc %>% summary()


## 2019
model80.3 = glmer.nb(Araneae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model80.3_sc <- update(model80.3,data=Year2019_2_Scaled)
model80.3_sc %>% summary()

model80.31<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model80.31_sc <- update(model80.31,data=Year2019_2_Scaled)
model80.31_sc %>% summary()

model80.32<-glmer.nb(Araneae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model80.32_sc <- update(model80.32,data=Year2019_2_Scaled)
model80.32_sc %>% summary()

p65<-ggeffect(model80.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "D Spiders 2019")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p65


########################### Opiliones
## 2017
model90.1 = glmer.nb(Opiliones ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model90.1_sc <- update(model90.1,data=Year2017_2_Scaled)
model90.1_sc %>% summary()

model90.11<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model90.11_sc <- update(model90.11,data=Year2017_2_Scaled)|> summary()
model90.11_sc %>% summary()

model90.12<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model90.12_sc <- update(model90.12,data=Year2017_2_Scaled)
model90.12_sc %>% summary()


## 2018
model90.2 = glmer.nb(Opiliones ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model90.2_sc <- update(model90.2,data=Year2018_2_Scaled)
model90.2_sc %>% summary()

model90.21<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model90.21_sc <- update(model90.21,data=Year2018_2_Scaled)
model90.21_sc %>% summary()

model90.22<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model90.22_sc <- update(model90.22,data=Year2018_2_Scaled)
model90.22_sc %>% summary()


## 2019
model90.3 = glmer.nb(Opiliones ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model90.3_sc <- update(model90.3,data=Year2019_2_Scaled)
model90.3_sc %>% summary()

model90.31<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model90.31_sc <- update(model90.31,data=Year2019_2_Scaled)
model90.31_sc %>% summary()

model90.32<-glmer.nb(Opiliones ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model90.32_sc <- update(model90.32,data=Year2019_2_Scaled)
model90.32_sc %>% summary()

p66<-ggeffect(model90.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "E Harvestmen 2019")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p66



########################### Anystidae
## 2017
model110.1 = glmer.nb(Anystidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model110.1_sc <- update(model110.1,data=Year2017_2_Scaled)
model110.1_sc %>% summary()

model110.11<-glmer.nb(Anystidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model110.11_sc <- update(model110.11,data=Year2017_2_Scaled)|> summary()
model110.11_sc %>% summary()

model110.12<-glmer.nb(Anystidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model110.12_sc <- update(model110.12,data=Year2017_2_Scaled)
model110.12_sc %>% summary()


## 2018
model110.2 = glmer.nb(Anystidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model110.2_sc <- update(model110.2,data=Year2018_2_Scaled)
model110.2_sc %>% summary()

model110.21<-glmer.nb(Anystidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model110.21_sc <- update(model110.21,data=Year2018_2_Scaled)
model110.21_sc %>% summary()

model110.22<-glmer.nb(Anystidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model110.22_sc <- update(model110.22,data=Year2018_2_Scaled)
model110.22_sc %>% summary()

p67<-ggeffect(model110.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "F Whirligig mites 2018")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0.4,2.7),breaks=seq(0, 3, 0.6))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p67

## 2019
model110.3 = glmer.nb(Anystidae ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model110.3_sc <- update(model110.3,data=Year2019_2_Scaled)
model110.3_sc %>% summary()

model110.31<-glmer.nb(Anystidae ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model110.31_sc <- update(model110.31,data=Year2019_2_Scaled)
model110.31_sc %>% summary()

model110.32<-glmer.nb(Anystidae ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model110.32_sc <- update(model110.32,data=Year2019_2_Scaled)
model110.32_sc %>% summary()


########################### Parasitoids
## 2017
model100.1 = glmer.nb(Parasitoid ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2017_2)
model100.1_sc <- update(model100.1,data=Year2017_2_Scaled)
model100.1_sc %>% summary()

model100.11<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                     + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model100.11_sc <- update(model100.11,data=Year2017_2_Scaled)
model100.11_sc %>% summary()

model100.12<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "CS") + Distance
                     + (1 | Site_Code/Orchard_Block), data = Year2017_2)
model100.12_sc <- update(model100.12,data=Year2017_2_Scaled)
model100.12_sc %>% summary()


## 2018
model100.2 = glmer.nb(Parasitoid ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2018_2)
model100.2_sc <- update(model100.2,data=Year2018_2_Scaled)
model100.2_sc %>% summary()

model100.21<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                     + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model100.21_sc <- update(model100.21,data=Year2018_2_Scaled)
model100.21_sc %>% summary()

model100.22<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "CS") + Distance
                     + (1 | Site_Code/Orchard_Block), data = Year2018_2)
model100.22_sc <- update(model100.22,data=Year2018_2_Scaled)
model100.22_sc %>% summary()


p68<-ggeffect(model100.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Mean number of individuals") +
  theme(legend.title=element_blank()) + ggtitle(label = "G Parasitoid wasps 2018")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(10,67),breaks=seq(10, 67, 10))+  
  scale_y_continuous(limits=c(0,0.36),breaks=seq(0, 0.4, 0.1))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p68


## 2019
model100.3 = glmer.nb(Parasitoid ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = Year2019_2)
model100.3_sc <- update(model100.3,data=Year2019_2_Scaled)
model100.3_sc %>% summary()

model100.31<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                     + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model100.31_sc <- update(model100.31,data=Year2019_2_Scaled)
model100.31_sc %>% summary()

model100.32<-glmer.nb(Parasitoid ~ Distance * fct_relevel(Treatment, "CS") + Distance
                     + (1 | Site_Code/Orchard_Block), data = Year2019_2)
model100.32_sc <- update(model100.32,data=Year2019_2_Scaled)
model100.32_sc %>% summary()


## FIGURE 3 ##### NATURAL ENEMY DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE IN CHERRY TREES
Figure3<-ggarrange(p61 + rremove("ylab") + rremove("xlab"), p62 + rremove("ylab") + rremove("xlab"),
                    p63 + rremove("ylab") + rremove("xlab"), p65 + rremove("ylab") + rremove("xlab"),
                    p66 + rremove("ylab") + rremove("xlab"), p67 + rremove("ylab") + rremove("xlab"),
                    p68 + rremove("ylab") + rremove("xlab"),
                    ncol=3, nrow=3, common.legend = TRUE, legend = "right",align = "hv",
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
Figure3
annotate_figure(Figure3, bottom = text_grob("Distance from the orchard edge (m)",
                                             hjust = 1, x = 0.65, face = "bold", size = 13),
                left = text_grob("Mean number of individuals", rot = 90, face = "bold", size = 13, y = 0.55))
ggsave("Figure3.png", width = 8.3, height = 5.2, bg = "white")
## 800x500



