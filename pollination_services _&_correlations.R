

####################### Fruit set and quality ###############################


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
library(effects)
library(ggpubr)
library(gridGraphics)
library(effects)
library(ggeffects)
library(tidyverse)
library(MuMIn)
library(afex)
library(emmeans)
library(lsmeans)
library(dplyr)
std.error<-function(x) sd(x)/sqrt(length(x))

## Data used:
fruit_set<-read.csv(file.choose())
fruit_quality<-read.csv(file.choose())



##############################################################################
## FRUIT SET
##############################################################################


## per year
fruit_set2018<-subset(fruit_set,Year=="Year_2")
fruit_set2019<-subset(fruit_set,Year=="Year_3")

######################### 2018
success3<-fruit_set2018$Fruit_set
fail3<-fruit_set2018$Blossoms-fruit_set2018$Fruit_set
FruitMatured3<-cbind(success3,fail3)

model1.31<-glmer(data=fruit_set2018, FruitMatured3~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block), family = "binomial")
summary(model1.31)
car::Anova(model1.31, type=3)

model1.32<-glmer(data=fruit_set2018, FruitMatured3~Distance* fct_relevel(Alleyway_Treatment, "SWS")+
                   (1|Site_Code/Orchard_Block), family = "binomial")
summary(model1.32)

model1.33<-glmer(data=fruit_set2018, FruitMatured3~Distance* fct_relevel(Alleyway_Treatment, "CS")+
                   (1|Site_Code/Orchard_Block), family = "binomial")
summary(model1.33)

p10<-ggeffect(model1.31, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Percentage of fruit set (%)") +
  theme(legend.title=element_blank()) + ggtitle(label = "A")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  ## scale_x_continuous(limits=c(4,62),breaks=seq(4,62, 10))+ 
  scale_y_continuous(limits=c(0.05,0.26),breaks=seq(0.05, 0.25, 0.05))+
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p10




######################### 2019
success4<-fruit_set2019$Fruit_set
fail4<-fruit_set2019$Blossoms-fruit_set2019$Fruit_set
FruitMatured4<-cbind(success4,fail4)

model1.41<-glmer(data=fruit_set2019, FruitMatured4~Distance*Alleyway_Treatment+Distance+
                   (1|Site_Code/Orchard_Block), family = "binomial")
summary(model1.41)
car::Anova(model1.41, type=3)

model1.42<-glmer(data=fruit_set2019, FruitMatured4~Distance* fct_relevel(Alleyway_Treatment, "SWS")+
                   (1|Site_Code/Orchard_Block), family = "binomial")
summary(model1.42)

model1.43<-glmer(data=fruit_set2019, FruitMatured4~Distance* fct_relevel(Alleyway_Treatment, "CS")+
                   (1|Site_Code/Orchard_Block), family = "binomial")
summary(model1.43)

p11<-ggeffect(model1.41, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Percentage of fruit set (%)") +
  theme(legend.title=element_blank()) + ggtitle(label = "B")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  ## scale_x_continuous(limits=c(4,62),breaks=seq(4,62, 10))+ 
  scale_y_continuous(limits=c(0.05,0.26),breaks=seq(0.05, 0.25, 0.05))+
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p11


## FIGURE 7 ##### fruit set of open pollination ACORDING TO DISTANCE 
Figure7<-ggarrange(p10 + rremove("xlab"), p11 + rremove("ylab") + rremove("xlab"),
                   ncol=2, nrow=1, common.legend = TRUE, legend = "right", align = "hv",
                   font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
Figure7
annotate_figure(Figure7, bottom = text_grob("Distance from the orchard edge (m)",
                                            hjust = 1, x = 0.65, face = "bold", size = 12))
ggsave("Figure7.png", width = 8.3, height = 2.6, bg = "white")
## 800x250







##############################################################################
## FRUIT QUALITY
##############################################################################

################### 
## 
## subsets per year
fruit_quality2017<-subset(fruit_quality,Year=="Year_1")
fruit_quality2018<-subset(fruit_quality,Year=="Year_2")
fruit_quality2019<-subset(fruit_quality,Year=="Year_3")


################################# FRESH MASS ##

## 2017
model11<-lmer(data = fruit_quality2017, Fresh_mass~Distance*Alleyway_Treatment+Distance+Alleyway_Treatment+
                (1|Site_Code/Orchard_Block))
model11 %>% summary()

model11.1<-lmer(data=fruit_quality2017,Fresh_mass~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                      +(1|Site_Code/Orchard_Block))
model11.1 %>% summary()

model11.2<-lmer(data=fruit_quality2017,Fresh_mass~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model11.2 %>% summary()


p181<-ggeffect(model11, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Mass (g)") +
  theme(legend.title=element_blank()) + ggtitle(label = "A Mass 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  ## scale_y_continuous(limits=c(8,13.5),breaks=seq(8, 13.5, 1))+  
  scale_y_continuous(limits=c(10.5,13.5),breaks=seq(10, 13.5, 0.8))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p181


## 2018

model11.3<-lmer(data = fruit_quality2018, Fresh_mass~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model11.3 %>% summary()

model11.4<-lmer(data=fruit_quality2018,Fresh_mass~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model11.4 %>% summary()

model11.5<-lmer(data=fruit_quality2018,Fresh_mass~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model11.5 %>% summary()


p182<-ggeffect(model11.3, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Mass (g)") +
  theme(legend.title=element_blank()) + ggtitle(label = "B Mass 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  ## scale_y_continuous(limits=c(8,13.5),breaks=seq(8, 13.5, 1))+  
  scale_y_continuous(limits=c(8.4,11.1),breaks=seq(8, 13.5, 0.6))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p182


## 2019
model11.6<-lmer(data = fruit_quality2019, Fresh_mass~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model11.6 %>% summary()

model11.7<-lmer(data=fruit_quality2019,Fresh_mass~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model11.7 %>% summary()

model11.8<-lmer(data=fruit_quality2019,Fresh_mass~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model11.8 %>% summary()


 

################################# Height ##

## 2017
model12<-lmer(data = fruit_quality2017, Height~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model12 %>% summary()

model12.1<-lmer(data=fruit_quality2017,Height~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model12.1 %>% summary()

model12.2<-lmer(data=fruit_quality2017,Height~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model12.2 %>% summary()


p183<-ggeffect(model12, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Height (mm)") +
  theme(legend.title=element_blank()) + ggtitle(label = "C Height 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  ## scale_y_continuous(limits=c(24.2,30),breaks=seq(25, 30, 1))+  
  scale_y_continuous(limits=c(28,30),breaks=seq(25, 30, 0.5))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p183

## 2018
model12.3<-lmer(data = fruit_quality2018, Height~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model12.3 %>% summary()

model12.4<-lmer(data=fruit_quality2018,Height~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model12.4 %>% summary()

model12.5<-lmer(data=fruit_quality2018,Height~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model12.5 %>% summary()

p184<-ggeffect(model12.3, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Height (mm)") +
  theme(legend.title=element_blank()) + ggtitle(label = "D Height 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  ## scale_y_continuous(limits=c(24.2,30),breaks=seq(25, 30, 1))+  
  scale_y_continuous(limits=c(24.2,26.5),breaks=seq(23.8, 30, 0.6))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p184

## 2019
model12.6<-lmer(data = fruit_quality2019, Height~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model12.6 %>% summary()

model12.7<-lmer(data=fruit_quality2019,Height~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model12.7 %>% summary()

model12.8<-lmer(data=fruit_quality2019,Height~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model12.8 %>% summary()




################################# Width ##

## 2017
model13<-lmer(data = fruit_quality2017, Width~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model13 %>% summary()

model13.1<-lmer(data=fruit_quality2017,Width~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model13.1 %>% summary()

model13.2<-lmer(data=fruit_quality2017,Width~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model13.2 %>% summary()


p185<-ggeffect(model13, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Width (mm)") +
  theme(legend.title=element_blank()) + ggtitle(label = "E Width 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  ## scale_y_continuous(limits=c(27.5,30.6),breaks=seq(27.5, 30.5, 0.5))+  
  scale_y_continuous(limits=c(28.3,30.6),breaks=seq(27.4, 30.5, 0.6))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p185


## 2018
model13.3<-lmer(data = fruit_quality2018, Width~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model13.3 %>% summary()

model13.4<-lmer(data=fruit_quality2018,Width~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model13.4 %>% summary()

model13.5<-lmer(data=fruit_quality2018,Width~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model13.5 %>% summary()


## 2019
model13.6<-lmer(data = fruit_quality2019, Width~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model13.6 %>% summary()

model13.7<-lmer(data=fruit_quality2019,Width~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model13.7 %>% summary()

model13.8<-lmer(data=fruit_quality2019,Width~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model13.8 %>% summary()


p186<-ggeffect(model13.6, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Width (mm)") +
  theme(legend.title=element_blank()) + ggtitle(label = "F Width 2019")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,62),breaks=seq(5,70, 10))+ 
  ## scale_y_continuous(limits=c(27.5,30.6),breaks=seq(27.5, 30.5, 0.5))+  
  scale_y_continuous(limits=c(27.6,30.1),breaks=seq(27.6, 30.4, 0.6))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p186




################################# Length ##

bargraph.CI(data = OnlyOpen, Year,Length, group = Alleyway_Treatment, legend = T,
            ylim=c(24,27), ylab="Mean Length (mm)", xlab="Pollination treatment",
            cex.lab=1.2,col = c("navy", "mediumblue", "deepskyblue"))

## 2017
model14<-lmer(data = fruit_quality2017, Length~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model14 %>% summary()

model14.1<-lmer(data=fruit_quality2017,Length~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model14.1 %>% summary()

model14.2<-lmer(data=fruit_quality2017,Length~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model14.2 %>% summary()


p187<-ggeffect(model14, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Length (mm)") +
  theme(legend.title=element_blank()) + ggtitle(label = "G Length 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  scale_y_continuous(limits=c(24.7,28.2),breaks=seq(25, 29, 1))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p187


## 2019
model14.6<-lmer(data = fruit_quality2019, Length~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model14.6 %>% summary()

model14.7<-lmer(data=fruit_quality2019,Length~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model14.7 %>% summary()

model14.8<-lmer(data=fruit_quality2019,Length~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model14.8 %>% summary()






################################# Firmness ##

## 2017
model15<-lmer(data = fruit_quality2017, Firmness~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model15 %>% summary()

model15.1<-lmer(data=fruit_quality2017,Firmness~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model15.1 %>% summary()

model15.2<-lmer(data=fruit_quality2017,Firmness~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model15.2 %>% summary()


## 2018
model15.3<-lmer(data = fruit_quality2018, Firmness~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model15.3 %>% summary()

model15.4<-lmer(data=fruit_quality2018,Firmness~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model15.4 %>% summary()

model15.5<-lmer(data=fruit_quality2018,Firmness~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model15.5 %>% summary()


## 2019
model15.6<-lmer(data = fruit_quality2019, Firmness~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model15.6 %>% summary()

model15.7<-lmer(data=fruit_quality2019,Firmness~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model15.7 %>% summary()

model15.8<-lmer(data=fruit_quality2019,Firmness~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model15.8 %>% summary()




################################# Dry_Matter ##

## 2017
model16<-lmer(data = fruit_quality2017, Dry_Matter~Distance*Alleyway_Treatment+Distance+
                (1|Site_Code/Orchard_Block))
model16 %>% summary()

model16.1<-lmer(data=fruit_quality2017,Dry_Matter~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model16.1 %>% summary()

model16.2<-lmer(data=fruit_quality2017,Dry_Matter~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model16.2 %>% summary()


p188<-ggeffect(model16, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Dry matter (g)") +
  theme(legend.title=element_blank()) + ggtitle(label = "H Dry matter 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
  scale_y_continuous(limits=c(1.4,2.1),breaks=seq(1, 2, 0.2))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p188


## 2018
model16.3<-lmer(data = fruit_quality2018, Dry_Matter~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model16.3 %>% summary()

model16.4<-lmer(data=fruit_quality2018,Dry_Matter~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model16.4 %>% summary()

model16.5<-lmer(data=fruit_quality2018,Dry_Matter~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model16.5 %>% summary()


p189<-ggeffect(model16.3, terms = c("Distance", "Alleyway_Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Dry matter (g)") +
  theme(legend.title=element_blank()) + ggtitle(label = "I Dry matter 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(4,72),breaks=seq(5,70, 10))+ 
##  scale_y_continuous(limits=c(1.4,2.1),breaks=seq(1, 2, 0.2))+  
  scale_y_continuous(limits=c(1.4,2),breaks=seq(1, 2, 0.2))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p189


## 2019
model16.6<-lmer(data = fruit_quality2019, Dry_Matter~Distance*Alleyway_Treatment+Distance+
                  (1|Site_Code/Orchard_Block))
model16.6 %>% summary()

model16.7<-lmer(data=fruit_quality2019,Dry_Matter~Distance*fct_relevel(Alleyway_Treatment,"SWS")+Distance
                +(1|Site_Code/Orchard_Block))
model16.7 %>% summary()

model16.8<-lmer(data=fruit_quality2019,Dry_Matter~Distance*fct_relevel(Alleyway_Treatment,"CS")+Distance
                +(1|Site_Code/Orchard_Block))
model16.8 %>% summary()




#############
## FIGURE 8 ##### FLOWER VISITOR DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE DURING CHERRY
Figure8<-ggarrange(p181 + rremove("xlab"), p182 + rremove("xlab"),
                    p183 + rremove("xlab"), p184 + rremove("xlab"),
                    p185 + rremove("xlab"), p186 + rremove("xlab"),
                    p187 + rremove("xlab"), p188 + rremove("xlab"),
                    p189 + rremove("xlab"),
                    align = "hv", ncol=3, nrow=3, common.legend = TRUE, legend = "right",
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
Figure8
annotate_figure(Figure8, bottom = text_grob("Distance from the orchard edge (m)",
                                             hjust = 1, x = 0.65, face = "bold", size = 13))
ggsave("Figure8.png", width = 8.3, height = 5.2, bg = "white")
## 800x500







############# CORRELATIONS BETWEEN FLOWER VISITORS AND FRUIT

Correlations<-read.csv(file.choose())

## FRUIT SET
## 2018 and 2019

Years2and3Cor <- Correlations |> filter(Year_num %in% c(2018,2019))
Year2017Cor<-subset(Correlations,Year=="Year_1")
Year2018Cor<-subset(Correlations,Year=="Year_2")
Year2019Cor<-subset(Correlations,Year=="Year_3")

## 2018 
success10<-Year2018Cor$Fruit_set
fail10<-Year2018Cor$Blossoms-Year2018Cor$Fruit_set
FruitMatured10<-cbind(success10,fail10)

model100.1<-glmer(data=Year2018Cor,FruitMatured10~Distance*Total*Alleyway_treatment+Distance+
                       (1|Site_Code/Orchard_Block), family = binomial)
numcols2018Cor <- grep("^c\\.",names(Year2018Cor))
Year2018Cor_Scaled <- Year2018Cor
Year2018Cor_Scaled[,numcols2018Cor] <- scale(Year2018Cor_Scaled[,numcols2018Cor])
model100.1_SC <- update(model100.1,data=Year2018Cor_Scaled)
model100.1_SC %>% summary()

model100.2<-glmer(data=Year2018Cor,FruitMatured10~Distance*Total*
                       fct_relevel(Alleyway_treatment,"SWS")+Distance+
                  (1|Site_Code/Orchard_Block), family = binomial)
model100.2_SC <- update(model100.2,data=Year2018Cor_Scaled)
model100.2_SC %>% summary()

model100.3<-glmer(data=Year2018Cor,FruitMatured10~Distance*Total*
                       fct_relevel(Alleyway_treatment,"CS")+Distance+
                  (1|Site_Code/Orchard_Block), family = binomial)
model100.3_SC <- update(model100.3,data=Year2018Cor_Scaled)
model100.3_SC %>% summary()



### 2019
success11<-Year2019Cor$Fruit_set
fail11<-Year2019Cor$Blossoms-Year2019Cor$Fruit_set
FruitMatured11<-cbind(success11,fail11)

model100.4<-glmer(data=Year2019Cor,FruitMatured11~Distance*Total*Alleyway_treatment+Distance+
                    (1|Site_Code/Orchard_Block), family = binomial)
numcols2019Cor <- grep("^c\\.",names(Year2019Cor))
Year2019Cor_Scaled <- Year2019Cor
Year2019Cor_Scaled[,numcols2019Cor] <- scale(Year2019Cor_Scaled[,numcols2019Cor])
model100.4_SC <- update(model100.4,data=Year2019Cor_Scaled)
model100.4_SC %>% summary()

model100.5<-glmer(data=Year2019Cor,FruitMatured11~Distance*Total*
                    fct_relevel(Alleyway_treatment,"SWS")+Distance+
                    (1|Site_Code/Orchard_Block), family = binomial)
model100.5_SC <- update(model100.5,data=Year2019Cor_Scaled)
model100.5_SC %>% summary()

model100.6<-glmer(data=Year2019Cor,FruitMatured11~Distance*Total*
                    fct_relevel(Alleyway_treatment,"CS")+Distance+
                    (1|Site_Code/Orchard_Block), family = binomial)
model100.6_SC <- update(model100.6,data=Year2019Cor_Scaled)
model100.6_SC %>% summary()





#### Fruit quality _______ data not presented in the manuscript
################################## mass
## 2017
model101.1<-lmer(data=Year2017Cor,Fresh_mass~Distance*Total*Alleyway_treatment+Distance+
                    (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2017Cor,Fresh_mass~Distance*Total*
                    fct_relevel(Alleyway_treatment,"SWS")+Distance+
                    (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2017Cor,Fresh_mass~Distance*Total*
                    fct_relevel(Alleyway_treatment,"CS")+Distance+
                    (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2018
model101.1<-lmer(data=Year2018Cor,Fresh_mass~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2018Cor,Fresh_mass~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2018Cor,Fresh_mass~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2019
model101.1<-lmer(data=Year2019Cor,Fresh_mass~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2019Cor,Fresh_mass~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2019Cor,Fresh_mass~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


################################## height
## 2017
model101.1<-lmer(data=Year2017Cor,Height~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2017Cor,Height~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2017Cor,Height~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2018
model101.1<-lmer(data=Year2018Cor,Height~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2018Cor,Height~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2018Cor,Height~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2019
model101.1<-lmer(data=Year2019Cor,Height~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2019Cor,Height~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2019Cor,Height~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


################################## width
## 2017
model101.1<-lmer(data=Year2017Cor,Width~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2017Cor,Width~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2017Cor,Width~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2018
model101.1<-lmer(data=Year2018Cor,Width~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2018Cor,Width~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2018Cor,Width~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2019
model101.1<-lmer(data=Year2019Cor,Width~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2019Cor,Width~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2019Cor,Width~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()



################################## length
## 2017
model101.1<-lmer(data=Year2017Cor,Length~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2017Cor,Length~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2017Cor,Length~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2018
model101.1<-lmer(data=Year2018Cor,Length~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2018Cor,Length~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2018Cor,Length~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2019
model101.1<-lmer(data=Year2019Cor,Length~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2019Cor,Length~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2019Cor,Length~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


################################## firmness
## 2017
model101.1<-lmer(data=Year2017Cor,Firmness~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2017Cor,Firmness~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2017Cor,Firmness~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2018
model101.1<-lmer(data=Year2018Cor,Firmness~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2018Cor,Firmness~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2018Cor,Firmness~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2019
model101.1<-lmer(data=Year2019Cor,Firmness~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2019Cor,Firmness~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2019Cor,Firmness~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


################################## dry matter
## 2017
model101.1<-lmer(data=Year2017Cor,Dry_Matter~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2017Cor,Dry_Matter~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2017Cor,Dry_Matter~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2018
model101.1<-lmer(data=Year2018Cor,Dry_Matter~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2018Cor,Dry_Matter~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2018Cor,Dry_Matter~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


## 2019
model101.1<-lmer(data=Year2019Cor,Dry_Matter~Distance*Total*Alleyway_treatment+Distance+
                   (1|Site_Code/Orchard_Block))
model101.1 %>% summary()

model101.2<-lmer(data=Year2019Cor,Dry_Matter~Distance*Total*
                   fct_relevel(Alleyway_treatment,"SWS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.2 %>% summary()

model101.3<-lmer(data=Year2019Cor,Dry_Matter~Distance*Total*
                   fct_relevel(Alleyway_treatment,"CS")+Distance+
                   (1|Site_Code/Orchard_Block))
model101.3 %>% summary()


