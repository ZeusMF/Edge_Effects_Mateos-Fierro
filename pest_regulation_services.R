

###           ###############            PEST REGULATION SERVICES

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
std.error<-function(x) sd(x)/sqrt(length(x))


#################################################
############################ BAIT CARDS
###################
Bait_cards=read.csv(file.choose())

success<-Bait_cards$Aphids_available-Bait_cards$Aphids_remaining_Day_5
prop_predated<-cbind(success,Bait_cards$Aphids_available)
prop_predated

model1000.1 = glmer(prop_predated ~ Distance*Alleyway_Treatment + Distance +
                  (1 | Site_Code/Orchard_Code), family = "binomial", data = Bait_cards)
model1000.1 %>% summary() 
car::Anova(model1000.1, type=3)

## Re-scale and center continuous parameters
numcols_BC <- grep("^c\\.",names(Bait_cards))
Bait_cards_Scaled <- Bait_cards
Bait_cards_Scaled[,numcols_BC] <- scale(Bait_cards_Scaled[,numcols_BC])
model1000.1_sc <- update(model1000.1,data=Bait_cards_Scaled)
model1000.1_sc %>% summary() 
car::Anova(model1000.1_sc, type=3)

model1000.2<-glmer(prop_predated ~ Distance * fct_relevel(Alleyway_Treatment, "SWS") + Distance +
                        (1 | Site_Code/Orchard_Code), family = "binomial", data = Bait_cards)
model1000.2_sc <- update(model1000.2,data=Bait_cards_Scaled)
model1000.2_sc  %>% summary() 

model1000.3<-glmer(prop_predated ~ Distance * fct_relevel(Alleyway_Treatment, "CS") + Distance +
                        (1 | Site_Code/Orchard_Code), family = "binomial", data = Bait_cards)
model1000.3_sc <- update(model1000.3,data=Bait_cards_Scaled)
model1000.3_sc  %>% summary() 

## Figure 4
p1000<-ggeffect(model1000.1_sc, terms = c("Distance", "Alleyway_Treatment"))  %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Depletion") +
  theme(legend.title=element_blank()) + ggtitle(label = "")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(5,71),breaks=seq(5, 71, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p1000
ggsave("Figure4.png", width = 6.25, height = 3.6, bg = "white")
## 600x350


tapply(Bait_cards$Percentage_of_depletion_Day_5,list(Bait_cards$Alleyway_Treatment,Bait_cards$Distance),mean)

