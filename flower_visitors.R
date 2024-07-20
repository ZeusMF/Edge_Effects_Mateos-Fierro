

###           ###############            FLOWER VISITORS = POLLINATORS

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


### FLOWER VISITOR DENSITY, RICHNESS AND DIVERSITY DURING CHERRY ANTHESIS
### 
pollinators<-read.csv(file.choose())



########################################### subsets
###############
#     two blossom periodS: cherry anthesis and post cherry anthesis
Cherry_anthesis<-subset(pollinators,Blossom_period=="Anthesis")
Post_cherry_anthesis<-subset(pollinators,Blossom_period=="Post_anthesis")

Cherry_anthesis_M<-subset(pollinators_mean,Blossom_period=="Anthesis")
Post_cherry_anthesis_M<-subset(pollinators_mean,Blossom_period=="Post_anthesis")

Cherry_anthesis_S<-subset(pollinators_sum,Blossom_period=="Anthesis")
Post_cherry_anthesis_S<-subset(pollinators_sum,Blossom_period=="Post_anthesis")


#     three years for each blossom period: 2017, 2018 and 2019
Year2017A<-subset(Cherry_anthesis,Year=="Year_1")
Year2018A<-subset(Cherry_anthesis,Year=="Year_2")
Year2019A<-subset(Cherry_anthesis,Year=="Year_3")

Year2017PA<-subset(Post_cherry_anthesis,Year=="Year_1")
Year2018PA<-subset(Post_cherry_anthesis,Year=="Year_2")
Year2019PA<-subset(Post_cherry_anthesis,Year=="Year_3")


#     four activities for each year and blossom period: 
#     cherry anthesis 2017, 2018 and 2019 
#     post cherry anthesis 2017, 2018 and 2019
cherryA2017<-subset(Year2017A,Activity=="cherry")
wildflowerA2017<-subset(Year2017A,Activity=="wildflower")
flyingA2017<-subset(Year2017A,Activity=="flying")
restingA2017<-subset(Year2017A,Activity=="resting")

cherryA2018<-subset(Year2018A,Activity=="cherry")
wildflowerA2018<-subset(Year2018A,Activity=="wildflower")
flyingA2018<-subset(Year2018A,Activity=="flying")
restingA2018<-subset(Year2018A,Activity=="resting")

cherryA2019<-subset(Year2019A,Activity=="cherry")
wildflowerA2019<-subset(Year2019A,Activity=="wildflower")
flyingA2019<-subset(Year2019A,Activity=="flying")
restingA2019<-subset(Year2019A,Activity=="resting")


nectaryPA2017<-subset(Year2017PA,Activity=="nectary")
wildflowerPA2017<-subset(Year2017PA,Activity=="wildflower")
flyingPA2017<-subset(Year2017PA,Activity=="flying")
restingPA2017<-subset(Year2017PA,Activity=="resting")

nectaryPA2018<-subset(Year2018PA,Activity=="nectary")
wildflowerPA2018<-subset(Year2018PA,Activity=="wildflower")
flyingPA2018<-subset(Year2018PA,Activity=="flying")
restingPA2018<-subset(Year2018PA,Activity=="resting")

nectaryPA2019<-subset(Year2019PA,Activity=="nectary")
wildflowerPA2019<-subset(Year2019PA,Activity=="wildflower")
flyingPA2019<-subset(Year2019PA,Activity=="flying")
restingPA2019<-subset(Year2019PA,Activity=="resting")


##################################
## sum, mean and se values ####       DURING CHERRY ANTHESIS
##################################

## Honeybee
tapply(Cherry_anthesis$Honeybee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),sum)
tapply(Cherry_anthesis$Honeybee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),mean)
tapply(Cherry_anthesis$Honeybee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),std.error)

## Buff_tailed_bumblebee
tapply(Cherry_anthesis$Buff_tailed_bumblebee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),sum)
tapply(Cherry_anthesis$Buff_tailed_bumblebee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),mean)
tapply(Cherry_anthesis$Buff_tailed_bumblebee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),std.error)

## Bumblebee
tapply(Cherry_anthesis$Bumblebee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),sum)
tapply(Cherry_anthesis$Bumblebee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),mean)
tapply(Cherry_anthesis$Bumblebee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),std.error)

## Solitary_bee
tapply(Cherry_anthesis$Solitary_bee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),sum)
tapply(Cherry_anthesis$Solitary_bee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),mean)
tapply(Cherry_anthesis$Solitary_bee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),std.error)

## Hoverfly
tapply(Cherry_anthesis$Hoverfly,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),sum)
tapply(Cherry_anthesis$Hoverfly,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),mean)
tapply(Cherry_anthesis$Hoverfly,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),std.error)


tapply(Cherry_anthesis$Total,list(Cherry_anthesis$Treatment,Cherry_anthesis$Section),sum)
tapply(Cherry_anthesis$Total,list(Cherry_anthesis$Treatment,Cherry_anthesis$Section),mean)
tapply(Cherry_anthesis$Total,list(Cherry_anthesis$Treatment,Cherry_anthesis$Section),std.error)

tapply(Cherry_anthesis$Total, Cherry_anthesis$Section,sum)
tapply(Cherry_anthesis$Total, Cherry_anthesis$Section,mean)
tapply(Cherry_anthesis$Total, Cherry_anthesis$Section,std.error)

sum(Cherry_anthesis$Total)
mean(Cherry_anthesis$Total)
std.error(Cherry_anthesis$Total)



tapply(Cherry_anthesis$Honeybee,
       list(Cherry_anthesis$Treatment,Cherry_anthesis$Section,Cherry_anthesis$Activity),mean)

CherryVisits<-subset(Cherry_anthesis,Activity=="cherry")
tapply(CherryVisits$Total,CherryVisits$Section,mean)
tapply(CherryVisits$Richness,CherryVisits$Section,mean)
tapply(CherryVisits$Shannon,CherryVisits$Section,mean)
tapply(CherryVisits$Honeybee,CherryVisits$Section,mean)
tapply(CherryVisits$Bumblebee,CherryVisits$Section,mean)
tapply(CherryVisits$Bumblebee,list(CherryVisits$Year,CherryVisits$Section),mean)
tapply(CherryVisits$Bumblebee,
       list(CherryVisits$Treatment,CherryVisits$Section,CherryVisits$Year),mean)
tapply(CherryVisits$Solitary_bee,
       list(CherryVisits$Treatment,CherryVisits$Section,CherryVisits$Year),mean)
tapply(CherryVisits$Hoverfly,
       list(CherryVisits$Treatment,CherryVisits$Section,CherryVisits$Year),mean)


WildflowerVisits<-subset(Post_cherry_anthesis,Activity=="wildflower")
tapply(WildflowerVisits$Total,
       list(WildflowerVisits$Treatment,WildflowerVisits$Section,WildflowerVisits$Year),mean)
tapply(WildflowerVisits$Honeybee,
       list(WildflowerVisits$Treatment,WildflowerVisits$Section,WildflowerVisits$Year),mean)
tapply(WildflowerVisits$Solitary_bee,
       list(WildflowerVisits$Treatment,WildflowerVisits$Section,WildflowerVisits$Year),mean)
tapply(WildflowerVisits$Hoverfly,
       list(WildflowerVisits$Treatment,WildflowerVisits$Section,WildflowerVisits$Year),mean)




##################################
## sum, mean and sd values ####       POST CHERRY ANTHESIS
##################################

## Honeybee
tapply(Post_cherry_anthesis$Honeybee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),sum)
tapply(Post_cherry_anthesis$Honeybee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),mean)
tapply(Post_cherry_anthesis$Honeybee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),std.error)

## Bumblebee
tapply(Post_cherry_anthesis$Bumblebee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),sum)
tapply(Post_cherry_anthesis$Bumblebee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),mean)
tapply(Post_cherry_anthesis$Bumblebee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),std.error)

## Solitary_bee
tapply(Post_cherry_anthesis$Solitary_bee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),sum)
tapply(Post_cherry_anthesis$Solitary_bee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),mean)
tapply(Post_cherry_anthesis$Solitary_bee,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),std.error)

## Hoverfly
tapply(Post_cherry_anthesis$Hoverfly,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),sum)
tapply(Post_cherry_anthesis$Hoverfly,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),mean)
tapply(Post_cherry_anthesis$Hoverfly,
       list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section,Post_cherry_anthesis$Activity),std.error)


tapply(Post_cherry_anthesis$Total,list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section),sum)
tapply(Post_cherry_anthesis$Total,list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section),mean)
tapply(Post_cherry_anthesis$Total,list(Post_cherry_anthesis$Treatment,Post_cherry_anthesis$Section),std.error)

tapply(Post_cherry_anthesis$Total, Post_cherry_anthesis$Section,sum)
tapply(Post_cherry_anthesis$Total, Post_cherry_anthesis$Section,mean)
tapply(Post_cherry_anthesis$Total, Post_cherry_anthesis$Section,std.error)

sum(Post_cherry_anthesis$Total)
mean(Post_cherry_anthesis$Total)
std.error(Post_cherry_anthesis$Total)










######################################################################
##################################
## shapiro-wilk tests 
##################################
  
shapiro.test(restingPA2019$Total)
shapiro.test(Year2017A$richness)
shapiro.test(Year2017A$Shannon)

shapiro.test(cherryA2017$Honeybee)
shapiro.test(Year2017A$Buff_tailed_bumblebee)
shapiro.test(Year2017A$Bumblebee)
shapiro.test(Year2017A$Solitary_bee)
shapiro.test(Year2017A$Hoverfly)

shapiro.test(Year2018A$Total)
shapiro.test(Year2018A$richness)
shapiro.test(Year2018A$Shannon)


shapiro.test(Year2019A$Total)
shapiro.test(Year2019A$richness)
shapiro.test(Year2019A$Shannon)

##################################


############################################# CHERRY ANTHESIS
#####################


############### GRAPHS AND MODELS FOR EACH ACTIVITY
#########################################################

########## visiting cherry flowers

###################################### DENSITY VISITING CHERRY ###################
################ 2017

model1.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=cherryA2017)
model1.20 %>% summary() 
## Re-scale and center continuous parameters
numcolscherryA2017 <- grep("^c\\.",names(cherryA2017))
cherryA2017_Scaled <- cherryA2017
cherryA2017_Scaled[,numcolscherryA2017] <- scale(cherryA2017_Scaled[,numcolscherryA2017])
model1.20_sc <- update(model1.20,data=cherryA2017_Scaled)
model1.20_sc %>% summary()

model1.20.0<-glmer.nb(Total~Distance*fct_relevel(Treatment, "AMWS") + Distance+
                        (1|Site_Code/Orchard_Block),data = cherryA2017) 
model1.20.0_sc <- update(model1.20.0,data=cherryA2017_Scaled)
model1.20.0_sc %>% summary()

model1.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model1.20.1_sc <- update(model1.20.1,data=cherryA2017_Scaled)
model1.20.1_sc %>% summary()

model1.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model1.20.2_sc <- update(model1.20.2,data=cherryA2017_Scaled)
model1.20.2_sc %>% summary()

##
p10<-ggeffect(model1.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "A")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_y_continuous(limits=c(0,2.1),breaks=seq(0, 2, 0.5))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p10



########################### 2018
model1.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = cherryA2018)
model1.11 %>% summary() 
## re-scale
numcolscherryA2018 <- grep("^c\\.",names(cherryA2018))
cherryA2018_Scaled <- cherryA2018
cherryA2018_Scaled[,numcolscherryA2018] <- scale(cherryA2018_Scaled[,numcolscherryA2018])
model1.11_sc <- update(model1.11,data=cherryA2018_Scaled)
model1.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model1.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model1.11.1_sc <- update(model1.11.1,data=cherryA2018_Scaled)|> summary()
summary(model1.11.1_sc)

model1.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model1.11.2_sc <- update(model1.11.2,data=cherryA2018_Scaled)
summary(model1.11.2_sc)

p11<-ggeffect(model1.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "B")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,2.1),breaks=seq(0, 2, 0.5))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p11


########################### 2019
model1.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = cherryA2019)
model1.12 %>% summary() 
## re-scale
numcolscherryA2019 <- grep("^c\\.",names(cherryA2019))
cherryA2019_Scaled <- cherryA2019
cherryA2019_Scaled[,numcolscherryA2019] <- scale(cherryA2019_Scaled[,numcolscherryA2019])
model1.12_sc <- update(model1.12,data=cherryA2019_Scaled)
model1.12_sc %>% summary()

model1.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model1.12.1_sc <- update(model1.12.1,data=cherryA2019_Scaled)|> summary()
summary(model1.12.1_sc)

model1.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model1.12.2_sc <- update(model1.12.2,data=cherryA2019_Scaled)
summary(model1.12.2_sc)

p12<-ggeffect(model1.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "C")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,2.1),breaks=seq(0, 2, 0.5))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p12




######################## SPECIES RICHNESS ## cherry
########################### 2017

model2.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = cherryA2017)
model2.10_sc <- update(model2.10,data=cherryA2017_Scaled)
model2.10_sc %>% summary()

model2.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model2.10.1_sc <- update(model2.10.1,data=cherryA2017_Scaled)|> summary()
summary(model2.10.1_sc)

model2.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model2.10.2_sc <- update(model2.10.2,data=cherryA2017_Scaled)
summary(model2.10.2_sc)

p20<-ggeffect(model2.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "D")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,1.1),breaks=seq(0, 1, 0.3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p20

## 2018
model2.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2018)
model2.11_sc <- update(model2.11,data=cherryA2018_Scaled)
model2.11_sc %>% summary()

model2.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model2.11.1_sc <- update(model2.11.1,data=cherryA2018_Scaled)|> summary()
summary(model2.11.1_sc)

model2.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model2.11.2_sc <- update(model2.11.2,data=cherryA2018_Scaled)
summary(model2.11.2_sc)

p21<-ggeffect(model2.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "E")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,1.1),breaks=seq(0, 1, 0.3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p21


## 2019
model2.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2019)
model2.12_sc <- update(model2.12,data=cherryA2019_Scaled)
model2.12_sc %>% summary()

model2.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model2.12.1_sc <- update(model2.12.1,data=cherryA2019_Scaled)|> summary()
summary(model2.12.1_sc)

model2.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model2.12.2_sc <- update(model2.12.2,data=cherryA2019_Scaled)
summary(model2.12.2_sc)

p22<-ggeffect(model2.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "F")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_y_continuous(limits=c(0,1.1),breaks=seq(0, 1, 0.3))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p22



######################### SHANNON DIVERSITY ## cherry
########################### 2017

model3.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2017)
model3.10_sc <- update(model3.10,data=cherryA2017_Scaled)
model3.10_sc %>% summary()

model3.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model3.10.1_sc <- update(model3.10.1,data=cherryA2017_Scaled)|> summary()
summary(model3.10.1_sc)

model3.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model3.10.2_sc <- update(model3.10.2,data=cherryA2017_Scaled)
summary(model3.10.2_sc)

p30<-ggeffect(model3.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "G")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(-0.04,0.19),breaks=seq(0, 0.19, 0.05))+
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p30


## 2018
model3.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = cherryA2018)
model3.11_sc <- update(model3.11,data=cherryA2018_Scaled)
model3.11_sc %>% summary()

model3.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model3.11.1_sc <- update(model3.11.1,data=cherryA2018_Scaled)|> summary()
summary(model3.11.1_sc)

model3.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model3.11.2_sc <- update(model3.11.2,data=cherryA2018_Scaled)
summary(model3.11.2_sc)


p31<-ggeffect(model3.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "H")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(-0.04,0.19),breaks=seq(0, 0.19, 0.05))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p31


## 2019
model3.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = cherryA2019)
model3.12_sc <- update(model3.12,data=cherryA2019_Scaled)
model3.12_sc %>% summary()

model3.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model3.12.1_sc <- update(model3.12.1,data=cherryA2019_Scaled)|> summary()
summary(model3.12.1_sc)

model3.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model3.12.2_sc <- update(model3.12.2,data=cherryA2019_Scaled)
summary(model3.12.2_sc)


p32<-ggeffect(model3.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "I")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(-0.04,0.19),breaks=seq(0, 0.19, 0.05))+
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p32


#############
## FIGURE s5 ##### FLOWER VISITOR DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE DURING CHERRY ANTHESIS
FigureS5<-ggarrange(p10 + rremove("xlab"), p11 + rremove("ylab") + rremove("xlab"),
          p12 + rremove("ylab") + rremove("xlab"), p20 + rremove("xlab"),
          p21 + rremove("ylab") + rremove("xlab"), p22 + rremove("ylab") + rremove("xlab"),
          p30 + rremove("xlab"), p31 + rremove("ylab") + rremove("xlab"),
          p32 + rremove("ylab") + rremove("xlab"), align = "hv",
          ncol=3, nrow=3, common.legend = TRUE, legend = "right",
          font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
FigureS5
annotate_figure(FigureS5, bottom = text_grob("Distance from the orchard edge (m)",
                                            hjust = 1, x = 0.65, face = "bold", size = 13))
ggsave("FigureS5.png", width = 8.3, height = 5.2, bg = "white")
## 800x500






#########################################################
########## visiting wildflowers


############################ DENSITY ## wildflowers
########################### 2017

model11.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=wildflowerA2017)
model11.20 %>% summary() 

## Re-scale and center continuous parameters
numcolswildflowerA2017 <- grep("^c\\.",names(wildflowerA2017))
wildflowerA2017_Scaled <- wildflowerA2017
wildflowerA2017_Scaled[,numcolswildflowerA2017] <- scale(wildflowerA2017_Scaled[,numcolswildflowerA2017])
model11.20_sc <- update(model11.20,data=wildflowerA2017_Scaled)
model11.20_sc %>% summary()

model11.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2017)
model11.20.1_sc <- update(model11.20.1,data=wildflowerA2017_Scaled)
model11.20.1_sc %>% summary()

model11.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2017)
model11.20.2_sc <- update(model11.20.2,data=wildflowerA2017_Scaled)
model11.20.2_sc %>% summary()

##
ggeffect(model11.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(a)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2018
model11.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = wildflowerA2018)
model11.11 %>% summary() 
## re-scale
numcolswildflowerA2018 <- grep("^c\\.",names(wildflowerA2018))
wildflowerA2018_Scaled <- wildflowerA2018
wildflowerA2018_Scaled[,numcolswildflowerA2018] <- scale(wildflowerA2018_Scaled[,numcolswildflowerA2018])
model11.11_sc <- update(model11.11,data=wildflowerA2018_Scaled)
model11.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model11.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model11.11.1_sc <- update(model11.11.1,data=cherryA2018_Scaled)|> summary()
summary(model11.11.1_sc)

model11.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model11.11.2_sc <- update(model11.11.2,data=cherryA2018_Scaled)
summary(model11.11.2_sc)

ggeffect(model11.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(b)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2019
model11.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = wildflowerA2019)
model11.12 %>% summary() 
## re-scale
numcolswildflowerA2019 <- grep("^c\\.",names(wildflowerA2019))
wildflowerA2019_Scaled <- wildflowerA2019
wildflowerA2019_Scaled[,numcolswildflowerA2019] <- scale(wildflowerA2019_Scaled[,numcolswildflowerA2019])
model11.12_sc <- update(model11.12,data=wildflowerA2019_Scaled)
model11.12_sc %>% summary()

model11.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model11.12.1_sc <- update(model11.12.1,data=wildflowerA2019_Scaled)|> summary()
summary(model11.12.1_sc)

model11.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model11.12.2_sc <- update(model11.12.2,data=wildflowerA2019_Scaled)
summary(model11.12.2_sc)

ggeffect(model11.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(c)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## Models per year and treatment for SPECIES RICHNESS
### 2017

## GRAPH FOR THE THREE YEARS _ SPECIES RICHNESS

########################### 2017
model12.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = wildflowerA2017)
model12.10_sc <- update(model12.10,data=wildflowerA2017_Scaled)
model12.10_sc %>% summary()

model12.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2017)
model12.10.1_sc <- update(model12.10.1,data=wildflowerA2017_Scaled)|> summary()
summary(model12.10.1_sc)

model12.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2017)
model12.10.2_sc <- update(model12.10.2,data=wildflowerA2017_Scaled)
summary(model12.10.2_sc)

ggeffect(model12.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(d)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")


## 2018
model12.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerA2018)
model12.11_sc <- update(model12.11,data=wildflowerA2018_Scaled)
model12.11_sc %>% summary()

model12.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2018)
model12.11.1_sc <- update(model12.11.1,data=wildflowerA2018_Scaled)|> summary()
summary(model12.11.1_sc)

model12.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2018)
model12.11.2_sc <- update(model12.11.2,data=wildflowerA2018_Scaled)
summary(model12.11.2_sc)

ggeffect(model12.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(e)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model12.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerA2019)
model12.12_sc <- update(model12.12,data=wildflowerA2019_Scaled)
model12.12_sc %>% summary()

model12.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model12.12.1_sc <- update(model12.12.1,data=wildflowerA2019_Scaled)|> summary()
summary(model12.12.1_sc)

model12.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model12.12.2_sc <- update(model12.12.2,data=wildflowerA2019_Scaled)
summary(model12.12.2_sc)

ggeffect(model12.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(f)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## GRAPHS AND MODELS FOR THE THREE YEARS _ SHANNON DIVERSITY

########################### 2017
model13.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerA2017)
model13.10_sc <- update(model13.10,data=wildflowerA2017_Scaled)
model13.10_sc %>% summary()

model13.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = wildflowerA2017)
model13.10.1_sc <- update(model13.10.1,data=wildflowerA2017_Scaled)|> summary()
summary(model13.10.1_sc)

model13.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = wildflowerA2017)
model13.10.2_sc <- update(model13.10.2,data=wildflowerA2017_Scaled)
summary(model13.10.2_sc)

ggeffect(model13.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(g)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2018
model13.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = wildflowerA2018)
model13.11_sc <- update(model3.11,data=wildflowerA2018_Scaled)
model13.11_sc %>% summary()

model13.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = wildflowerA2018)
model13.11.1_sc <- update(model13.11.1,data=wildflowerA2018_Scaled)|> summary()
summary(model13.11.1_sc)

model13.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = wildflowerA2018)
model13.11.2_sc <- update(model13.11.2,data=wildflowerA2018_Scaled)
summary(model13.11.2_sc)


ggeffect(model13.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(h)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model13.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model13.12_sc <- update(model13.12,data=wildflowerA2019_Scaled)
model13.12_sc %>% summary()

model13.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model13.12.1_sc <- update(model13.12.1,data=wildflowerA2019_Scaled)|> summary()
summary(model13.12.1_sc)

model13.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = wildflowerA2019)
model13.12.2_sc <- update(model13.12.2,data=wildflowerA2019_Scaled)
summary(model13.12.2_sc)


ggeffect(model13.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(i)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")











#########################################################
########## flying


## GRAPHS AND MODELS FOR THE THREE YEARS _ DENSITY
########################### 2017

model21.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=flyingA2017)
## Re-scale and center continuous parameters
numcolsflyingA2017 <- grep("^c\\.",names(flyingA2017))
flyingA2017_Scaled <- flyingA2017
flyingA2017_Scaled[,numcolsflyingA2017] <- scale(flyingA2017_Scaled[,numcolsflyingA2017])
model21.20_sc <- update(model21.20,data=flyingA2017_Scaled)
model21.20_sc %>% summary()

model21.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2017)
model21.20.1_sc <- update(model21.20.1,data=flyingA2017_Scaled)
model21.20.1_sc %>% summary()

model21.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2017)
model21.20.2_sc <- update(model21.20.2,data=flyingA2017_Scaled)
model21.20.2_sc %>% summary()

##
ggeffect(model21.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(a)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2018
model21.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = flyingA2018)
## re-scale
numcolsflyingA2018 <- grep("^c\\.",names(flyingA2018))
flyingA2018_Scaled <- flyingA2018
flyingA2018_Scaled[,numcolsflyingA2018] <- scale(flyingA2018_Scaled[,numcolsflyingA2018])
model21.11_sc <- update(model21.11,data=flyingA2018_Scaled)
model21.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model21.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2018)
model21.11.1_sc <- update(model21.11.1,data=flyingA2018_Scaled)|> summary()
summary(model21.11.1_sc)

model21.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2018)
model21.11.2_sc <- update(model21.11.2,data=flyingA2018_Scaled)
summary(model21.11.2_sc)

ggeffect(model21.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(b)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2019
model21.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = flyingA2019)
## re-scale
numcolsflyingA2019 <- grep("^c\\.",names(flyingA2019))
flyingA2019_Scaled <- flyingA2019
flyingA2019_Scaled[,numcolsflyingA2019] <- scale(flyingA2019_Scaled[,numcolsflyingA2019])
model21.12_sc <- update(model21.12,data=flyingA2019_Scaled)
model21.12_sc %>% summary()

model21.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2019)
model21.12.1_sc <- update(model21.12.1,data=flyingA2019_Scaled)|> summary()
summary(model21.12.1_sc)

model21.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2019)
model21.12.2_sc <- update(model21.12.2,data=flyingA2019_Scaled)
summary(model21.12.2_sc)

ggeffect(model21.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(c)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## Models per year and treatment for SPECIES RICHNESS
### 2017

## GRAPH FOR THE THREE YEARS _ SPECIES RICHNESS

########################### 2017
model22.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = flyingA2017)
model22.10_sc <- update(model22.10,data=flyingA2017_Scaled)
model22.10_sc %>% summary()

model22.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2017)
model22.10.1_sc <- update(model22.10.1,data=flyingA2017_Scaled)|> summary()
summary(model22.10.1_sc)

model22.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2017)
model22.10.2_sc <- update(model22.10.2,data=flyingA2017_Scaled)
summary(model22.10.2_sc)

ggeffect(model22.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(d)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")


## 2018
model22.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = flyingA2018)
model22.11_sc <- update(model22.11,data=flyingA2018_Scaled)
model22.11_sc %>% summary()

model22.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2018)
model22.11.1_sc <- update(model22.11.1,data=flyingA2018_Scaled)|> summary()
summary(model22.11.1_sc)

model22.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2018)
model22.11.2_sc <- update(model22.11.2,data=flyingA2018_Scaled)
summary(model22.11.2_sc)

ggeffect(model22.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(e)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model22.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = flyingA2019)
model22.12_sc <- update(model22.12,data=flyingA2019_Scaled)
model22.12_sc %>% summary()

model22.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2019)
model22.12.1_sc <- update(model22.12.1,data=flyingA2019_Scaled)|> summary()
summary(model22.12.1_sc)

model22.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingA2019)
model22.12.2_sc <- update(model22.12.2,data=flyingA2019_Scaled)
summary(model22.12.2_sc)

ggeffect(model22.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(f)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## GRAPHS AND MODELS FOR THE THREE YEARS _ SHANNON DIVERSITY

########################### 2017
model23.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = flyingA2017)
model23.10_sc <- update(model23.10,data=flyingA2017_Scaled)
model23.10_sc %>% summary()

model23.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingA2017)
model23.10.1_sc <- update(model23.10.1,data=flyingA2017_Scaled)|> summary()
summary(model23.10.1_sc)

model23.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingA2017)
model23.10.2_sc <- update(model23.10.2,data=flyingA2017_Scaled)
summary(model23.10.2_sc)

ggeffect(model23.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(g)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2018
model23.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = flyingA2018)
model23.11_sc <- update(model23.11,data=flyingA2018_Scaled)
model23.11_sc %>% summary()

model23.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingA2018)
model23.11.1_sc <- update(model23.11.1,data=flyingA2018_Scaled)|> summary()
summary(model23.11.1_sc)

model23.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingA2018)
model23.11.2_sc <- update(model23.11.2,data=flyingA2018_Scaled)
summary(model23.11.2_sc)


ggeffect(model23.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(h)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model23.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = flyingA2019)
model23.12_sc <- update(model23.12,data=flyingA2019_Scaled)
model23.12_sc %>% summary()

model23.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingA2019)
model23.12.1_sc <- update(model23.12.1,data=flyingA2019_Scaled)|> summary()
summary(model23.12.1_sc)

model23.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingA2019)
model23.12.2_sc <- update(model23.12.2,data=flyingA2019_Scaled)
summary(model23.12.2_sc)

ggeffect(model23.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(i)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")












#########################################################
########## resting


## GRAPHS AND MODELS FOR THE THREE YEARS _ DENSITY
########################### 2017

model31.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=restingA2017)
## Re-scale and center continuous parameters
numcolsrestingA2017 <- grep("^c\\.",names(restingA2017))
restingA2017_Scaled <- restingA2017
restingA2017_Scaled[,numcolsrestingA2017] <- scale(restingA2017_Scaled[,numcolsrestingA2017])
model31.20_sc <- update(model31.20,data=restingA2017_Scaled)
model31.20_sc %>% summary()

model31.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2017)
model31.20.1_sc <- update(model31.20.1,data=restingA2017_Scaled)
model31.20.1_sc %>% summary()

model31.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2017)
model31.20.2_sc <- update(model31.20.2,data=restingA2017_Scaled)
model31.20.2_sc %>% summary()

##
ggeffect(model31.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(a)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2018
model31.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = restingA2018)
## re-scale
numcolsrestingA2018 <- grep("^c\\.",names(restingA2018))
restingA2018_Scaled <- restingA2018
restingA2018_Scaled[,numcolsrestingA2018] <- scale(restingA2018_Scaled[,numcolsrestingA2018])
model31.11_sc <- update(model31.11,data=restingA2018_Scaled)
model31.11_sc %>% summary() 

model31.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2018)
model31.11.1_sc <- update(model31.11.1,data=restingA2018_Scaled)|> summary()
summary(model31.11.1_sc)

model31.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2018)
model31.11.2_sc <- update(model31.11.2,data=restingA2018_Scaled)
summary(model31.11.2_sc)

ggeffect(model31.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(b)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2019
model31.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = restingA2019)
## re-scale
numcolsrestingA2019 <- grep("^c\\.",names(restingA2019))
restingA2019_Scaled <- restingA2019
restingA2019_Scaled[,numcolsrestingA2019] <- scale(restingA2019_Scaled[,numcolsrestingA2019])
model31.12_sc <- update(model31.12,data=restingA2019_Scaled)
model31.12_sc %>% summary()

model31.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2019)
model31.12.1_sc <- update(model31.12.1,data=restingA2019_Scaled)|> summary()
summary(model31.12.1_sc)

model31.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2019)
model31.12.2_sc <- update(model31.12.2,data=restingA2019_Scaled)
summary(model31.12.2_sc)

ggeffect(model31.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(c)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## Models per year and treatment for SPECIES RICHNESS
### 2017

## GRAPH FOR THE THREE YEARS _ SPECIES RICHNESS

########################### 2017
model32.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = restingA2017)
model32.10_sc <- update(model32.10,data=restingA2017_Scaled)
model32.10_sc %>% summary()

model32.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2017)
model32.10.1_sc <- update(model32.10.1,data=restingA2017_Scaled)|> summary()
summary(model32.10.1_sc)

model32.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2017)
model32.10.2_sc <- update(model32.10.2,data=restingA2017_Scaled)
summary(model32.10.2_sc)

ggeffect(model32.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(d)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")


## 2018
model32.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = restingA2018)
model32.11_sc <- update(model32.11,data=restingA2018_Scaled)
model32.11_sc %>% summary()

model32.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2018)
model32.11.1_sc <- update(model32.11.1,data=restingA2018_Scaled)|> summary()
summary(model32.11.1_sc)

model32.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2018)
model32.11.2_sc <- update(model32.11.2,data=restingA2018_Scaled)
summary(model32.11.2_sc)

ggeffect(model32.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(e)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model32.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = restingA2019)
model32.12_sc <- update(model32.12,data=restingA2019_Scaled)
model32.12_sc %>% summary()

model32.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2019)
model32.12.1_sc <- update(model32.12.1,data=restingA2019_Scaled)|> summary()
summary(model32.12.1_sc)

model32.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingA2019)
model32.12.2_sc <- update(model32.12.2,data=restingA2019_Scaled)
summary(model32.12.2_sc)

ggeffect(model32.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(f)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## GRAPHS AND MODELS FOR THE THREE YEARS _ SHANNON DIVERSITY

########################### 2017
model33.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = restingA2017)
model33.10_sc <- update(model33.10,data=restingA2017_Scaled)
model33.10_sc %>% summary()

model33.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingA2017)
model33.10.1_sc <- update(model33.10.1,data=restingA2017_Scaled)|> summary()
summary(model33.10.1_sc)

model33.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingA2017)
model33.10.2_sc <- update(model33.10.2,data=restingA2017_Scaled)
summary(model33.10.2_sc)

ggeffect(model33.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(g)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2018
model33.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = restingA2018)
model33.11_sc <- update(model33.11,data=restingA2018_Scaled)
model33.11_sc %>% summary()

model33.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingA2018)
model33.11.1_sc <- update(model33.11.1,data=restingA2018_Scaled)|> summary()
summary(model33.11.1_sc)

model33.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingA2018)
model33.11.2_sc <- update(model33.11.2,data=restingA2018_Scaled)
summary(model33.11.2_sc)

ggeffect(model33.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(h)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model33.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = restingA2019)
model33.12_sc <- update(model33.12,data=restingA2019_Scaled)
model33.12_sc %>% summary()

model33.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingA2019)
model33.12.1_sc <- update(model33.12.1,data=restingA2019_Scaled)|> summary()
summary(model33.12.1_sc)

model33.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingA2019)
model33.12.2_sc <- update(model33.12.2,data=restingA2019_Scaled)
summary(model33.12.2_sc)

ggeffect(model33.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(i)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



















###########################
##### individual taxa
########################### Honeybee
## 2017
cherryA2017
wildflowerA2017
flyingA2017
restingA2017

##
model4.1 = glmer.nb(Honeybee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2017)
model4.1_sc <- update(model4.1,data=cherryA2017_Scaled)
model4.1_sc %>% summary()

model4.11<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model4.11_sc <- update(model4.11,data=cherryA2017_Scaled)
summary(model4.11_sc)

model4.12<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model4.12_sc <- update(model4.12,data=cherryA2017_Scaled)
summary(model4.12_sc)


P161<-ggeffect(model4.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit_range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "A Honeybees 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
P161


## 2018
cherryA2018
wildflowerA2018
flyingA2018
restingA2018
##

model4.2 = glmer.nb(Honeybee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2018)
model4.2_sc <- update(model4.2,data=cherryA2018_Scaled)
model4.2_sc %>% summary()

model4.21<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model4.21_sc <- update(model4.21,data=cherryA2018_Scaled)
summary(model4.21_sc)

model4.22<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model4.22_sc <- update(model4.22,data=cherryA2018_Scaled)
summary(model4.22_sc)

P162<-ggeffect(model4.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "B Honeybees 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2019
cherryA2019
wildflowerA2019
flyingA2019
restingA2019
##

model4.3 = glmer.nb(Honeybee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2019)
model4.3_sc <- update(model4.3,data=cherryA2019_Scaled)
model4.3_sc %>% summary()

model4.31<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model4.31_sc <- update(model4.31,data=cherryA2019_Scaled)
summary(model4.31_sc)

model4.32<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model4.32_sc <- update(model4.32,data=cherryA2019_Scaled)
summary(model4.32_sc)

P163<-ggeffect(model4.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "C Honeybees 2019")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))




########################### Buff_tailed_bumblebee
## 2017
model5.1 = glmer.nb(Buff_tailed_bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2017)
model5.1_sc <- update(model5.1,data=cherryA2017_Scaled)
model5.1_sc %>% summary()

model5.11<-glmer.nb(Buff_tailed_bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model5.11_sc <- update(model5.11,data=cherryA2017_Scaled)|> summary()
summary(model5.11_sc)

model5.12<-glmer.nb(Buff_tailed_bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model5.12_sc <- update(model5.12,data=cherryA2017_Scaled)
summary(model5.12_sc)


## 2018
model5.2 = glmer.nb(Buff_tailed_bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2018)
model5.2_sc <- update(model5.2,data=cherryA2018_Scaled)
model5.2_sc %>% summary()

model5.21<-glmer.nb(Buff_tailed_bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model5.21_sc <- update(model5.21,data=cherryA2018_Scaled)
summary(model5.21_sc)

model5.22<-glmer.nb(Buff_tailed_bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model5.22_sc <- update(model5.22,data=cherryA2018_Scaled)
summary(model5.22_sc)


## 2019
model5.3 = glmer.nb(Buff_tailed_bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2019)
model5.3_sc <- update(model5.3,data=cherryA2019_Scaled)
model5.3_sc %>% summary()

model5.31<-glmer.nb(Buff_tailed_bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model5.31_sc <- update(model5.31,data=cherryA2019_Scaled)
summary(model5.31_sc)

model5.32<-glmer.nb(Buff_tailed_bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model5.32_sc <- update(model5.32,data=cherryA2019_Scaled)
summary(model5.32_sc)


P164<-ggeffect(model5.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "D B-t bumblebees 2019")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))



########################### Bumblebee
## 2017
model6.1 = glmer.nb(Bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2017)
model6.1_sc <- update(model6.1,data=cherryA2017_Scaled)
model6.1_sc %>% summary()

model6.11<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model6.11_sc <- update(model6.11,data=cherryA2017_Scaled)
summary(model6.11_sc)

model6.12<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model6.12_sc <- update(model6.12,data=cherryA2017_Scaled)
summary(model6.12_sc)

P165<-ggeffect(model6.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "E Bumblebees 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2018
model6.2 = glmer.nb(Bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2018)
model6.2_sc <- update(model6.2,data=cherryA2018_Scaled)
model6.2_sc %>% summary()

model6.21<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model6.21_sc <- update(model6.21,data=cherryA2018_Scaled)
summary(model6.21_sc)

model6.22<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model6.22_sc <- update(model6.22,data=cherryA2018_Scaled)
summary(model6.22_sc)

P166<-ggeffect(model6.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "F Bumblebees 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2019
model6.3 = glmer.nb(Bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2019)
model6.3_sc <- update(model6.3,data=cherryA2019_Scaled)
model6.3_sc %>% summary()

model6.31<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model6.31_sc <- update(model6.31,data=cherryA2019_Scaled)
summary(model6.31_sc)

model6.32<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model6.32_sc <- update(model6.32,data=cherryA2019_Scaled)
summary(model6.32_sc)



########################### Solitary_bee
## 2017
model7.1 = glmer.nb(Solitary_bee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2017)
model7.1_sc <- update(model7.1,data=cherryA2017_Scaled)
model7.1_sc %>% summary()

model7.11<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model7.11_sc <- update(model7.11,data=cherryA2017_Scaled)|> summary()
summary(model7.11_sc)

model7.12<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model7.12_sc <- update(model7.12,data=cherryA2017_Scaled)
summary(model7.12_sc)


## 2018
model7.2 = glmer.nb(Solitary_bee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2018)
model7.2_sc <- update(model7.2,data=cherryA2018_Scaled)
model7.2_sc %>% summary()

model7.21<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model7.21_sc <- update(model7.21,data=cherryA2018_Scaled)
summary(model7.21_sc)

model7.22<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model7.22_sc <- update(model7.22,data=cherryA2018_Scaled)
summary(model7.22_sc)


P167<-ggeffect(model7.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "G Solitary bees 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2019
model7.3 = glmer.nb(Solitary_bee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2019)
model7.3_sc <- update(model7.3,data=cherryA2019_Scaled)
model7.3_sc %>% summary()

model7.31<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model7.31_sc <- update(model7.31,data=cherryA2019_Scaled)
summary(model7.31_sc)

model7.32<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model7.32_sc <- update(model7.32,data=cherryA2019_Scaled)
model7.32_sc %>% summary()



########################### Hoverfly
## 2017
model8.1 = glmer.nb(Hoverfly ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2017)
model8.1_sc <- update(model8.1,data=cherryA2017_Scaled)
model8.1_sc %>% summary()

model8.11<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model8.11_sc <- update(model8.11,data=cherryA2017_Scaled)
model8.11_sc %>% summary()

model8.12<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2017)
model8.12_sc <- update(model8.12,data=cherryA2017_Scaled)
summary(model8.12_sc)


P168<-ggeffect(model8.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "H Hoverflies 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2018
model8.2 = glmer.nb(Hoverfly ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2018)
model8.2_sc <- update(model8.2,data=cherryA2018_Scaled)
model8.2_sc %>% summary()

model8.21<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model8.21_sc <- update(model8.21,data=cherryA2018_Scaled)
summary(model8.21_sc)

model8.22<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2018)
model8.22_sc <- update(model8.22,data=cherryA2018_Scaled)
summary(model8.22_sc)


## 2019
model8.3 = glmer.nb(Hoverfly ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = cherryA2019)
model8.3_sc <- update(model8.3,data=cherryA2019_Scaled)
model8.3_sc %>% summary()

model8.31<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model8.31_sc <- update(model8.31,data=cherryA2019_Scaled)
summary(model8.31_sc)

model8.32<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = cherryA2019)
model8.32_sc <- update(model8.32,data=cherryA2019_Scaled)
summary(model8.32_sc)


P169<-ggeffect(model8.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "I Hoverflies 2019")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))





#############
## FIGURE 5 ##### FLOWER VISITOR DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE DURING CHERRY
Figure5<-ggarrange(P161 + rremove("ylab") + rremove("xlab"), P162 + rremove("ylab") + rremove("xlab"),
                   P163 + rremove("ylab") + rremove("xlab"), P164 + rremove("ylab") + rremove("xlab"),
                   P165 + rremove("ylab") + rremove("xlab"), P166 + rremove("ylab") + rremove("xlab"),
                   P167 + rremove("ylab") + rremove("xlab"), P168 + rremove("ylab") + rremove("xlab"),
                   P169 + rremove("ylab") + rremove("xlab"), align = "hv",
                   ncol=3, nrow=3, common.legend = TRUE, legend = "right",
                   font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
Figure5
annotate_figure(Figure5, bottom = text_grob("Distance from the orchard edge (m)",
                                            hjust = 1, x = 0.65, face = "bold", size = 13),
                left = text_grob("Mean number of individuals", rot = 90, face = "bold", size = 13, y = 0.55))
ggsave("Figure5.png", width = 8.3, height = 5.2, bg = "white")
## 800x500









  
####################################### POST CHERRY ANTHESIS
##########################

############### models for each activity  

########################## ## nectary
## 2017

model100.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=nectaryPA2017)
## Re-scale and center continuous parameters
numcolsnectaryPA2017 <- grep("^c\\.",names(nectaryPA2017))
nectaryPA2017_Scaled <- nectaryPA2017
nectaryPA2017_Scaled[,numcolsnectaryPA2017] <- scale(nectaryPA2017_Scaled[,numcolsnectaryPA2017])
model100.20_sc <- update(model100.20,data=nectaryPA2017_Scaled)
model100.20_sc %>% summary()

model100.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2017)
model100.20.1_sc <- update(model100.20.1,data=nectaryPA2017_Scaled)
model100.20.1_sc %>% summary()

model100.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2017)
model100.20.2_sc <- update(model100.20.2,data=nectaryPA2017_Scaled)
model100.20.2_sc %>% summary()


ggeffect(model8.3_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(h)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")


########################### 2018
model100.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = nectaryPA2018)
## re-scale
numcolsnectaryPA2018 <- grep("^c\\.",names(nectaryPA2018))
nectaryPA2018_Scaled <- nectaryPA2018
nectaryPA2018_Scaled[,numcolsnectaryPA2018] <- scale(nectaryPA2018_Scaled[,numcolsnectaryPA2018])
model100.11_sc <- update(model100.11,data=nectaryPA2018_Scaled)
model100.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model100.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model100.11.1_sc <- update(model100.11.1,data=nectaryPA2018_Scaled)|> summary()
summary(model100.11.1_sc)

model100.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model100.11.2_sc <- update(model100.11.2,data=nectaryPA2018_Scaled)
summary(model100.11.2_sc)


########################### 2019
model100.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = nectaryPA2019)
## re-scale
numcolsnectaryPA2019 <- grep("^c\\.",names(nectaryPA2019))
nectaryPA2019_Scaled <- nectaryPA2019
nectaryPA2019_Scaled[,numcolsnectaryPA2019] <- scale(nectaryPA2019_Scaled[,numcolsnectaryPA2019])
model100.12_sc <- update(model100.12,data=nectaryPA2019_Scaled)
model100.12_sc %>% summary()

model100.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model100.12.1_sc <- update(model100.12.1,data=nectaryPA2019_Scaled)|> summary()
summary(model100.12.1_sc)

model100.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model100.12.2_sc <- update(model100.12.2,data=nectaryPA2019_Scaled)
summary(model100.12.2_sc)




####################### SPECIES RICHNESS
### 2017 ## nectary



########################### 2017
model200.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = nectaryPA2017)
model200.10_sc <- update(model200.10,data=nectaryPA2017_Scaled)
model200.10_sc %>% summary()

model200.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2017)
model200.10.1_sc <- update(model200.10.1,data=nectaryPA2017_Scaled)|> summary()
summary(model200.10.1_sc)

model200.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2017)
model200.10.2_sc <- update(model200.10.2,data=nectaryPA2017_Scaled)
summary(model200.10.2_sc)

## 2018
model200.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = nectaryPA2018)
model200.11_sc <- update(model200.11,data=nectaryPA2018_Scaled)
model200.11_sc %>% summary()

model200.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model200.11.1_sc <- update(model200.11.1,data=nectaryPA2018_Scaled)|> summary()
summary(model200.11.1_sc)

model200.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model200.11.2_sc <- update(model200.11.2,data=nectaryPA2018_Scaled)
summary(model200.11.2_sc)


## 2019
model200.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = nectaryPA2019)
model200.12_sc <- update(model200.12,data=nectaryPA2019_Scaled)
model200.12_sc %>% summary()

model200.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model200.12.1_sc <- update(model200.12.1,data=nectaryPA2019_Scaled)|> summary()
summary(model200.12.1_sc)

model200.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                      + (1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model200.12.2_sc <- update(model200.12.2,data=nectaryPA2019_Scaled)
summary(model200.12.2_sc)




## ###################### SHANNON DIVERSITY

########################### 2017 ## nectary

model300.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = nectaryPA2017)
model300.10_sc <- update(model300.10,data=nectaryPA2017_Scaled)
model300.10_sc %>% summary()

model300.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = nectaryPA2017)
model300.10.1_sc <- update(model300.10.1,data=nectaryPA2017_Scaled)|> summary()
summary(model300.10.1_sc)

model300.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = nectaryPA2017)
model300.10.2_sc <- update(model300.10.2,data=nectaryPA2017_Scaled)
summary(model300.10.2_sc)


## 2018
model300.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model300.11_sc <- update(model300.11,data=nectaryPA2018_Scaled)
model300.11_sc %>% summary()

model300.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model300.11.1_sc <- update(model300.11.1,data=nectaryPA2018_Scaled)|> summary()
summary(model300.11.1_sc)

model300.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = nectaryPA2018)
model300.11.2_sc <- update(model300.11.2,data=nectaryPA2018_Scaled)
summary(model300.11.2_sc)


## 2019
model300.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model300.12_sc <- update(model300.12,data=nectaryPA2019_Scaled)
model300.12_sc %>% summary()

model300.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model300.12.1_sc <- update(model300.12.1,data=nectaryPA2019_Scaled)|> summary()
summary(model300.12.1_sc)

model300.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                  + (1 | Site_Code/Orchard_Block), data = nectaryPA2019)
model300.12.2_sc <- update(model300.12.2,data=nectaryPA2019_Scaled)
summary(model300.12.2_sc)





#########################################################
########## visiting wildflowers


########################### DENSITY
########################### 2017

model110.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=wildflowerPA2017)
## Re-scale and center continuous parameters
numcolswildflowerPA2017 <- grep("^c\\.",names(wildflowerPA2017))
wildflowerPA2017_Scaled <- wildflowerPA2017
wildflowerPA2017_Scaled[,numcolswildflowerPA2017] <- scale(wildflowerPA2017_Scaled[,numcolswildflowerPA2017])
model110.20_sc <- update(model110.20,data=wildflowerPA2017_Scaled)
model110.20_sc %>% summary()

model110.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model110.20.1_sc <- update(model110.20.1,data=wildflowerPA2017_Scaled)
model110.20.1_sc %>% summary()

model110.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model110.20.2_sc <- update(model110.20.2,data=wildflowerPA2017_Scaled)
model110.20.2_sc %>% summary()

##
p100<-ggeffect(model110.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "A")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_y_continuous(limits=c(0,2.2),breaks=seq(0, 2, 0.5))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p100


########################### 2018
model110.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = wildflowerPA2018)
## re-scale
numcolswildflowerPA2018 <- grep("^c\\.",names(wildflowerPA2018))
wildflowerPA2018_Scaled <- wildflowerPA2018
wildflowerPA2018_Scaled[,numcolswildflowerPA2018] <- scale(wildflowerPA2018_Scaled[,numcolswildflowerPA2018])
model110.11_sc <- update(model110.11,data=wildflowerPA2018_Scaled)
model110.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model110.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model110.11.1_sc <- update(model110.11.1,data=wildflowerPA2018_Scaled)|> summary()
summary(model110.11.1_sc)

model110.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model110.11.2_sc <- update(model110.11.2,data=wildflowerPA2018_Scaled)
summary(model110.11.2_sc)

p110<-ggeffect(model110.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "B")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,2.2),breaks=seq(0, 2, 0.5))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p110


########################### 2019
model110.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = wildflowerPA2019)
## re-scale
numcolswildflowerPA2019 <- grep("^c\\.",names(wildflowerPA2019))
wildflowerPA2019_Scaled <- wildflowerPA2019
wildflowerPA2019_Scaled[,numcolswildflowerPA2019] <- scale(wildflowerPA2019_Scaled[,numcolswildflowerPA2019])
model110.12_sc <- update(model110.12,data=wildflowerPA2019_Scaled)
model110.12_sc %>% summary()

model110.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model110.12.1_sc <- update(model110.12.1,data=wildflowerPA2019_Scaled)|> summary()
summary(model110.12.1_sc)

model110.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model110.12.2_sc <- update(model110.12.2,data=wildflowerPA2019_Scaled)
summary(model110.12.2_sc)

p120<-ggeffect(model110.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "C")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,2.3),breaks=seq(0, 2, 0.5))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p120





############################## SPECIES RICHNESS
### 2017
########################### 2017
model120.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = wildflowerPA2017)
model120.10_sc <- update(model120.10,data=wildflowerPA2017_Scaled)
model120.10_sc %>% summary()

model120.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model120.10.1_sc <- update(model120.10.1,data=wildflowerPA2017_Scaled)|> summary()
summary(model120.10.1_sc)

model120.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model120.10.2_sc <- update(model120.10.2,data=wildflowerPA2017_Scaled)
summary(model120.10.2_sc)

p200<-ggeffect(model120.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "D")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,1.5),breaks=seq(0, 1.5, 0.5))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p200


## 2018
model120.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2018)
model120.11_sc <- update(model120.11,data=wildflowerPA2018_Scaled)
model120.11_sc %>% summary()

model120.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model120.11.1_sc <- update(model120.11.1,data=wildflowerPA2018_Scaled)|> summary()
summary(model120.11.1_sc)

model120.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model120.11.2_sc <- update(model120.11.2,data=wildflowerPA2018_Scaled)
summary(model120.11.2_sc)

p210<-ggeffect(model120.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "E")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(0,1.5),breaks=seq(0, 1.5, 0.5))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p210


## 2019
model120.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2019)
model120.12_sc <- update(model120.12,data=wildflowerPA2019_Scaled)
model120.12_sc %>% summary()

model120.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model120.12.1_sc <- update(model120.12.1,data=wildflowerPA2019_Scaled)|> summary()
summary(model120.12.1_sc)

model120.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model120.12.2_sc <- update(model120.12.2,data=wildflowerPA2019_Scaled)
summary(model120.12.2_sc)

p220<-ggeffect(model120.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "F")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_y_continuous(limits=c(0,1.5),breaks=seq(0, 1.5, 0.5))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p220




####################### SHANNON DIVERSITY
################ 2017

model130.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2017)
model130.10_sc <- update(model130.10,data=wildflowerPA2017_Scaled)
model130.10_sc %>% summary()

model130.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model130.10.1_sc <- update(model130.10.1,data=wildflowerPA2017_Scaled)|> summary()
summary(model130.10.1_sc)

model130.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model130.10.2_sc <- update(model130.10.2,data=wildflowerPA2017_Scaled)
summary(model130.10.2_sc)

p300<-ggeffect(model130.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "G")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(-0.1,0.3),breaks=seq(0, 0.3, 0.1))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p300


## 2018
model130.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model130.11_sc <- update(model130.11,data=wildflowerPA2018_Scaled)
model130.11_sc %>% summary()

model130.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model130.11.1_sc <- update(model130.11.1,data=wildflowerPA2018_Scaled)|> summary()
summary(model130.11.1_sc)

model130.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model130.11.2_sc <- update(model130.11.2,data=wildflowerPA2018_Scaled)
summary(model130.11.2_sc)


p310<-ggeffect(model130.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "H")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(-0.1,0.3),breaks=seq(0, 0.3, 0.1))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p310


## 2019
model130.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model130.12_sc <- update(model130.12,data=wildflowerPA2019_Scaled)
model130.12_sc %>% summary()

model130.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model130.12.1_sc <- update(model130.12.1,data=wildflowerPA2019_Scaled)|> summary()
summary(model130.12.1_sc)

model130.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model130.12.2_sc <- update(model130.12.2,data=wildflowerPA2019_Scaled)
summary(model130.12.2_sc)


p320<-ggeffect(model130.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "I")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_y_continuous(limits=c(-0.1,0.3),breaks=seq(0, 0.3, 0.1))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))
p320




#############
## FIGURE S6 ##### FLOWER VISITOR, RICHNESS AND DIVERSITY ACORDING TO DISTANCE POST CHERRY
FigureS6<-ggarrange(p100 + rremove("xlab"), p110 + rremove("ylab") + rremove("xlab"),
                   p120 + rremove("ylab") + rremove("xlab"), p200 + rremove("xlab"),
                   p210 + rremove("ylab") + rremove("xlab"), p220 + rremove("ylab") + rremove("xlab"),
                   p300 + rremove("xlab"), p310 + rremove("ylab") + rremove("xlab"),
                   p320 + rremove("ylab") + rremove("xlab"), align = "hv",
                   ncol=3, nrow=3, common.legend = TRUE, legend = "right",
                   font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
FigureS6
annotate_figure(FigureS6, bottom = text_grob("Distance from the orchard edge (m)",
                                            hjust = 1, x = 0.65, face = "bold", size = 13))
ggsave("FigureS6.png", width = 8.3, height = 5.2, bg = "white")
## 800x500






#########################################################
########## flying


## GRAPHS AND MODELS FOR THE THREE YEARS _ DENSITY
########################### 2017

model210.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=flyingPA2017)
## Re-scale and center continuous parameters
numcolsflyingPA2017 <- grep("^c\\.",names(flyingPA2017))
flyingPA2017_Scaled <- flyingPA2017
flyingPA2017_Scaled[,numcolsflyingPA2017] <- scale(flyingPA2017_Scaled[,numcolsflyingPA2017])
model210.20_sc <- update(model210.20,data=flyingPA2017_Scaled)
model210.20_sc %>% summary()

model210.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2017)
model210.20.1_sc <- update(model210.20.1,data=flyingPA2017_Scaled)
model210.20.1_sc %>% summary()

model210.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2017)
model210.20.2_sc <- update(model210.20.2,data=flyingPA2017_Scaled)
model210.20.2_sc %>% summary()

##
ggeffect(model210.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(a)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2018
model210.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = flyingPA2018)
## re-scale
numcolsflyingPA2018 <- grep("^c\\.",names(flyingPA2018))
flyingPA2018_Scaled <- flyingPA2018
flyingPA2018_Scaled[,numcolsflyingPA2018] <- scale(flyingPA2018_Scaled[,numcolsflyingPA2018])
model210.11_sc <- update(model210.11,data=flyingPA2018_Scaled)
model210.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model210.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2018)
model210.11.1_sc <- update(model210.11.1,data=flyingPA2018_Scaled)|> summary()
summary(model210.11.1_sc)

model210.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2018)
model210.11.2_sc <- update(model210.11.2,data=flyingPA2018_Scaled)
summary(model210.11.2_sc)

ggeffect(model210.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(b)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2019
model210.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = flyingPA2019)
## re-scale
numcolsflyingPA2019 <- grep("^c\\.",names(flyingPA2019))
flyingPA2019_Scaled <- flyingPA2019
flyingPA2019_Scaled[,numcolsflyingPA2019] <- scale(flyingPA2019_Scaled[,numcolsflyingPA2019])
model210.12_sc <- update(model210.12,data=flyingPA2019_Scaled)
model210.12_sc %>% summary()

model210.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2019)
model210.12.1_sc <- update(model210.12.1,data=flyingPA2019_Scaled)|> summary()
summary(model210.12.1_sc)

model210.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2019)
model210.12.2_sc <- update(model210.12.2,data=flyingPA2019_Scaled)
summary(model210.12.2_sc)

ggeffect(model210.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(c)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## Models per year and treatment for SPECIES RICHNESS
### 2017

## GRAPH FOR THE THREE YEARS _ SPECIES RICHNESS

########################### 2017
model220.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = flyingPA2017)
model220.10_sc <- update(model220.10,data=flyingPA2017_Scaled)
model220.10_sc %>% summary()

model220.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2017)
model220.10.1_sc <- update(model220.10.1,data=flyingPA2017_Scaled)|> summary()
summary(model220.10.1_sc)

model220.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2017)
model220.10.2_sc <- update(model220.10.2,data=flyingPA2017_Scaled)
summary(model220.10.2_sc)

ggeffect(model220.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(d)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")


## 2018
model220.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = flyingPA2018)
model220.11_sc <- update(model220.11,data=flyingPA2018_Scaled)
model220.11_sc %>% summary()

model220.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2018)
model220.11.1_sc <- update(model220.11.1,data=flyingPA2018_Scaled)|> summary()
summary(model220.11.1_sc)

model220.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2018)
model220.11.2_sc <- update(model220.11.2,data=flyingPA2018_Scaled)
summary(model220.11.2_sc)

ggeffect(model220.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(e)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model220.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = flyingPA2019)
model220.12_sc <- update(model220.12,data=flyingPA2019_Scaled)
model220.12_sc %>% summary()

model220.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2019)
model220.12.1_sc <- update(model220.12.1,data=flyingPA2019_Scaled)
summary(model220.12.1_sc)

model220.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = flyingPA2019)
model220.12.2_sc <- update(model220.12.2,data=flyingPA2019_Scaled)
summary(model220.12.2_sc)

ggeffect(model220.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(f)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





## GRAPHS AND MODELS FOR THE THREE YEARS _ SHANNON DIVERSITY

########################### 2017
model230.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = flyingPA2017)
model230.10_sc <- update(model230.10,data=flyingPA2017_Scaled)
model230.10_sc %>% summary()

model230.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingPA2017)
model230.10.1_sc <- update(model230.10.1,data=flyingPA2017_Scaled)|> summary()
summary(model230.10.1_sc)

model230.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingPA2017)
model230.10.2_sc <- update(model230.10.2,data=flyingPA2017_Scaled)
summary(model230.10.2_sc)

ggeffect(model230.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(g)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2018
model230.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = flyingPA2018)
model230.11_sc <- update(model230.11,data=flyingPA2018_Scaled)
model230.11_sc %>% summary()

model230.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingPA2018)
model230.11.1_sc <- update(model230.11.1,data=flyingPA2018_Scaled)|> summary()
summary(model230.11.1_sc)

model230.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingPA2018)
model230.11.2_sc <- update(model230.11.2,data=flyingPA2018_Scaled)
summary(model230.11.2_sc)


ggeffect(model230.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(h)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model230.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = flyingPA2019)
model230.12_sc <- update(model230.12,data=flyingPA2019_Scaled)
model230.12_sc %>% summary()

model230.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingPA2019)
model230.12.1_sc <- update(model230.12.1,data=flyingPA2019_Scaled)|> summary()
summary(model230.12.1_sc)

model230.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = flyingPA2019)
model230.12.2_sc <- update(model230.12.2,data=flyingPA2019_Scaled)
summary(model230.12.2_sc)

ggeffect(model230.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(i)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")









#########################################################
########## resting


################## DENSITY
########################### 2017

model310.20 = glmer.nb(Total~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data=restingPA2017)
## Re-scale and center continuous parameters
numcolsrestingPA2017 <- grep("^c\\.",names(restingPA2017))
restingPA2017_Scaled <- restingPA2017
restingPA2017_Scaled[,numcolsrestingPA2017] <- scale(restingPA2017_Scaled[,numcolsrestingPA2017])
model310.20_sc <- update(model310.20,data=restingPA2017_Scaled)
model310.20_sc %>% summary()

model310.20.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2017)
model310.20.1_sc <- update(model310.20.1,data=restingPA2017_Scaled)
model310.20.1_sc %>% summary()

model310.20.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2017)
model310.20.2_sc <- update(model310.20.2,data=restingPA2017_Scaled)
model310.20.2_sc %>% summary()

##
ggeffect(model310.20_sc, terms = c("Distance", "Treatment")) |> 
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(a)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2018
model310.11 = glmer.nb(Total ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = restingPA2018)
## re-scale
numcolsrestingPA2018 <- grep("^c\\.",names(restingPA2018))
restingPA2018_Scaled <- restingPA2018
restingPA2018_Scaled[,numcolsrestingPA2018] <- scale(restingPA2018_Scaled[,numcolsrestingPA2018])
model310.11_sc <- update(model310.11,data=restingPA2018_Scaled)
model310.11_sc %>% summary() ## same results after scaling, also when including only orchard as random effect

model310.11.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2018)
model310.11.1_sc <- update(model310.11.1,data=restingPA2018_Scaled)|> summary()
summary(model310.11.1_sc)

model310.11.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2018)
model310.11.2_sc <- update(model310.11.2,data=restingPA2018_Scaled)
summary(model310.11.2_sc)

ggeffect(model310.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(b)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



########################### 2019
model310.12 = glmer.nb(Total ~ Distance*Treatment+Distance+ (1|Site_Code/Orchard_Block), data = restingPA2019)
## re-scale
numcolsrestingPA2019 <- grep("^c\\.",names(restingPA2019))
restingPA2019_Scaled <- restingPA2019
restingPA2019_Scaled[,numcolsrestingPA2019] <- scale(restingPA2019_Scaled[,numcolsrestingPA2019])
model310.12_sc <- update(model310.12,data=restingPA2019_Scaled)
model310.12_sc %>% summary()

model310.12.1<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2019)
model310.12.1_sc <- update(model310.12.1,data=restingPA2019_Scaled)|> summary()
summary(model310.12.1_sc)

model310.12.2<-glmer.nb(Total ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2019)
model310.12.2_sc <- update(model310.12.2,data=restingPA2019_Scaled)
summary(model310.12.2_sc)

ggeffect(model310.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Density") +
  theme(legend.title=element_blank()) + ggtitle(label = "(c)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





################################ SPECIES RICHNESS
########################### 2017

model320.10 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block), data = restingPA2017)
model320.10_sc <- update(model320.10,data=restingPA2017_Scaled)
model320.10_sc %>% summary()

model320.10.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2017)
model320.10.1_sc <- update(model320.10.1,data=restingPA2017_Scaled)|> summary()
summary(model320.10.1_sc)

model320.10.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2017)
model320.10.2_sc <- update(model320.10.2,data=restingPA2017_Scaled)
summary(model320.10.2_sc)

ggeffect(model320.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(d)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")


## 2018
model320.11 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = restingPA2018)
model320.11_sc <- update(model320.11,data=restingPA2018_Scaled)
model320.11_sc %>% summary()

model320.11.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2018)
model320.11.1_sc <- update(model320.11.1,data=restingPA2018_Scaled)|> summary()
summary(model320.11.1_sc)

model320.11.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2018)
model320.11.2_sc <- update(model320.11.2,data=restingPA2018_Scaled)
summary(model320.11.2_sc)

ggeffect(model320.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(e)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model320.12 = glmer.nb(Richness~Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = restingPA2019)
model320.12_sc <- update(model320.12,data=restingPA2019_Scaled)
model320.12_sc %>% summary()

model320.12.1<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2019)
model320.12.1_sc <- update(model320.12.1,data=restingPA2019_Scaled)|> summary()
summary(model320.12.1_sc)

model320.12.2<-glmer.nb(Richness ~ Distance * fct_relevel(Treatment, "CS") + Distance
                       + (1 | Site_Code/Orchard_Block), data = restingPA2019)
model320.12.2_sc <- update(model320.12.2,data=restingPA2019_Scaled)
summary(model320.12.2_sc)

ggeffect(model320.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Species richness") +
  theme(legend.title=element_blank()) + ggtitle(label = "(f)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+  
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")





########################### SHANNON DIVERSITY
########################### 2017

model330.10 = lmer(Shannon ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = restingPA2017)
model330.10_sc <- update(model330.10,data=restingPA2017_Scaled)
model330.10_sc %>% summary()

model330.10.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingPA2017)
model330.10.1_sc <- update(model330.10.1,data=restingPA2017_Scaled)|> summary()
summary(model330.10.1_sc)

model330.10.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingPA2017)
model330.10.2_sc <- update(model330.10.2,data=restingPA2017_Scaled)
summary(model330.10.2_sc)

ggeffect(model330.10_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(g)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2018
model330.11 = lmer(Shannon ~ Distance*Treatment+Distance+(1 | Site_Code/Orchard_Block), data = restingPA2018)
model330.11_sc <- update(model330.11,data=restingPA2018_Scaled)
model330.11_sc %>% summary()

model330.11.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingPA2018)
model330.11.1_sc <- update(model330.11.1,data=restingPA2018_Scaled)|> summary()
summary(model330.11.1_sc)

model330.11.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingPA2018)
model330.11.2_sc <- update(model330.11.2,data=restingPA2018_Scaled)
summary(model330.11.2_sc)

ggeffect(model330.11_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(h)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")



## 2019
model330.12 = lmer(Shannon ~ Distance*Treatment+ Distance+(1 | Site_Code/Orchard_Block), data = restingPA2019)
model330.12_sc <- update(model330.12,data=restingPA2019_Scaled)
model330.12_sc %>% summary()

model330.12.1<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingPA2019)
model330.12.1_sc <- update(model330.12.1,data=restingPA2019_Scaled)|> summary()
summary(model330.12.1_sc)

model330.12.2<-lmer(Shannon ~ Distance * fct_relevel(Treatment, "CS") + Distance
                   + (1 | Site_Code/Orchard_Block), data = restingPA2019)
model330.12.2_sc <- update(model330.12.2,data=restingPA2019_Scaled)
summary(model330.12.2_sc)

ggeffect(model330.12_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "(i)")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="green4","AMWS"="firebrick2"))+
  scale_fill_brewer(palette = "Set1")













###########################
##### individual taxa
########################### Honeybee
## 2017
nectaryPA2017
wildflowerPA2017
flyingPA2017
restingPA2017

##
model4.1 = glmer.nb(Honeybee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2017)
model4.1_sc <- update(model4.1,data=wildflowerPA2017_Scaled)
model4.1_sc %>% summary()

model4.11<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model4.11_sc <- update(model4.11,data=wildflowerPA2017_Scaled)
summary(model4.11_sc)

model4.12<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model4.12_sc <- update(model4.12,data=wildflowerPA2017_Scaled)
summary(model4.12_sc)


P171<-ggeffect(model4.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "A Honeybees 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2018
##

model4.2 = glmer.nb(Honeybee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2018)
model4.2_sc <- update(model4.2,data=wildflowerPA2018_Scaled)
model4.2_sc %>% summary()

model4.21<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model4.21_sc <- update(model4.21,data=wildflowerPA2018_Scaled)
summary(model4.21_sc)

model4.22<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model4.22_sc <- update(model4.22,data=wildflowerPA2018_Scaled)
summary(model4.22_sc)


P172<-ggeffect(model4.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "B Honeybees 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2019
##

model4.3 = glmer.nb(Honeybee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2019)
model4.3_sc <- update(model4.3,data=wildflowerPA2019_Scaled)
model4.3_sc %>% summary()

model4.31<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model4.31_sc <- update(model4.31,data=wildflowerPA2019_Scaled)
summary(model4.31_sc)

model4.32<-glmer.nb(Honeybee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model4.32_sc <- update(model4.32,data=wildflowerPA2019_Scaled)
summary(model4.32_sc)




########################### Bumblebee
## 2017
model5.1 = glmer.nb(Bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2017)
model5.1_sc <- update(model5.1,data=wildflowerPA2017_Scaled)
model5.1_sc %>% summary()

model5.11<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model5.11_sc <- update(model5.11,data=wildflowerPA2017_Scaled)|> summary()
summary(model5.11_sc)

model5.12<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model5.12_sc <- update(model5.12,data=wildflowerPA2017_Scaled)
summary(model5.12_sc)


## 2018
model5.2 = glmer.nb(Bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2018)
model5.2_sc <- update(model5.2,data=wildflowerPA2018_Scaled)
model5.2_sc %>% summary()

model5.21<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model5.21_sc <- update(model5.21,data=wildflowerPA2018_Scaled)
summary(model5.21_sc)

model5.22<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model5.22_sc <- update(model5.22,data=wildflowerPA2018_Scaled)
summary(model5.22_sc)


## 2019
model5.3 = glmer.nb(Bumblebee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2019)
model5.3_sc <- update(model5.3,data=wildflowerPA2019_Scaled)
model5.3_sc %>% summary()

model5.31<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model5.31_sc <- update(model5.31,data=wildflowerPA2019_Scaled)
summary(model5.31_sc)

model5.32<-glmer.nb(Bumblebee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model5.32_sc <- update(model5.32,data=wildflowerPA2019_Scaled)
summary(model5.32_sc)




########################### Solitary_bee
## 2017
model7.1 = glmer.nb(Solitary_bee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2017)
model7.1_sc <- update(model7.1,data=wildflowerPA2017_Scaled)
model7.1_sc %>% summary()

model7.11<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model7.11_sc <- update(model7.11,data=wildflowerPA2017_Scaled)
summary(model7.11_sc)

model7.12<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model7.12_sc <- update(model7.12,data=wildflowerPA2017_Scaled)
summary(model7.12_sc)


## 2018
model7.2 = glmer.nb(Solitary_bee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2018)
model7.2_sc <- update(model7.2,data=wildflowerPA2018_Scaled)
model7.2_sc %>% summary()

model7.21<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model7.21_sc <- update(model7.21,data=wildflowerPA2018_Scaled)
summary(model7.21_sc)

model7.22<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model7.22_sc <- update(model7.22,data=wildflowerPA2018_Scaled)
summary(model7.22_sc)


P173<-ggeffect(model7.2_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "C Solitary bees 2018")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2019
model7.3 = glmer.nb(Solitary_bee ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2019)
model7.3_sc <- update(model7.3,data=wildflowerPA2019_Scaled)
model7.3_sc %>% summary()

model7.31<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model7.31_sc <- update(model7.31,data=wildflowerPA2019_Scaled)
summary(model7.31_sc)

model7.32<-glmer.nb(Solitary_bee ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model7.32_sc <- update(model7.32,data=wildflowerPA2019_Scaled)
model7.32_sc %>% summary()





########################### Hoverfly
## 2017
model8.1 = glmer.nb(Hoverfly ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2017)
model8.1_sc <- update(model8.1,data=wildflowerPA2017_Scaled)
model8.1_sc %>% summary()

model8.11<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model8.11_sc <- update(model8.11,data=wildflowerPA2017_Scaled)
model8.11_sc %>% summary()

model8.12<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2017)
model8.12_sc <- update(model8.12,data=wildflowerPA2017_Scaled)
summary(model8.12_sc)


P174<-ggeffect(model8.1_sc, terms = c("Distance", "Treatment")) %>%
  plot(line_size = 1.5, use_theme = F,limit.range = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(x = "Distance from the orchard edge (m)", y = "Shannon diversity") +
  theme(legend.title=element_blank()) + ggtitle(label = "D Hoverflies 2017")+ 
  theme(plot.title = element_text(size=11, hjust = -0.05, face = "bold"))+
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(limits=c(0,70),breaks=seq(0, 70, 10))+ 
  scale_color_manual(labels = c("CS", "SWS", "AMWS"),  limits = c("CS", "SWS", "AMWS"),
                     values = c("CS"="dodgerblue3","SWS"="yellow3","AMWS"="violetred"))+
  scale_fill_manual(values = c("violet", "steelblue2", "yellow2"))


## 2018
model8.2 = glmer.nb(Hoverfly ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2018)
model8.2_sc <- update(model8.2,data=wildflowerPA2018_Scaled)
model8.2_sc %>% summary()

model8.21<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model8.21_sc <- update(model8.21,data=wildflowerPA2018_Scaled)
summary(model8.21_sc)

model8.22<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2018)
model8.22_sc <- update(model8.22,data=wildflowerPA2018_Scaled)
summary(model8.22_sc)


## 2019
model8.3 = glmer.nb(Hoverfly ~ Distance*Treatment+Distance+(1|Site_Code/Orchard_Block),data = wildflowerPA2019)
model8.3_sc <- update(model8.3,data=wildflowerPA2019_Scaled)
model8.3_sc %>% summary()

model8.31<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "SWS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model8.31_sc <- update(model8.31,data=wildflowerPA2019_Scaled)
summary(model8.31_sc)

model8.32<-glmer.nb(Hoverfly ~ Distance * fct_relevel(Treatment, "CS") + Distance
                    + (1 | Site_Code/Orchard_Block), data = wildflowerPA2019)
model8.32_sc <- update(model8.32,data=wildflowerPA2019_Scaled)
summary(model8.32_sc)





#############
## FIGURE 6 ##### FLOWER VISITOR DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE DURING CHERRY
Figure6<-ggarrange(P171 + rremove("ylab") + rremove("xlab"), P172 + rremove("ylab") + rremove("xlab"),
                    P173 + rremove("ylab") + rremove("xlab"), P174 + rremove("ylab") + rremove("xlab"),
                    align = "hv", ncol=3, nrow=2, common.legend = TRUE, legend = "right",
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
Figure6
annotate_figure(Figure6, bottom = text_grob("Distance from the orchard edge (m)",
                                             hjust = 1, x = 0.65, face = "bold", size = 13),
                left = text_grob("Mean number of individuals", rot = 90, face = "bold", size = 13, y = 0.55))
ggsave("Figure6.png", width = 8.3, height = 3.6, bg = "white")
## 800x350






