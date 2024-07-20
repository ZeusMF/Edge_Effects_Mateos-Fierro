

###           ###############            ENVIRONMENTAL FACTORS

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



##############################################
## ENVIRONMENTAL FACTORS
## 
kestrel<-read.csv(file.choose())

hist(kestrel$Temperature)
hist(kestrel$Humidity)
hist(kestrel$Wind_Speed)

SectionF<-as.factor(kestrel$Section)

## temperature
model1000.1b = glmer.nb(Temperature ~ SectionF + Year + Blooming +
                          (1 | Site_Code/Orchard_Code/Treatment), data = kestrel)
numcols_Temp <- grep("^c\\.",names(kestrel))
Temperature_Scaled <- kestrel
Temperature_Scaled[,numcols_Temp] <- scale(Temperature_Scaled[,numcols_Temp])
model1000.1b_sc <- update(model1000.1b,data=Temperature_Scaled)

drop1(model1000.1b_sc, test = "Chisq")
car::Anova(model1000.1b_sc, type=3)

model1000.1b_sc%>% summary() 
AICc(model1000.1b_sc)
summary(glht(model1000.1b_sc, mcp(Year="Tukey")), test=adjusted(type="holm"))
summary(glht(model1000.1b_sc, mcp(SectionF="Tukey")), test=adjusted(type="holm"))

tapply(kestrel$Temperature, kestrel$Section,mean)
tapply(kestrel$Temperature, kestrel$Year,std.error)


## humidity
model1000.2b<-glmer.nb(Humidity ~ SectionF + Year + Blooming +
                         (1 | Site_Code/Orchard_Code/Treatment), data = kestrel)
numcols_Hum <- grep("^c\\.",names(kestrel))
Humidity_Scaled <- kestrel
Humidity_Scaled[,numcols_Hum] <- scale(Humidity_Scaled[,numcols_Hum])
model1000.2b_sc <- update(model1000.2b,data=Humidity_Scaled)

drop1(model1000.2b_sc,test = "Chisq")
model1000.2b_sc  %>% summary() 
AICc(model1000.2b_sc)
summary(glht(model1000.2b_sc, mcp(Year="Tukey")), test=adjusted(type="holm"))
summary(glht(model1000.2b_sc, mcp(SectionF="Tukey")), test=adjusted(type="holm"))

tapply(kestrel$Humidity, kestrel$Section,mean)
tapply(kestrel$Temperature, kestrel$Year,std.error)


## wind speed
model1000.3b<-glmer.nb(Wind_Speed ~ SectionF + Year + Blooming +
                         (1 | Site_Code/Orchard_Code/Treatment), data = kestrel)
numcols_Wind <- grep("^c\\.",names(kestrel))
Wind_Scaled <- kestrel
Wind_Scaled[,numcols_Wind] <- scale(Wind_Scaled[,numcols_Wind])
model1000.3b_sc <- update(model1000.3b,data=Wind_Scaled)

drop1(model1000.3b_sc,test = "Chisq")
model1000.3b_sc  %>% summary() 
AICc(model1000.3b_sc)
summary(glht(model1000.3b_sc, mcp(Year="Tukey")), test=adjusted(type="holm"))
summary(glht(model1000.3b_sc, mcp(SectionF="Tukey")), test=adjusted(type="holm"))

tapply(kestrel$Wind_Speed, kestrel$Section,mean)
tapply(kestrel$Temperature, kestrel$Year,std.error)




Pa<-ggplot(kestrel,aes(x = Distance, y = Temperature, fill=Year, color = Year)) + 
  labs(x = "Distance from the orchard edge (m)", y = "Temperature (°C)") +
  geom_smooth(method = loess, formula = y ~ x, size = 1.5) + theme(
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(breaks=seq(0, 70, 10)) + ##scale_y_sqrt()+
  scale_y_continuous(breaks=seq(15, 24, 3)) +
  ## theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+ ggtitle(label = "A")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = 0.0))+
  facet_grid(~Blooming, scales = "free_x", space = "free_x", 
             labeller = label_wrap_gen(width = 4, multi_line = TRUE))+
    theme(strip.placement = "outside", strip.background = element_rect(fill = "white"),
          strip.text.x = element_blank()) +
  scale_color_manual(labels = c("2017", "2018", "2019"),  limits = c("Year_1", "Year_2", "Year_3"),
                     values = c("Year_1"="yellowgreen","Year_2"="peru","Year_3"="lightslateblue"))+
  scale_fill_manual(labels = c("2017", "2018", "2019"),  limits = c("Year_1", "Year_2", "Year_3"),
                    values = c("Year_1"="palegreen","Year_2"="tan1","Year_3"="lightsteelblue"))
Pa

Pb<-ggplot(kestrel,aes(x = Distance, y = Humidity, fill=Year, color = Year)) + 
  labs(x = "Distance from the orchard edge (m)", y = "Humidity (%)") +
  geom_smooth(method = loess, formula = y ~ x, size = 1.5) + theme(
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(breaks=seq(0, 70, 10)) + ##scale_y_sqrt()+
  ## theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+ ggtitle(label = "B")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = 0.0))+
  facet_grid(~Blooming, scales = "free_x", space = "free_x", 
             labeller = label_wrap_gen(width = 4, multi_line = TRUE))+
  theme(strip.placement = "outside", strip.background = element_rect(fill = "white"),
        strip.text.x = element_blank()) +
  scale_color_manual(labels = c("2017", "2018", "2019"),  limits = c("Year_1", "Year_2", "Year_3"),
                     values = c("Year_1"="yellowgreen","Year_2"="peru","Year_3"="lightslateblue"))+
  scale_fill_manual(labels = c("2017", "2018", "2019"),  limits = c("Year_1", "Year_2", "Year_3"),
                    values = c("Year_1"="palegreen","Year_2"="tan1","Year_3"="lightsteelblue"))
Pb

Pc<-ggplot(kestrel,aes(x = Distance, y = Wind_Speed, fill=Year, color = Year)) + 
  labs(x = "Distance from the orchard edge (m)", y = "Wind speed (m/s)") +
  geom_smooth(method = loess, formula = y ~ x, size = 1.5) + theme(
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold")) +
  scale_x_continuous(breaks=seq(0, 70, 10)) + ##scale_y_sqrt()+
  scale_y_continuous(breaks=seq(0, 0.8, 0.2)) +
  ## theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))+ ggtitle(label = "C")+ 
  theme(plot.title = element_text(size=11, face = "bold", hjust = 0.0))+
  facet_grid(~Blooming, scales = "free_x", space = "free_x", 
             labeller = label_wrap_gen(width = 4, multi_line = TRUE))+
  theme(strip.placement = "outside", strip.background = element_rect(fill = "white"),
        strip.text.x = element_blank()) +
  scale_color_manual(labels = c("2017", "2018", "2019"),  limits = c("Year_1", "Year_2", "Year_3"),
                     values = c("Year_1"="yellowgreen","Year_2"="peru","Year_3"="lightslateblue"))+
  scale_fill_manual(labels = c("2017", "2018", "2019"),  limits = c("Year_1", "Year_2", "Year_3"),
                    values = c("Year_1"="palegreen","Year_2"="tan1","Year_3"="lightsteelblue"))
Pc
  
## FIGURE S7 ##### FLOWER VISITOR DENSITY, RICHNESS AND DIVERSITY ACORDING TO DISTANCE DURING CHERRY
FigureS7<-ggarrange(Pa + rremove("xlab"), ggplot() + theme_void(), Pb + rremove("xlab"),
                    ggplot() + theme_void(), Pc + rremove("xlab"), heights = c(1, 0.1, 1, 0.1,1),
                    ncol=1, nrow=5, common.legend = TRUE, legend = "right", ## align = "hv", 
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL, position = "top"))
FigureS7
annotate_figure(FigureS7, bottom = text_grob("Distance from the orchard edge (m)",
                                             hjust = 1, x = 0.65, face = "bold", size = 13))
ggsave("FigureS7.png", width = 7.3, height = 6.8, bg = "white")
## 700x650






