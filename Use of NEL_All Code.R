#### Predictors of use of NELL R code ####
library(readr) # read CSV
library(dplyr) # pipe function
library(car) # VIF
library(ggplot2) # Plotting 
library(scales) # Percentage on y axis
library(ggpubr) # GGarrange

#### Poisson GLM - Figure 2, S1 + Tables 1, S2 ####
# All data #
setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data")
dat <- read_csv("Ch1_Analysis.csv")
Bdat <- dat %>% mutate(spatial_scope=as.factor(spatial_scope), ### Data for binomial figure
                       sptl_two=as.factor(sptl_two),
                       sptl_three=as.factor(sptl_three), 
                       new_cat=as.factor(new_cat))
dat <- dat %>% mutate(spatial_scope=as.factor(spatial_scope), 
                      sptl_two=as.factor(sptl_two),
                      sptl_three=as.factor(sptl_three), 
                      new_cat=as.factor(new_cat),
                      inclusive=as.factor(inclusive))
levels(dat$sptl_two) <- c("Multi-National and Greater", "National and Smaller")
levels(dat$inclusive) <- c("0","1")

m1 <- glm(lang_searched~year+sptl_two+no_authors+no_auth_country+percent_english_auth_countries, data = dat, family=poisson())
summary(m1) 
vif(m1)

### Figure 1 ###
setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data/GLM files")
# percent Eng auth countrues
predat3 <- read.csv("GLM_alldata_percentEnglishAuthCountries.csv")
predat3$predictions <- predict(m1,newdata = predat3,type = 'response')
tmp3 <- predict(m1,newdata = predat3,se.fit=T,type="link")
predat3$lowerCI <- exp(tmp3$fit - 2*tmp3$se.fit) # lower CI
predat3$upperCI <- exp(tmp3$fit + 2*tmp3$se.fit) # upper CI

jitter1 <- position_jitter(width = 0.05, height = 0.1)

pred3 <- ggplot(predat3,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+
  geom_point(position=jitter1,data = dat, aes(y=lang_searched, x=percent_english_auth_countries))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)

# Number of author countries
predat4 <- read.csv("GLM_alldata_authorcountries.csv")
predat4$predictions <- predict(m1,newdata = predat4,type = 'response')
tmp4 <- predict(m1,newdata = predat4,se.fit=T,type="link")
predat4$lowerCI <- exp(tmp4$fit - 2*tmp4$se.fit) # lower CI
predat4$upperCI <- exp(tmp4$fit + 2*tmp4$se.fit) # upper CI

pred4 <- ggplot(predat4,aes(y=predictions, x=no_auth_country))+
  geom_line()+
  geom_point(position=jitter1,data = dat, aes(y=lang_searched, x=no_auth_country))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Number of Author Countries")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))

ggarrange(pred3,pred4, ncol = 2, nrow = 1, labels="AUTO",common.legend = T,legend="right",align = "hv") 

### Supplementary Figure S1 and Table S2
dat2 <- dat[-c(4), ]
m2 <- glm(lang_searched~year+sptl_two+no_authors+no_auth_country+percent_english_auth_countries, data=dat2, family=poisson())
summary(m2)
vif(m2)

setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data/GLM files")
predat1 <- read.csv("GLM_alldata_percentEnglishAuthCountries.csv") 
predat1$predictions <- predict(m2,newdata = predat1,type = 'response')
tmp1 <- predict(m2,newdata = predat1,se.fit=T,type="link")
predat1$lowerCI <- exp(tmp1$fit - 2*tmp1$se.fit) # this is the lower CI
predat1$upperCI <- exp(tmp1$fit + 2*tmp1$se.fit) # upper CI

pred1 <- ggplot(predat1,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+
  geom_point(position=jitter1,data = dat2, aes(y=lang_searched, x=percent_english_auth_countries))+ 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)

# Number of author countries
predat2 <- read.csv("GLM_alldata_nooutlier_authorcountries.csv")
predat2$predictions <- predict(m2,newdata = predat2,type = 'response')
tmp2 <- predict(m2,newdata = predat2,se.fit=T,type="link")
predat2$lowerCI <- exp(tmp2$fit - 2*tmp2$se.fit) # this is the lower CI
predat2$upperCI <- exp(tmp2$fit + 2*tmp2$se.fit) # upper CI

pred2 <- ggplot(predat2,aes(y=predictions, x=no_auth_country))+
  geom_line()+
  geom_point(position=jitter1,data = dat2, aes(y=lang_searched, x=no_auth_country))+ 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Number of Author Countries")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))

ggarrange(pred1,pred2, ncol = 2, nrow = 1,labels="AUTO", common.legend = TRUE, legend="right",align = "hv") 

#### Survey Poisson GLM - Figure 3, Table 2 ####
setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data")
surv <- read_csv("SurveyDataAnalysis.csv")
surv <- surv[1:31,]
bsurv <- surv %>% mutate(spatial_scope=as.factor(spatial_scope),
                         sptl_two=as.factor(sptl_two),
                         sptl_three=as.factor(sptl_three), 
                         new_cat=as.factor(new_cat))
surv <- surv %>% mutate(spatial_scope=as.factor(spatial_scope),
                        sptl_two=as.factor(sptl_two),
                        sptl_three=as.factor(sptl_three), 
                        new_cat=as.factor(new_cat),
                        inclusive=as.factor(inclusive))
levels(surv$sptl_two) <- c("Multi-National and Greater", "National and Smaller")

s1 <- glm(lang_searched~year+sptl_two+no_authors+survey_lang+percent_english_auth_countries, data=surv, family=poisson())
summary(s1)
vif(s1)

setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data/GLM files")
predat6 <- read.csv("GLM_survey_langs.csv")
predat6$predictions <- predict(s1,newdata = predat6,type = 'response')
tmp6 <- predict(s1,newdata = predat6,se.fit=T,type="link")
predat6$lowerCI <- exp(tmp6$fit - 2*tmp6$se.fit) # this is the lower CI
predat6$upperCI <- exp(tmp6$fit + 2*tmp6$se.fit) # upper CI

ggplot(predat6,aes(y=predictions, x=survey_lang))+ ## Supp figure 2
  geom_line()+
  geom_point(position=jitter1,data = surv, aes(y=lang_searched,x=survey_lang))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  ylab("Number of Languages Spoken")+
  xlab("Number of Languages Searched")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(breaks=seq(0,10,2))+
  scale_y_continuous(breaks = seq(0,12,2))

#### Binomial GLM - Figure 4, Table 3 ####
m3 <- glm(inclusive~year+sptl_two+no_authors+no_auth_country+percent_english_auth_countries, data=dat,family = binomial)
summary(m3)

setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data/GLM files")
predatB1 <- read.csv("GLM_alldata_percentEnglishAuthCountries.csv") 
predatB1$predictions <- predict(m3,newdata = predatB1,type = 'response')
tmpB1 <- predict(m3,newdata = predatB1,se.fit=T,type="link")
predatB1$lowerCI <- 1/(1 + exp(-(tmpB1$fit - 2*tmpB1$se.fit)))
predatB1$upperCI <- 1/(1 + exp(-(tmpB1$fit + 2*tmpB1$se.fit)))

jitter2 <- position_jitter(width = 0.02, height = 0.02)

ggplot(predatB1,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+ 
  geom_point(position=jitter2,data = Bdat, aes(y=inclusive, x=percent_english_auth_countries))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)+
  scale_y_continuous(breaks=seq(0,1,1),labels = c("Not\nInclusive","Inclusive"))

#### Survey Binomial GLM - Figure 5, Table 4 ####
s3 <- glm(inclusive~year+sptl_two+no_authors+survey_lang+percent_english_auth_countries, data=surv, family=binomial())
summary(s3)
vif(s3) 

setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Data/GLM files")
predatBS1 <- read.csv("GLM_survey_percentEng.csv") 
predatBS1$predictions <- predict(s3,newdata = predatBS1,type = 'response')
tmpBS1 <- predict(s3,newdata = predatBS1,se.fit=T,type="link")
predatBS1$lowerCI <- 1/(1 + exp(-(tmpBS1$fit - 2*tmpBS1$se.fit)))
predatBS1$upperCI <- 1/(1 + exp(-(tmpBS1$fit + 2*tmpBS1$se.fit)))

ggplot(predatBS1,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+ 
  geom_point(position=jitter2,data = bsurv, aes(y=inclusive, x=percent_english_auth_countries))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)+
  scale_y_continuous(breaks=seq(0,1,1),labels = c("Not\nInclusive","Inclusive"))

#### Figure 1 - Languages used by region ####
library(tidyverse)
setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Figures")
fig1 <- read.csv("Fig1_data.csv")
fig1 <- fig1[,-2]
fig1.1 <- pivot_longer(fig1,cols=Russian:Italian,names_to="country", values_to="count")
fig1.1$Scale <- fct_relevel(fig1.1$Scale,"Global","Europe","Africa","North America","North Asia","Central and Western Europe","Southern Africa")
fig1.1$country <- fct_relevel(fig1.1$country,"Swedish","French","Finnish","Danish","German","Spanish","Dutch","Norwegian","Russian",
                              "Polish","Italian","Icelandic")

fig1_global <- fig1.1[1:12,]
fig1_continents <- fig1.1[c(25:36,61:84), ]
fig1_regions <- fig1.1[c(13:24,37:60),]
fig1_regions$Scale <- as.character(fig1_regions$Scale)
fig1_regions$Scale <- factor(fig1_regions$Scale, levels=c("Central and Western Europe","Southern Africa","North Asia"))

fig_glob <- ggplot(fig1_global, aes(x=Scale, y=count, fill=country))+ # global figure
  geom_bar(position = "dodge", stat="identity", width = 0.33)+
  scale_fill_brewer(palette = "Paired")+
  labs(y="Count", x= "Spatial Scale")+
  theme(text = element_text(size = 22))+
  theme(legend.position = c(0.9, 0.65))+
  guides(fill=guide_legend(title="Language"))

fig_cont <- ggplot(fig1_continents, aes(x=Scale, y=count, fill=country))+ # continental figure
  geom_bar(position = "dodge", stat="identity", width = 1)+
  scale_fill_brewer(palette = "Paired")+
  labs(y="Count", x= "Spatial Scale")+
  theme(text = element_text(size = 22))+
  theme(legend.position = c(0.9, 0.65))+
  guides(fill=guide_legend(title="Language"))

fig_reg <- ggplot(fig1_regions, aes(x=Scale, y=count, fill=country))+ # regional figure
  geom_bar(position = "dodge", stat="identity", width = 1)+
  scale_fill_brewer(palette = "Paired")+
  labs(y="Count", x= "Spatial Scale")+
  theme(text = element_text(size = 22))+
  theme(legend.position = c(0.9, 0.65))+
  guides(fill=guide_legend(title="Language"))

ggarrange(fig_glob,fig_cont,fig_reg,ncol=1,nrow=3,common.legend = TRUE, legend="right", labels = "AUTO")

#### Figure 6 - Barriers faced by authors ####
setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Figures")
dat <- read.csv("BarriersFigure.csv")
dat$newBarrier <- str_wrap(dat$Barrier, width=30)
dat$newBarrier <- factor(dat$newBarrier)

ggplot(dat, aes(x=fct_inorder(newBarrier),y=Count, color=newBarrier))+
  geom_bar(stat="identity", colour="black",fill="#B2DF8A")+
  coord_flip()+
  labs(y="Count of Responses", x= " ")+
  theme(text = element_text(size = 22))

#### Figure 7 - Barrers related to different methods ####
setwd("~/Nextcloud/PHDKH2021-A5859/Kelsey's Files/Chapter 2/Figures")

dat <- read.csv("NewSurvey Figures.csv")
Q9 <- dat[c(1:14),] 
Q9$Answer <- factor(Q9$Answer)
Q11 <- dat[c(15:20),] 
Q13 <- dat[c(21:26),]

plotQ9 <- ggplot(Q9, aes(x=Stage, y=Count,fill=fct_inorder(Answer)))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab("Proportion of Responses"))+
  labs(x=" ",title = "Main barriers to using machine\ntranslation at each stage")+
  scale_fill_manual(values = c("#33A02C","#B2DF8A","#6A3D9A","#FFFF99","#FB9A99","#A6CEE3","#FF7F00"))+
  theme(legend.position = "bottom", legend.title = element_blank(),text = element_text(size = 22))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  geom_text(aes(label = "N=", y = -0.05)) ## What to put as N??

plotQ11 <- ggplot(Q11, aes(x=Stage, y=Count,fill=fct_inorder(Answer)))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab(" "))+
  labs(x="Review Stage",y=" ",title = "Main barriers to using\nprofessional human translation")+
  scale_fill_manual(values = c("#FF7F00","#A6CEE3","#6A3D9A"))+
  theme(legend.position = "bottom", legend.title = element_blank(),text = element_text(size = 22))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

plotQ13 <- ggplot(Q13, aes(x=Stage, y=Count,fill=fct_inorder(Answer)))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab(" "))+
  labs(x=" ",y=" ",title = "Main barriers to engaging others\nwith relevant language skills")+
  scale_fill_manual(values = c("#FF7F00", "#A6CEE3","#FFFF99"))+
  theme(legend.position = "bottom", legend.title = element_blank(),text = element_text(size = 22))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggarrange(plotQ9,plotQ11, plotQ13, ncol=3, nrow=1, common.legend = TRUE, legend="bottom", labels = "AUTO")

#### Figure 8 - Use of language exchage system ####
dat8 <- read.csv("Q15_figure.csv")
colnames(dat8) <- c("Level","Total","Too time intensive","Unbalanced Workloads of Participants","Unable to find interested parties with relevant skillset")
dat8 <- dat8 %>% mutate(Level=as.factor(Level))
dat8$Level <- factor(dat8$Level, levels = dat8$Level)
p1 <- dat8[,c(1,3:5)]
p1$Level<- str_wrap(p1$Level, width=10)
p2 <- dat8[,c(1,2)]
p2$Level<- str_wrap(p2$Level, width=10)
p1long <- gather(p1,response,count,'Too time intensive':'Unable to find interested parties with relevant skillset',factor_key=TRUE)

plot1 <- ggplot(data=p2, aes(x=Level,y=Total, color=Level))+
  geom_bar(stat="identity", colour="black",fill="#B2DF8A")+
  labs(y="Count of Responses", x= " ")+
  theme(text = element_text(size = 17))

plot2 <- ggplot(p1long,aes(x=Level,y=count,fill= response))+
  geom_col(colour="black", position = "fill")+
  labs(y="Proportion of Responses", x= "Likelihood of Participating")+
  scale_y_continuous(labels = scales::percent,ylab("Proportion of Responses"))+
  scale_fill_manual(values = c("#FDBF6F", "#CAB2D6", "#6A3D9A"),
                    labels=c("Too time intensive", "Unbalanced workloads of\n participants",
                             "Unable to find interested\nparties with relevant skillset"))+
  guides(fill=guide_legend(title="Barriers to Participating"))+
  theme(legend.title = element_blank())+
  theme(text = element_text(size = 17))

ggarrange(plot1,plot2, ncol = 1, nrow = 2, labels="AUTO", align = "hv",common.legend= T,legend = "right")
