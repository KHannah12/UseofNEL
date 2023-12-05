#### Language Inclusion in Ecological Systematic Reviews and Maps: Barriers and Perspectives - R code ####

library(RColorBrewer)
library(readr)
library(tidyverse)
library(visreg)
library(cowplot)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(devtools)
library(car)
library(ordinal)
library(scales)
library(lme4) 

#### Figure 2 - Non-English languages searched by systematic reviews/maps with a specific geographic scope ####
fig2 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/Figure2.csv")
fig2 <- fig2[,-2]
fig2.1 <- pivot_longer(fig2,cols=Russian:Italian,names_to="country", values_to="count")
fig2.1$Scale <- fct_relevel(fig2.1$Scale,"Global","Europe","Africa","North America","North Asia","Central and Western Europe","Southern Africa")
fig2.1$country <- fct_relevel(fig2.1$country,"Swedish","French","Finnish","Danish","German","Spanish","Dutch","Norwegian","Russian",
                              "Polish","Italian","Icelandic")

## Fig 1 v4
fig2_global <- fig2.1[1:12,]
fig2_CWEu <- fig2.1[c(13:24),]
fig2_Europe <- fig2.1[c(61:72),]

fig_glob <- ggplot(fig2_global, aes(x=Scale, y=count, fill=country))+ # global figure
  geom_bar(position = "dodge", stat="identity", width = 1)+
  scale_fill_brewer(palette = "Paired")+
  labs(y="", x= " ")+
  theme(text = element_text(size = 22))+
  guides(fill=guide_legend(title="Language"))+
  scale_y_continuous(breaks = seq(0,20,5))+
  theme(panel.grid.minor = element_blank())+
  ggtitle("Reviews/Maps with a Global Scope")+ 
  theme(plot.title = element_text(size=20,hjust = 0.5))

fig_CWEu <- ggplot(fig2_CWEu, aes(x=Scale, y=count, fill=country))+ # CandW Europe figure
  geom_bar(position = "dodge", stat="identity", width = 1)+
  scale_fill_brewer(palette = "Paired")+
  labs(y="", x= " ")+
  theme(text = element_text(size = 22))+
  guides(fill=guide_legend(title="Language"))+
  scale_y_continuous(breaks = seq(0,2,1))+
  theme(panel.grid.minor = element_blank())+
  ggtitle("Reviews/Maps with a Smaller Scope")+ 
  theme(plot.title = element_text(size=20,hjust = 0.5))

fig_Europe <- ggplot(fig2_Europe, aes(x=Scale, y=count, fill=country))+ # Europe figure
  geom_bar(position = "dodge", stat="identity", width = 1)+
  scale_fill_brewer(palette = "Paired")+
  labs(y="Count of reviews/maps", x= "")+
  theme(text = element_text(size = 22))+
  guides(fill=guide_legend(title="Language"))+
  scale_y_continuous(breaks = seq(0,2,1))+
  theme(panel.grid.minor = element_blank())+
  ggtitle("Reviews/Maps with a Single Continent Scope")+ 
  theme(plot.title = element_text(size=20,hjust = 0.5))

ggarrange(fig_glob,fig_Europe,fig_CWEu,nrow = 3,ncol = 1, common.legend = T,legend = "right",align = "hv")

####  Poisson GLM testing associations between no. of langs searched and 5 explanatory variables ####
dat <- read_csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/Ch1_Analysis.csv")
Bdat <- dat %>% mutate(spatial_scope=as.factor(spatial_scope),
                       sptl_two=as.factor(sptl_two),
                       sptl_three=as.factor(sptl_three), 
                       new_cat=as.factor(new_cat))
dat <- dat %>% mutate(spatial_scope=as.factor(spatial_scope),
                      sptl_two=as.factor(sptl_two),
                      sptl_three=as.factor(sptl_three), 
                      new_cat=as.factor(new_cat),
                      inclusive=as.factor(inclusive),
                      First_Auth=as.factor(First_Auth))

levels(dat$sptl_two) <- c("Multi-National and Greater", "National and Smaller")
levels(dat$inclusive) <- c("0","1")
dat2 <- dat[-c(4), ]

## Main text analysis - excluding an outlier ##
m2 <- glm(lang_searched~year+sptl_two+no_authors+initial+no_auth_country+percent_english_auth_countries+initial, data=dat2, family=poisson())
summary(m2)
vif(m2)
## Supplementary analysis - all data ##
m1 <- glm(lang_searched~year+sptl_two+no_authors+initial+no_auth_country+percent_english_auth_countries, data = dat, family=poisson())
summary(m1) 
vif(m1)

## Testing random effects ##
# Excluding outlier #
m2_rand <- glmer(lang_searched~year+sptl_two+no_authors+initial+no_auth_country+percent_english_auth_countries+ (1|First_Auth), 
                 data = dat2, family=poisson())
summary(m2_rand) 
vif(m2_rand)

# All data #
m1_rand <- glmer(lang_searched~year+sptl_two+no_authors+initial+no_auth_country+percent_english_auth_countries+ (1|First_Auth), 
                 data = dat, family=poisson())
summary(m1_rand) 
vif(m1_rand)

#### Figure 3 - Poisson GLM with all data ####
## Main text figure - excluding outlier ##
# Percentage of authors #
predat1 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/GLM_AuthCount.csv") 
predat1$predictions <- predict(m2,newdata = predat1,type = 'response')
tmp1 <- predict(m2,newdata = predat1,se.fit=T,type="link")
predat1$lowerCI <- exp(tmp1$fit - 2*tmp1$se.fit) # this is the lower CI
predat1$upperCI <- exp(tmp1$fit + 2*tmp1$se.fit) # upper CI

jitter <- position_jitter(width = 0.1, height = 0.1)

fig3.1 <- ggplot(predat1,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+
  geom_point(position=jitter,data = dat2, aes(y=lang_searched, x=percent_english_auth_countries))+ 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)
pred1

# Number of author countries #
predat2 <- read.csv("GLM_alldata_nooutlier_authorcountries.csv")
predat2$predictions <- predict(m2,newdata = predat2,type = 'response')
tmp2 <- predict(m2,newdata = predat2,se.fit=T,type="link")
predat2$lowerCI <- exp(tmp2$fit - 2*tmp2$se.fit) # this is the lower CI
predat2$upperCI <- exp(tmp2$fit + 2*tmp2$se.fit) # upper CI

fig3.2 <- ggplot(predat2,aes(y=predictions, x=no_auth_country))+
  geom_line()+
  geom_point(position=jitter,data = dat2, aes(y=lang_searched, x=no_auth_country))+ 
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Number of Author Countries")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))
# Figure 3 #
ggarrange(fig3.1,fig3.2, ncol = 2, nrow = 1,labels="AUTO", common.legend = TRUE, legend="right",align = "hv") 

## Supp text figure - all data ##

predat3 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/GLM_PercAuth.csv")
predat3$predictions <- predict(m1,newdata = predat3,type = 'response')
tmp3 <- predict(m1,newdata = predat3,se.fit=T,type="link")
predat3$lowerCI <- exp(tmp3$fit - 2*tmp3$se.fit) # this is the lower CI
predat3$upperCI <- exp(tmp3$fit + 2*tmp3$se.fit) # upper CI

supp1.1 <- ggplot(predat3,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+
  geom_point(position=jitter,data = dat, aes(y=lang_searched, x=percent_english_auth_countries))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)

# Number of author countries
predat4 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/GLM_AuthCount.csv")
predat4$predictions <- predict(m1,newdata = predat4,type = 'response')
tmp4 <- predict(m1,newdata = predat4,se.fit=T,type="link")
predat4$lowerCI <- exp(tmp4$fit - 2*tmp4$se.fit) # this is the lower CI
predat4$upperCI <- exp(tmp4$fit + 2*tmp4$se.fit) # upper CI

supp1.2 <- ggplot(predat4,aes(y=predictions, x=no_auth_country))+
  geom_line()+
  geom_point(position=jitter,data = dat, aes(y=lang_searched, x=no_auth_country))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Number of Author Countries")+
  ylab("Number of Languages Searched")+
  theme(text = element_text(size = 18))
# Supplementary figure 1 #
ggarrange(supp1.1,supp1.2, ncol = 2, nrow = 1, labels="AUTO",common.legend = T,legend="right",align = "hv") 

#### Poisson GLM with survey data ####
surv <- read_csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/SurveyDataAnalysis.csv")
surv <- surv[1:31,]
surv <- surv %>% mutate(spatial_scope=as.factor(spatial_scope),
                        sptl_two=as.factor(sptl_two),
                        sptl_three=as.factor(sptl_three), 
                        new_cat=as.factor(new_cat),
                        inclusive=as.factor(inclusive))
levels(surv$sptl_two) <- c("Multi-National and Greater","National and Smaller")
s1 <- glm(lang_searched~year+sptl_two+no_authors+survey_lang, data=surv, family=poisson())
summary(s1)

#### Figure 4 - Poisson GLM with survey data ####
predat6 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/GLM_Surv_Langs.csv")
predat6$predictions <- predict(s1,newdata = predat6,type = 'response')
tmp6 <- predict(s1,newdata = predat6,se.fit=T,type="link")
predat6$lowerCI <- exp(tmp6$fit - 2*tmp6$se.fit) # this is the lower CI
predat6$upperCI <- exp(tmp6$fit + 2*tmp6$se.fit) # upper CI

ggplot(predat6,aes(y=predictions, x=survey_lang))+ ## Supp figure 2
  geom_line()+
  geom_point(position=jitter,data = surv, aes(y=lang_searched,x=survey_lang))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  ylab("Number of Languages Spoken")+
  xlab("Number of Languages Searched")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(breaks=seq(0,10,2))+
  scale_y_continuous(breaks = seq(0,12,2))

#### Binomial GLM ####
m3 <- glm(inclusive~year+sptl_two+no_authors+no_auth_country+initial+percent_english_auth_countries, data=dat,family = binomial)
summary(m3)
vif(m3)
## Checking random effects ##
m3_rand <- glmer(inclusive~year+sptl_two+no_authors+no_auth_country+percent_english_auth_countries+ (1|First_Auth), 
data = dat, family=binomial())
summary(m3_rand) 
vif(m3_rand)

#### Figure 5 - Binomial GLM with all data ####
predatB1 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/GLM_PercAuth.csv") 
predatB1$predictions <- predict(m3,newdata = predatB1,type = 'response')
tmpB1 <- predict(m3,newdata = predatB1,se.fit=T,type="link")
predatB1$lowerCI <- 1/(1 + exp(-(tmpB1$fit - 2*tmpB1$se.fit)))
predatB1$upperCI <- 1/(1 + exp(-(tmpB1$fit + 2*tmpB1$se.fit)))

jitter <- position_jitter(width = 0.02, height = 0.02)

ggplot(predatB1,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+ 
  geom_point(position=jitter,data = Bdat, aes(y=inclusive, x=percent_english_auth_countries))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)+
  scale_y_continuous(breaks=seq(0,1,1),labels = c("Not\nInclusive","Inclusive"))

#### Binomial GLM with survey data ####
s3 <- glm(inclusive~year+sptl_two+no_authors+survey_lang+initial+percent_english_auth_countries, data=surv, family=binomial())
summary(s3)
vif(s3) 

#### Figure 5 - Binomial GLM with survey data ####
predatBS1 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/GLM_Surv_PercEng.csv") 
predatBS1$predictions <- predict(s3,newdata = predatBS1,type = 'response')
tmpBS1 <- predict(s3,newdata = predatBS1,se.fit=T,type="link")
predatBS1$lowerCI <- 1/(1 + exp(-(tmpBS1$fit - 2*tmpBS1$se.fit)))
predatBS1$upperCI <- 1/(1 + exp(-(tmpBS1$fit + 2*tmpBS1$se.fit)))

jitter <- position_jitter(width = 0.02, height = 0.02)

ggplot(predatBS1,aes(y=predictions, x=percent_english_auth_countries))+
  geom_line()+ 
  geom_point(position=jitter,data = bsurv, aes(y=inclusive, x=percent_english_auth_countries))+
  geom_ribbon(aes(ymin=lowerCI,ymax=upperCI), fill="#FF7F00", alpha=0.3)+
  xlab("Percentage of Authors from Countries where\nEnglish is the Primary Language")+
  ylab("")+
  theme(text = element_text(size = 18))+
  scale_x_continuous(labels = percent)+
  scale_y_continuous(breaks=seq(0,1,1),labels = c("Not\nInclusive","Inclusive"))

#### Figure 7 - Barriers faced by authors ####
library(stringr)
library(forcats)
library(EnvStats)

fig7 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/Figure7.csv")
fig7$newBarrier <- str_wrap(fig7$Barrier, width=30)
fig7$newBarrier <- factor(fig7$newBarrier)

ggplot(fig7, aes(x=fct_inorder(newBarrier),y=Count, color=newBarrier))+
  geom_bar(stat="identity", colour="black",fill="#B2DF8A")+
  coord_flip()+
  labs(y="Count of Responses", x= " ")+
  theme(text = element_text(size = 22))

#### Figure 8 - Barriers at each stage of the SR process ####
fig8 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/Figure8.csv")
Q9 <- fig8[c(1:14),] 
Q9$Answer <- factor(Q9$Answer)
Q11 <- fig8[c(15:20),] 
Q13 <- fig8[c(21:26),]

Q9$Stage <- factor(Q9$Stage, levels=c("Searching","Screening"))
Q11$Stage <- factor(Q11$Stage, levels=c("Searching","Screening"))
Q13$Stage <- factor(Q13$Stage, levels=c("Searching","Screening"))

plotQ9 <- ggplot(Q9, aes(x=Stage, y=Count,fill=fct_inorder(Answer)))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab("Proportion of Responses"))+
  labs(x=" ",title = "Main barriers to using machine\ntranslation at each stage")+
  scale_fill_manual(values = c("#33A02C","#B2DF8A","#6A3D9A","#FFFF99","#FB9A99","#A6CEE3","#FF7F00"))+
  theme(legend.position = "bottom", legend.title = element_blank(),text = element_text(size = 20))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme(panel.grid.minor = element_blank())

plotQ11 <- ggplot(Q11, aes(x=Stage, y=Count,fill=fct_inorder(Answer)))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab(" "))+
  labs(x="Review Stage",y=" ",title = "Main barriers to using\nprofessional human translation")+
  scale_fill_manual(values = c("#FF7F00","#A6CEE3","#6A3D9A"))+
  theme(legend.position = "bottom", legend.title = element_blank(),text = element_text(size = 20))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme(panel.grid.minor = element_blank())

plotQ13 <- ggplot(Q13, aes(x=Stage, y=Count,fill=fct_inorder(Answer)))+
  geom_col(colour="black", position = "fill")+
  scale_y_continuous(labels = scales::percent,ylab(" "))+
  labs(x=" ",y=" ",title = "Main barriers to engaging others\nwith relevant language skills")+
  scale_fill_manual(values = c("#FF7F00", "#A6CEE3","#FFFF99"))+
  theme(legend.position = "bottom", legend.title = element_blank(),text = element_text(size = 20))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme(panel.grid.minor = element_blank())

ggarrange(plotQ9,plotQ11, plotQ13, ncol=3, nrow=1, common.legend = TRUE, legend="bottom", labels = "AUTO")

#### Figure 9 - Language exchange system survey responses ####
fig9 <- read.csv("https://raw.githubusercontent.com/KHannah12/UseofNEL/main/Figure9.csv")
colnames(fig9) <- c("Level","Total","Too time intensive","Unbalanced Workloads of Participants",
                    "Unable to find interested parties with relevant skillset")
p1 <- fig9[,c(1,3:5)]
p1long <- gather(p1,response,count,'Too time intensive':'Unable to find interested parties with relevant skillset',
                 factor_key=TRUE)
p1long$response <- factor(p1long$response, levels = p1long$response)
p1long$Level<- str_wrap(p1long$Level, width=10)

p2 <- fig9[,c(1,2)]
p2$Level<- str_wrap(p2$Level, width=10)

level_order <- c("Extremely\nunlikely","Somewhat\nunlikely","Neither\nlikely nor\nunlikely",
                 "Somewhat\nlikely","Extremely\nlikely")

plot1 <- ggplot(data=p2, aes(x=Level,y=Total, color=Level))+
  geom_bar(stat="identity", colour="black",fill="#B2DF8A")+
  scale_x_discrete(limits = level_order)+
  labs(y="Count of Responses", x= " ")+
  theme(text = element_text(size = 17))

plot2 <- ggplot(p1long,aes(x=Level,y=count,fill= response))+
  geom_col(colour="black", position = "fill")+
  scale_x_discrete(limits = level_order)+
  labs(y="Proportion of Responses", x= "Likelihood of Participating")+
  scale_y_continuous(labels = scales::percent,ylab("Proportion of Responses"))+
  scale_fill_manual(values = c("#FDBF6F", "#CAB2D6", "#6A3D9A"),
                    labels=c("Too time intensive", "Unbalanced workloads of\n participants",
                             "Unable to find interested\nparties with relevant skillset"))+
  guides(fill=guide_legend(title="Barriers to Participating"))+
  theme(legend.title = element_blank())+
  theme(text = element_text(size = 17))

ggarrange(plot1,plot2, ncol = 1, nrow = 2, labels="AUTO", align = "hv",common.legend= T,legend = "right")
