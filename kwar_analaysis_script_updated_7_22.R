library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)

#load yearly kWAR data and add rate metrics for kWAR, gWAR, fWAR and RE24#

kwar_2019<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2019_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF)%>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2018<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2018_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2017<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2017_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2016<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2016_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2015<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2015_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2014<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2014_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2013<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2013_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2012<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2012_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF)%>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2011<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2011_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

kwar_2010<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2010_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF) %>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9)) 

kwar_2009<-read_excel("Baseball/spreadsheets/kWAR/kwar_analysis.xlsx", 
                      sheet="2009_full_data")%>% 
  mutate(fWAR_TBF=fWAR/TBF)%>%
  mutate(kWAR_TBF=kWAR/TBF) %>%
  mutate(gWAR_TBF=gWAR/TBF) %>%
  mutate(RE24_TBF=RE24/TBF) %>%
  filter(!is.na(RA9))

#bind yearly kWAR data#

kwar_all <- bind_rows(kwar_2019, kwar_2018, kwar_2017, kwar_2016, kwar_2015, kwar_2014,
                      kwar_2013,kwar_2012, kwar_2011, kwar_2010, kwar_2009)


#preparing >170 TBF Data #

#2019#

tab_2019<-bind_rows(kwar_2018, kwar_2019)
tab_2019<-tab_2019 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2018<-tab_2019 %>% 
  filter(Season==2018)

tab_2019<-tab_2019 %>%
  filter(Season==2019)

tab_2018<-tab_2018[order(tab_2018$playerid),]
tab_2019<-tab_2019[order(tab_2019$playerid),]

tab_2019<-bind_cols(tab_2018, tab_2019) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2019<- tab_2019 %>% filter(Team_1!=Team_2)
rm(tab_2018)
#2019 correlation data#

cor_2019<-cor(tab_2019[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2019_fwar<-cor_2019[c(1,2,3,4,5),6]
predictive_cor_2019_RA9<-cor_2019[c(1,2,3,4,5),10]

descriptive_cor_2019_RE24<-cor_2019[c(6,7,8,10),9]
descriptive_cor_2019_RA9<-cor_2019[c(6,7,8,9),10]

#2018#

tab_2018<-bind_rows(kwar_2017, kwar_2018)
tab_2018<-tab_2018 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2017<-tab_2018 %>% 
  filter(Season==2017)

tab_2018<-tab_2018 %>%
  filter(Season==2018)

tab_2017<-tab_2017[order(tab_2017$playerid),]
tab_2018<-tab_2018[order(tab_2018$playerid),]

tab_2018<-bind_cols(tab_2017, tab_2018) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2018<- tab_2018 %>% filter(Team_1!=Team_2)
rm(tab_2017)
#2018 correlation data#
cor_2018<-cor(tab_2018[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2018_fwar<-cor_2018[c(1,2,3,4,5),6]
predictive_cor_2018_RA9<-cor_2018[c(1,2,3,4,5),10]

descriptive_cor_2018_RE24<-cor_2018[c(6,7,8,10),9]
descriptive_cor_2018_RA9<-cor_2018[c(6,7,8,9),10]

#2017#

tab_2017<-bind_rows(kwar_2016, kwar_2017)
tab_2017<-tab_2017 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2016<-tab_2017 %>% 
  filter(Season==2016)

tab_2017<-tab_2017 %>%
  filter(Season==2017)

tab_2016<-tab_2016[order(tab_2016$playerid),]
tab_2017<-tab_2017[order(tab_2017$playerid),]

tab_2017<-bind_cols(tab_2016, tab_2017) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2017<- tab_2017 %>% filter(Team_1!=Team_2)
rm(tab_2016)
#2017 correlation data#
cor_2017<-cor(tab_2017[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2017_fwar<-cor_2017[c(1,2,3,4,5),6]
predictive_cor_2017_RA9<-cor_2017[c(1,2,3,4,5),10]

descriptive_cor_2017_RE24<-cor_2017[c(6,7,8,10),9]
descriptive_cor_2017_RA9<-cor_2017[c(6,7,8,9),10]

#2016#

tab_2016<-bind_rows(kwar_2015, kwar_2016)
tab_2016<-tab_2016 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2015<-tab_2016 %>% 
  filter(Season==2015)

tab_2016<-tab_2016 %>%
  filter(Season==2016)

tab_2015<-tab_2015[order(tab_2015$playerid),]
tab_2016<-tab_2016[order(tab_2016$playerid),]

tab_2016<-bind_cols(tab_2015, tab_2016) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2016<- tab_2016 %>% filter(Team_1!=Team_2)
rm(tab_2015)
#2016 correlation data#
cor_2016<-cor(tab_2016[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2016_fwar<-cor_2016[c(1,2,3,4,5),6]
predictive_cor_2016_RA9<-cor_2016[c(1,2,3,4,5),10]

descriptive_cor_2016_RE24<-cor_2016[c(6,7,8,10),9]
descriptive_cor_2016_RA9<-cor_2016[c(6,7,8,9),10]

#2015#

tab_2015<-bind_rows(kwar_2014, kwar_2015)
tab_2015<-tab_2015 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2014<-tab_2015 %>% 
  filter(Season==2014)

tab_2015<-tab_2015 %>%
  filter(Season==2015)

tab_2014<-tab_2014[order(tab_2014$playerid),]
tab_2015<-tab_2015[order(tab_2015$playerid),]

tab_2015<-bind_cols(tab_2014, tab_2015) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2015<- tab_2015 %>% filter(Team_1!=Team_2)
rm(tab_2014)
#2015 correlation data#
cor_2015<-cor(tab_2015[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2015_fwar<-cor_2015[c(1,2,3,4,5),6]
predictive_cor_2015_RA9<-cor_2015[c(1,2,3,4,5),10]

descriptive_cor_2015_RE24<-cor_2015[c(6,7,8,10),9]
descriptive_cor_2015_RA9<-cor_2015[c(6,7,8,9),10]

#2014#

tab_2014<-bind_rows(kwar_2013, kwar_2014)
tab_2014<-tab_2014 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2013<-tab_2014 %>% 
  filter(Season==2013)

tab_2014<-tab_2014 %>%
  filter(Season==2014)

tab_2013<-tab_2013[order(tab_2013$playerid),]
tab_2014<-tab_2014[order(tab_2014$playerid),]

tab_2014<-bind_cols(tab_2013, tab_2014) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2014<- tab_2014 %>% filter(Team_1!=Team_2)
rm(tab_2013)
#2014 correlation data#
cor_2014<-cor(tab_2014[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2014_fwar<-cor_2014[c(1,2,3,4,5),6]
predictive_cor_2014_RA9<-cor_2014[c(1,2,3,4,5),10]

descriptive_cor_2014_RE24<-cor_2014[c(6,7,8,10),9]
descriptive_cor_2014_RA9<-cor_2014[c(6,7,8,9),10]

#2013#

tab_2013<-bind_rows(kwar_2012, kwar_2013)
tab_2013<-tab_2013 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2012<-tab_2013 %>% 
  filter(Season==2012)

tab_2013<-tab_2013 %>%
  filter(Season==2013)

tab_2012<-tab_2012[order(tab_2012$playerid),]
tab_2013<-tab_2013[order(tab_2013$playerid),]

tab_2013<-bind_cols(tab_2012, tab_2013) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2013<- tab_2013 %>% filter(Team_1!=Team_2)
rm(tab_2012)
#2013 correlation data#
cor_2013<-cor(tab_2013[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2013_fwar<-cor_2013[c(1,2,3,4,5),6]
predictive_cor_2013_RA9<-cor_2013[c(1,2,3,4,5),10]

descriptive_cor_2013_RE24<-cor_2013[c(6,7,8,10),9]
descriptive_cor_2013_RA9<-cor_2013[c(6,7,8,9),10]

#2012#

tab_2012<-bind_rows(kwar_2011, kwar_2012)
tab_2012<-tab_2012 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2011<-tab_2012 %>% 
  filter(Season==2011)

tab_2012<-tab_2012 %>%
  filter(Season==2012)

tab_2011<-tab_2011[order(tab_2011$playerid),]
tab_2012<-tab_2012[order(tab_2012$playerid),]

tab_2012<-bind_cols(tab_2011, tab_2012) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2012<- tab_2012 %>% filter(Team_1!=Team_2)
rm(tab_2011)
#2012 correlation data#
cor_2012<-cor(tab_2012[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2012_fwar<-cor_2012[c(1,2,3,4,5),6]
predictive_cor_2012_RA9<-cor_2012[c(1,2,3,4,5),10]

descriptive_cor_2012_RE24<-cor_2012[c(6,7,8,10),9]
descriptive_cor_2012_RA9<-cor_2012[c(6,7,8,9),10]

#2011#

tab_2011<-bind_rows(kwar_2010, kwar_2011)
tab_2011<-tab_2011 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2010<-tab_2011 %>% 
  filter(Season==2010)

tab_2011<-tab_2011 %>%
  filter(Season==2011)

tab_2010<-tab_2010[order(tab_2010$playerid),]
tab_2011<-tab_2011[order(tab_2011$playerid),]

tab_2011<-bind_cols(tab_2010, tab_2011) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2011<- tab_2011 %>% filter(Team_1!=Team_2)
rm(tab_2010)
#2011 correlation data#
cor_2011<-cor(tab_2011[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2011_fwar<-cor_2011[c(1,2,3,4,5),6]
predictive_cor_2011_RA9<-cor_2011[c(1,2,3,4,5),10]

descriptive_cor_2011_RE24<-cor_2011[c(6,7,8,10),9]
descriptive_cor_2011_RA9<-cor_2011[c(6,7,8,9),10]

#2010#

tab_2010<-bind_rows(kwar_2009, kwar_2010)
tab_2010<-tab_2010 %>% group_by(playerid) %>% 
  filter(n()>1) %>%
  filter(TBF>170) %>%
  filter(n()>1)

tab_2009<-tab_2010 %>% 
  filter(Season==2009)

tab_2010<-tab_2010 %>%
  filter(Season==2010)

tab_2009<-tab_2009[order(tab_2009$playerid),]
tab_2010<-tab_2010[order(tab_2010$playerid),]

tab_2010<-bind_cols(tab_2009, tab_2010) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2010<- tab_2010 %>% filter(Team_1!=Team_2)
rm(tab_2009)
#2010 correlation data#
cor_2010<-cor(tab_2010[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                           "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2010_fwar<-cor_2010[c(1,2,3,4,5),6]
predictive_cor_2010_RA9<-cor_2010[c(1,2,3,4,5),10]

descriptive_cor_2010_RE24<-cor_2010[c(6,7,8,10),9]
descriptive_cor_2010_RA9<-cor_2010[c(6,7,8,9),10]

#2010-2019 Data Binding for TBF>170#

tab_2010_2019<-bind_rows(tab_2019, tab_2018, tab_2017, tab_2016, tab_2015, tab_2014, tab_2013, tab_2012, tab_2011,
                         tab_2010)

cor_2010_2019<-cor(tab_2010_2019[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                     "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_fwar_2010_2019 <-data.frame(cbind(predictive_cor_2010_fwar, predictive_cor_2011_fwar, predictive_cor_2012_fwar, predictive_cor_2013_fwar, predictive_cor_2014_fwar, predictive_cor_2015_fwar, predictive_cor_2016_fwar, predictive_cor_2017_fwar, predictive_cor_2018_fwar, predictive_cor_2019_fwar))
predictive_RA9_2010_2019<-data.frame(cbind(predictive_cor_2010_RA9, predictive_cor_2011_RA9, predictive_cor_2012_RA9, predictive_cor_2013_RA9, predictive_cor_2014_RA9, predictive_cor_2015_RA9, predictive_cor_2016_RA9, predictive_cor_2017_RA9, predictive_cor_2018_RA9, predictive_cor_2019_RA9))
descriptive_RE24_2010_2019 <- data.frame(cbind(descriptive_cor_2010_RE24, descriptive_cor_2011_RE24, descriptive_cor_2012_RE24, descriptive_cor_2013_RE24, descriptive_cor_2014_RE24, descriptive_cor_2015_RE24, descriptive_cor_2016_RE24, descriptive_cor_2017_RE24, descriptive_cor_2018_RE24, descriptive_cor_2019_RE24))
descriptive_RA9_2010_2019<- data.frame(cbind(descriptive_cor_2010_RA9, descriptive_cor_2011_RA9, descriptive_cor_2012_RA9, descriptive_cor_2013_RA9, descriptive_cor_2014_RA9, descriptive_cor_2015_RA9, descriptive_cor_2016_RA9, descriptive_cor_2017_RA9, descriptive_cor_2018_RA9, descriptive_cor_2019_RA9))

write.csv(predictive_fwar_2010_2019,"Baseball/spreadsheets/kWAR/charts/predictive_fwar_2010_2019.csv")
write.csv(predictive_RA9_2010_2019,"Baseball/spreadsheets/kWAR/charts/predictive_RA9_2010_2019.csv")
write.csv(descriptive_RE24_2010_2019,"Baseball/spreadsheets/kWAR/charts/descriptive_RE24_2010_2019.csv")
write.csv(descriptive_RA9_2010_2019,"Baseball/spreadsheets/kWAR/charts/descriptive_RA9_2010_2019.csv")
#preparing >0 TBF Data #

#2019#

tab_2019_all<-bind_rows(kwar_2018, kwar_2019)
tab_2019_all<-tab_2019_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2018_all<-tab_2019_all %>% 
  filter(Season==2018)

tab_2019_all<-tab_2019_all %>%
  filter(Season==2019)

tab_2018_all<-tab_2018_all[order(tab_2018_all$playerid),]
tab_2019_all<-tab_2019_all[order(tab_2019_all$playerid),]

tab_2019_all<-bind_cols(tab_2018_all, tab_2019_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2019_all<- tab_2019_all %>% filter(Team_1!=Team_2)
rm(tab_2018_all)
#2019 correlation data#
cor_2019_all<-cor(tab_2019_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2019_all_fwar<-cor_2019_all[c(1,2,3,4,5),6]
predictive_cor_2019_all_RA9<-cor_2019_all[c(1,2,3,4,5),10]

descriptive_cor_2019_all_RE24<-cor_2019_all[c(6,7,8,10),9]
descriptive_cor_2019_all_RA9<-cor_2019_all[c(6,7,8,9),10]

#2018#

tab_2018_all<-bind_rows(kwar_2017, kwar_2018)
tab_2018_all<-tab_2018_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2017_all<-tab_2018_all %>% 
  filter(Season==2017)

tab_2018_all<-tab_2018_all %>%
  filter(Season==2018)

tab_2017_all<-tab_2017_all[order(tab_2017_all$playerid),]
tab_2018_all<-tab_2018_all[order(tab_2018_all$playerid),]

tab_2018_all<-bind_cols(tab_2017_all, tab_2018_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2018_all<- tab_2018_all %>% filter(Team_1!=Team_2)
rm(tab_2017_all)
#2018 correlation data#
cor_2018_all<-cor(tab_2018_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2018_all_fwar<-cor_2018_all[c(1,2,3,4,5),6]
predictive_cor_2018_all_RA9<-cor_2018_all[c(1,2,3,4,5),10]

descriptive_cor_2018_all_RE24<-cor_2018_all[c(6,7,8,10),9]
descriptive_cor_2018_all_RA9<-cor_2018_all[c(6,7,8,9),10]

#2017#

tab_2017_all<-bind_rows(kwar_2016, kwar_2017)
tab_2017_all<-tab_2017_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2016_all<-tab_2017_all %>% 
  filter(Season==2016)

tab_2017_all<-tab_2017_all %>%
  filter(Season==2017)

tab_2016_all<-tab_2016_all[order(tab_2016_all$playerid),]
tab_2017_all<-tab_2017_all[order(tab_2017_all$playerid),]

tab_2017_all<-bind_cols(tab_2016_all, tab_2017_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2017_all<- tab_2017_all %>% filter(Team_1!=Team_2)
rm(tab_2016_all)
#2017 correlation data#
cor_2017_all<-cor(tab_2017_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2017_all_fwar<-cor_2017_all[c(1,2,3,4,5),6]
predictive_cor_2017_all_RA9<-cor_2017_all[c(1,2,3,4,5),10]

descriptive_cor_2017_all_RE24<-cor_2017_all[c(6,7,8,10),9]
descriptive_cor_2017_all_RA9<-cor_2017_all[c(6,7,8,9),10]

#2016#

tab_2016_all<-bind_rows(kwar_2015, kwar_2016)
tab_2016_all<-tab_2016_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2015_all<-tab_2016_all %>% 
  filter(Season==2015)

tab_2016_all<-tab_2016_all %>%
  filter(Season==2016)

tab_2015_all<-tab_2015_all[order(tab_2015_all$playerid),]
tab_2016_all<-tab_2016_all[order(tab_2016_all$playerid),]

tab_2016_all<-bind_cols(tab_2015_all, tab_2016_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2016_all<- tab_2016_all %>% filter(Team_1!=Team_2)
rm(tab_2015_all)
#2016 correlation data#
cor_2016_all<-cor(tab_2016_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2016_all_fwar<-cor_2016_all[c(1,2,3,4,5),6]
predictive_cor_2016_all_RA9<-cor_2016_all[c(1,2,3,4,5),10]

descriptive_cor_2016_all_RE24<-cor_2016_all[c(6,7,8,10),9]
descriptive_cor_2016_all_RA9<-cor_2016_all[c(6,7,8,9),10]

#2015#

tab_2015_all<-bind_rows(kwar_2014, kwar_2015)
tab_2015_all<-tab_2015_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2014_all<-tab_2015_all %>% 
  filter(Season==2014)

tab_2015_all<-tab_2015_all %>%
  filter(Season==2015)

tab_2014_all<-tab_2014_all[order(tab_2014_all$playerid),]
tab_2015_all<-tab_2015_all[order(tab_2015_all$playerid),]

tab_2015_all<-bind_cols(tab_2014_all, tab_2015_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2015_all<- tab_2015_all %>% filter(Team_1!=Team_2)
rm(tab_2014_all)
#2015 correlation data#
cor_2015_all<-cor(tab_2015_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2015_all_fwar<-cor_2015_all[c(1,2,3,4,5),6]
predictive_cor_2015_all_RA9<-cor_2015_all[c(1,2,3,4,5),10]

descriptive_cor_2015_all_RE24<-cor_2015_all[c(6,7,8,10),9]
descriptive_cor_2015_all_RA9<-cor_2015_all[c(6,7,8,9),10]

#2014#

tab_2014_all<-bind_rows(kwar_2013, kwar_2014)
tab_2014_all<-tab_2014_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2013_all<-tab_2014_all %>% 
  filter(Season==2013)

tab_2014_all<-tab_2014_all %>%
  filter(Season==2014)

tab_2013_all<-tab_2013_all[order(tab_2013_all$playerid),]
tab_2014_all<-tab_2014_all[order(tab_2014_all$playerid),]

tab_2014_all<-bind_cols(tab_2013_all, tab_2014_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2014_all<- tab_2014_all %>% filter(Team_1!=Team_2)
rm(tab_2013_all)
#2014 correlation data#
cor_2014_all<-cor(tab_2014_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2014_all_fwar<-cor_2014_all[c(1,2,3,4,5),6]
predictive_cor_2014_all_RA9<-cor_2014_all[c(1,2,3,4,5),10]

descriptive_cor_2014_all_RE24<-cor_2014_all[c(6,7,8,10),9]
descriptive_cor_2014_all_RA9<-cor_2014_all[c(6,7,8,9),10]

#2013#

tab_2013_all<-bind_rows(kwar_2012, kwar_2013)
tab_2013_all<-tab_2013_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2012_all<-tab_2013_all %>% 
  filter(Season==2012)

tab_2013_all<-tab_2013_all %>%
  filter(Season==2013)

tab_2012_all<-tab_2012_all[order(tab_2012_all$playerid),]
tab_2013_all<-tab_2013_all[order(tab_2013_all$playerid),]

tab_2013_all<-bind_cols(tab_2012_all, tab_2013_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2013_all<- tab_2013_all %>% filter(Team_1!=Team_2)
rm(tab_2012_all)
#2013 correlation data#
cor_2013_all<-cor(tab_2013_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2013_all_fwar<-cor_2013_all[c(1,2,3,4,5),6]
predictive_cor_2013_all_RA9<-cor_2013_all[c(1,2,3,4,5),10]

descriptive_cor_2013_all_RE24<-cor_2013_all[c(6,7,8,10),9]
descriptive_cor_2013_all_RA9<-cor_2013_all[c(6,7,8,9),10]

#2012#

tab_2012_all<-bind_rows(kwar_2011, kwar_2012)
tab_2012_all<-tab_2012_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2011_all<-tab_2012_all %>% 
  filter(Season==2011)

tab_2012_all<-tab_2012_all %>%
  filter(Season==2012)

tab_2011_all<-tab_2011_all[order(tab_2011_all$playerid),]
tab_2012_all<-tab_2012_all[order(tab_2012_all$playerid),]

tab_2012_all<-bind_cols(tab_2011_all, tab_2012_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2012_all<- tab_2012_all %>% filter(Team_1!=Team_2)
rm(tab_2011_all)
#2012 correlation data#
cor_2012_all<-cor(tab_2012_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2012_all_fwar<-cor_2012_all[c(1,2,3,4,5),6]
predictive_cor_2012_all_RA9<-cor_2012_all[c(1,2,3,4,5),10]

descriptive_cor_2012_all_RE24<-cor_2012_all[c(6,7,8,10),9]
descriptive_cor_2012_all_RA9<-cor_2012_all[c(6,7,8,9),10]

#2011#

tab_2011_all<-bind_rows(kwar_2010, kwar_2011)
tab_2011_all<-tab_2011_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2010_all<-tab_2011_all %>% 
  filter(Season==2010)

tab_2011_all<-tab_2011_all %>%
  filter(Season==2011)

tab_2010_all<-tab_2010_all[order(tab_2010_all$playerid),]
tab_2011_all<-tab_2011_all[order(tab_2011_all$playerid),]

tab_2011_all<-bind_cols(tab_2010_all, tab_2011_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2011_all<- tab_2011_all %>% filter(Team_1!=Team_2)
rm(tab_2010_all)
#2011 correlation data#
cor_2011_all<-cor(tab_2011_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2011_all_fwar<-cor_2011_all[c(1,2,3,4,5),6]
predictive_cor_2011_all_RA9<-cor_2011_all[c(1,2,3,4,5),10]

descriptive_cor_2011_all_RE24<-cor_2011_all[c(6,7,8,10),9]
descriptive_cor_2011_all_RA9<-cor_2011_all[c(6,7,8,9),10]

#2010#

tab_2010_all<-bind_rows(kwar_2009, kwar_2010)
tab_2010_all<-tab_2010_all %>% group_by(playerid) %>% 
  filter(n()>1)

tab_2009_all<-tab_2010_all %>% 
  filter(Season==2009)

tab_2010_all<-tab_2010_all %>%
  filter(Season==2010)

tab_2009_all<-tab_2009_all[order(tab_2009_all$playerid),]
tab_2010_all<-tab_2010_all[order(tab_2010_all$playerid),]

tab_2010_all<-bind_cols(tab_2009_all, tab_2010_all) %>% 
  select(playerid...1, Name...2, Team...3, Team...26, Season...4, Season...27, TBF...6,TBF...29,
         BB...7, BB...30, SO...8, SO...31, ERA...9, ERA...32, FIP...10, FIP...33,
         xFIP...11, xFIP...34, SIERA...12, SIERA...35, kwERA...13, kwERA...36,
         GBkwERA...14, GBkwERA...37, fWAR...15, fWAR...38, kWAR...16, kWAR...39,
         gWAR...17, gWAR...40, RE24...18, RE24...41, RA9...19, RA9...42, fWAR_TBF...20, fWAR_TBF...43,
         kWAR_TBF...21, kWAR_TBF...44, gWAR_TBF...22, gWAR_TBF...45, RE24_TBF...23, RE24_TBF...46) %>%
  rename(playerid=playerid...1, Name=Name...2, Season_1=Season...4, Season_2=Season...27, Team_1=Team...3, 
         Team_2=Team...26, TBF_1=TBF...6, TBF_2=TBF...29, BB_1=BB...7, BB_2=BB...30, SO_1=SO...8, SO_2=SO...31,
         ERA_1=ERA...9, ERA_2=ERA...32, FIP_1=FIP...10, FIP_2=FIP...33, xFIP_1=xFIP...11, xFIP_2=xFIP...34,
         SIERA_1=SIERA...12, SIERA_2=SIERA...35, kwERA_1=kwERA...13, kwERA_2=kwERA...36, 
         GBkwERA_1=GBkwERA...14, GBkwERA_2=GBkwERA...37, fWAR_1=fWAR...15, fWAR_2=fWAR...38,
         kWAR_1=kWAR...16, kWAR_2=kWAR...39, gWAR_1=gWAR...17, gWAR_2=gWAR...40,RE24_1=RE24...18, 
         RE24_2=RE24...41,RA9_1=RA9...19, RA9_2=RA9...42, fWAR_TBF_1=fWAR_TBF...20, fWAR_TBF_2=fWAR_TBF...43,
         kWAR_TBF_1=kWAR_TBF...21, kWAR_TBF_2=kWAR_TBF...44, 
         gWAR_TBF_1=gWAR_TBF...22, gWAR_TBF_2=gWAR_TBF...45,
         RE24_TBF_1=RE24_TBF...23, RE24_TBF_2=RE24_TBF...46)
tab_2010_all<- tab_2010_all %>% filter(Team_1!=Team_2)
rm(tab_2009_all)
#2010 correlation data#
cor_2010_all<-cor(tab_2010_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                   "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 

predictive_cor_2010_fwar<-cor_2010[c(1,2,3,4,5),6]
predictive_cor_2010_RA9<-cor_2010[c(1,2,3,4,5),10]

descriptive_cor_2010_RE24<-cor_2010[c(6,7,8,10),9]
descriptive_cor_2010_RA9<-cor_2010[c(6,7,8,9),10]

#2010-2019 Data Binding for TBF>0#

tab_2010_2019_all<-bind_rows(tab_2019_all, tab_2018_all, tab_2017_all, tab_2016_all, tab_2015_all, tab_2014_all, tab_2013_all, tab_2012_all, tab_2011_all,
                             tab_2010_all)

cor_2010_2019_all<-cor(tab_2010_2019_all[, c("fWAR_TBF_1","kWAR_TBF_1", "gWAR_TBF_1", "RE24_TBF_1", "RA9_1",
                                             "fWAR_TBF_2","kWAR_TBF_2", "gWAR_TBF_2", "RE24_TBF_2", "RA9_2")]) 


predictive_fwar_2010_2019 <-data.frame(cbind(predictive_cor_2010_fwar, predictive_cor_2011_all_fwar, predictive_cor_2012_all_fwar, predictive_cor_2013_all_fwar, predictive_cor_2014_all_fwar, predictive_cor_2015_all_fwar, predictive_cor_2016_all_fwar, predictive_cor_2017_all_fwar, predictive_cor_2018_all_fwar, predictive_cor_2019_all_fwar))
predictive_RA9_2010_2019<-data.frame(cbind(predictive_cor_2010_RA9, predictive_cor_2011_all_RA9, predictive_cor_2012_all_RA9, predictive_cor_2013_all_RA9, predictive_cor_2014_all_RA9, predictive_cor_2015_all_RA9, predictive_cor_2016_all_RA9, predictive_cor_2017_all_RA9, predictive_cor_2018_all_RA9, predictive_cor_2019_all_RA9))
descriptive_RE24_2010_2019 <- data.frame(cbind(descriptive_cor_2010_RE24, descriptive_cor_2011_all_RE24, descriptive_cor_2012_all_RE24, descriptive_cor_2013_all_RE24, descriptive_cor_2014_all_RE24, descriptive_cor_2015_all_RE24, descriptive_cor_2016_all_RE24, descriptive_cor_2017_all_RE24, descriptive_cor_2018_all_RE24, descriptive_cor_2019_all_RE24))
descriptive_RA9_2010_2019<- data.frame(cbind(descriptive_cor_2010_RA9, descriptive_cor_2011_all_RA9, descriptive_cor_2012_all_RA9, descriptive_cor_2013_all_RA9, descriptive_cor_2014_all_RA9, descriptive_cor_2015_all_RA9, descriptive_cor_2016_all_RA9, descriptive_cor_2017_all_RA9, descriptive_cor_2018_all_RA9, descriptive_cor_2019_all_RA9))

write.csv(predictive_fwar_2010_2019,"Baseball/spreadsheets/kWAR/charts/predictive_fwar_2010_2019_all.csv")
write.csv(predictive_RA9_2010_2019,"Baseball/spreadsheets/kWAR/charts/predictive_RA9_2010_2019_all.csv")
write.csv(descriptive_RE24_2010_2019,"Baseball/spreadsheets/kWAR/charts/descriptive_RE24_2010_2019_all.csv")
write.csv(descriptive_RA9_2010_2019,"Baseball/spreadsheets/kWAR/charts/descriptive_RA9_2010_2019_all.csv")


#linear regressions for predicting Y+1 RA9#

model_fwar_170<-lm(RA9_2~fWAR_TBF_1, data=tab_2010_2019)

model_kwar_170<-lm(RA9_2~kWAR_TBF_1, data=tab_2010_2019)

model_gwar_170<-lm(RA9_2~gWAR_TBF_1, data=tab_2010_2019)

model_combined_170<-lm(RA9_2~fWAR_TBF_1+kWAR_TBF_1+gWAR_TBF_1, data=tab_2010_2019)

model_fwar_all<-lm(RA9_2~fWAR_TBF_1, data=tab_2010_2019_all)

model_kwar_all<-lm(RA9_2~kWAR_TBF_1, data=tab_2010_2019_all)

model_gwar_all<-lm(RA9_2~gWAR_TBF_1, data=tab_2010_2019_all)

model_combined_all<-lm(RA9_2~fWAR_TBF_1+kWAR_TBF_1+gWAR_TBF_1, data=tab_2010_2019_all)

#graphs for >=170 TBF#

label_fwar<-tab_2010_2019 %>% lm(fWAR_TBF_2~fWAR_TBF_1, data=.)
label_fwar<-paste("R^2 == ", round(summary(label_fwar)$r.squared,3))

label_gwar<-tab_2010_2019 %>% lm(fWAR_TBF_2~gWAR_TBF_1, data=.)
label_gwar<-paste("R^2 == ", round(summary(label_gwar)$r.squared,3))

label_kwar<-tab_2010_2019 %>% lm(fWAR_TBF_2~kWAR_TBF_1, data=.)
label_kwar<-paste("R^2 == ", round(summary(label_kwar)$r.squared,3))

fwar_plot<-tab_2010_2019 %>%
  ggplot(aes(fWAR_TBF_1, fWAR_TBF_2))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="fWAR")+
  xlab("Year X fWAR per TBF")+
  ylab("Year X+1 fWAR per TBF")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=.01, y=-.003, label=label_fwar, color="black", parse=TRUE)

gwar_plot<-tab_2010_2019 %>%
  ggplot(aes(gWAR_TBF_1, fWAR_TBF_2))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="gWAR")+
  xlab("Year X gWAR per TBF")+
  ylab("Year X+1 fWAR per TBF")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=.01, y=-.003, label=label_gwar, color="black", parse=TRUE)

kwar_plot<-tab_2010_2019 %>%
  ggplot(aes(kWAR_TBF_1, fWAR_TBF_2))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="kWAR")+
  xlab("Year X kWAR per TBF")+
  ylab("Year X+1 fWAR per TBF")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=.01, y=-.003, label=label_kwar, color="black", parse=TRUE)

#graphs for >=0 TBF data#

label_fwar_all<-tab_2010_2019_all %>% lm(fWAR_TBF_2~fWAR_TBF_1, data=.)
label_fwar_all<-paste("R^2 == ", round(summary(label_fwar_all)$r.squared,3))

label_gwar_all<-tab_2010_2019_all %>% lm(fWAR_TBF_2~gWAR_TBF_1, data=.)
label_gwar_all<-paste("R^2 == ", round(summary(label_gwar_all)$r.squared,3))

label_kwar_all<-tab_2010_2019_all %>% lm(fWAR_TBF_2~kWAR_TBF_1, data=.)
label_kwar_all<-paste("R^2 == ", round(summary(label_kwar_all)$r.squared,3))

fwar_plot_all<-tab_2010_2019_all %>%
  ggplot(aes(fWAR_TBF_1, fWAR_TBF_2))+
  geom_point(alpha=0.25)+
  geom_smooth(method="lm")+
  labs(title="fWAR")+
  xlab("Year X fWAR per TBF")+
  ylab("Year X+1 fWAR per TBF")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=-.035, y=.01, label=label_fwar_all, color="black", parse=TRUE)

gwar_plot_all<-tab_2010_2019 %>%
  ggplot(aes(gWAR_TBF_1, fWAR_TBF_2))+
  geom_point(alpha=0.25)+
  geom_smooth(method="lm")+
  labs(title="gWAR")+
  xlab("Year X gWAR per TBF")+
  ylab("Year X+1 fWAR per TBF")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=.01, y=-.003, label=label_gwar_all, color="black", parse=TRUE)

kwar_plot_all<-tab_2010_2019 %>%
  ggplot(aes(kWAR_TBF_1, fWAR_TBF_2))+
  geom_point(alpha=0.25)+
  geom_smooth(method="lm")+
  labs(title="kWAR")+
  xlab("Year X kWAR per TBF")+
  ylab("Year X+1 fWAR per TBF")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=.01, y=-.003, label=label_kwar_all, color="black", parse=TRUE)

#RA9 graphs for >=170 TBF#

label_fwar_ra9<-tab_2010_2019 %>%
  mutate(fWAR_TBF_1=fWAR_TBF_1*100)%>%
  lm(RA9_2~fWAR_TBF_1, data=.)
label_fwar_ra9<-paste("R^2 == ", round(summary(label_fwar_ra9)$r.squared,3))

label_gwar_ra9<-tab_2010_2019 %>%
  mutate(gWAR_TBF_1=gWAR_TBF_1*100)%>%
  lm(RA9_2~gWAR_TBF_1, data=.)
label_gwar_ra9<-paste("R^2 == ", round(summary(label_gwar_ra9)$r.squared,3))

label_kwar_ra9<-tab_2010_2019 %>%
  mutate(kWAR_TBF_1=kWAR_TBF_1*100)%>%
  lm(RA9_2~kWAR_TBF_1, data=.)
label_kwar_ra9<-paste("R^2 == ", round(summary(label_kwar_ra9)$r.squared,3))

fwar_plot_ra9<-tab_2010_2019 %>%
  ggplot(aes(fWAR_TBF_1*100, RA9_2))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="fWAR")+
  xlab("Year X fWAR per 100 TBF")+
  ylab("Year X+1 RA9")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=1.5, y=9, label=label_fwar_ra9, color="black", parse=TRUE)

gwar_plot_ra9<-tab_2010_2019 %>%
  ggplot(aes(gWAR_TBF_1*100, RA9_2))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="gWAR")+
  xlab("Year X gWAR per 100 TBF")+
  ylab("Year X+1 RA9")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=1.5, y=9, label=label_gwar_ra9, color="black", parse=TRUE)

kwar_plot_ra9<-tab_2010_2019 %>%
  ggplot(aes(kWAR_TBF_1*100, RA9_2))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="kWAR")+
  xlab("Year X kWAR per 100 TBF")+
  ylab("Year X+1 RA9")+
  theme_stata()+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="label", x=1.5, y=9, label=label_kwar_ra9, color="black", parse=TRUE)
