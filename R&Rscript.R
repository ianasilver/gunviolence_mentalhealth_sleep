# Getting Started ####
# Written Ay: Ian A. Silver
# Purpose: Gun Licenses and Homicide

library(psych)
library(lavaan)
library("moments")
library("pastecs")
library("ggplot2")
library("extrafont")
library("data.table")
library(dplyr)
library(tidyverse)
library("sjmisc")
library(expss)
library(readstata13)
library(ggplot2)
library(dplyr)
library(lme4)
library(ggridges)
library(extrafont)
library(Hmisc)
library(psych)
library(lmerTest)
library(pbkrtest)
library(MuMIn)
library(pastecs)
library(pracma)
library(matrixStats)
library(purrr)
library(mice)
library(VIM)
library(tidyverse)
library(egg)
library(data.table)
library(doParallel)
library(parallel)
library(foreach)
library(doRNG)
library(mice)
detectCores()
registerDoParallel(2)

options(scipen=100, digits=12)
options(max.print = 500000000)



setwd("G:/My Drive/External Work/004-Dan & Richard Projects/004-Gun Violence, Mental Health, Sleep/R&R")





# Loading CSV (Use Me)####
A<-fread("G:/My Drive/External Work/004-Dan & Richard Projects/Data/MergedMainData_12.27.22.csv", sep = "\t", fill=TRUE, quote=F, na.strings="NA")

names(A)

C<-fread("G:/My Drive/External Work/004-Dan & Richard Projects/004-Gun Violence, Mental Health, Sleep/R&R/Data/PopDensity_SSM.csv", sep = ",", fill=TRUE, quote=F, na.strings="NA")
D<-fread("G:/My Drive/External Work/004-Dan & Richard Projects/004-Gun Violence, Mental Health, Sleep/R&R/Data/Data 10.26.23.csv", sep = "\t", fill=TRUE, quote=F, na.strings="NA")
names(C)
names(D)


Test<-merge(C,D, by = "GEOID", all.y=F)
names(Test)
A<-merge(A,Test, by = "gisjoin", all.y=F)
names(A)



# Cleanind Data ####
## Total Gun Violence Measure ####
A$total_2014<-A$Fatal2014+A$nonfatal2014
stat.desc(A$total_2014)
A$total_2015<-A$Fatal2015+A$nonfatal2015
stat.desc(A$total_2015)
A$total_2016<-A$Fatal2016+A$nonfatal2016
stat.desc(A$total_2016)
A$total_2017<-A$Fatal2017+A$nonfatal2017
stat.desc(A$total_2017)
A$total_2018<-A$Fatal2018+A$nonfatal2018
stat.desc(A$total_2018)
A$total_2019<-A$Fatal2019+A$nonfatal2019
stat.desc(A$total_2019)
## Creating time invariant Community Measures ####
names(A)

## Community Disadvantage Measures ####
table(A$pctfem_head_house_2014)
table(A$pctpoverty_2014)
table(A$pctunemployedmale_2014)

A$pctfem_head_house_2014R<-A$pctfem_head_house_2014*.1
A$pctpoverty_2014R<-A$pctpoverty_2014*.1
A$pctunemployedmale_2014R<-A$pctunemployedmale_2014*.1

A$CDIS<-rowMeans(data.frame(A$pctfem_head_house_2014R,A$pctpoverty_2014R,A$pctunemployedmale_2014R), na.rm=T)



## Proportion Measures ####
names(A)
A$prowhite_2014<-(A$nhwhite_2014/A$totalpop_2014)
A$prohispanic_2014<-(A$hispanic_2014/A$totalpop_2014)
A$problack_2014<-(A$nhblack_2014/A$totalpop_2014)
summary(A$prowhite_2014)
summary(A$prohispanic_2014)
summary(A$problack_2014)

A$prowhite_2015<-(A$nhwhite_2015/A$totalpop_2015)
A$prohispanic_2015<-(A$hispanic_2015/A$totalpop_2015)
A$problack_2015<-(A$nhblack_2015/A$totalpop_2015)
summary(A$prowhite_2015)
summary(A$prohispanic_2015)
summary(A$problack_2015)


A$prowhite_2016<-(A$nhwhite_2016/A$totalpop_2016)
A$prohispanic_2016<-(A$hispanic_2016/A$totalpop_2016)
A$problack_2016<-(A$nhblack_2016/A$totalpop_2016)
summary(A$prowhite_2016)
summary(A$prohispanic_2016)
summary(A$problack_2016)


A$prowhite_2017<-(A$nhwhite_2017/A$totalpop_2017)
A$prohispanic_2017<-(A$hispanic_2017/A$totalpop_2017)
A$problack_2017<-(A$nhblack_2017/A$totalpop_2017)
summary(A$prowhite_2017)
summary(A$prohispanic_2017)
summary(A$problack_2017)



A$prowhite_2018<-(A$nhwhite_2018/A$totalpop_2018)
A$prohispanic_2018<-(A$hispanic_2018/A$totalpop_2018)
A$problack_2018<-(A$nhblack_2018/A$totalpop_2018)
summary(A$prowhite_2018)
summary(A$prohispanic_2018)
summary(A$problack_2018)


A$prowhite_2019<-(A$nhwhite_2019/A$totalpop_2019)
A$prohispanic_2019<-(A$hispanic_2019/A$totalpop_2019)
A$problack_2019<-(A$nhblack_2019/A$totalpop_2019)
summary(A$prowhite_2019)
summary(A$prohispanic_2019)
summary(A$problack_2019)


A$A_problack<-rowMeans(data.frame(A$problack_2014,A$problack_2015,
                                      A$problack_2016,A$problack_2017,
                                      A$problack_2018,A$problack_2019), na.rm = T)
summary(A$A_problack)

A$A_prohispanic<-rowMeans(data.frame(A$prohispanic_2014,A$prohispanic_2015,
                                  A$prohispanic_2016,A$prohispanic_2017,
                                  A$prohispanic_2018,A$prohispanic_2019), na.rm = T)
summary(A$A_prohispanic)

## Community Disadvantage Measures (14-19) ####
#### 2014

table(A$pctFemHeadedH_2014)
table(A$pctpoverty_2014)
table(A$pctunemployed_2014)
table(A$problack_2014)

A$pctFemHeadedH_2014R<-A$pctFemHeadedH_2014
A$pctpoverty_2014R<-A$pctpoverty_2014
A$pctunemployed_2014R<-A$pctunemployed_2014
A$problack_2014R<-A$problack_2014*100

A$CDIS_2014<-rowMeans(data.frame(A$pctFemHeadedH_2014R,A$pctpoverty_2014R,A$pctunemployed_2014R), na.rm=T)
summary(A$CDIS_2014)

#### 2015
summary(A$pctFemHeadedH_2014)
summary(A$pctFemHeadedH_2015)
summary(A$pctpoverty_2014)
summary(A$pctpoverty_2015)
summary(A$pctunemployed_2014)
summary(A$pctunemployed_2015)
table(A$pctpoverty_2015)
table(A$pctunemployed_2015)

A$pctFemHeadedH_2015R<-A$pctFemHeadedH_2015
A$pctpoverty_2015R<-A$pctpoverty_2015
A$pctunemployed_2015R<-A$pctunemployed_2015
A$problack_2015R<-A$problack_2015*100

A$CDIS_2015<-rowMeans(data.frame(A$pctFemHeadedH_2015R,A$pctpoverty_2015R,A$pctunemployed_2015R), na.rm=T)
#### 2016
table(A$pctFemHeadedH_2016)
table(A$pctpoverty_2016)
table(A$pctunemployed_2016)

A$pctFemHeadedH_2016R<-A$pctFemHeadedH_2016
A$pctpoverty_2016R<-A$pctpoverty_2016
A$pctunemployed_2016R<-A$pctunemployed_2016
A$problack_2016R<-A$problack_2016*100

A$CDIS_2016<-rowMeans(data.frame(A$pctFemHeadedH_2016R,A$pctpoverty_2016R,A$pctunemployed_2016R), na.rm=T)
#### 2017
table(A$pctFemHeadedH_2017)
table(A$pctpoverty_2017)
table(A$pctunemployed_2017)

A$pctFemHeadedH_2017R<-A$pctFemHeadedH_2017
A$pctpoverty_2017R<-A$pctpoverty_2017
A$pctunemployed_2017R<-A$pctunemployed_2017
A$problack_2017R<-A$problack_2017*100

A$CDIS_2017<-rowMeans(data.frame(A$pctFemHeadedH_2017R,A$pctpoverty_2017R,A$pctunemployed_2017R), na.rm=T)
#### 2018
table(A$pctFemHeadedH_2018)
table(A$pctpoverty_2018)
table(A$pctunemployed_2018)

A$pctFemHeadedH_2018R<-A$pctFemHeadedH_2018
A$pctpoverty_2018R<-A$pctpoverty_2018
A$pctunemployed_2018R<-A$pctunemployed_2018
A$problack_2018R<-A$problack_2018*100

A$CDIS_2018<-rowMeans(data.frame(A$pctFemHeadedH_2018R,A$pctpoverty_2018R,A$pctunemployed_2018R), na.rm=T)

summary(A$CDIS_2018)
#### 2019
table(A$pctFemHeadedH_2019)
table(A$pctpoverty_2019)
table(A$pctunemployed_2019)

A$pctFemHeadedH_2019R<-A$pctFemHeadedH_2019
A$pctpoverty_2019R<-A$pctpoverty_2019
A$pctunemployed_2019R<-A$pctunemployed_2019
A$problack_2019R<-A$problack_2019*100

A$CDIS_2019<-rowMeans(data.frame(A$pctFemHeadedH_2019R,A$pctpoverty_2019R,A$pctunemployed_2019R), na.rm=T)
summary(A$CDIS_2019)


A$A_CDIS<-rowMeans(data.frame(A$CDIS_2014,A$CDIS_2015,
                                      A$CDIS_2016,A$CDIS_2017,
                                      A$CDIS_2018,A$CDIS_2019), na.rm = T)
stat.desc(A$A_CDIS)



## Male Female Sex Ratio (14-19) ####
table(A$f_m_sexratio_2014)

A$A_f_m_sexratio<-rowMeans(data.frame(A$f_m_sexratio_2014,A$f_m_sexratio_2015,
                                      A$f_m_sexratio_2016,A$f_m_sexratio_2017,
                                      A$f_m_sexratio_2018,A$f_m_sexratio_2019), na.rm = T)
stat.desc(A$A_f_m_sexratio)
## Living Alone (14-19) ####
table(A$livingalone_2014)

A$livingalone_2014R<-A$livingalone_2014*.01
A$livingalone_2015R<-A$livingalone_2015*.01
A$livingalone_2016R<-A$livingalone_2016*.01
A$livingalone_2017R<-A$livingalone_2017*.01
A$livingalone_2018R<-A$livingalone_2018*.01
A$livingalone_2019R<-A$livingalone_2019*.01

A$A_livingaloneR<-rowMeans(data.frame(A$livingalone_2014R,A$livingalone_2015R,
                                      A$livingalone_2016R,A$livingalone_2017R,
                                      A$livingalone_2018R,A$livingalone_2019R), na.rm = T)
stat.desc(A$A_livingaloneR)

## Young Male (14-19) ####
stat.desc(A$yngmale1524_2014)
A$yngmale1524_2014R<-A$yngmale1524_2014*.01
A$yngmale1524_2015R<-A$yngmale1524_2015*.01
A$yngmale1524_2016R<-A$yngmale1524_2016*.01
A$yngmale1524_2017R<-A$yngmale1524_2017*.01
A$yngmale1524_2018R<-A$yngmale1524_2018*.01
A$yngmale1524_2019R<-A$yngmale1524_2019*.01

A$A_yngmale1524R<-rowMeans(data.frame(A$yngmale1524_2014R,A$yngmale1524_2015R,
                                      A$yngmale1524_2016R,A$yngmale1524_2017R,
                                      A$yngmale1524_2018R,A$yngmale1524_2019R), na.rm = T)

A$A_yngmale1524<-rowMeans(data.frame(A$yngmale1524_2014,A$yngmale1524_2015,
                                      A$yngmale1524_2016,A$yngmale1524_2017,
                                      A$yngmale1524_2018,A$yngmale1524_2019), na.rm = T)
stat.desc(A$yngmale1524_2014R)
stat.desc(A$A_yngmale1524)
names(A)
## Percent Age (2014) ####

A$PctAge0_17R<-A$PctAge0_17*.1
A$PctAge18_34R<-A$PctAge18_34*.1
A$PctAge35_64R<-A$PctAge35_64*.1
A$PctAge65_69R<-A$PctAge65_69*.1
A$PctAge70_74R<-A$PctAge70_74*.1
A$PctAge75_79R<-A$PctAge75_79*.1
A$PctAge80_84R<-A$PctAge80_84*.1
A$PctAge85_PlusR<-A$PctAge85_Plus*.1
summary(A$PctAge85_PlusR)
summary(A$PctAge18_34)


## Person Households (14-19) #####
names(A)

A$pct2personhh_2014R<-A$pct2personhh_2014*.1
A$pct2personhh_2015R<-A$pct2personhh_2015*.1
A$pct2personhh_2016R<-A$pct2personhh_2016*.1
A$pct2personhh_2017R<-A$pct2personhh_2017*.1
A$pct2personhh_2018R<-A$pct2personhh_2018*.1
A$pct2personhh_2019R<-A$pct2personhh_2019*.1

A$A_pct2personhhR<-rowMeans(data.frame(A$pct2personhh_2014R,A$pct2personhh_2015R,
                                      A$pct2personhh_2016R,A$pct2personhh_2017R,
                                      A$pct2personhh_2018R,A$pct2personhh_2019R), na.rm = T)

stat.desc(A$A_pct2personhhR)

A$pct3personhh_2014R<-A$pct3personhh_2014*.1
A$pct3personhh_2015R<-A$pct3personhh_2015*.1
A$pct3personhh_2016R<-A$pct3personhh_2016*.1
A$pct3personhh_2017R<-A$pct3personhh_2017*.1
A$pct3personhh_2018R<-A$pct3personhh_2018*.1
A$pct3personhh_2019R<-A$pct3personhh_2019*.1

A$A_pct3personhhR<-rowMeans(data.frame(A$pct3personhh_2014R,A$pct3personhh_2015R,
                                       A$pct3personhh_2016R,A$pct3personhh_2017R,
                                       A$pct3personhh_2018R,A$pct3personhh_2019R), na.rm = T)

stat.desc(A$A_pct3personhhR)


A$pct4personhh_2014R<-A$pct4personhh_2014*.1
A$pct4personhh_2015R<-A$pct4personhh_2015*.1
A$pct4personhh_2016R<-A$pct4personhh_2016*.1
A$pct4personhh_2017R<-A$pct4personhh_2017*.1
A$pct4personhh_2018R<-A$pct4personhh_2018*.1
A$pct4personhh_2019R<-A$pct4personhh_2019*.1

A$A_pct4personhhR<-rowMeans(data.frame(A$pct4personhh_2014R,A$pct4personhh_2015R,
                                       A$pct4personhh_2016R,A$pct4personhh_2017R,
                                       A$pct4personhh_2018R,A$pct4personhh_2019R), na.rm = T)

stat.desc(A$A_pct4personhhR)


A$pct5personhh_2014R<-A$pct5personhh_2014*.1
A$pct5personhh_2015R<-A$pct5personhh_2015*.1
A$pct5personhh_2016R<-A$pct5personhh_2016*.1
A$pct5personhh_2017R<-A$pct5personhh_2017*.1
A$pct5personhh_2018R<-A$pct5personhh_2018*.1
A$pct5personhh_2019R<-A$pct5personhh_2019*.1

A$A_pct5personhhR<-rowMeans(data.frame(A$pct5personhh_2014R,A$pct5personhh_2015R,
                                       A$pct5personhh_2016R,A$pct5personhh_2017R,
                                       A$pct5personhh_2018R,A$pct5personhh_2019R), na.rm = T)

stat.desc(A$A_pct5personhhR)


A$pct6personhh_2014R<-A$pct6personhh_2014*.1
A$pct6personhh_2015R<-A$pct6personhh_2015*.1
A$pct6personhh_2016R<-A$pct6personhh_2016*.1
A$pct6personhh_2017R<-A$pct6personhh_2017*.1
A$pct6personhh_2018R<-A$pct6personhh_2018*.1
A$pct6personhh_2019R<-A$pct6personhh_2019*.1

A$A_pct6personhhR<-rowMeans(data.frame(A$pct6personhh_2014R,A$pct6personhh_2015R,
                                       A$pct6personhh_2016R,A$pct6personhh_2017R,
                                       A$pct6personhh_2018R,A$pct6personhh_2019R), na.rm = T)

stat.desc(A$A_pct6personhhR)


A$pct7personhh_2014R<-A$pct7personhh_2014*.1
A$pct7personhh_2015R<-A$pct7personhh_2015*.1
A$pct7personhh_2016R<-A$pct7personhh_2016*.1
A$pct7personhh_2017R<-A$pct7personhh_2017*.1
A$pct7personhh_2018R<-A$pct7personhh_2018*.1
A$pct7personhh_2019R<-A$pct7personhh_2019*.1

A$A_pct7personhhR<-rowMeans(data.frame(A$pct7personhh_2014R,A$pct7personhh_2015R,
                                       A$pct7personhh_2016R,A$pct7personhh_2017R,
                                       A$pct7personhh_2018R,A$pct7personhh_2019R), na.rm = T)

stat.desc(A$A_pct7personhhR)


## Population Density ####
names(A)

stat.desc(A$totalpop_2014)

A$totalpop_2014R<-A$totalpop_2014*.001
A$totalpop_2015R<-A$totalpop_2015*.001
A$totalpop_2016R<-A$totalpop_2016*.001
A$totalpop_2017R<-A$totalpop_2017*.001
A$totalpop_2018R<-A$totalpop_2018*.001
A$totalpop_2019R<-A$totalpop_2019*.001

stat.desc(A$totalpop_2014R)

A$A_totalpop<-rowMeans(data.frame(A$totalpop_2014,A$totalpop_2015,
                                   A$totalpop_2016,A$totalpop_2017,
                                   A$totalpop_2018,A$totalpop_2019), na.rm = T)

A$A_totalpopR<-rowMeans(data.frame(A$totalpop_2014R,A$totalpop_2015R,
                                       A$totalpop_2016R,A$totalpop_2017R,
                                       A$totalpop_2018R,A$totalpop_2019R), na.rm = T)

stat.desc(A$A_totalpopR)
stat.desc(A$squaremiles)
A$squaremilesR<-A$squaremiles*.1
stat.desc(A$squaremilesR)

A$A_popdensity<-A$A_totalpopR/A$squaremilesR
stat.desc(A$A_popdensity)
A$A_popdensityR<-A$A_popdensity*.01
stat.desc(A$A_popdensityR)

A$A_popdensityD<-A$A_totalpop/A$squaremiles
stat.desc(A$A_popdensity)

# Limiting Data Frame ####
names(A)

B<-A[,c("gisjoin","total_2014","total_2015","total_2016","total_2017","total_2018","total_2019",
        "Fatal2014","Fatal2015","Fatal2016","Fatal2017","Fatal2018","Fatal2019",
        "nonfatal2014","nonfatal2015","nonfatal2016","nonfatal2017","nonfatal2018","nonfatal2019",
        "mhlth2014","mhlth2015","mhlth2016", "mhlth2017", "mhlth2018", "mhlth2019",
        "sleep2014","sleep2015","sleep2016","sleep2017","sleep2018","sleep2019",
        "CDIS_2014","CDIS_2015","CDIS_2016","CDIS_2017","CDIS_2018","CDIS_2019","A_CDIS",
        "miles_to_nearest_trauma","A_livingaloneR","A_f_m_sexratio","A_yngmale1524R","A_problack","A_prohispanic",
        "PctAge18_34R","PctAge35_64R","PctAge65_69R","PctAge70_74R","PctAge75_79R","PctAge80_84R","A_totalpopR",
        "A_pct2personhhR","A_pct3personhhR","A_pct4personhhR","A_pct5personhhR","A_pct6personhhR","A_pct7personhhR","A_popdensityR",
        "A_popdensityD"
)]



sink("Results/Descriptive Statistics_before FIML.txt")
print(stat.desc(B),digits = 3)
sink()

# Creating Long Dataset ####
table(B$mhlth2014)
table(B$sleep2014)
names(B)
DF_3.1<-reshape(B, varying =c(2:6), v.names = "total_GV14_18", timevar = "Year", direction = "long")
names(DF_3.1)
DF_3.2<-reshape(B, varying =c(21:25), v.names = "mhlth15_19", timevar = "Year", direction = "long")
names(DF_3.2)
DF_3.2<-(DF_3.2[, c("gisjoin","mhlth15_19","Year")])
DF_3.3<-reshape(B, varying =c(27:31), v.names = "sleep15_19", timevar = "Year", direction = "long")
names(DF_3.3)
DF_3.3<-(DF_3.3[, c("gisjoin","sleep15_19","Year")])
DF_3.4<-reshape(B, varying =c(20:24), v.names = "mhlth14_18", timevar = "Year", direction = "long")
names(DF_3.4)
DF_3.4<-(DF_3.4[, c("gisjoin","mhlth14_18","Year")])
DF_3.5<-reshape(B, varying =c(26:30), v.names = "sleep14_18", timevar = "Year", direction = "long")
names(DF_3.5)
DF_3.5<-(DF_3.5[, c("gisjoin","sleep14_18","Year")])
DF_3.6<-reshape(B, varying =c(22:25), v.names = "mhlth16_19", timevar = "Year", direction = "long")
names(DF_3.6)
DF_3.6<-(DF_3.6[, c("gisjoin","mhlth16_19","Year")])
DF_3.7<-reshape(B, varying =c(28:31), v.names = "sleep16_19", timevar = "Year", direction = "long")
names(DF_3.7)
DF_3.7<-(DF_3.7[, c("gisjoin","sleep16_19","Year")])
C<-Reduce(function(x, y) merge(x, y, by = c("gisjoin","Year"), all.x = T),
                   list(DF_3.1,DF_3.2,DF_3.3,DF_3.4,DF_3.5,DF_3.6,DF_3.7))
names(C)

# Dummy Coding Year ####
table(C$Year)

C$Y_1[C$Year!=1]<-0
C$Y_1[C$Year==1]<-1
table(C$Y_1)

C$Y_2[C$Year!=2]<-0
C$Y_2[C$Year==2]<-1
table(C$Y_2)

C$Y_3[C$Year!=3]<-0
C$Y_3[C$Year==3]<-1
table(C$Y_3)

C$Y_4[C$Year!=4]<-0
C$Y_4[C$Year==4]<-1
table(C$Y_4)

C$Y_5[C$Year!=5]<-0
C$Y_5[C$Year==5]<-1
table(C$Y_5)

# Descriptive Statistics ####

sink("Results/Descriptive Statistics_Long.txt")
print(stat.desc(C),digits = 3)
sink()

M<-data.frame(C$A_problack,C$A_prohispanic,C$mhlth15_19,C$total_GV14_18,C$sleep15_19)

corr.test(M)


# Primary Models ####
# Model 1: Mental Health and Sleep on Gun Violence ####
names(C)
F1<-'

mhlth15_19~total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR+
           Y_1+Y_2+Y_3+Y_4


sleep15_19~total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR+
           Y_1+Y_2+Y_3+Y_4

mhlth15_19~~sleep15_19

'

M1<-sem(F1, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M1)

sink("Results/Mental Health and Sleep on Gun Violence.txt")
print(summary(M1, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M1))
print(inspect(M1))
print(resid(M1, type = "cor"))
sink()
# Model 2: Mental Health on Sleep and Gun Violence ####
names(C)
F2<-'

mhlth15_19~sleep14_18+total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR+Y_1+Y_2+Y_3+Y_4


'

M2<-sem(F2, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M2)

sink("Results/Mental Health on Sleep and Gun Violence.txt")
print(summary(M2, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M2))
print(inspect(M2))
print(resid(M2, type = "cor"))
sink()
# Model 3: Sleep on Mental Health and Gun Violence ####
names(C)
F3<-'

sleep15_19~mhlth14_18+total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR+Y_1+Y_2+Y_3+Y_4


'

M3<-sem(F3, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M3)

sink("Results/Sleep on Mental Health and Gun Violence.txt")
print(summary(M3, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M3))
print(inspect(M3))
print(resid(M3, type = "cor"))
sink()
# Replications with High Gun Violence Indicator #### 
names(C)

quantile(C$total_GV14_18, .95)

C$High_Gun_90<-NA
C$High_Gun_90[C$total_GV14_18 > 3 & C$total_GV14_18 <= 5]<-1
C$High_Gun_90[C$total_GV14_18 <= 3| C$total_GV14_18 > 5]<-0
table(C$High_Gun_90)

C$High_Gun_95<-NA
C$High_Gun_95[C$total_GV14_18 > 5]<-1
C$High_Gun_95[C$total_GV14_18 <= 5]<-0
table(C$High_Gun_95)

table(C$High_Gun_90,C$High_Gun_95)

# High Gun Model 1: Mental Health and Sleep on Gun Violence ####
names(C)
F1<-'

mhlth15_19~total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+A_popdensityR+
           A_pct2personhhR+A_pct3personhhR+A_pct4personhhR+Y_1+Y_2+Y_3+Y_4+High_Gun_90+High_Gun_95


sleep15_19~total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR+Y_1+Y_2+Y_3+Y_4+High_Gun_90+High_Gun_95

mhlth15_19~~sleep15_19

'

M1<-sem(F1, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M1)

sink("Results/Mental Health and Sleep on Gun Violence (High Gun Violence Indicator).txt")
print(summary(M1, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M1))
print(inspect(M1))
print(resid(M1, type = "cor"))
sink()
# High Gun Model 2: Mental Health on Sleep and Gun Violence ####
names(C)
F2<-'

mhlth15_19~sleep14_18+total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+Y_1+Y_2+Y_3+Y_4+High_Gun_90+High_Gun_95+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR


'

M2<-sem(F2, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M2)

sink("Results/Mental Health on Sleep and Gun Violence (High Gun Violence Indicator).txt")
print(summary(M2, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M2))
print(inspect(M2))
print(resid(M2, type = "cor"))
sink()
# High Gun Model 3: Sleep on Mental Health and Gun Violence ####
names(C)
F3<-'

sleep15_19~mhlth14_18+total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+Y_1+Y_2+Y_3+Y_4+High_Gun_90+High_Gun_95+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR


'

M3<-sem(F3, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M3)

sink("Results/Sleep on Mental Health and Gun Violence (High Gun Violence Indicator).txt")
print(summary(M3, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M3))
print(inspect(M3))
print(resid(M3, type = "cor"))
sink()


#___________________________________________________________####
# Supplemental                                              ####
#___________________________________________________________####
# Mediation Model 1: gun violence > sleep > mental health ####
F1<-'

mhlth15_19~a*total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+Y_1+Y_2+Y_3+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR


sleep16_19~b*mhlth15_19+c*total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+Y_1+Y_2+Y_3+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR


IDE:=(a*b)
TE:=(a*b)+c

'

M1<-sem(F1, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M1)

sink("Results/Mediation Model 1 gun violence_sleep_mental health.txt")
print(summary(M1, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M1))
print(inspect(M1))
print(resid(M1, type = "cor"))
sink()

# Mediation Model 2: gun violence > mental health > sleep ####

F1<-'

sleep15_19~a*total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+Y_1+Y_2+Y_3+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR


mhlth16_19~b*sleep15_19+c*total_GV14_18+A_CDIS+A_livingaloneR+A_f_m_sexratio+
           A_yngmale1524R+
           miles_to_nearest_trauma+PctAge18_34R+PctAge35_64R+PctAge65_69R+
           PctAge70_74R+PctAge75_79R+PctAge80_84R+Y_1+Y_2+Y_3+
           A_popdensityR+A_pct2personhhR+A_pct3personhhR+A_pct4personhhR


IDE:=(a*b)
TE:=(a*b)+c

'

M1<-sem(F1, fixed.x = F, data=C, orthogonal = F, estimator = "ML", missing = "ML")
summary(M1)

sink("Results/Mediation Model 2 gun violence_mental health_sleep.txt")
print(summary(M1, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T))
print(partable(M1))
print(inspect(M1))
print(resid(M1, type = "cor"))
sink()
