library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(lme4)
library(glmm)
library(lmerTest)
library(MuMIn)
library(bestNormalize)
library(MASS)
library(moments)
library(jtools)
library(devtools)
library(ggcorrplot)
library(sjPlot)
library(car)
library(jtools)
library(coefplot)

#############################################
#  OVERALL MARRIAGE PATTERNS (no specific age cohorts)
#############################################

#-------------------------------------------
# marriage rate
#-------------------------------------------

df1<- read.csv("overall_marriage_rate.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df1.1 <- df1[c(1:18)]
df1.1$country_code <- as.factor(df1.1$country_code)
df1.1$religion <- as.factor(df1.1$Main.religion)
df1.1$year <- as.factor(df1.1$year)
df1.1$Continent <- as.factor(df1.1$Continent)
df1.1_na <- na.omit(df1.1)
View(df1.1_na)
str(df1.1_na)

# 2.1.2 Data visualisation
hist(df1.1_na$men.marriage.rate,xlab ="marriage rate in men", main = NULL)
hist(df1.1_na$women.marriage.rate, xlab ="marriage rate in women", main = NULL)
hist(df1.1_na$ASR,xlab ="Adult sex ratio", main = NULL)

# data transform box cox 
bestNormalize::boxcox(df1.1_na$men.marriage.rate)
# lambda = 1.999958 

bestNormalize::boxcox(df1.1_na$women.marriage.rate)
# lambda = 1.999958 

bestNormalize::boxcox(df1.1_na$ASR)
# lambda = -0.9999576 

df1.1_na$ASR_bc <- (((df1.1_na$ASR)^-0.9999576)-1)/-0.9999576 
hist(df1.1_na$ASR_bc)

df1.1_na$MRM_bc <- (((df1.1_na$men.marriage.rate)^1.999958)-1)/1.999958 
hist(df1.1_na$MRM_bc)

df1.1_na$MRF_bc <- (((df1.1_na$women.marriage.rate)^1.999958 )-1)/1.999958 
hist(df1.1_na$MRF_bc)

## Female marriage rate-----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(MRF_bc ~ ASR_bc  + (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # full model
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(MRF_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude), # ** best model **
  
  # including year as a fixed effect (original model)
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) + 
         (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T)


## Male marriage rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(MRM_bc ~ ASR_bc  + (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # full model
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(MRM_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),# ** best model **
  
  # including year as a fixed effect (original model)
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) +
         (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df1.1_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T)


#------------------------------------------
# divorce rate
#-------------------------------------------

df2<- read.csv("overall_divorce_rate.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df2.1 <- df2[c(1:18)]
df2.1$country_code <- as.factor(df2.1$country_code)
df2.1$religion <- as.factor(df2.1$Main.religion)
df2.1$year <- as.factor(df2.1$year)
df2.1$Continent <- as.factor(df2.1$Continent)
df2.1_na <- na.omit(df2.1)

# Data visualisation
hist(df2.1_na$men.divorce.rate, xlab="divorce rate in men", main = NULL)
hist(log(df2.1_na$men.divorce.rate))
hist(df2.1_na$women.divorce.rate, xlab="divorce rate in women", main = NULL)
hist(log(df2.1_na$women.divorce.rate))
hist(df2.1_na$ASR)
hist(log(df2.1_na$ASR))

# data transform box cox 
bestNormalize::boxcox(df2.1_na$men.divorce.rate)
# lambda = 0.1789885

bestNormalize::boxcox(df2.1_na$women.divorce.rate)
# lambda = 0.321432

bestNormalize::boxcox(df2.1_na$ASR)
# lambda = -0.9999576 

df2.1_na$ASR_bc <- (((df2.1_na$ASR)^-0.9999576)-1)/-0.9999576 
hist(df2.1_na$ASR_bc)

df2.1_na$DRM_bc <- (((df2.1_na$men.divorce.rate)^0.1789885)-1)/0.1789885
hist(df2.1_na$DRM_bc)

df2.1_na$DRF_bc <- (((df2.1_na$women.divorce.rate)^0.321432)-1)/0.321432
hist(df2.1_na$DRF_bc)


## female divorce rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRF_bc ~ ASR_bc +(1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # full model
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(DRF_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),# ** best model **
  
  # including year as a fixed effect (original model)
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) +
         (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T)


## Male divorce rate  -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRM_bc ~ ASR_bc +(1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # full model
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(DRM_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),# ** best model **
  
  # including year as a fixed effect (original model)
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) +
         (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity")+
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df2.1_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T)


#############################################
#  15-49 AGE COHORT MARRIAGE PATTERNS
#############################################

#-------------------------------------------
# marriage rate
#-------------------------------------------

df3.1<- read.csv("MR(15-49)_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df3.2 <- df3.1[c(1:12,14:15,17)]
df3.2$country_code <- as.factor(df3.1$country_code)
df3.2$religion <- as.factor(df3.1$Main.religion)
df3.2$year <- as.factor(df3.1$year)
df3.2$Continent <- as.factor(df3.1$Continent)
df3.2_na <- na.omit(df3.2)

# Data visualisation
hist(df3.2_na$MR.men.15.49,xlab ="marriage rate in males (15-49 age cohort)", main = NULL)
hist(df3.2_na$MR.women.15.49, xlab ="marriage rate in females (15-49 age cohort)", main = NULL)
hist(df3.2_na$ASR.15.49,xlab ="Adult sex ratio (15-49 age cohort)", main = NULL)


# data transform box cox 
bc1 <- boxcox(df2.2_na$ASR.15.49)
# lambda = -0.9999576 

bc2 <- boxcox(df2.2_na$MR.men.15.49)
# lambda = 1.999941

bc3 <- boxcox(df2.2_na$MR.women.15.49)
# lambda = 1.524434

df3.2_na$ASR_bc <- (((df3.2_na$ASR.15.49)^-0.9999576)-1)/-0.9999576 
hist(df3.2_na$ASR_bc)

df3.2_na$MRM_bc <- (((df3.2_na$MR.men.15.49)^1.999941)-1)/1.999941
hist(df3.2_na$MRM_bc)

df3.2_na$MRF_bc <- (((df3.2_na$MR.women.15.49)^1.524434)-1)/1.524434
hist(df3.2_na$MRF_bc)

df3.2_na_asr_ON <- orderNorm(df3.2_na$ASR.15.49)
hist(df3.2_na_asr_ON$x.t)


## Female marriage rate 15-49-----------------------------------------------------


# Models
tab_model(
  
  # univariable model
  lmer(MRF_bc ~ ASR_bc + (1|Continent) +(1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # full model
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # minimal model
  lmer(MRF_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude), # ** best model **
  
  # including year as a fixed effect (original model)
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) + 
         (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T)

## Male marriage rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(MRM_bc ~ ASR_bc + (1|Continent) +(1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # full model
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # minimal model
  lmer(MRM_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),# ** best model **
  
  # including year as a fixed effect (original model)
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) +
         (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.2_na, na.action = na.exclude), 
  
  show.aic = T,
  collapse.ci = T)


#-------------------------------------------
# divorce rate
#-------------------------------------------

df3.3<- read.csv("DR(15-49)_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df3.4 <- df3.3[c(1:12,14:15,17)]
df3.4$country_code <- as.factor(df3.4$country_code)
df3.4$religion <- as.factor(df3.4$Main.religion)
df3.4$year <- as.factor(df3.4$year)
df3.4$Continent <- as.factor(df3.4$Continent)
df3.4_na <- na.omit(df3.4)

# Data visualisation
hist(df3.4_na$DR.men.15.49, xlab="divorce rate in males (15-49 age cohort)", main = NULL)
hist(df3.4_na$DR.women.15.49, xlab="divorce rate in females (15-49 age cohort)", main = NULL)
hist(df3.4_na$ASR.15.49)

# data transform box cox 
bestNormalize::boxcox(df3.4_na$DR.men.15.49)
# lambda = 0.1778075

bestNormalize::boxcox(df3.4_na$DR.women.15.49)
# lambda = 0.332582

bestNormalize::boxcox(df3.4_na$ASR.15.49)
# lambda = -0.9999576 

df3.4_na$ASR_bc <- (((df3.4_na$ASR)^-0.9999576)-1)/-0.9999576 
hist(df3.4_na$ASR_bc)

df3.4_na$DRM_bc <- (((df3.4_na$DR.men.15.49)^0.1778075)-1)/0.1778075
hist(df3.4_na$DRM_bc)

df3.4_na$DRF_bc <- (((df3.4_na$DR.women.15.49)^0.332582)-1)/0.332582
hist(df3.4_na$DRF_bc)

## Female divorce rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRF_bc ~ ASR_bc +  (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # full model
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # minimal model
  lmer(DRF_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude), # ** best model **
  
  # including year as a fixed effect (original model)
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) + 
         (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full +  relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T)

## Male divorce rate -----------------------------------------------------

# Models
tab_model(
  
  # univariable model
  lmer(DRM_bc ~ ASR_bc +  (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # full model
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # minimal model
  lmer(DRM_bc ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude), # ** best model **
  
  # including year as a fixed effect (original model)
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) + 
         (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year))) +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.4_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T)

#############################################
# FAMILY STRUCTURES
#############################################

df3<- read.csv("family_structure_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df3$relative_parental_care_1 <- df3$relative_parental_care*(-1)
df3.1 <- df3[c(1:9,10:12,16:25,27:28,30,31)]
View(df3.1)
df3.1$country_code <- as.factor(df3.1$country_code)
df3.1$religion <- as.factor(df3.1$Main.religion)
df3.1$Continent <- as.factor(df3.1$Continent)
df3.1$year <- as.factor(df3.1$year)
df3.1_na <- na.omit(df3.1)

# data visulisation
hist(df3.1_na$ASR.15.49)
hist(df3.1_na$Single.mother.with.children)
hist(df3.1_na$Single.father.with.children)


bc1 <- boxcox(df3.1_na$ASR.15.49)
bc1
#lambda = -0.9999576 

df3.1_na$ASR_bc_1 <- (((df3.1_na$ASR.15.49)^-0.9999576)-1)/-0.9999576 
hist(df3.1_na$ASR_bc_1)
shapiro.test(df3.1_na$ASR_bc_1)


# share of single-mother family model selection
tab_model(
  
  # univariable model
  lmer(Single.mother.with.children ~ ASR_bc_1 + (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # full model
  lmer(Single.mother.with.children ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(Single.mother.with.children  ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(Single.mother.with.children ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude), # ** best model **
  
  # including year as a fixed effect (original model)
  lmer(Single.mother.with.children  ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) +
         (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(Single.mother.with.children  ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year)))  +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T)

# share of single-father family model selection
tab_model(
  
  # univariable model
  lmer(Single.father.with.children ~ ASR_bc_1 + (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # full model
  lmer(Single.father.with.children ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(Single.father.with.children  ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # including continent as a random effect
  lmer(Single.father.with.children ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude), # ** best model **
  
  # including year as a fixed effect (original model)
  lmer(Single.father.with.children  ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         scale(as.numeric(as.character(year))) +
         (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  # including year as a fixed effect + continent as random effect
  lmer(Single.father.with.children  ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(year)))  +
         (1|Continent) + (1|country_code),
       REML = TRUE, data = df3.1_na, na.action = na.exclude),
  
  show.aic = T,
  collapse.ci = T)


#############################################
# REPRODUCTIVE TIMING
#############################################

df4<- read.csv("Mean age of women at childbirth_full.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df4.1 <- df4[c(1:16,18,20:21)]
df4.1$country_code <- as.factor(df4.1$country_code)
df4.1$religion <- as.factor(df4.1$Main.religion)
df4.1$Year <- as.factor(df4.1$Year)
df4.1$Continent<- as.factor(df4.1$Continent)
df4.1_na <- na.omit(df4.1)

# histogram
hist(df4.1_na$age_of_women_at_childbirth, xlab = "average age of women at chilbirth", main = NULL)
hist(df4.1_na$ASR.15.49.)


# Models
tab_model(
  
  # univariaBLE model
  lmer(scale(age_of_women_at_childbirth) ~ scale(ASR.15.49.) + (1|Continent) + (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude),
  
  # full model
  lmer(scale(age_of_women_at_childbirth) ~ scale(ASR.15.49.) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude),
  
  # minimal model
  lmer(scale(age_of_women_at_childbirth) ~ scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") + Continent +
         (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude),
  
  # including year as a fixed effect (original model)
  lmer(scale(age_of_women_at_childbirth) ~ scale(ASR.15.49.) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
         scale(as.numeric(as.character(Year)))  + (1|Continent) +
         (1|country_code),
       REML = TRUE, data = df4.1_na, na.action = na.exclude), # ** best model **
  
  show.aic = T,
  collapse.ci = T)


#############################################
# FIGURES
#############################################

## Male marriage rate -----------

# univariable model
mod1.1 <- lmer(MRM_bc ~ ASR_bc  + (1|Continent) + (1|country_code), REML = TRUE, data = df1.1_na, na.action = na.exclude)
summ(mod1.1)

# best full model
mod1.2 <-lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),
              REML = TRUE, data = df1.1_na, na.action = na.exclude)
summ(mod1.2)
coefplot(mod1.2,intercept = FALSE)


multiplot(mod1.1, mod1.2, intercept=FALSE,
          title = "Marriage rate in males",
          xlab = "Estimates",
          ylab = NULL,
          dodgeHeight = 2)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())

fig1.1 <- plot_model(mod1.2,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","ASR"),
                     title = "(a) Marriage rate in males")+ylim(-.1, .1)
fig1.1

plot_summs(mod1.2)

## Female marriage rate-----

# univariable model
mod1.3 <- lmer(MRF_bc ~ ASR_bc   +(1|Continent) + (1|country_code),REML = TRUE, data = df1.1_na, na.action = na.exclude)
plot_model(mod1.1,show.value=TRUE)

# best full model
mod1.4 <- lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                 (1|Continent) + (1|country_code),
               REML = TRUE, data = df1.1_na, na.action = na.exclude)
summary(mod1.4)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig1.2 <- plot_model(mod1.4,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","ASR"),
                     title = "(b) Marriage rate in females")+ylim(-.1, .1)
fig1.2

plot_summs(mod1.3,mod1.4)

## Male divorce rate -----------

# univariable model
mod2.1 <-lmer(DRM_bc ~ ASR_bc +(1|Continent) + (1|country_code), REML = TRUE, data = df2.1_na, na.action = na.exclude)

# best full model
mod2.2 <- lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                 (1|Continent) + (1|country_code),
               REML = TRUE, data = df2.1_na, na.action = na.exclude)
summary(mod2.2)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig2.1 <- plot_model(mod2.2,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","ASR"),
                     title = "(c) Divorce rate in males")
fig2.1

## female divorce rate -----------

# univariable model
mod2.3 <-lmer(DRF_bc ~ ASR_bc +(1|Continent) + (1|country_code), REML = TRUE, data = df2.1_na, na.action = na.exclude)

# best full model
mod2.4 <- lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                 (1|Continent) + (1|country_code),
               REML = TRUE, data = df2.1_na, na.action = na.exclude)
summary(mod2.4)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig2.2 <- plot_model(mod2.4,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","ASR"),
                     title = "(d) Divorce rate in females")
fig2.2


#  15-49 AGE COHORT MARRIAGE PATTERNS

# Marriage rate_Male

# univariable model
mod3.1 <- lmer(MRM_bc ~ ASR_bc + (1|Continent) +(1|country_code), REML = TRUE, data = df3.2_na, na.action = na.exclude)

# best full model
mod3.2 <-lmer(MRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),REML = TRUE, data = df3.2_na, na.action = na.exclude)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig3.1 <- plot_model(mod3.2,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(a) Cohort-based marriage rate in males")+ylim(-.2, .2)
fig3.1

# Marriage rate_female

# univariable model
mod3.3 <- lmer(MRF_bc ~ ASR_bc + (1|Continent) +(1|country_code), REML = TRUE, data = df3.2_na, na.action = na.exclude)

# best full model
mod3.4 <-lmer(MRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),REML = TRUE, data = df3.2_na, na.action = na.exclude)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig3.2 <- plot_model(mod3.4,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(b) Cohort-based marriage rate in females")+ylim(-.2, .2)
fig3.2


# Divorce rate_male

# univariable model
mod4.1 <- lmer(DRM_bc ~ ASR_bc +  (1|Continent) + (1|country_code),REML = TRUE, data = df3.4_na, na.action = na.exclude)

# best full model
mod4.2 <-lmer(DRM_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),REML = TRUE, data = df3.4_na, na.action = na.exclude)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig4.1 <- plot_model(mod4.2,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(c) Cohort-based divorce rate in males") + ylim(-1.5, 1.5)
fig4.1

# Divorce rate_female

# univariable model
mod4.3 <- lmer(DRF_bc ~ ASR_bc +  (1|Continent) + (1|country_code),REML = TRUE, data = df3.4_na, na.action = na.exclude)

# best full model
mod4.4 <-lmer(DRF_bc ~ ASR_bc + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),REML = TRUE, data = df3.4_na, na.action = na.exclude)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig4.2 <- plot_model(mod4.4,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(d) Cohort-based divorce rate in females")+ylim(-1.5, 1.5)
fig4.2


# family structure_single-father

# univariable model
mod5.1 <-lmer(Single.father.with.children ~ ASR_bc_1 + (1|Continent) + (1|country_code), REML = TRUE, data = df3.1_na, na.action = na.exclude)

# best full model
mod5.2 <-lmer(Single.father.with.children ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),
              REML = TRUE, data = df3.1_na, na.action = na.exclude)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig5.1 <- plot_model(mod5.2,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Folk Religions","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(a) Share of single-father families") + ylim(-2.1, 2.1)

fig5.1


# family structure_single-father

# univariable model
mod5.3 <-lmer(Single.mother.with.children ~ ASR_bc_1 + (1|Continent) + (1|country_code),REML = TRUE, data = df3.1_na, na.action = na.exclude)

# best full model
mod5.4 <-lmer(Single.mother.with.children ~ ASR_bc_1 + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                (1|Continent) + (1|country_code),
              REML = TRUE, data = df3.1_na, na.action = na.exclude)

set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig5.2 <- plot_model(mod5.4,show.value=TRUE,
                     axis.labels = c("Unaffiliated religions","Judaism","Islam","Hinduism","Folk Religions","Buddhism","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(b) Share of single-mother families") 

fig5.2


# age of women at childbirth

# univariable model
mod6.1 <- lmer(scale(age_of_women_at_childbirth) ~ scale(ASR.15.49.) + (1|Continent) + (1|country_code), REML = TRUE, data = df4.1_na, na.action = na.exclude)

# best full model
mod6.2 <- lmer(scale(age_of_women_at_childbirth) ~ scale(ASR.15.49.) + scale(GDP_per_capita) + mean_years_of_schooling + unemployment_rate_full + median.age_full + relevel(religion, ref="Christianity") +
                 scale(as.numeric(as.character(Year)))  + (1|Continent) +(1|country_code), REML = TRUE, data = df4.1_na, na.action = na.exclude)
summary(mod6.2)
set_theme(axis.textsize = 1.1, 
          axis.title.size =1.2,
          title.size =1.5,
          base = theme_bw())
fig6.1 <- plot_model(mod6.2,show.value=TRUE,
                     axis.labels = c("Year","Unaffiliated religions","Judaism","Islam","Median age","Female to male unemployment rate","Mean years of schooling","GDP per capita","Cohort-based ASR"),
                     title = "(c) Average age of women at childbirth") 

fig6.1

