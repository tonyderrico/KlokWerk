library(psych)
library(dplyr)
library(officer)
library('flextable')
library(magrittr)
library(tidyverse)
library(MiMIR)
library(nnet)
library(MASS)
library(splines) 
library(lme4)
library("rms")
library('VGAM')
library(ggplot2)
library(quantreg)
library(emmeans)
library('performance')
library('quantregGrowth')
library(logspline)
library("fitdistrplus")
library(corrplot)
library(polycor)
library('faraway')
library(conflicted)
library('ggcorrplot')
library('olsrr')
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)

MiMIR::startApp()

#logtrasform and scale 

names(log_scaled_metab)

log_scaled_metab = metabolieten

log_scaled_metab <- log_scaled_metab %>%
  mutate(across(where(is.character), ~ na_if(., "NDEF")))

log_scaled_metab <- log_scaled_metab %>%
  mutate(across(where(is.character), as.numeric))

log_scaled_metab = log_scaled_metab[4:228] + 1

log_scaled_metab = as.data.frame(scale(log_scaled_metab))

info = metabolieten[1:3]

log_scaled_metab = data.frame(info, log_scaled_metab)

#logtrasform and scale kw dataset
names(kw)
kw_log_scaled = kw

kw_log_scaled[15:258] <- kw_log_scaled[15:258] %>%
  mutate(across(where(is.character), as.numeric))

kw_log_scaled[15:258] = kw_log_scaled[15:258] + 1

kw_log_scaled[15:258] = scale(kw_log_scaled[15:258])

kwtot = merge(Baseline,kw,by='subjectid')
kwtot$employment_status = ifelse(is.na(kwtot$employment_status),
                                 kwtot$employment_status1,
                                 kwtot$employment_status)
table(kwtot$employment_status)
kwtot$employment_status1 = NULL



#merging datasets

kw = cbind(checkshiftsample_kw$sampleid,kw)
kwtot= cbind(checkshiftsample_kwtot$sampleid, kwtot)
names(todefine_nightshiftdic)[1] = 'sample_id'

kwtot2 = merge(kwtot,todefine_nightshiftdic, by='sample_id')
kwtot3=kwtot2
duplicated(kwtot2$sample_id)
x=kwtot2 %>% filter(duplicated(sample_id))
x$sample_id
kwtot2= kwtot2 %>% distinct((sample_id), .keep_all = T)
table(kwtot2$shift_sample.y)
table(kwtot$shift_sample)

kwtot2 = kwtot2%>%
  mutate(mortScore_exp = ifelse(mortScore_exp > 3, NA, mortScore_exp))
kwtot2 = cbind(kwtot2,metabage)
names(kwtot2)[529] = 'metabAge'
kwtot2 = kwtot2%>%
  mutate(metabAge = ifelse(metabAge >= 80, NA, metabAge))
kwtot2 = cbind(kwtot2,metabage)

#dataset for PS
df = kwtot %>% dplyr::select(subjectid,sampleid, employment_status,total_years_working_night_shifts,
                      working_hours_contract)
df=semi_join(todefine_nightshiftdic,kwtot, by='subjectid')
df = df %>%
  distinct(subjectid, .keep_all = TRUE)
sum(is.na(df))

kwtot$sampleid

#percentile and quartile mortscore
kwtot$mort
names(kwtot)
kwtot=cbind(x$mortScore,kwtot)
kwtot <- kwtot %>%
  rename(mortScore_exp = expmortScore)

kwtot$mortScore_perc = cut(kwtot$mortScore_orig, 
                           breaks = quantile(kwtot$mortScore_orig, probs = seq(0, 1, by = 0.01), na.rm = TRUE),
                           include.lowest = TRUE,
                           labels = FALSE)
kwtot$mortScore_quart = cut(kwtot$mortScore_orig, 
                           breaks = quantile(kwtot$mortScore_orig, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                           include.lowest = TRUE,
                           labels = FALSE)
kwtot$mortScore_quart1 = factor(kwtot$mortScore_quart, ordered = T)

kwtot4$t2diab_quart = cut(kwtot4$T2Dscore, 
                            breaks = quantile(kwtot4$T2Dscore, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = FALSE)
#quartile metabAge
kwtot2$metabAge_quart = cut(kwtot2$metabAge, 
                            breaks = quantile(kwtot2$metabAge, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = FALSE)
kwtot2$metabAge_quart1 = factor(kwtot2$metabAge_quart, ordered = T)
kwtot2$metabAge_log = log2(kwtot2$metabAge)

#average within subject
x = kwtot2 %>%
  group_by(subjectid.x) %>%
  summarise(metabAge_mean = mean(metabAge, na.rm = TRUE))
kwtot2 = merge(kwtot2,x,by ='subjectid.x')

#dicot mortality
kwtot4$mortScore_dic = ifelse(kwtot4$mortScore_orig > -0.03238053, 1,0)
kwtot4$CVD_score_dic = ifelse(kwtot4$CVD_score > 13.76196, 1,0)
kwtot4$T2Dscore_dic = ifelse(kwtot4$T2Dscore > 9.0336, 1,0)
kwtot4$metaboage_dic = ifelse(kwtot4$metaboage> 59.55, 1,0)

median(kwtot4$metaboage, na.rm = T)

#replace values
x = kwtot4 %>%
  dplyr::mutate(mortScore_orig = replace(mortScore_orig, mortScore_orig > 1.56, 0))