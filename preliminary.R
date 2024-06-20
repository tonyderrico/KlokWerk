library(psych)
library(dplyr)
library(officer)
library(flextable)
library(magrittr)
library(tidyverse)
library(MiMIR)

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
table(kwtot$employment_status2)
kwtot$employment_status1 = NULL
