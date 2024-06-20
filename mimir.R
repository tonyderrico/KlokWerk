library(MiMIR)
library(dplyr)
#datasets from MIMIR
data("covid_betas")
data("CVD_score_betas")
data("metabolites_subsets")
data("synthetic_metabolic_dataset")
data("synthetic_phenotypic_dataset")
data("phenotypes_names")
data("mort_betas")
data('Ahola_Olli_betas')
data('acc_LOBOV')
data("PARAM_metaboAge")

#mort.score
kw$M.VLDL.P = as.numeric(kw$M.VLDL.P)
table(kw$nightshifts_monthly)

kw_ms = kw %>%
  select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
                      Ile,Leu,Val,Phe,AcAce,Alb,Gp)
names(kw_ms)[1:14] = c("xxl_vldl_l",'vldl_d','s_hdl_l','pufa_fa','glc','lac',
                       'his','ile','leu','val','phe','acace','alb','gp')
kw_ms = kw_ms[, c('pufa_fa','gp','glc','s_hdl_l',"xxl_vldl_l",'alb','phe','acace',
          'ile', 'vldl_d','leu','val','his','lac')]    

prep_met_for_scores
x=exp(comp.mort_score(kw_ms))
x1=comp.mort_score(kw_ms)
x1$mortScore
hist(x$mortScore)
mean(x1$mortScore, na.rm = TRUE)
max(x1$mortScore, na.rm = TRUE)
mean(x$mortScore,na.rm=TRUE)
sd(x$mortScore,na.rm = TRUE)

#risk of severe covid19
kw_cs = kw %>% filter(nightshift==2) %>%
  select(Gp,DHA,Crea,MUFA,ApoB.ApoA1,Tyr,
         Ile,SFA.FA,Glc,Lac,FAw6/FAw3,Phe,Serum.C,FAw6.FA,Ala,
         PUFA,glycine,His,PUFA.FA,Val,Leu,Alb,FAw3,LDL.C,Serum.TG)
kw$glycine = kw$Ala #b-alanine precursor of glycine?

names(kw_cs)[1:25] = covid_betas$Abbreviation

x = exp(comp_covid_score(kw_cs,betas = covid_betas))
(x$covidScore,na.rm = TRUE)

#t2diabetis score (=phe,l_vldl_ce_perc,l_hdl_fc,sex,age,BMI,glucose)
bmi = Baseline %>% select(subjectid,weight,height)
bmi$height = bmi$height*0.01
bmi$bmi = bmi$weight/(bmi$height*bmi$height)
kw = merge(kw,bmi,by = 'subjectid')
kw$sex = 0
names(kw)
names(kw_diab)

kw_diab = kw %>% filter(nightshift==2) %>%
  select(Phe,L.VLDL.CE_., L.HDL.FC,sex,age,bmi,Glc)
names(kw_diab)[1:7] = Ahola_Olli_betas$Abbreviation
kw_diab$sex = 0
kw_diab[1:3] = prep_met_for_scores(kw_diab[1:3],
                              plusone = TRUE, quiet = FALSE)
x=comp.T2D_Ahola_Olli(kw_diab[1:3],phen = kw_diab[4:7],
                        betas = Ahola_Olli_betas)
mean(x$T2Dscore,na.rm = TRUE)
write.csv(kw_phe,file='O:DGK/IRAS/EEPI/Projects/Shift Work Project/Tony-KW/datasets/kw_phe.csv', row.names=FALSE)
#CVD SCORE (see systolicbloodpressure)
MiMIR::startApp()
kw_met = kw_diab[1:4]
kw_phe = kw_diab[5:8]
