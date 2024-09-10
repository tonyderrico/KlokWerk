
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

#risk of severe covid19
kw$glycine = kw$Ala #b-alanine precursor of glycine?

kw_cs = kw %>% filter(nightshifts_monthly>4) %>%
  select(Gp,DHA,Crea,MUFA,ApoB.ApoA1,Tyr,
         Ile,SFA.FA,Glc,Lac,FAw6/FAw3,Phe,Serum.C,FAw6.FA,Ala,
         PUFA,glycine,His,PUFA.FA,Val,Leu,Alb,FAw3,LDL.C,Serum.TG)

names(kw_cs)[1:25] = covid_betas$Abbreviation

x = exp(comp_covid_score(kw_cs,betas = covid_betas))
x1=mean(x$covidScore,na.rm = TRUE)
x2=mean(x$covidScore, na.rm = TRUE)
x2/x1

dummy_names <- paste0("dummy", 1:94)
kw_cs1 = cbind(dummy_names,kw_cs)

#csv
write.csv(kw_cs1,"C:/Users/DErr001/OneDrive - Universiteit Utrecht/Documents/kw_cs.csv", row.names=TRUE)

#t2diabetis score (=phe,l_vldl_ce_perc,l_hdl_fc,sex,age,BMI,glucose)
bmi = Baseline %>% select(subjectid,weight,height)
bmi$height = bmi$height*0.01
bmi$bmi = bmi$weight/(bmi$height*bmi$height)
kw = merge(kw,bmi,by = 'subjectid')
kw$sex = 0
names(kw)
names(kw_diab)

kw_diab = kw %>% filter(nightshifts_monthly>4) %>%
  select(Phe,L.VLDL.CE_., L.HDL.FC,sex,age,bmi,Glc)
names(kw_diab)[1:7] = Ahola_Olli_betas$Abbreviation
kw_diab$sex = 0
kw_diab[1:3] = prep_met_for_scores(kw_diab[1:3],
                              plusone = TRUE, quiet = FALSE)
x=comp.T2D_Ahola_Olli(kw_diab[1:3],phen = kw_diab[4:7],
                        betas = Ahola_Olli_betas)
x1=mean(x$T2Dscore, na.rm = TRUE)
x2=mean(x$T2Dscore,na.rm = TRUE)
x2/x1
write.csv(kw_phe,file='O:DGK/IRAS/EEPI/Projects/Shift Work Project/Tony-KW/datasets/kw_phe.csv', row.names=TRUE)
sd(x$T2Dscore, na.rm = T)

#CVD SCORE (see systolicbloodpressure)

#shinyapp
MiMIR::startApp()
kw_met = kw_diab[1:4]
kw_phe = kw_diab[5:8]

table(kw$nightshifts_monthly)

