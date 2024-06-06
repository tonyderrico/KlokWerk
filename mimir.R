library(MiMIR)
library(dplyr)
#datasets from MIMIR
data("covid_betas")
data("CVD_score_betas")
data("metabolites_subsets")
data("mort_betas")
data("PARAM_metaboAge")
mort_betas$Abbreviation
#mort.score
kw$M.VLDL.P = as.numeric(kw$M.VLDL.P)
table(kw$age)
#filter(age<59) nightshift==1  
kw_ms = kw %>%
  select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
                      Ile,Leu,Val,Phe,AcAce,Alb,Gp)
names(kw_ms)[1:14] = c("sex","xxl_vldl_l",'vldl_d','s_hdl_l','pufa_fa','glc','lac',
                       'his','ile','leu','val','phe','acace','alb','gp')
kw_ms = kw_ms[ , c('sex','pufa_fa','gp','glc','s_hdl_l',"xxl_vldl_l",'alb','phe','acace',
          'ile', 'vldl_d','leu','val','his','lac')]    

prep_met_for_scores
x=exp(comp.mort_score(kw_ms))
table(x>1.1)
