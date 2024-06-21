#mortality score
data("mort_betas")

table(kwtot$quality_sleep_between_2nightshifts_cat)
median(kwtot$quality_sleep_between_2nightshifts_cat, na.rm = TRUE)
hist(kwtot$n_years_nightshift)

kw_ms = kwtot %>% filter(quality_sleep_between_2nightshifts_cat>3) %>%
  select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
         Ile,Leu,Val,Phe,AcAce,Alb,Gp)
names(kw_ms)[1:14] = c("xxl_vldl_l",'vldl_d','s_hdl_l','pufa_fa','glc','lac',
                       'his','ile','leu','val','phe','acace','alb','gp')
kw_ms = kw_ms[, c('pufa_fa','gp','glc','s_hdl_l',"xxl_vldl_l",'alb','phe','acace',
                  'ile', 'vldl_d','leu','val','his','lac')]    

x=exp(comp.mort_score(kw_ms))
mean(x$mortScore,na.rm=TRUE)
sd(x$mortScore,na.rm = TRUE)

