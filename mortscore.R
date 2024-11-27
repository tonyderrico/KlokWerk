#mort.score


kw_ms = total %>% 
  dplyr::select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
         Ile,Leu,Val,Phe,AcAce,Alb,Gp)

names(kw_ms)[1:14] = c("xxl_vldl_l",'vldl_d','s_hdl_l','pufa_fa','glc','lac',
                       'his','ile','leu','val','phe','acace','alb','gp')
kw_ms = kw_ms[, c('pufa_fa','gp','glc','s_hdl_l',"xxl_vldl_l",'alb','phe','acace',
                  'ile', 'vldl_d','leu','val','his','lac')]    

prep_met_for_scores
x = comp.mort_score(kw_ms)
mean(x$mortScore, na.rm = TRUE)
max(x$mortScore, na.rm = TRUE)
sd(x$mortScore,na.rm = TRUE)

total = cbind(total,x)

total$controle = as.factor(total$controle)

x= total %>% dplyr::filter(controle==0)
mean(x$working_hours_contract, na.rm = T)
names(subtot_c)
m=glm(mortScore~MVPA, data=subtot_c)
m=multinom(as.factor(kwtot4$t2diab_quart)~as.factor(kwtot4$shift_sample.y))
m1=polr(as.factor(kwtot4$mortScore_quart1) ~ kwtot4$shift_sample.y, method = "logistic")
m =orm(mortScore_quart1 ~ shift_sample.y ,data = kwtot4) #ordinal regression models
m=rq(mortScore_orig~shift_sample.y,tau = c(0.1,0.25,0.5,0.75,0.9),data=kwtot4)
m=rq(mortScore_orig~shift_sample.y,tau = 0.5,data=kwtot4)
m=lmer(metaboage ~shift_sample.x + (1|chrono), data = kwtot4)
a = ranef(m)
dotplot(a)

kwtot4$mortScore_orig
exp(c(.10,0.06,.13))
summary(m)
confint(m)
ilogit(m1$zeta)

#check for heteroscedasticity
ols_test_breusch_pagan(m)
#calculation CI manual
0.41 - (1.96*0.22)
exp(-0.92)



#csv
write.csv(kw_ms1,"C:/Users/DErr001/OneDrive - Universiteit Utrecht/Documents/kw_ms1.csv", row.names=TRUE)


#mort.score with all measurements

kw$M.VLDL.P = as.numeric(kw$M.VLDL.P)
table(kw$nightshifts_monthly <= 4)
sum(table(kw$nightshifts_monthly))

kw_ms = bsmt %>% filter(night_shift_dic == 2) %>%
  select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
         Ile,Leu,Val,Phe,AcAce,Alb,Gp)
names(kw_ms)[1:14] = c("xxl_vldl_l",'vldl_d','s_hdl_l','pufa_fa','glc','lac',
                       'his','ile','leu','val','phe','acace','alb','gp')
kw_ms = kw_ms[, c('pufa_fa','gp','glc','s_hdl_l',"xxl_vldl_l",'alb','phe','acace',
                  'ile', 'vldl_d','leu','val','his','lac')]    

prep_met_for_scores
x=exp(comp.mort_score(kw_ms))
x1=comp.mort_score(kw_ms)
mean(x1$mortScore, na.rm = TRUE)
max(x1$mortScore, na.rm = TRUE)
mean(x$mortScore,na.rm=TRUE)
sd(x$mortScore,na.rm = TRUE)

table(shift_samples$shift_sample)

xx=kwtot %>% filter(mortScore_quart == 1)
mean(xx$productive_working_hours, na.rm = T)

#stratified regressions
m=glm(subtot_n$mortScore~subtot_n$sleephr)
summary(m)
