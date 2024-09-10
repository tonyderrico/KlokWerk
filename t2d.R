#diabetis score

Ahola_Olli_betas[1:3]
t2dscore_met = kwtot3 %>% dplyr::select(Phe,L.VLDL.CE_., L.HDL.FC)
names(t2dscore_met)[1:3] = c('phe','l_vldl_ce_percentage','l_hdl_fc')
t2dscore_pheno = kwtot3 %>% dplyr::select(sex,age,bmi,Glc)
names(t2dscore_pheno)[1:4] = c('sex','age','BMI','glucose')
comp.T2D_Ahola_Olli(t2dscore_met,phen = t2dscore_pheno,
                    betas = Ahola_Olli_betas)
t2dscore = comp.T2D_Ahola_Olli(t2dscore_met,phen = t2dscore_pheno,
                               betas = Ahola_Olli_betas)

hist(kwtot3$T2Dscore)
m=lm(kwtot4$T2Dscore~ kwtot4$shift_sample.y)
m=glm(kwtot4$T2Dscore_dic~kwtot4$shift_sample.y, family = 'binomial')
m=multinom(as.factor(kwtot4$t2diab_quart)~as.factor(kwtot4$shift_sample.y))
m=rq(T2Dscore~shift_sample.y,tau = c(0.25,0.5,0.75),data=kwtot4)
m=polr(as.factor(kwtot4$t2diab_quart) ~ kwtot4$shift_sample.y, method = "logistic")

summary(m)
#totalworkinghrs
m=lm(kwtot4$T2Dscore~ kwtot4$total_years_working_night_shifts)
m=glm(kwtot4$T2Dscore_dic~kwtot4$total_years_working_night_shifts, family = 'binomial')
m=rq(T2Dscore~total_years_working_night_shifts,tau = c(0.25, 0.5,0.75),method='fn',data=kwtot4)

kwtot4$T2Dscore
summary(m)
confint(m)
exp(c(0.11105, 0.08435, 0.14524,0.13095, 0.07947, 0.15625,0.12437, 0.08446 , 0.14212  ))
exp(c(0.04155596,0.1059831))
cor(kwtot4$T2Dscore,kwtot4$CVD_score,use = 'complete.obs')

