#metaboAge
anyNA(kw_metage)
sum(is.na(kw_metage))

kw_metage = total %>% 
  dplyr::select(AcAce, Ace, Ala, Alb, ApoA1, ApoB,
                              Cit,Crea,DHA,FAw3,FAw3.FA,FAw6,
                              FAw6.FA,Glc,Gln,Gp,HDL2.C,HDL3.C,
                              HDL.C,HDL.D,His,IDL.C,IDL.L,Ile,
                              L.LDL.L,LA,Lac,LDL.C,LDL.D,Leu,
                              M.HDL.L,M.LDL.L,M.VLDL.L,MUFA,MUFA.FA,PC,
                              Phe,PUFA,PUFA.FA,S.HDL.L,S.LDL.L,S.VLDL.L,
                              Serum.C,Serum.TG,SFA,SFA.FA,SM, TotCho,
                              TotFA,TotPG,Tyr,UnSat,Val,VLDL.C,
                              VLDL.D,XS.VLDL.L)
names(kw_metage)[1:56] = c("acace", "ace", "ala", "alb", "apoa1", "apob",
                       "cit", "crea", "dha", "faw3", "faw3_fa", "faw6",
                       "faw6_fa", "glc", "gln", "gp", "hdl2_c", "hdl3_c",
                       "hdl_c", "hdl_d", "his", "idl_c", "idl_l", "ile",
                       "l_ldl_l", "la", "lac", "ldl_c", "ldl_d", "leu",
                       "m_hdl_l", "m_ldl_l", "m_vldl_l", "mufa", "mufa_fa", "pc",
                       "phe", "pufa", "pufa_fa", "s_hdl_l", "s_ldl_l", "s_vldl_l",
                       "serum_c", "serum_tg", "sfa", "sfa_fa", "sm", "totcho",
                       "totfa", "totpg", "tyr", "unsatdeg", "val", "vldl_c",
                       "vldl_d", "xs_vldl_l")

a=QCprep(kw_metage,PARAM_metaboAge)
metaboage = apply.fit(as.matrix(a),PARAM_metaboAge$FIT_COEF)
write_csv2(metaboage,'C:/Users/DErr001/OneDrive - Universiteit Utrecht/Desktop/metabage.csv')
total = cbind(total,metabage)

metabage$MetaboAge = as.numeric(substr(as.character(metabage$MetaboAge), 1, 2))


summary(m)
m=lm(kwtot4$metaboage~as.factor(kwtot4$shift_sample.y))
m=lm(metaboage~shift_sample.y +age, data = kwtot4)
m=glm(kwtot4$metaboage_dic~kwtot4$shift_sample.y +kwtot4$age, family = 'binomial')
m=lmer(metaboage~shift_sample.x + age + (1|subjectid), data = kwtot4)
m=multinom(as.factor(kwtot2$metabAge_quart)~as.factor(kwtot2$shift_sample.y))
m=polr(kwtot2$metabAge_quart1 ~ kwtot2$shift_sample.y, method = "logistic")
m=rq(metabAge~shift_sample.y, tau = c(0.25,0.5, 0.75),method='fn',data=kwtot2)
m=gcrq(metabAge_log~shift_sample.y, tau = seq(0.2,0.8,l=3),data=kwtot2)
kwtot4$shi
min(kwtot4$age, na.rm = T)
summary(m)
confint(m)
exp(c(0.09,-0.009,0.20))
cor(kwtot4$metaboage,kwtot4$age,use = "complete.obs")
#totalworkinghrs
m=lm(kwtot4$metaboage~ kwtot4$total_years_working_night_shifts)
m=glm(kwtot4$metaboage_dic~kwtot4$total_years_working_night_shifts + kwtot4$age, family = 'binomial')
m=rq(metaboage~total_years_working_night_shifts + age + bmi,tau = c(0.25, 0.5,0.75),method='fn',data=kwtot4)
summary(m)
confint(m)
exp(c(0.009767968, 0.06277961))
#statistics
check_heteroskedasticity(m)
emmeans(m1,
        metabAge_log~shift_sample.y),
        by= 'tau',
        tau=c(.25,.75),
        weights = 'prop')
#some calculations
x = kwtot4 %>% select(shift_sample.y, age, metaboage)
x1 = x %>% filter(shift_sample.y == 1)
mean(x1$metaboage, na.rm = T)

#correlations adjustement checking
hetcor(kwtot4$age,kwtot4$metaboage,use = 'complete.obs')
m=rq(kwtot4$mortScore_orig~ kwtot4$shift_sample.y + kwtot4$age + kwtot4$bmi + kwtot4$marital_status)
m=rq(mortScore_orig~shift_sample.y + age +bmi + marital_status,tau = c(0.25,0.5,0.75), method = 'fn', data=kwtot4)

x=car::vif(m)
x
summary(m)
exp(0.046)
