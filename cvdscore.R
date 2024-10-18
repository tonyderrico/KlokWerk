#CVD score

bloodpressure_kw1 = bloodpressure_kw %>% group_by(subjectid) %>% 
  summarise(
    sbp_1_avg=mean(sbp_1,na.rm=T), 
    sbp_2_avg=mean(sbp_2,na.rm=T),
    sbp_3_avg=mean(sbp_3,na.rm=T))

bloodpressure_kw1 = bloodpressure_kw1 %>% group_by(subjectid) %>%
  mutate(sbp_tot = mean(c(sbp_1_avg, sbp_2_avg, sbp_3_avg), na.rm = TRUE))

write_csv(bloodpressure_kw,'C:/Users/DErr001/OneDrive - Universiteit Utrecht/Desktop/sbp.csv')
write_csv(total,'C:/Users/DErr001/OneDrive - Universiteit Utrecht/Desktop/total.csv')

kw_cvd_met = total1 %>%
  dplyr::select(Phe, MUFA.FA, FAw6,DHA)

kw_cvd_pheno = total1 %>%
  dplyr::select(sex, sbp_1, smoking,
                diabetes, blood_pressure_lowering_med,lipidmed,
                TotCho,HDL.C)
kw_cvd = data.frame(kw_cvd_met,kw_cvd_pheno)

names(kw_cvd_met)[1:4] = c("phe", "mufa_fa", "faw6", "dha")
names(kw_cvd_pheno)[1:8] = c("sex", "systolic_blood_pressure", "current_smoking", "diabetes",
                             'blood_pressure_lowering_med',"lipidmed","totchol",'hdlchol')
cvdscore_off=comp.CVD_score(kw_cvd_met,phen=kw_cvd_pheno, betas = CVD_score_betas)
total1 = cbind(total1,cvdscore_off)

#quartile variable
kwtot4$CVDscore_quart = cut(kwtot4$CVD_score, 
                            breaks = quantile(kwtot4$CVD_score, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = FALSE)
#some models
m = lm(total1$CVD_score~total1$controle)
m=glm(kwtot4$CVD_score_dic~kwtot4$shift_sample.y, family = 'binomial')
m=multinom(as.factor(kwtot4$CVDscore_quart)~as.factor(kwtot4$shift_sample.y))
m=polr(as.factor(kwtot4$CVDscore_quart) ~ kwtot4$shift_sample.y, method = "logistic")
m=rq(as.numeric(CVD_score)~shift_sample.y,tau = c(0.25,0.5,0.75),data=kwtot4)

m=lm(as.numeric(kwtot4$CVD_score) ~ ns(kwtot4$total_years_working_night_shifts, df = 4), data = kwtot2)
summary(m)
confint(m)
exp(c(0.53100 , -0.609980.87051))
#statistics
check_distribution(m)
x=na.omit(kwtot2$CVD_score)
descdist(x)
f=fitdist(x,'norm')
plot(f)
x=as.numeric(x)
summary.rq(m)
class(m)
summary(m) %>%
  plot()
table(kwtot2$CVDscore_quart,kwtot2$shift_sample.y)

write.csv(kw_cvd_pheno,'C:/Users/DErr001/OneDrive - Universiteit Utrecht/Desktop/kw_cvd_pheno.csv')


