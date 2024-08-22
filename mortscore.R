#mort.score

kwtot$M.VLDL.P = as.numeric(kwtot$M.VLDL.P)
table(kwtot$shift_sample)
sum(table(kwtot$nightshifts_monthly))

kw_ms = kwtot %>% 
  dplyr::select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
                Ile,Leu,Val,Phe,AcAce,Alb,Gp)

kw_ms = kwtot %>% filter(kwtot$shift_sample == 1) %>%
  dplyr::select(XXL.VLDL.L,VLDL.D,S.HDL.L,PUFA.FA,Glc,Lac,His,
         Ile,Leu,Val,Phe,AcAce,Alb,Gp)


names(kw_ms)[1:14] = c("xxl_vldl_l",'vldl_d','s_hdl_l','pufa_fa','glc','lac',
                       'his','ile','leu','val','phe','acace','alb','gp')
kw_ms = kw_ms[, c('pufa_fa','gp','glc','s_hdl_l',"xxl_vldl_l",'alb','phe','acace',
                  'ile', 'vldl_d','leu','val','his','lac')]    

prep_met_for_scores
x=comp.mort_score(kw_ms)
mean(x$mortScore, na.rm = TRUE)
max(x$mortScore, na.rm = TRUE)
sd(x$mortScore,na.rm = TRUE)
names(kw)
table(kw$n_years_nightshift)

m=lm(kwtot$mortScore_perc1~as.factor(kwtot$shift_sample))
m=multinom(as.factor(kwtot$mortScore_quart)~as.factor(kwtot$shift_sample))
m=polr(as.factor(kwtot$mortScore_quart) ~ kwtot$fever_dic, method = "logistic")
summary(m)
plot(m$coefficients[1:15]~nightshift_m) +
  lines(m$coefficients)
table(kw$nightshift)

summary(kw$nightshifts_monthly)
xx=0:100

dummy_names <- paste0("dummy", 1:372)
kw_ms1 = cbind(dummy_names,kw_ms)

#csv
write.csv(kw_ms1,"C:/Users/DErr001/OneDrive - Universiteit Utrecht/Documents/kw_ms1.csv", row.names=TRUE)

#uguali ns(xx,knots = c(25,50,75)) & ns(xx,df=4)

m1=lm(mortScore~bs(nightshifts_monthly,df=4),data = kw)
summary(m1)
kw_seq = na.omit(kw$nightshifts_monthly)
x_seq <- seq(min(kw_seq), max(kw_seq), length.out = 200)
y_pred = predict(m1, newdata = data.frame(nightshifts_monthly = x_seq))

plot(kw$nightshifts_monthly, kw$mortScore, main = "B-Spline Fit")
lines(x_seq, y_pred, col = 'red', lwd = 2)

confint(m1)




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
