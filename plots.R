#filtering subject with repeats 
subject_counts <- kwtot4 %>%
  group_by(subjectid) %>%
  summarise(count = n()) %>%
  filter(count > 1) 
kwtot4_filtered <- kwtot4 %>%
  filter(subjectid %in% subject_counts$subjectid)

kwtot4group = kwtot4_filtered %>% select(subjectid,mortScore_exp,CVD_score,
                               metaboage,T2Dscore) %>%
                               group_by(subjectid) %>% group_split()

#------------------------------------------------------------------------------
#pdf

par(mfrow = c(2,2))


subtotplot = subtot %>% select(mortScore, MetaboAge,CVD_score,MVPA,totaalkcal,sleephr,shift_dic,
                               bmimeasured,age,luxmean)
names(subtot)


plots = lapply(varnames[1:7],function(variable) {
  ggplot(subtotplot, aes(x = get(variable), fill = factor(shift_dic))) +
    geom_density(alpha = 0.2) +
    labs(
      x = variable,
      y = "PDF",
      fill = '') 
  })



cvd_pl = ggplot(subtotplot, aes(x = CVD_score, fill = factor(shift_dic))) +
    geom_density(alpha = 0.2) +
    labs(x = 'CVD score',y = "",fill = '') + 
   scale_x_continuous(limits = c(5.5,23.5))
 
mort_score = ggplot(subtotplot, aes(x = mortScore, fill = factor(shift_dic))) +
   geom_density(alpha = 0.2) +
   labs(
     x = 'Mortality Score',
     y = "",
     fill = '') +
  scale_x_continuous(limits = c(-1.5,2))

metabage_plot= ggplot(subtotplot, aes(x = MetaboAge, fill = factor(shift_dic))) +
   geom_density(alpha = 0.2) +
   labs(
     x = 'Metabolic Age',
     y = "",
     fill = '') +
   scale_x_continuous(limits = c(28,88))
 
age_plot = ggplot(subtotplot, aes(x = age, fill = factor(shift_dic))) +
   geom_density(alpha = 0.2) +
   labs(
     x = 'Age',
     y = "",
     fill = '') +
   scale_x_continuous(limits = c(18,80))


MVPAplot = ggplot(subtotplot, aes(x = MVPA, fill = factor(shift_dic))) +
  geom_density(alpha = 0.2) +
  labs(
    x = 'MVPA',
    y = "",
    fill = '') +
  scale_x_continuous(limits = c(-20,140))

totkalplot = ggplot(subtotplot, aes(x =totaalkcal , fill = factor(shift_dic))) +
  geom_density(alpha = 0.2) +
  labs(
    x = 'Total Caloric Intake (mean/24 hr)',
    y = "",
    fill = '') +
  scale_x_continuous(limits = c(50,5000))

sleephrplot = ggplot(subtotplot, aes(x =sleephr , fill = factor(shift_dic))) +
  geom_density(alpha = 0.2) +
  labs(
    x = 'Sleep Hours',
    y = "",
    fill = '') +
  scale_x_continuous(limits = c(3,11))

bmiplot = ggplot(subtotplot, aes(x = bmimeasured , fill = factor(shift_dic))) +
  geom_density(alpha = 0.2) +
  labs(
    x = 'BMI',
    y = "",
    fill = '') +
  scale_x_continuous(limits = c(15,46))

luxplot = ggplot(subtotplot, aes(x = log2(luxmean), fill = factor(shift_dic))) +
  geom_density(alpha = 0.2) +
  labs(
    x = 'Daylight exposure',
    y = "",
    fill = '') +
  scale_x_continuous(limits = c(1,15))


ggobj <- ggarrange(mort_score,cvd_pl,metabage_plot,age_plot,totkalplot,bmiplot, MVPAplot, sleephrplot,luxplot, common.legend = TRUE,legend="right")


#percentage of chrono by shift
a = subtot %>% filter(shift_dic == 'control') %>% freqs(chrono)
subtot %>% filter(shift_dic == 'night') %>% freqs(koffie24.y)
b = subtot %>% filter(shift_dic == 'night') %>% freqs(chrono)
c = merge(a,b, by = intersect(names(a), names(b)), all = TRUE)
c = c[-c(11:12),]

chronoplot = ggplot(c, aes(x = chrono, y = p, fill = factor(shift_dic))) +
  geom_bar(stat = "identity", position = "dodge",alpha = 0.5)  +
  labs(
    x = 'Chronotype',
    y = "Percentage (%)",
    fill = '') 

#donuts plots

ggplot(a, aes(x = 2, y = p, fill = factor(chrono))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Donut Chart Example", fill = "Chronotype")

#check corr between scores

ggplot(kwtot4, aes(x=mortScore_exp, y=CVD_score)) +
  geom_point() + 
  geom_smooth(method=lm)

ggplot(kwtot2, aes(x=CVD_score, y=metabAge)) +
  geom_point() +
geom_smooth(method=lm)


ggplot(kwtot4_filtered, aes(x = subjectid, y = mortScore)) +
  geom_boxplot(alpha = 0.1) +
  labs(
    x = "workers with repeated measurements",
    y = "mortscore",
    fill= ''
  ) +
  theme_dark() +
  theme(
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank())  # Remove x-axis ticks
#quantile regress
qreg_25=rq(mortScore_orig~shift_sample.y,tau = c(0.25),data=kwtot4)
qreg_50=rq(mortScore_orig~shift_sample.y,tau = c(0.5),data=kwtot4)
qreg_75=rq(mortScore_orig~shift_sample.y,tau = c(0.75),data=kwtot4)
ols_model = lm(mortScore_orig~shift_sample.y,data=kwtot4)

ggplot(kwtot4[complete.cases(kwtot4$shift_sample.y),], aes(x = shift_sample.y, y = mortScore )) +
  geom_jitter(width = 0.2, color = 'black', alpha = 0.6) + 
  geom_abline(intercept = coef(qreg_25)[1], slope = coef(qreg_25)[2], color = "red", linetype = "dashed",linewidth = 1) +
  geom_abline(intercept = coef(qreg_50)[1], slope = coef(qreg_50)[2], color = "green", linetype = "solid",linewidth = 1) +
  geom_abline(intercept = coef(qreg_75)[1], slope = coef(qreg_75)[2], color = "purple", linetype = "dotted",linewidth = 1) +
  geom_abline(intercept = coef(ols_model)[1], slope = coef(ols_model)[2], color = "black", linetype = "twodash",linewidth = 1) +
  labs(subtitle = "Red: 25th percentile, Green: 50th percentile, Purple: 75th percentile, Black: OLS",
       x = "Nightshift",
       y = "Mortality Score") +
  theme_minimal()

#spline plots
#uguali ns(xx,knots = c(25,50,75)) & ns(xx,df=4)
kwtot2$total_years_working_irregular_shifts
m1=lm(kwtot2$CVD_score~bs(total_years_working_irregular_shifts,df=5) + age,data = kwtot2)
summary(m1)
kw_seq = na.omit(kwtot2$total_years_working_irregular_shifts)
x_seq <- seq(min(kw_seq), max(kw_seq), length.out = 200)
y_pred = predict(m1, newdata = data.frame(total_years_working_irregular_shifts = x_seq, age=0))

plot(kwtot2$total_years_working_irregular_shifts, kwtot2$CVD_score, main = "B-Spline Fit")
lines(x_seq, y_pred, col = 'red', lwd = 2)


# heatmaps
#example
names(corr.clust2)
subset_values <- as.data.frame(as.table(corr.clust2$correlations))
subset_values <- subset_values[subset_values$Freq < 1, ]
subset_values <- subset_values[order(subset_values$Freq), ]
subset_values <- subset_values[!duplicated(subset_values$Freq), ]

subset_valuespl = subset_values %>% filter(Var1 %in% c('mortScore','MetaboAge','CVD_score'))
subset_valuespl = subset_valuespl[-c(11,13,15),]
subset_valuespl = subset_valuespl[-c(24,12,18),]

subset_valuespl <- subset_valuespl[order(subset_valuespl$Var1, subset_valuespl$Var2), ]
subset_valuespl$Var1 <- factor(subset_valuespl$Var1, levels = c('mortScore','CVD_score','MetaboAge'))
subset_valuespl$Var2 =factor(subset_valuespl$Var2)
subset_valuespl$Var3 = subset_valuespl$Var1



ggplot(subset_valuespl, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_binned(limits = c(-1, 1)) +
  theme_minimal() +
  ggtitle("Heatmap: Risk Scores vs Exposures")

######
#scripts for boxplots
#a function to have three plots in the same figure?.



plot1 = ggbetweenstats(
  data = df_fin, 
  x = "shift_dic", 
  y = "mortScore", 
  plot = TRUE, 
  conf.level = 0.95,
  bf.message = FALSE,
  centrality.type = 'nonparametric') + 
  labs(
    x = "", 
    y = "Mortality Risk Score"
  ) + 
  scale_x_discrete(labels = c("Day Shift", "Night Shift")) 

plot2 = ggbetweenstats(
  data = df_fin, 
  x = "shift_dic", 
  y = "CVD_score", 
  plot = TRUE, 
  conf.level = 0.95,
  bf.message = FALSE,
  centrality.type = 'nonparametric'
) + 
  labs(
    x = "", 
    y = "CVD Risk Score"
  ) + 
  scale_x_discrete(labels = c("Day Shift", "Night Shift")) 

plot3 = ggbetweenstats(
  data = df_fin, 
  x = "shift_dic", 
  y = "MetaboAge", 
  plot = TRUE, 
  conf.level = 0.95,
  bf.message = FALSE,
  centrality.type = 'nonparametric'
) + 
  labs(
    x = "", 
    y = "Metabolic Age"
  ) + 
  scale_x_discrete(labels = c("Day Shift", "Night Shift"))



plot_grid(plot1, plot2, plot3, ncol = 3)

