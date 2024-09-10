kwtot_plot = kwtot2[!is.na(kwtot2$shift_sample.y), ]

ggplot(kwtot2, aes(x = CVD_score , fill = factor(shift_sample.y))) +
    geom_density(alpha = 0.2) +
    labs(
      x = "CVD score",
      y = "PDF",
      fill= ''
    ) +
    theme_dark()

ggplot(kwtot4, aes(x=mortScore_exp, y=CVD_score)) +
  geom_point() + 
  geom_smooth(method=lm)

ggplot(kwtot2, aes(x=CVD_score, y=metabAge)) +
  geom_point() +
geom_smooth(method=lm)


ggplot(kwtot_plot, aes(x = workload_night, fill = factor(shift_sample.y))) +
  geom_density(alpha = 0.2) +
  labs(
    x = "",
    y = "PDF",
    fill= ''
  ) +
  theme_dark()

#quantile regress
qreg_25=rq(mortScore_orig~shift_sample.y,tau = c(0.25),data=kwtot4)
qreg_50=rq(mortScore_orig~shift_sample.y,tau = c(0.5),data=kwtot4)
qreg_75=rq(mortScore_orig~shift_sample.y,tau = c(0.75),data=kwtot4)
ols_model = lm(mortScore_orig~shift_sample.y,data=kwtot4)

ggplot(kwtot4[complete.cases(kwtot4$shift_sample.y),], aes(x = shift_sample.y, y = mortScore_orig )) +
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

