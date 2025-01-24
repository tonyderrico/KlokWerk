#descriptive
ngt = df_fin %>% filter(shift_dic == 'night') %>% select(age,edu, edu2,marital_status,bmimeasured, 
                                                         employment_status,country_birth,
                                                         sleephr,luxmean,working_hours_contract,
                                                         productive_working_hours,
                                                         MVPA,minutesLAN.y,alcohol24.y,totaalkcal,
                                                         chrono,alcohol24.x,chrono2, chrono,
                                                         mortScore,CVD_score,MetaboAge)
control = df_fin %>% filter(shift_dic == 'control')  %>%  select(age, edu,edu2,marital_status,bmimeasured, 
                                                          employment_status,country_birth,
                                                          sleephr,luxmean,working_hours_contract,
                                                          productive_working_hours,
                                                          MVPA,minutesLAN.y,alcohol24.y,totaalkcal,
                                                          chrono,alcohol24.x,chrono2,
                                                          mortScore,CVD_score,MetaboAge)

df_fin$
sapply(ngt,table)
table(ngt$alcohol24.y)
10/16
table(ngt$employment_status)
table(is.na(ngt$chrono))
summary(lm(ngt$MetaboAge ~ ngt$chrono))
a = total2 %>% filter(controle == '1')
  table(a$chrono)

sapply(control, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA)
sapply(ngt,table)
mean(control$totaalkcal, na.rm = T)
sd(control$totaalkcal, na.rm = T)
ngt$minutesLAN.y 
19+26
26/(19+26)
3/13
summary(lm(mortScore ~ shift_dic + age  + bmimeasured +
             employment_status + chrono + country_birth + winter, data = df_fin))

plot(lm(MetaboAge ~ shift_dic + age  + bmimeasured +
             employment_status + chrono + country_birth, data = df_fin))

m = lm(mortScore ~ shift_dic + age  + bmimeasured +
         employment_status + chrono + country_birth + winter, data = df_fin)
m = lm(CVD_score ~  shift_dic, data = df_fin)
Confint(m)
summary(m)
plot(m)


##### random forest

rf_model <- randomForest(
  CVD_score ~ shift_dic + age + bmimeasured + employment_status + chrono + country_birth, 
  data = df_fin,
  ntree = 200,           # Number of trees to grow
  mtry = 2,              # Number of variables randomly sampled as candidates at each split
  importance = TRUE,     # To compute variable importance
  na.action = NULL    # Remove NAs if any
)

plot(rf_model)

# 2️⃣ View the model summary
print(rf_model)

# 3️⃣ View variable importance
importance(rf_model)
varImpPlot(rf_model)

library(vip)
vip(rf_model, metric = "permute", nsim = 50, train = df_fin,
    target = df_fin$CVD_score)

df_fin$predicted <- predict(m, newdata = df_fin)
ggplot(df_fin, aes(x = predicted, y = MetaboAge)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted MetaboAge", y = "Observed MetaboAge") +
  theme_minimal()

plot(m$fitted.values, m$residuals)
abline(h = 0, col = "red", lty = 2)
calibration_model <- lm(MetaboAge ~ predicted, data = df_fin)
summary(calibration_model)
m_refit <- lm(MetaboAge ~ shift_dic + age  + bmimeasured + employment_status + chrono + country_birth, data = df_fin)
# Calibration regression
calibration_model <- lm(MetaboAge ~ predicted, data = df_fin)
# Get the new calibrated predictions
df_fin$calibrated_pred <- predict(calibration_model, newdata = df_fin)
df_fin$predicted - df_fin$calibrated_pred
ggplot(df_fin, aes(x = predicted, y = calibrated_pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted MetaboAge", y = "Observed MetaboAge") +
  theme_minimal()


range(df_fin$age, na.rm = T)
