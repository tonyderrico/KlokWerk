#stratified analysis
#1068,1087 repeated
total = total[-c(186:187),]

rownames(total) = NULL
#merging
total1 = left_join(total1,sbp, by='sampleid')

kwtot5 = kwtot4
names(kwtot5)[names(kwtot5)=='sample_id']='sampleid'
kwtot5 = kwtot5[1:236]
total2 = right_join(kwtot5,total1, by='sampleid')

write_csv(other_measured_factors,'C:/Users/DErr001/OneDrive - Universiteit Utrecht/Desktop/OMF.csv')

#merge unique sampleid
df_fin = right_join(total2,todefine_nightshiftdic_17_10, by = 'sampleid')
table(df_fin$controle.y,df_fin$controle.x.x)
names(df_fin)[595]='shift_dic'
df_fin$winter = as.factor(df_fin$winter)
df_fin$MetaboAge[df_fin$MetaboAge > 87] = NA
df_fin$chrono[df_fin$chrono==8]=4
#datasets tests
names(df_fin)
subtot = df_fin %>% select(sampleid,shift_dic,age,bmimeasured,sleephr,luxmean,
                         MVPA,minutesLAN.y,alcohol24.y,totaalkcal,
                         chrono,
                         mortScore,CVD_score,MetaboAge
                         )
sapply(subtot,class)
subtot$chrono = as.factor(subtot$chrono)

subtot_c = subtot %>% filter(shift_dic == 'control')
subtot_n = subtot %>% filter(shift_dic == 'night')

find_mode(subtot_c$mortScore)
find_mode(subtot_n$mortScore)

median(subtot_c$mortScore, na.rm = T)
median(subtot_n$mortScore, na.rm = T)
#preliminary analysis
#Nparametrictests
WT = wilcox.test(mortScore ~ Alcohol, data = kw_str, alternative = 'greater')
KT = kruskal.test(metaboage ~ shift_sample.y, data = kw_str)
summary(WT)

plot(tukey.plot.test, las = 1)


#[parametric tests]

m=glm(CVD_score~minutesLAN.y , data=subtot_c)
summary(m)
table(subtot$age,subtot$shift_dic)

#imputation for test

imputed_subtot <- mice(subtot, m = 5, method = 'pmm', maxit = 50, seed = 123)
