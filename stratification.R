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
df_fin$MetaboAge[df_fin$MetaboAge > 87] = NA

#datasets tests
subtot = df_fin %>% select(sampleid,shift_dic,winter,sleephr,luxmean,luxmedian,
                         minutesLAN.y,ENMO,MVPA,
                         chrono,
                         bmimeasured,
                         mortScore,CVD_score,MetaboAge, alcohol24.y,koffie24.y,totaalkcal,age,productive_working_hours,
                         )



#preliminary analysis
#Nparametrictests
WT = wilcox.test(mortScore ~ Alcohol, data = kw_str, alternative = 'greater')
KT = kruskal.test(metaboage ~ shift_sample.y, data = kw_str)
summary(WT)

plot(tukey.plot.test, las = 1)


#[parametric tests]

m=glm(CVD_score~ MVPA + maand, data=a)
summary(m)
table(subtot$age,subtot$shift_dic)