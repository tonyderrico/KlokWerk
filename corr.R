#between scores calculation


correl = hetcor(total2[, c('CVD_score', 'MetaboAge', 'mortScore',
                           'T2Dscore')],use = 'complete.obs' )
new_names <- c("CVDscore", "MetabAge", "MortScore", 
               "T2Dscore")
colnames(correl$correlations) <- new_names
rownames(correl$correlations) <- new_names

ggcorrplot(correl$correlations, 
           method = "square",    # Use squares for correlation
           type = "upper",       # Show only the upper triangle of the matrix
           lab = TRUE,           # Add correlation values inside the squares
           colors = c("blue", "white", "red"),  # Color gradient for correlations
           title = "Correlation Plot of Risk Scores") 

#correlation structure relevant variables
sapply(subtot, class)
names(subtot)

corr.clust = hetcor(subtot[3:13])
corr.clust1 = hetcor(subtot_c[3:13])
corr.clust2 = hetcor(subtot_n[3:13])

a = as.data.frame(corr.clust$correlations)
a > 0.6

subtot_n = subtot %>% filter(shift_dic == 'night')
subtot_c = subtot %>% filter(shift_dic == 'control')

subtot[complete.cases(subtot), ]

summary(subtot)
colSums(is.na(df_fin))


