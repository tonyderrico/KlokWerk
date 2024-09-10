#between scores calculation

m=lm(kwtot4$CVD_score~kwtot4$metaboage)

 = hetcor(kwtot4[, c('CVD_score', 'metaboage', 'mortScore_orig',
                           'T2Dscore','total_years_working_night_shifts',"shift_sample.y")],use = 'complete.obs' )
new_names <- c("CVDscore", "MetabAge", "MortScore", 
               "T2Dscore", "Total Working Years Nightshift","Working nightshift")
colnames(correl$correlations) <- new_names
rownames(correl$correlations) <- new_names
cor(kwtot4$age,kwtot4$metaboage,use = 'complete.obs')

ggcorrplot(correl$correlations, 
           method = "square",    # Use squares for correlation
           type = "upper",       # Show only the upper triangle of the matrix
           lab = TRUE,           # Add correlation values inside the squares
           colors = c("blue", "white", "red"),  # Color gradient for correlations
           title = "Correlation Plot of Risk Scores") 
