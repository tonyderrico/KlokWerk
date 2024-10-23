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

names(subtot)
corr.clust = hetcor(subtot[2:15])
corr.clust1 = hetcor(subtot_c[3:15])
corr.clust2 = hetcor(subtot_n[3:15])

hetcor(subtot_c$winter,subtot_c$MVPA)
corr.clust1 = as.data.frame(corr.clust1$correlations) 

colnames(corr.clust$correlations) <- c("Night Shift", "Season (summer/winter time)", "Sleep Hours", "Daylight Exposure",
                                       "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                                       "Alcohol (mean/24hr)", "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)", 'Age')

rownames(corr.clust$correlations) <- c("Night Shift", "Season (summer/winter time)", "Sleep Hours", "Daylight Exposure",
                                       "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                                       "Alcohol (mean/24hr)", "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)", 'Age')


colnames(corr.clust1) <- c("Season (summer/winter time)", "Sleep Hours", "Daylight Exposure",
                                        "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                                        "Alcohol (mean/24hr)", "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)", 'Age')
rownames(corr.clust1) <- c("Season (summer/winter time)", "Sleep Hours", "Daylight Exposure",
                                        "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                                        "Alcohol (mean/24hr)", "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)", 'Age')

corr.clust1[is.na(corr.clust1)] = -.6

colnames(corr.clust2$correlations) <- factor(colnames(corr.clust1$correlations), levels=unique(colnames(corr.clust1$correlations)))

ggcorrplot(corr.clust1, 
           method = "square",    # Use squares for correlation      
           lab = F,           # Add correlation values inside the squares
           hc.order = F,
           colors = c("blue", "white", "red"),  # Color gradient for correlations
           title = "") 

subset_values <- corr.clust1$correlations[corr.clust1$correlations < 1]

row_names <- rownames(corr.clust1$correlations)[row(corr.clust1$correlations)[corr.clust1$correlations < 1]]
col_names <- colnames(corr.clust1$correlations)[col(corr.clust1$correlations)[corr.clust1$correlations < 1]] 

subset_values <- corr.clust2$correlations[corr.clust2$correlations < 1]

row_names <- rownames(corr.clust2$correlations)[row(corr.clust2$correlations)[corr.clust2$correlations < 1]]
col_names <- colnames(corr.clust2$correlations)[col(corr.clust2$correlations)[corr.clust2$correlations < 1]] 

result <- data.frame(Row = row_names, Column = col_names, Correlation = subset_values)
result = result %>% 
  filter(Correlation < 1) %>% 
  arrange(Correlation) %>%
  distinct(Correlation,.keep_all = T)

#cross ranked correlations
corr_cross(subtot[2:15], rm.na = T, max_pvalue = 0.10, grid = T, type = 1)
corr_cross(subtot_c[3:15])
corr_cross(subtot_n[3:15], rm.na = T, max_pvalue = 0.10, top = 15, grid = T)


# Sample data
set.seed(123)
binary_var <- sample(c(0, 1), 100, replace = TRUE)
continuous_var <- rnorm(100)

# Rank transformation
ranked_binary <- rank(binary_var)
ranked_continuous <- rank(continuous_var)

# Calculate cross-correlation
cross_corr <- ccf(subtot$MetaboAge, as.numeric(subtot$chrono), lag.max = 1, plot = TRUE, na.action = na.pass)

# Display the result
print(cross_corr)
