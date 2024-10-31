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
corr.clust1 = hetcor(subtot_c[3:14])
corr.clust2 = hetcor(subtot_n[3:14])

hetcor(subtot_c$winter,subtot_c$MVPA)
corr.clust1 = as.data.frame(corr.clust1$correlations) 
corr.clust2 = as.data.frame(corr.clust2$correlations) 


colnames(corr.clust1) <- c("Season (summer/winter time)", "Sleep Hours", "Daylight Exposure", "Age",
                                        "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                                        "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)")
rownames(corr.clust1) <- c("Season (summer/winter time)", "Sleep Hours", "Daylight Exposure", "Age",
                           "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                           "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)")

colnames(corr.clust2) <- c("Season (summer/winter time)", "Sleep Hours", "Daylight Exposure", "Age",
                           "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                           "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)")
rownames(corr.clust2) <- c("Season (summer/winter time)", "Sleep Hours", "Daylight Exposure", "Age",
                           "Physical Activity MVPA", "Chronotype", "BMI", "Mortality Score", "CVD Score", "Metabolic Age",
                           "Coffee (mean/24hr)", "Total caloric intake (mean/24hr)")

corr.clust1[is.na(corr.clust1)] = -.6
corr.clust1$correlations[is.na(corr.clust1$correlations)] = .6

colnames(corr.clust2) <- factor(colnames(corr.clust2), 
                                             levels=unique(colnames(corr.clust2)))

m = ggcorrplot(corr.clust1, 
           method = "square",    # Use squares for correlation      
           type = 'upper',           # Add correlation values inside the squares
           hc.order = F,
           colors = c("blue", "white", "red"),  # Color gradient for correlations
           title = "") 

m1 = ggcorrplot(corr.clust2, 
           method = "square",
           type = 'lower'
           hc.order = F,
           colors = c("blue", "white", "red"),  # Color gradient for correlations
           title = "") 

combined_plot <- m + m1 + plot_layout(ncol = 2)
combined_plot <- m + 
  geom_tile(data = m1$data, aes("", value), inherit.aes = FALSE)

subset_values <- as.data.frame(as.table(corr.clust1$correlations))
subset_values <- subset_values[subset_values$Freq < 1, ]

subset_values <- na.omit(subset_values)

subset_values <- subset_values[order(subset_values$Freq), ]
subset_values <- subset_values[!duplicated(subset_values$Freq), ]
subset_values1 = as.data.frame(subset_values)
subset_values2 = as.data.frame(subset_values)
subset_values3 = cbind(subset_values1,subset_values2)

names(subset_values3) = c('va1','var2','freq1','var3','var4','freq2')
subset_values3$diff = subset_values3$freq1 - subset_values3$freq2

write.csv(subset_values3,"O:/DGK/IRAS/EEPI/Projects/Shift Work Project/Toni-KW/prel results/corr.poster.csv")

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
