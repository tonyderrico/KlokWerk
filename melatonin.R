#MELATONIN

library(readxl)


file_path <- "O:/DGK/IRAS/EEPI/Projects/Shift Work Project/Toni-KW/datasets/melatonin_df.xlsx"

melatonin_sheets <- excel_sheets(file_path)

data_list <- lapply(melatonin_sheets, function(sheet) read_excel(file_path, sheet = sheet))

names(data_list) <- melatonin_sheets

# I WANT TO MERGE The sheets of my excel file into one dataframe but before i want to extract just the columns number
#14 and 15 from each sheet and then merge them into one dataframe 

melatonin_df <- do.call(rbind, lapply(data_list, function(x) x[, c(1, 14)]))
  
mel1 =  data_list[[1]][, c(1, 14)]
mel2 =  data_list[[2]][, c(1, 14)]
mel3 =  data_list[[3]][, c(1, 14)]
mel4 =  data_list[[4]][, c(1, 14)]
mel5 =  data_list[[5]][, c(1, 14)]
mel6 =  data_list[[6]][, c(1, 14)]
mel7 =  data_list[[7]][, c(1, 14)]
mel8 =  data_list[[8]][, c(1, 14)]
mel9 =  data_list[[9]][, c(1, 14)]
mel10 =  data_list[[10]][, c(1, 14)]
mel11 =  data_list[[11]][, c(1, 14)]
mel12 =  data_list[[12]][, c(1, 14)]
mel13 =  data_list[[13]][, c(1, 14)]
mel14 =  data_list[[14]][, c(1, 14)]
mel15 =  data_list[[15]][, c(1, 14)]
mel16 =  data_list[[16]][, c(1, 14)]
#i want to merge all them into one dataframe
melatonin_df <- rbind(mel1, mel2, mel3, mel4, mel5, mel6, mel7, mel8, mel9, mel10, mel11, mel12, mel13, mel14, mel15, mel16)

names(melatonin_df)[1] = 'samples'
names(melatonin_df)[2] = 'melatonin'

#i want to perform a mean of all observations by subject id based on the first 7 digits of the subject id such as 1001-00 U1 +
#1001-00 U2 + 1001-00 U3
melatonin_df$subject_id <- substr(melatonin_df$samples, 1, 7)

melatonin_df$subject_id <- as.factor(melatonin_df$subject_id) 
#now the mean by subject id
melatonin_df <- aggregate(melatonin_df$melatonin, by = list(melatonin_df$subject_id), FUN = mean)

melatonin_df$Group.1 <- substr(melatonin_df$Group.1, 1, 4)

melatonin_df <- aggregate(melatonin_df$x, by = list(melatonin_df$Group.1), FUN = mean)
colnames(melatonin_df) <- c("subjectid", "melatonin")

#now i want to merge the melatonin_df with df_fin  by subjectid but just the one that match with melatonin_df with the function 
#merge
df_fin <- merge(df_fin, melatonin_df, by = "subjectid", all.x = TRUE)

#now i want to perform a linear regression between melatonin and the variable of interest
model <- lm(x$melatonin ~ x$CVD_score, data = df_fin)
summary(model)

cohens_w(df_fin$melatonin, df_fin$shift_dic)

#difference on melatonin levels between night shift and day shift workers
t.test(df_fin$melatonin ~ df_fin$shift_dic)
