#DF cleanup
health_atlas <- health_atlas %>%
  rename(Neighborhood = Name)

health_data <- subset(health_atlas, select = -Layer)
health_data <- health_data[-c(1:4), ]
health_data <- health_data |>
  select_if(function(x) !(all(is.na(x)) | all(x == ""))) 
health_data <- health_data %>%
  mutate(across(-c(Neighborhood, GEOID), as.numeric))
health_data <- health_data %>%
health_data %>%
  select(-contains("_se"))

#General formula for binarizing by median. and test on FIR
binarize_column <- function(df, col_name) {
  # Compute median of the column
  cutoff <- median(df[[col_name]], na.rm = TRUE)
  
  # Replace values with 0 (below median) or 1 (at/above median)
  df[[col_name]] <- ifelse(df[[col_name]] < cutoff, 0, 1)
  
  return(df)
}

#Binarizing for all cloumns
for (col in cols_to_binarize) {
  health_data <- binarize_column(health_data, col)
}

#remove no numerical columns, na -> 0, and remove "_se" column
no_na_health_data <- no_na_health_data[,-(1:3)]
no_na_health_data <- no_na_health_data %>%
  select(-contains("_se"))

# Extract the minimal sufficient conditions data frame
res <- cna(ct, outcome = "VRFIR_2019.2023", con = 0.75, cov = 0.75)

msc <- res$solution$VRFIR_2019.2023$msc
asf <- res$solution$VRFIR_2019.2023$asf

# Now check column names
colnames(msc)
colnames(asf)

# Convert con/cov to numeric if needed
msc$con <- as.numeric(msc$con)
msc$cov <- as.numeric(msc$cov)
asf$con <- as.numeric(asf$con)
asf$cov <- as.numeric(asf$cov)

# Filter top predictors
top_msc <- subset(msc, con >= 0.8 & cov >= 0.8)
top_asf <- subset(asf, con >= 0.8 & cov >= 0.8)

# View top 10
head(top_msc, 10)
head(top_asf, 10)

# Coerce asf to a plain data frame
asf <- as.data.frame(asf)

# Convert columns to numeric explicitly
asf$con <- as.numeric(as.character(asf_df$con))
asf$cov <- as.numeric(as.character(asf_df$cov))
asf$complexity <- as.numeric(as.character(asf_df$complexity))

#organize for con >= 0.8, cov >= 0.8, and asf_df$complexity > 1, 
filtered_asf <- asf[asf$con >= 0.8 & asf$cov >= 0.8 & asf_df$complexity > 1, ]
filtered_asf

results_tbl <- res_fr[[1]] %>%
  arrange(desc(norm.score), desc(con), desc(cov)) %>%
  select(condition, con, cov, complexity, score, norm.score)

# View top 15
head(results_tbl, 15)