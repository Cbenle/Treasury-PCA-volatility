#For PC1 raw data

fred_data <- read_excel(
  "fredgraph (1).xlsx",
  sheet = "Daily",
)
fred_data <- na.omit(fred_data)
dim(fred_data)
fred_yields_raw <- fred_data[, -1]
pca_fred_raw <- prcomp(
  fred_yields_raw,
  center = TRUE,
  scale. = TRUE
)


PC1_fred_raw <- pca_fred_raw$x[, 1]
 
#Calculaitng sd for obs t in month m
PC1_diff_fred_raw <- diff(PC1_fred_raw)

PC1_dates_fred_raw <- fred_data$observation_date[-1]

PC1_fred_raw_df <- data.frame(
  Date = PC1_dates_fred_raw,
  PC1_diff = PC1_diff_fred_raw
)

PC1_fred_raw_df$Month <- format(
  PC1_fred_raw_df$Date,
  "%Y-%m"
)

PC1_monthly_vol_fred_raw <- aggregate(
  PC1_diff ~ Month,
  data = PC1_fred_raw_df,
  FUN = sd
)

head(PC1_monthly_vol_fred_raw)


#merging with vix
Vix<- read_excel(
  "vix-monthly.xlsx",
  sheet = 2
)
Vix$Month <- format(Vix$Month, "%Y-%m")
PC1_vs_Vix_fred_raw <- merge(
  PC1_monthly_vol_fred_raw,
  Vix,
  by = "Month"
)

head(PC1_vs_Vix_fred_raw)

#Plotting
plot(
  PC1_vs_Vix_fred_raw$VIX,
  PC1_vs_Vix_fred_raw$PC1_diff,
  xlab = "VIX",
  ylab = "PC1 Monthly Volatility",
  main = "FRED Raw PC1 Monthly Volatility vs VIX"
)
cor(
  PC1_vs_Vix_fred_raw$PC1_diff,
  PC1_vs_Vix_fred_raw$VIX
)


#PC2 Raw

#FOr pc2 raw
PC2_fred_raw <- pca_fred_raw$x[, 2]

PC2_diff_fred_raw <- diff(PC2_fred_raw)


PC2_dates_fred_raw <- fred_data$observation_date[-1]

PC2_fred_raw_df <- data.frame(
  Date = PC2_dates_fred_raw,
  PC2_diff = PC2_diff_fred_raw
)

PC2_fred_raw_df$Month <- format(
  PC2_fred_raw_df$Date,
  "%Y-%m"
)

PC2_monthly_vol_fred_raw <- aggregate(
  PC2_diff ~ Month,
  data = PC2_fred_raw_df,
  FUN = sd
)

head(PC2_monthly_vol_fred_raw)
#Merging with VIX
PC2_vs_Vix_fred_raw <- merge(
  PC2_monthly_vol_fred_raw,
  Vix,
  by = "Month"
)

head(PC2_vs_Vix_fred_raw)

#Plot
plot(
  PC2_vs_Vix_fred_raw$VIX,
  PC2_vs_Vix_fred_raw$PC2_diff,
  xlab = "VIX",
  ylab = "PC2 Monthly Volatility",
  main = "FRED Raw PC2 Monthly Volatility vs VIX"
)
cor(
  PC2_vs_Vix_fred_raw$PC2_diff,
  PC2_vs_Vix_fred_raw$VIX
)
