#Annual PC1 for raw data

library(readxl)
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

#Calculaitng sd for obs t in year m
PC1_diff_fred_raw <- diff(PC1_fred_raw)
PC1_dates_fred_raw <- fred_data$observation_date[-1]

PC1_fred_raw_df <- data.frame(
  Date = PC1_dates_fred_raw,
  PC1_diff = PC1_diff_fred_raw
)
 head(PC1_fred_raw_df)
 
 PC1_fred_raw_df$Year <- format(
  PC1_fred_raw_df$Date,
  "%Y"
)
 PC1_annual_vol_fred_raw <- aggregate(
  PC1_diff ~ Year,
  data = PC1_fred_raw_df,
  FUN = sd
)

head(PC1_annual_vol_fred_raw)

annual_data<- read_excel(
  "full-data.xlsx",
  sheet = "data"
)

#Merging by year
annual_data <- annual_data[!is.na(annual_data$Volatility), ]
annual_data$Year <- as.character(annual_data$Year)

PC1_vs_annual_vol_fred_raw <- merge(
  PC1_annual_vol_fred_raw,
  annual_data[, c("Year", "Volatility")],
  by = "Year"
)

head(PC1_vs_annual_vol_fred_raw)

#Plotting annual volatilities
plot(
  PC1_vs_annual_vol_fred_raw$Volatility,
  PC1_vs_annual_vol_fred_raw$PC1_diff,
  xlab = "S&P Annual Realized Volatility",
  ylab = "PC1 Annual Volatility",
  main = "FRED Raw data PC1 Annual Volatility vs S&P Volatility"
)

cor(
  PC1_vs_annual_vol_fred_raw$PC1_diff,
  PC1_vs_annual_vol_fred_raw$Volatility
)


#Annual PC1 for logged yields

#PCA for logged yields

fred_yields_log <- log(fred_data[, -1])

keep_rows <- complete.cases(fred_yields_log)

fred_yields_log <- fred_yields_log[keep_rows, ]
fred_data_log <- fred_data[keep_rows, ]

pca_fred_log <- prcomp(
  fred_yields_log,
  center = TRUE,
  scale. = TRUE
)
PC1_fred_log <- pca_fred_log$x[, 1]

#Calculating sd for annual volatiltity
PC1_diff_fred_log <- diff(PC1_fred_log)
PC1_dates_fred_log <- fred_data_log$observation_date[-1]

PC1_fred_log_df <- data.frame(
  Date = PC1_dates_fred_log,
  PC1_diff = PC1_diff_fred_log
)

PC1_fred_log_df$Year <- format(
  PC1_fred_log_df$Date,
  "%Y"
)

PC1_annual_vol_fred_log <- aggregate(
  PC1_diff ~ Year,
  data = PC1_fred_log_df,
  FUN = sd
)

head(PC1_annual_vol_fred_log)

#Merging with S$P and it's plot
PC1_vs_annual_vol_fred_log <- merge(
  PC1_annual_vol_fred_log,
  annual_data[, c("Year", "Volatility")],
  by = "Year"
)

head(PC1_vs_annual_vol_fred_log)

plot(
  PC1_vs_annual_vol_fred_log$Volatility,
  PC1_vs_annual_vol_fred_log$PC1_diff,
  xlab = "S&P Annual Realized Volatility",
  ylab = "PC1 Annual Volatility",
  main = "FRED Log PC1 Annual Volatility vs S&P Volatility"
)

cor(
  PC1_vs_annual_vol_fred_log$PC1_diff,
  PC1_vs_annual_vol_fred_log$Volatility
)


#Annual PC2 for raw data

fred_yields_raw <- fred_data[, -1]

pca_fred_raw <- prcomp(
  fred_yields_raw,
  center = TRUE,
  scale. = TRUE
)

PC2_fred_raw <- pca_fred_raw$x[, 2]

# Calculating sd for obs t in year m
PC2_diff_fred_raw <- diff(PC2_fred_raw)
PC2_dates_fred_raw <- fred_data$observation_date[-1]

PC2_fred_raw_df <- data.frame(
  Date = PC2_dates_fred_raw,
  PC2_diff = PC2_diff_fred_raw
)

head(PC2_fred_raw_df)

PC2_fred_raw_df$Year <- format(
  PC2_fred_raw_df$Date,
  "%Y"
)

PC2_annual_vol_fred_raw <- aggregate(
  PC2_diff ~ Year,
  data = PC2_fred_raw_df,
  FUN = sd
)

head(PC2_annual_vol_fred_raw)

annual_data <- read_excel(
  "full-data.xlsx",
  sheet = "data"
)

# Merging by year
annual_data <- annual_data[!is.na(annual_data$Volatility), ]
annual_data$Year <- as.character(annual_data$Year)

PC2_vs_annual_vol_fred_raw <- merge(
  PC2_annual_vol_fred_raw,
  annual_data[, c("Year", "Volatility")],
  by = "Year"
)

head(PC2_vs_annual_vol_fred_raw)

# Plotting annual volatilities
plot(
  PC2_vs_annual_vol_fred_raw$Volatility,
  PC2_vs_annual_vol_fred_raw$PC2_diff,
  xlab = "S&P Annual Realized Volatility",
  ylab = "PC2 Annual Volatility",
  main = "FRED Raw data PC2 Annual Volatility vs S&P Volatility"
)

cor(
  PC2_vs_annual_vol_fred_raw$PC2_diff,
  PC2_vs_annual_vol_fred_raw$Volatility
)


#Annual PC2 for the log yields

# PCA for logged yields

fred_yields_log <- log(fred_data[, -1])

keep_rows <- complete.cases(fred_yields_log)

fred_yields_log <- fred_yields_log[keep_rows, ]
fred_data_log <- fred_data[keep_rows, ]

pca_fred_log <- prcomp(
  fred_yields_log,
  center = TRUE,
  scale. = TRUE
)

PC2_fred_log <- pca_fred_log$x[, 2]

# Calculating sd for annual volatility
PC2_diff_fred_log <- diff(PC2_fred_log)
PC2_dates_fred_log <- fred_data_log$observation_date[-1]

PC2_fred_log_df <- data.frame(
  Date = PC2_dates_fred_log,
  PC2_diff = PC2_diff_fred_log
)

PC2_fred_log_df$Year <- format(
  PC2_fred_log_df$Date,
  "%Y"
)

PC2_annual_vol_fred_log <- aggregate(
  PC2_diff ~ Year,
  data = PC2_fred_log_df,
  FUN = sd
)

head(PC2_annual_vol_fred_log)

# Merging with S&P and its plot
PC2_vs_annual_vol_fred_log <- merge(
  PC2_annual_vol_fred_log,
  annual_data[, c("Year", "Volatility")],
  by = "Year"
)

head(PC2_vs_annual_vol_fred_log)

plot(
  PC2_vs_annual_vol_fred_log$Volatility,
  PC2_vs_annual_vol_fred_log$PC2_diff,
  xlab = "S&P Annual Realized Volatility",
  ylab = "PC2 Annual Volatility",
  main = "FRED Log PC2 Annual Volatility vs S&P Volatility"
)

cor(
  PC2_vs_annual_vol_fred_log$PC2_diff,
  PC2_vs_annual_vol_fred_log$Volatility
)
