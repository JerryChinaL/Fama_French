library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Read the CSV files
ff5_original <- read.csv("results_analysis/monthly_ff5_original.csv") %>%
  select(YYYYMM = X, everything())

# ff5 <- read.csv("data/ff5.csv")
ff5 <- read.csv("data/ff5.csv")

# Ensure the date column is in Date format
ff5_original$YYYYMM <- as.Date(paste0(ff5_original$YYYYMM, "01"), format = "%Y%m%d")
ff5$YYYYMM <- as.Date(paste0(ff5$YYYYMM, "01"), format = "%Y%m%d")

# Join the datasets on the common YYYYMM values
combined_data <- inner_join(ff5_original, ff5, by = "YYYYMM", suffix = c("_original", "_replicated"))

# combined_data <- combined_data %>%
#   filter(YYYYMM >= as.Date("1999-07-01") & YYYYMM <= as.Date("2022-07-01"))

# Convert percentage values to decimals
combined_data <- combined_data %>%
  mutate(
    SMB_original = SMB_original / 100,
    HML_original = HML_original / 100,
    RMW_original = RMW_original / 100,
    CMA_original = CMA_original / 100,
    SMB_replicated = SMB_replicated / 100,
    HML_replicated = HML_replicated / 100,
    RMW_replicated = RMW_replicated / 100,
    CMA_replicated = CMA_replicated / 100
  )

# Calculate the compounded return for each factor
combined_data <- combined_data %>%
  arrange(YYYYMM) %>%
  mutate(
    compound_SMB_original = cumprod(1 + SMB_original) - 1,
    compound_HML_original = cumprod(1 + HML_original) - 1,
    compound_RMW_original = cumprod(1 + RMW_original) - 1,
    compound_CMA_original = cumprod(1 + CMA_original) - 1,
    compound_SMB_replicated = cumprod(1 + SMB_replicated) - 1,
    compound_HML_replicated = cumprod(1 + HML_replicated) - 1,
    compound_RMW_replicated = cumprod(1 + RMW_replicated) - 1,
    compound_CMA_replicated = cumprod(1 + CMA_replicated) - 1
  )

# Define a function to plot and show correlation
plot_compound <- function(data, factor) {
  original_col <- factor
  replicated_col <- factor
  compound_original_col <- paste0("compound_", factor, "_original")
  compound_replicated_col <- paste0("compound_", factor, "_replicated")
  
  original_correlation <- cor(data[[paste0(original_col, "_original")]], data[[paste0(replicated_col, "_replicated")]], use = "complete.obs")
  compound_correlation <- cor(data[[compound_original_col]], data[[compound_replicated_col]], use = "complete.obs")
  
  ggplot(data) +
    geom_line(aes(x = YYYYMM, y = !!sym(compound_original_col), color = "Original")) +
    geom_line(aes(x = YYYYMM, y = !!sym(compound_replicated_col), color = "Replicated")) +
    labs(title = paste("Compounded Returns of", factor),
         subtitle = paste("Factor Correlation:", round(original_correlation, 4), ", Compound Correlation:", round(compound_correlation, 4)),
         x = "Date", y = "Compounded Return") +
    scale_color_manual(name = "Series", values = c("Original" = "blue", "Replicated" = "red")) +
    theme_minimal()
}

# Plot compounded returns and show correlation for each factor
plot_list <- lapply(c("SMB", "HML", "RMW", "CMA"), function(factor) {
  plot_compound(combined_data, factor)
})

# Display the plots
do.call(grid.arrange, c(plot_list, ncol = 2))
