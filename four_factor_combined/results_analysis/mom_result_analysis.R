library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(gridExtra)

# Read the CSV file
mom_permno <- read.csv("data/momentum_factor_permno_vol.csv") 

# mom_permno <- mom_permno %>% filter(YYYYMM >= 196207)
# Read the Excel file
mom_original <- read_excel("data/mom_original.xlsx")

# Ensure the columns are in Date format and have the same format
mom_permno <- mom_permno %>%
  mutate(YYYYMM = as.Date(paste0(YYYYMM, "01"), format = "%Y%m%d"))

mom_original <- mom_original %>%
  mutate(YYYYMM = as.Date(paste0(YYYYMM, "01"), format = "%Y%m%d"),
         Mom = Mom / 100)  # Convert percentage to decimal

# Rename columns to ensure consistency and avoid conflicts
mom_permno <- mom_permno %>%
  rename(MOM_permno = MOM)

mom_original <- mom_original %>%
  rename(MOM_original = Mom)

# Inner Join by YYYYMM
combined_data <- inner_join(mom_permno, mom_original, by = "YYYYMM")

# Calculate compounded returns and their logarithms
combined_data <- combined_data %>%
  mutate(MOM_permno_compound = cumprod(1 + replace_na(MOM_permno, 0)),
         MOM_original_compound = cumprod(1 + replace_na(MOM_original, 0)),
         MOM_permno_log_compound = log(MOM_permno_compound),
         MOM_original_log_compound = log(MOM_original_compound))

# Calculate correlations
compound_corr <- cor(combined_data$MOM_permno_compound, combined_data$MOM_original_compound, use = "complete.obs")
log_compound_corr <- cor(combined_data$MOM_permno_log_compound, combined_data$MOM_original_log_compound, use = "complete.obs")

# Plot the compounded returns
plot_compound <- ggplot(combined_data, aes(x = YYYYMM)) +
  geom_line(aes(y = MOM_permno_compound, color = "MOM_permno_compound")) +
  geom_line(aes(y = MOM_original_compound, color = "MOM_original_compound")) +
  labs(title = paste("Compounded Momentum Factors (Correlation:", round(compound_corr, 4), ")"),
       x = "Date",
       y = "Compounded Value",
       color = "Legend") +
  theme_minimal()

# Plot the logarithms of compounded returns
plot_log_compound <- ggplot(combined_data, aes(x = YYYYMM)) +
  geom_line(aes(y = MOM_permno_log_compound, color = "MOM_permno_log_compound")) +
  geom_line(aes(y = MOM_original_log_compound, color = "MOM_original_log_compound")) +
  labs(title = paste("Logarithm of Compounded Momentum Factors (Correlation:", round(log_compound_corr, 4), ")"),
       x = "Date",
       y = "Log Compounded Value",
       color = "Legend") +
  theme_minimal()

# Arrange the plots vertically
grid.arrange(plot_compound, plot_log_compound, ncol = 1)
