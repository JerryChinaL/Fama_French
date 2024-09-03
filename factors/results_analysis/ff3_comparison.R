library(dplyr)
library(ggplot2)
library(tidyr)

# Read data
factors_mine <- read.csv("data/ff3_d.csv")
factors_check <- read.csv("../monthly_rf.csv") %>% select(YYYYMM = X, everything())
factors_comb <- left_join(factors_mine, factors_check, by = "YYYYMM")

# Prepare data
factors_comb$Date <- as.Date(paste0(substr(factors_comb$YYYYMM, 1, 4), "-", substr(factors_comb$YYYYMM, 5, 6), "-01"))

# Calculate correlations
cor_smb <- cor(factors_comb$smb_replicated, factors_comb$SMB, use = "complete.obs")
cor_hml <- cor(factors_comb$hml_replicated, factors_comb$HML, use = "complete.obs")

# Cumulative calculations
factors_comb <- factors_comb %>%
  mutate(cum_smb_replicated = cumprod(1 + smb_replicated / 100),
         cum_SMB = cumprod(1 + SMB / 100),
         cum_hml_replicated = cumprod(1 + hml_replicated / 100),
         cum_HML = cumprod(1 + HML / 100))

# Calculate cumulative correlations
cor_cum_smb <- cor(factors_comb$cum_smb_replicated, factors_comb$cum_SMB, use = "complete.obs")
cor_cum_hml <- cor(factors_comb$cum_hml_replicated, factors_comb$cum_HML, use = "complete.obs")

# Plot for cumulative SMB
plot_smb <- ggplot(factors_comb, aes(x = Date)) +
  geom_line(aes(y = cum_smb_replicated, color = "Replicated SMB")) +
  geom_line(aes(y = cum_SMB, color = "Original SMB")) +
  labs(y = "Cumulative SMB (%)", title = "Cumulative SMB Replicated vs. Original SMB") +
  scale_color_manual("", values = c("Replicated SMB" = "blue", "Original SMB" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$cum_smb_replicated, factors_comb$cum_SMB, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_smb, 2)), hjust = 0) +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$cum_smb_replicated, factors_comb$cum_SMB, na.rm = TRUE) - 0.1, 
           label = paste("Cumulative Correlation: ", round(cor_cum_smb, 5)), vjust = 2, hjust = 0)

# Plot for cumulative HML
plot_hml <- ggplot(factors_comb, aes(x = Date)) +
  geom_line(aes(y = cum_hml_replicated, color = "Replicated HML")) +
  geom_line(aes(y = cum_HML, color = "Original HML")) +
  labs(y = "Cumulative HML (%)", title = "Cumulative HML Replicated vs. Original HML") +
  scale_color_manual("", values = c("Replicated HML" = "blue", "Original HML" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$cum_hml_replicated, factors_comb$cum_HML, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_hml, 2)), hjust = 0) +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$cum_hml_replicated, factors_comb$cum_HML, na.rm = TRUE) - 0.15, 
           label = paste("Cumulative Correlation: ", round(cor_cum_hml, 5)), vjust = 2, hjust = 0)

# Combine plots
library(gridExtra)
grid.arrange(plot_smb, plot_hml, ncol = 1)
