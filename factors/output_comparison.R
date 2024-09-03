library(dplyr)
library(ggplot2)
boi <-  read.csv("be_op_in.csv") 

boi_filtered <- boi%>% filter(KYGVKEY == 1050)

# Load your data
fund <- read.csv("be_op_in.csv") %>%
  filter(FYYYY == 2020) %>% 
  mutate(YYYYMM = sprintf("%04d%02d", FYYYY, FYRA)) %>%
  select(gvkey = KYGVKEY, YYYYMM, be1, op1, op2, at = AT, at_lag1, in. = inv)

fund_check <- read.csv("../step1_202012.csv") %>% 
  filter(datadate < as.Date("2021-01-01") & datadate > as.Date("2019-12-31")) %>%
  mutate(YYYYMM = format(as.Date(datadate), "%Y%m")) %>%
  select(-c(datadate))

# Find rows in fund but not in fund_check
rows_in_fund_not_in_fund_check <- anti_join(fund, fund_check)

# Find rows in fund_check but not in fund
rows_in_fund_check_not_in_fund <- anti_join(fund_check, fund)




codes <- readRDS("../mkt_cap/sfz_agg_mth_short.rds") %>% 
  select(gvkey = KYGVKEY, permno = KYPERMNO, SHRCDS) %>%
  mutate(YYYYMM = format(as.Date(datadate), "%Y%m")) %>%
  select(-c(datadate))



mktcap <- read.csv("../mkt_cap/mktcap_combined.csv")
View(mktcap %>% filter(KYGVKEY == 1050))

mktcap_rename <-  mktcap %>% filter(YYYYMM == 202007) %>%
  mutate(mktcap = coalesce(MKVALTQ, MTHCAP, PRCCM * CSHOQ)) %>%
  select(gvkey = KYGVKEY, prccm = PRCCM, me = mktcap, MKVALTQ) %>%
  group_by(gvkey) %>%
  summarize(prccm = max(prccm, na.rm = TRUE), me = mean(me, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(me) & me != 0)

mktcap_check <-  read.csv("../step2_202007_202207/step2_202007.csv") %>%
  select(prccm = prccm_historical , gvkey, me = me3) %>%
  mutate(me = me/1000000) %>% 
  group_by(gvkey) %>%
  summarize(prccm = max(prccm, na.rm = TRUE), me = mean(me, na.rm = TRUE)) %>%
  ungroup()

fund_not_check <- anti_join(mktcap_rename, mktcap_check, by = c("gvkey", "me"))

check_not_fund <- anti_join(mktcap_check, mktcap_rename, by = c("gvkey", "me"))


# Perform a left join to keep all rows for me
joined_me <- left_join(mktcap_check %>% select(gvkey, me_check = me), 
                       mktcap_rename %>% select(gvkey, me_rename = me), 
                       by = "gvkey") %>%
  mutate(me_percent_diff = abs(me_check - me_rename) / me_check * 100)

# Perform a left join to keep all rows for prccm
joined_prccm <- left_join(mktcap_check %>% select(gvkey, prccm_check = prccm), 
                          mktcap_rename %>% select(gvkey, prccm_rename = prccm), 
                          by = "gvkey") %>%
  mutate(prccm_percent_diff = abs(prccm_check - prccm_rename) / prccm_check * 100)

# Plot histogram for percent difference in me
ggplot(joined_me, aes(x = me_percent_diff)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Percent Differences in ME", x = "Percent Difference in ME (%)", y = "Frequency") +
  xlim(0, quantile(joined_me$me_percent_diff, 0.995, na.rm = TRUE)) + 
  ylim(0, 400)
# Focus on the majority of data

# Identify rows with multiple matches
multiple_matches_x <- mktcap_check %>%
  group_by(gvkey) %>%
  filter(n() > 1)

multiple_matches_y <- mktcap_rename %>%
  group_by(gvkey) %>%
  filter(n() > 1)

# Print rows with multiple matches
print("Rows with multiple matches in mktcap_check:")
print(multiple_matches_x)

print("Rows with multiple matches in mktcap_rename:")
print(multiple_matches_y)

# Filter rows with market cap difference > 50%
diff_greater_50 <- joined_me %>%
  filter(me_percent_diff > 50)

# Print rows with market cap difference > 50%
print("Rows with market cap difference greater than 50%:")
print(diff_greater_50)

# Plot histogram for percent difference in prccm
ggplot(joined_prccm, aes(x = prccm_percent_diff)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Percent Differences in PRCCM", x = "Percent Difference in PRCCM (%)", y = "Frequency") +
  xlim(-1, quantile(joined_prccm$prccm_percent_diff, 0.99, na.rm = TRUE))  # Focus on the majority of data



library(dplyr)
library(ggplot2)
library(tidyr)
factors_mine <- read.csv("data/ff3_a.csv")
factors_check <- read.csv("../monthly_rf.csv") %>% select(YYYYMM = X, everything())
factors_comb <- left_join(factors_mine, factors_check, by = "YYYYMM")

factors_long <- factors_comb %>%
  pivot_longer(cols = c(smb_replicated, SMB, hml_replicated, HML),
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(.*)")

factors_comb$Date <- as.Date(paste0(substr(factors_comb$YYYYMM, 1, 4), "-", substr(factors_comb$YYYYMM, 5, 6), "-01"))

cor_smb <- cor(factors_comb$smb_replicated, factors_comb$SMB, use = "complete.obs")
cor_hml <- cor(factors_comb$hml_replicated, factors_comb$HML, use = "complete.obs")

# Plot for SMB
ggplot(factors_comb, aes(x = Date)) +
  geom_line(aes(y = smb_replicated, color = "Replicated SMB")) +
  geom_line(aes(y = SMB, color = "Original SMB")) +
  labs(y = "SMB (%)", title = "SMB Replicated vs. Original SMB") +
  scale_color_manual("", values = c("Replicated SMB" = "blue", "Original SMB" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$smb_replicated, factors_comb$SMB, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_smb, 2)), hjust = 0)

# Plot for HML
ggplot(factors_comb, aes(x = Date)) +
  geom_line(aes(y = hml_replicated, color = "Replicated HML")) +
  geom_line(aes(y = HML, color = "Original HML")) +
  labs(y = "HML (%)", title = "HML Replicated vs. Original HML") +
  scale_color_manual("", values = c("Replicated HML" = "blue", "Original HML" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$hml_replicated, factors_comb$HML, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_hml, 2)), hjust = 0)

factors_comb <- factors_comb %>%
  mutate(cum_smb_replicated = cumprod(1+smb_replicated/100),
         cum_SMB = cumprod(1+SMB/100),
         cum_hml_replicated = cumprod(1+hml_replicated/100),
         cum_HML = cumprod(1+HML/100))

cor_cum_smb <- cor(factors_comb$cum_smb_replicated, factors_comb$cum_SMB, use = "complete.obs")
cor_cum_hml <- cor(factors_comb$cum_hml_replicated, factors_comb$cum_HML, use = "complete.obs")

# Plot for cumulative SMB
ggplot(factors_comb, aes(x = Date)) +
  geom_line(aes(y = cum_smb_replicated, color = "Replicated SMB")) +
  geom_line(aes(y = cum_SMB, color = "Original SMB")) +
  labs(y = "Cumulative SMB (%)", title = "Cumulative SMB Replicated vs. Original SMB") +
  scale_color_manual("", values = c("Replicated SMB" = "blue", "Original SMB" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$cum_smb_replicated, factors_comb$cum_SMB, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_cum_smb, 5)), hjust = 0)

# Plot for cumulative HML
ggplot(factors_comb, aes(x = Date)) +
  geom_line(aes(y = cum_hml_replicated, color = "Replicated HML")) +
  geom_line(aes(y = cum_HML, color = "Original HML")) +
  labs(y = "Cumulative HML (%)", title = "Cumulative HML Replicated vs. Original HML") +
  scale_color_manual("", values = c("Replicated HML" = "blue", "Original HML" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb$Date), y = max(factors_comb$cum_hml_replicated, factors_comb$cum_HML, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_cum_hml, 5)), hjust = 0)






# Ensure Date column is in Date format
factors_comb$Date <- as.Date(paste0(substr(factors_comb$YYYYMM, 1, 4), "-", substr(factors_comb$YYYYMM, 5, 6), "-01"))

# Filter data for 2023 and beyond
factors_comb_filtered <- factors_comb %>% filter(Date >= as.Date("1999-01-01"))
factors_comb_filtered <- factors_comb_filtered %>%
  mutate(cum_smb_replicated = cumsum(smb_replicated),
         cum_SMB = cumsum(SMB),
         cum_hml_replicated = cumsum(hml_replicated),
         cum_HML = cumsum(HML))
cor_cum_smb <- cor(factors_comb_filtered$cum_smb_replicated, factors_comb_filtered$cum_SMB, use = "complete.obs")
cor_cum_hml <- cor(factors_comb_filtered$cum_hml_replicated, factors_comb_filtered$cum_HML, use = "complete.obs")
library(ggplot2)

# Plot for cumulative SMB
ggplot(factors_comb_filtered, aes(x = Date)) +
  geom_line(aes(y = cum_smb_replicated, color = "Replicated SMB")) +
  geom_line(aes(y = cum_SMB, color = "Original SMB")) +
  labs(y = "Cumulative SMB (%)", title = "Cumulative SMB Replicated vs. Original SMB") +
  scale_color_manual("", values = c("Replicated SMB" = "blue", "Original SMB" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb_filtered$Date), y = max(factors_comb_filtered$cum_smb_replicated, factors_comb_filtered$cum_SMB, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_cum_smb, 2)), hjust = 0)

# Plot for cumulative HML
ggplot(factors_comb_filtered, aes(x = Date)) +
  geom_line(aes(y = cum_hml_replicated, color = "Replicated HML")) +
  geom_line(aes(y = cum_HML, color = "Original HML")) +
  labs(y = "Cumulative HML (%)", title = "Cumulative HML Replicated vs. Original HML") +
  scale_color_manual("", values = c("Replicated HML" = "blue", "Original HML" = "red")) +
  theme_minimal() +
  annotate("text", x = min(factors_comb_filtered$Date), y = max(factors_comb_filtered$cum_hml_replicated, factors_comb_filtered$cum_HML, na.rm = TRUE), 
           label = paste("Correlation: ", round(cor_cum_hml, 2)), hjust = 0)




plot(factors_comb$smb_replicated, factors_comb$SMB, xlab = "SMB Replicated (%)", ylab = "SMB (%)", main = "SMB Replicated vs. SMB")

plot(factors_comb$hml_replicated, factors_comb$HML, xlab = "HML Replicated (%)", ylab = "HML (%)", main = "HML Replicated vs. HML")
