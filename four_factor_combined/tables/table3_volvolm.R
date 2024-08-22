rm(list = ls())

library(dplyr)

min_Date = as.Date("1963-07-01")
max_Date = as.Date("2013-12-31")

min_date <- as.Date("1900-07-01")
max_date <- as.Date("2099-12-31")

# Load the factors data
factors_replicated <- read.csv("data/ff5_vol.csv") %>%
  mutate(monthly_date = as.Date(as.character(YYYYMM))) %>%
  select(monthly_date, SMB, HML, RMW, CMA, SMB_bm, SMB_op, SMB_inv)

# VOLM
momentum <- readRDS("../../ELM/data/ELM-4-factors-replicated-nordq.rds") %>%
  select(monthly_date, MOM = r_volm)

# Carhart MOM
momentum <- read.csv("data/momentum_factor_permno_nd.csv") %>%
  mutate(monthly_date = as.Date(paste0(YYYYMM, "01"), format = "%Y%m%d"))

factors_replicated <- factors_replicated %>%
  left_join(momentum, by = "monthly_date")

rf_data <- read.csv("data/monthly_rf.csv") %>%
  mutate(monthly_date = as.Date(as.character(X * 100 + 1), format = "%Y%m%d")) %>%
  select(monthly_date, r_mkt = Mkt.RF)

factors_filtered <- factors_replicated %>%
  left_join(rf_data, by = "monthly_date") %>%
  filter(monthly_date >= min_Date & monthly_date <= max_Date)

# Calculate summary statistics including MOM
mean_HML <- mean(factors_filtered$HML, na.rm = TRUE)
mean_RMW <- mean(factors_filtered$RMW, na.rm = TRUE)
mean_CMA <- mean(factors_filtered$CMA, na.rm = TRUE)
mean_SMB <- mean(factors_filtered$SMB, na.rm = TRUE)
mean_MOM <- mean(factors_filtered$MOM, na.rm = TRUE)
mean_r_mkt <- mean(factors_filtered$r_mkt, na.rm = TRUE)

sd_HML <- sd(factors_filtered$HML, na.rm = TRUE)
sd_RMW <- sd(factors_filtered$RMW, na.rm = TRUE)
sd_CMA <- sd(factors_filtered$CMA, na.rm = TRUE)
sd_SMB <- sd(factors_filtered$SMB, na.rm = TRUE)
sd_MOM <- sd(factors_filtered$MOM, na.rm = TRUE)
sd_r_mkt <- sd(factors_filtered$r_mkt, na.rm = TRUE)

t_HML <- as.numeric(t.test(factors_filtered$HML)$statistic)
t_RMW <- as.numeric(t.test(factors_filtered$RMW)$statistic)
t_CMA <- as.numeric(t.test(factors_filtered$CMA)$statistic)
t_SMB <- as.numeric(t.test(factors_filtered$SMB)$statistic)
t_MOM <- as.numeric(t.test(factors_filtered$MOM)$statistic)
t_r_mkt <- as.numeric(t.test(factors_filtered$r_mkt)$statistic)

# Create Panel A with MOM
summary_stats_table <- data.frame(
  Statistic = c("Arithmetic Mean (%)", "Standard Deviation (%)", "t-statistic"),
  MKT_R = c(mean_r_mkt, sd_r_mkt, t_r_mkt),
  SMB = c(mean_SMB, sd_SMB, t_SMB),
  HML = c(mean_HML, sd_HML, t_HML),
  RMW = c(mean_RMW, sd_RMW, t_RMW),
  CMA = c(mean_CMA, sd_CMA, t_CMA),
  MOM = c(mean_MOM, sd_MOM, t_MOM)
)

# Panel B: Correlation Matrix for the Five Factors plus MOM
cor_matrix_five_factors <- factors_filtered %>%
  select(MKT_R = r_mkt, SMB, HML, RMW, CMA, MOM) %>%
  cor(use = "pairwise.complete.obs")

# Panel C: Correlation Matrix for the SMB Sub-Factors plus MOM
cor_matrix_SMB_factors <- factors_filtered %>%
  select(SMB_bm, SMB_op, SMB_inv, SMB, MOM) %>%
  cor(use = "pairwise.complete.obs")

# Format numbers to two decimal places
summary_stats_table_formatted <- summary_stats_table %>%
  mutate(across(-Statistic, ~ sprintf("%.2f", .)))

cor_matrix_five_factors_formatted <- apply(cor_matrix_five_factors, 2, function(x) sprintf("%.2f", x))
cor_matrix_SMB_factors_formatted <- apply(cor_matrix_SMB_factors, 2, function(x) sprintf("%.2f", x))

# LaTeX Table Generation with MOM
latex_output <- paste0("

\\begin{table}[H]
\\small
\\begin{tabular}{p{4.5cm} p{2cm} p{2cm} p{2cm} p{2cm} p{2cm} p{2cm}}
\\hline
\\multicolumn{7}{l}{Panel A: Summary statistics of the five factors plus MOM} \\\\
 & $MKT_R$ & $SMB$ & $HML$ & $RMW$ & $CMA$ & $MOM$ \\\\
 \\hline
Arithmetic mean & ", paste(summary_stats_table_formatted$MKT_R[1], summary_stats_table_formatted$SMB[1], summary_stats_table_formatted$HML[1], summary_stats_table_formatted$RMW[1], summary_stats_table_formatted$CMA[1], summary_stats_table_formatted$MOM[1], sep = " & "), " \\\\
Standard deviation & ", paste(summary_stats_table_formatted$MKT_R[2], summary_stats_table_formatted$SMB[2], summary_stats_table_formatted$HML[2], summary_stats_table_formatted$RMW[2], summary_stats_table_formatted$CMA[2], summary_stats_table_formatted$MOM[2], sep = " & "), " \\\\
T-statistic & ", paste(summary_stats_table_formatted$MKT_R[3], summary_stats_table_formatted$SMB[3], summary_stats_table_formatted$HML[3], summary_stats_table_formatted$RMW[3], summary_stats_table_formatted$CMA[3], summary_stats_table_formatted$MOM[3], sep = " & "), " \\\\
\\\\
\\multicolumn{7}{l}{Panel B: Correlation matrix for the five factors plus MOM} \\\\
 & $MKT_R$ & $SMB$ & $HML$ & $RMW$ & $CMA$ & $MOM$ \\\\
 \\hline
$MKT_R$ & ", paste(cor_matrix_five_factors_formatted[1, ], collapse = " & "), " \\\\
$SMB$ & ", paste(cor_matrix_five_factors_formatted[2, ], collapse = " & "), " \\\\
$HML$ & ", paste(cor_matrix_five_factors_formatted[3, ], collapse = " & "), " \\\\
$RMW$ & ", paste(cor_matrix_five_factors_formatted[4, ], collapse = " & "), " \\\\
$CMA$ & ", paste(cor_matrix_five_factors_formatted[5, ], collapse = " & "), " \\\\
$MOM$ & ", paste(cor_matrix_five_factors_formatted[6, ], collapse = " & "), " \\\\
\\\\
\\multicolumn{7}{l}{Panel C: Correlation matrix for the SMB sub-factors plus MOM} \\\\
 & $SMB_{bm}$ & $SMB_{op}$ & $SMB_{inv}$ & $SMB$ & $MOM$ \\\\
 \\hline
$SMB_{bm}$ & ", paste(cor_matrix_SMB_factors_formatted[1, ], collapse = " & "), " \\\\
$SMB_{op}$ & ", paste(cor_matrix_SMB_factors_formatted[2, ], collapse = " & "), " \\\\
$SMB_{inv}$ & ", paste(cor_matrix_SMB_factors_formatted[3, ], collapse = " & "), " \\\\
$SMB$ & ", paste(cor_matrix_SMB_factors_formatted[4, ], collapse = " & "), " \\\\
$MOM$ & ", paste(cor_matrix_SMB_factors_formatted[5, ], collapse = " & "), " \\\\
\\hline
\\end{tabular}
\\end{table}

")


# Print Panel A
cat("Panel A: Summary Statistics of the Five Factors plus MOM\n")
print(summary_stats_table)

# Print Panel B
cat("\nPanel B: Correlation Matrix for the Five Factors plus MOM\n")
print(cor_matrix_five_factors)

# Print Panel C
cat("\nPanel C: Correlation Matrix for the SMB Sub-Factors plus MOM\n")
print(cor_matrix_SMB_factors)

# Save the tables to CSV files
write.csv(summary_stats_table, "tables/data/table3_Panel_A_Summary_Statistics_with_MOM.csv", row.names = FALSE)
write.csv(cor_matrix_five_factors, "tables/data/table3_Panel_B_Correlation_Matrix_Five_Factors_with_MOM.csv")
write.csv(cor_matrix_SMB_factors, "tables/data/table3_Panel_C_Correlation_Matrix_SMB_Sub_Factors_with_MOM.csv")

# Print the LaTeX table
cat(latex_output)
