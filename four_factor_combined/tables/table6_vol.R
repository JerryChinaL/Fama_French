rm(list=ls())

library(dplyr)
library(GRS.test)
library(xtable)

min_date <- as.Date("1963-07-01")
max_date <- as.Date("2013-12-31")

min_date <- as.Date("1911-07-01")
max_date <- as.Date("2055-12-31")

# Function to perform GRS test and extract results
perform_grs_test <- function(file_path, factor_sets) {
  # Load the portfolio returns and factors data
  portfolio_returns <- readRDS(file_path) %>%
    filter(YYYYMM >= min_date & YYYYMM <= max_date)
  
  # Remove rows with missing values
  portfolio_returns <- portfolio_returns %>%
    filter(complete.cases(.))
  
  # Assuming portfolio returns are in columns 2 to 26
  ret.mat <- as.matrix(portfolio_returns[, 2:26])
  
  # Initialize a list to store results
  all_results <- list()
  
  # Loop through each factor set
  for (i in seq_along(factor_sets)) {
    # Extract the factor matrix for the current set
    factor.mat <- as.matrix(portfolio_returns[, factor_sets[[i]]])
    
    # Perform the GRS test
    grs_result <- GRS.test(ret.mat, factor.mat)
    
    # Extract the results
    grs_stat <- grs_result$GRS.stat
    grs_pval <- grs_result$GRS.pval
    coef_matrix <- grs_result$coef
    tstat_matrix <- grs_result$tstat
    r2_matrix <- grs_result$R2
    stderr_matrix <- grs_result$se
    
    # Calculate average absolute intercepts
    avg_abs_intercept <- mean(abs(coef_matrix[, 1]))
    
    # Calculate dispersion measures
    avg_squared_intercept <- mean(coef_matrix[, 1]^2)
    
    # Compute the overall mean of all elements in ret.mat, ignoring NA values
    overall_mean <- mean(ret.mat, na.rm = TRUE)
    column_means <- colMeans(ret.mat, na.rm = TRUE)
    temp_mystery <- column_means - overall_mean
    avg_return_deviation <- mean(temp_mystery^2)
    dispersion_ratio <- avg_squared_intercept / avg_return_deviation
    
    # Calculate the average of the intercept estimate sample variance
    intercept_variances <- stderr_matrix[, 1]^2
    avg_intercept_variance <- mean(intercept_variances)
    column_six <- avg_intercept_variance / avg_squared_intercept
    
    # Prepare the results for the current factor set
    results <- data.frame(
      factor_set = paste(factor_sets[[i]][-1], collapse = ","),
      GRS_stat = round(grs_stat, 2),
      GRS_pval = round(grs_pval, 3),
      avg_abs_intercept = round(avg_abs_intercept, 3),
      dispersion_ratio = round(dispersion_ratio, 2),
      avg_intercept_var_ratio = round(column_six, 2),
      avg_r_squared = round(mean(r2_matrix), 2)
    )
    
    # Append the results to the list
    all_results[[i]] <- results
  }
  
  # Combine all results into a single data frame
  final_results <- do.call(rbind, all_results)
  return(final_results)
}

# Define the different sets of factors
factor_sets <- list(
  c("r_mkt", "SMB", "HML"),
  c("r_mkt", "SMB", "HML", "RMW"),
  c("r_mkt", "SMB", "HML", "CMA"),
  c("r_mkt", "SMB", "RMW", "CMA"),
  c("r_mkt", "SMB", "HML", "RMW", "CMA")
)

# Define file paths
file_paths <- c("tables/data/table6_vol_op_ret.rds", "tables/data/table6_vol_inv_ret.rds", "tables/data/table6_vol_bm_ret.rds")

# Perform the GRS test for each file and combine the results
all_final_results <- lapply(file_paths, perform_grs_test, factor_sets = factor_sets)

# Combine results from all files
combined_results <- do.call(rbind, all_final_results)

# Create an xtable object for LaTeX formatting
table_latex <- xtable(combined_results)

# Print the xtable object to LaTeX format
print(table_latex)

# Save results to a CSV file
write.csv(combined_results, "tables/data/table6_combined_results_vol.csv", row.names = FALSE)

# Print the results for inspection
print(combined_results)
# Expected:
# file                factor_set  GRS GRS.1 avg_abs_intercept dispersion_ratio avg_intercept_var_ratio avg_r_squared
# 1   table6_vol_op_ret.rds           r_mkt, SMB, HML 1.45 0.076             0.134             1.20                    0.28          0.85
# 2   table6_vol_op_ret.rds      r_mkt, SMB, HML, RMW 0.99 0.484             0.082             0.39                    0.70          0.87
# 3   table6_vol_op_ret.rds      r_mkt, SMB, HML, CMA 1.82 0.009             0.157             1.38                    0.24          0.85
# 4   table6_vol_op_ret.rds      r_mkt, SMB, RMW, CMA 1.09 0.344             0.074             0.30                    0.96          0.87
# 5   table6_vol_op_ret.rds r_mkt, SMB, HML, RMW, CMA 1.10 0.332             0.070             0.26                    1.07          0.88
# 6  table6_vol_inv_ret.rds           r_mkt, SMB, HML 4.94 0.000             0.166             0.99                    0.17          0.86
# 7  table6_vol_inv_ret.rds      r_mkt, SMB, HML, RMW 4.62 0.000             0.166             0.87                    0.18          0.87
# 8  table6_vol_inv_ret.rds      r_mkt, SMB, HML, CMA 4.47 0.000             0.152             0.75                    0.21          0.87
# 9  table6_vol_inv_ret.rds      r_mkt, SMB, RMW, CMA 3.48 0.000             0.127             0.49                    0.32          0.87
# 10 table6_vol_inv_ret.rds r_mkt, SMB, HML, RMW, CMA 3.82 0.000             0.133             0.50                    0.31          0.88
# 11  table6_vol_bm_ret.rds           r_mkt, SMB, HML 2.42 0.000             0.130             0.83                    0.23          0.86
# 12  table6_vol_bm_ret.rds      r_mkt, SMB, HML, RMW 1.96 0.004             0.112             0.50                    0.38          0.86
# 13  table6_vol_bm_ret.rds      r_mkt, SMB, HML, CMA 2.41 0.000             0.124             0.70                    0.28          0.86
# 14  table6_vol_bm_ret.rds      r_mkt, SMB, RMW, CMA 1.92 0.005             0.092             0.31                    0.70          0.84
# 15  table6_vol_bm_ret.rds r_mkt, SMB, HML, RMW, CMA 1.87 0.007             0.102             0.36                    0.53          0.86

