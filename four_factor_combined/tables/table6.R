library(dplyr)
library(GRS.test)
library(xtable)

# Function to perform GRS test and extract results
perform_grs_test <- function(file_path, factor_sets) {
  # Load the portfolio returns and factors data
  portfolio_returns <- readRDS(file_path) %>%
    filter(YYYYMM >= as.Date("1968-01-01") & YYYYMM <= as.Date("2018-12-31"))
  
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
      file = basename(file_path),
      factor_set = paste(factor_sets[[i]], collapse = ", "),
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
file_paths <- c("tables/data/table6_size_op_ret.rds", "tables/data/table6_size_inv_ret.rds", "tables/data/table6_size_bm_ret.rds")

# Perform the GRS test for each file and combine the results
all_final_results <- lapply(file_paths, perform_grs_test, factor_sets = factor_sets)

# Combine results from all files
combined_results <- do.call(rbind, all_final_results)

# Create an xtable object for LaTeX formatting
table_latex <- xtable(combined_results)

# Print the xtable object to LaTeX format
print(table_latex)

# Save results to a CSV file
write.csv(combined_results, "tables/data/table6_combined_results.csv", row.names = FALSE)

# Print the results for inspection
print(combined_results)

# Expected:
# file                factor_set  GRS GRS.1 avg_abs_intercept dispersion_ratio avg_intercept_var_ratio avg_r_squared
# 1   table6_size_op_ret.rds           r_mkt, SMB, HML 2.22 0.001             0.106             0.79                    0.31          0.90
# 2   table6_size_op_ret.rds      r_mkt, SMB, HML, RMW 1.78 0.011             0.053             0.21                    0.91          0.92
# 3   table6_size_op_ret.rds      r_mkt, SMB, HML, CMA 2.38 0.000             0.119             0.90                    0.28          0.91
# 4   table6_size_op_ret.rds      r_mkt, SMB, RMW, CMA 1.89 0.006             0.049             0.23                    0.92          0.92
# 5   table6_size_op_ret.rds r_mkt, SMB, HML, RMW, CMA 1.82 0.009             0.056             0.23                    0.84          0.93
# 6  table6_size_inv_ret.rds           r_mkt, SMB, HML 6.07 0.000             0.134             0.71                    0.16          0.92
# 7  table6_size_inv_ret.rds      r_mkt, SMB, HML, RMW 5.65 0.000             0.122             0.57                    0.18          0.92
# 8  table6_size_inv_ret.rds      r_mkt, SMB, HML, CMA 5.09 0.000             0.113             0.50                    0.21          0.92
# 9  table6_size_inv_ret.rds      r_mkt, SMB, RMW, CMA 4.35 0.000             0.089             0.30                    0.35          0.92
# 10 table6_size_inv_ret.rds r_mkt, SMB, HML, RMW, CMA 4.57 0.000             0.101             0.36                    0.28          0.93
# 11  table6_size_bm_ret.rds           r_mkt, SMB, HML 3.37 0.000             0.088             0.49                    0.27          0.91
# 12  table6_size_bm_ret.rds      r_mkt, SMB, HML, RMW 2.86 0.000             0.067             0.27                    0.47          0.91
# 13  table6_size_bm_ret.rds      r_mkt, SMB, HML, CMA 3.34 0.000             0.090             0.48                    0.28          0.91
# 14  table6_size_bm_ret.rds      r_mkt, SMB, RMW, CMA 2.90 0.000             0.083             0.31                    0.55          0.88
# 15  table6_size_bm_ret.rds r_mkt, SMB, HML, RMW, CMA 2.82 0.000             0.074             0.30                    0.44          0.91
