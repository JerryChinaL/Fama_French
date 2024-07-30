library(dplyr)
library(tidyr)
library(broom)
library(xtable)

# Load necessary libraries and data
factors <- readRDS("data/portfolios_w_return_vol.rds")

# Load the factors data
factors_replicated <- read.csv("data/ff5_vol.csv") %>%
  mutate(monthly_date = as.Date(YYYYMM)) %>%
  select(monthly_date, SMB, HML, RMW, CMA)

rf_data <- read.csv("data/monthly_rf.csv") %>%
  mutate(monthly_date = as.Date(as.character(X*100+1), format = "%Y%m%d")) %>%
  select(monthly_date, r_mkt = Mkt.RF)

r_factors <- factors_replicated %>%
  left_join(rf_data, by = "monthly_date") %>%
  filter(monthly_date >= as.Date("1968-01-01") & monthly_date <= as.Date("2018-12-31"))

# Define the function to assign portfolios
assign_portfolio <- function(data, sorting_variable, percentiles) {
  if (all(is.na(data[[deparse(substitute(sorting_variable))]]))) {
    return(rep(NA, nrow(data)))
  }
  breakpoints <- data %>%
    filter(PRIMEXCH == "N") %>%
    filter(!is.na({{ sorting_variable }})) %>%
    pull({{ sorting_variable }}) %>%
    quantile(probs = percentiles, na.rm = TRUE, names = FALSE)
  
  assigned_portfolios <- findInterval(data[[deparse(substitute(sorting_variable))]], breakpoints, all.inside = TRUE)
  
  return(assigned_portfolios)
}

# Apply the portfolio assignment
portfolios_5x5 <- factors %>%
  group_by(monthly_date) %>%
  mutate(
    portfolio_size = assign_portfolio(
      data = pick(everything()),
      sorting_variable = SIZE,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_bm = assign_portfolio(
      data = pick(everything()),
      sorting_variable = bm,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_op = assign_portfolio(
      data = pick(everything()),
      sorting_variable = op,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_inv = assign_portfolio(
      data = pick(everything()),
      sorting_variable = inv,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, YYYYMM = monthly_date, MTHRET, SIZE, portfolio_size, portfolio_bm, portfolio_op, portfolio_inv)

rf_data <- read.csv("data/monthly_rf.csv")
# Add the excess return column by appending rf rate then subtracting.
portfolios_5x5 <- portfolios_5x5 %>% 
  mutate(rf_date = format(as.Date(YYYYMM), "%Y%m")) %>%
  left_join(rf_data %>% mutate(rf_date = as.character(X)) %>% select(rf_date, RF), by = c("rf_date")) %>%
  select(-c(rf_date)) %>%
  mutate(MTHRET = MTHRET * 100 - RF)

# List of variables to loop through
variables <- c("op", "inv", "bm")

# Initialize a list to store regression results
regression_results <- list()
t_test_results <- list()
r_squared_results <- list()
intercept_t_stats <- list()

for (var in variables) {
  # Filter for non-NA values at the beginning of the loop
  portfolios_5x5_filtered <- portfolios_5x5 %>%
    filter(!is.na(!!sym(paste0("portfolio_", var))) & !is.na(SIZE) & !is.na(MTHRET),
           YYYYMM >= as.Date("1968-10-01") & YYYYMM <= as.Date("2018-12-31"))
  
  # Calculate the average monthly return for each quantile pair for each month
  monthly_grid <- portfolios_5x5_filtered %>%
    group_by(YYYYMM, portfolio_size, !!sym(paste0("portfolio_", var))) %>%
    mutate(SIZE_weight = ifelse(is.na(SIZE), 0, SIZE)) %>%
    summarize(avg_monthly_return = weighted.mean(MTHRET, SIZE_weight, na.rm = TRUE), .groups = "drop")
  
  # Pivot the data to a wider format
  monthly_grid_wide <- monthly_grid %>%
    unite("quantile", portfolio_size, !!sym(paste0("portfolio_", var)), sep = "") %>%
    pivot_wider(names_from = quantile, values_from = avg_monthly_return) %>%
    rename_with(~ paste0("f", .), -YYYYMM)
  
  # Sort the columns by name
  sorted_columns <- c("YYYYMM", sort(names(monthly_grid_wide)[-1]))
  monthly_grid_wide <- monthly_grid_wide %>% 
    select(all_of(sorted_columns))
  
  # Merge with r_factors
  joined_data <- monthly_grid_wide %>% left_join(r_factors, by = c("YYYYMM" = "monthly_date"))
  
  saveRDS(joined_data, paste0("tables/data/table6_vol_", var, "_ret.rds"))
  
  # Perform regressions for each quantile pair
  reg_results <- lapply(names(joined_data)[2:26], function(quantile_col) {
    formula <- as.formula(paste(quantile_col, "~ r_mkt + SMB + HML + RMW + CMA"))
    lm(formula, data = joined_data)
  })
  
  # Extract coefficients and organize into matrices
  coef_names <- c("(Intercept)", "r_mkt", "SMB", "HML", "RMW", "CMA")
  for (coef_name in coef_names) {
    coef_matrix <- matrix(nrow = 5, ncol = 5)
    t_test_matrix <- matrix(nrow = 5, ncol = 5)
    dimnames(coef_matrix) <- list(paste0("SIZE_", 1:5), paste0(var, "_", 1:5))
    dimnames(t_test_matrix) <- list(paste0("SIZE_", 1:5), paste0(var, "_", 1:5))
    for (i in 1:25) {
      coef_matrix[i] <- round(coef(summary(reg_results[[i]]))[coef_name, "Estimate"], 2)
      t_test_matrix[i] <- round(coef(summary(reg_results[[i]]))[coef_name, "t value"], 2)
    }
    regression_results[[paste0(var, "_", coef_name)]] <- coef_matrix
    t_test_results[[paste0(var, "_", coef_name, "_t_value")]] <- t_test_matrix
    if (coef_name == "(Intercept)") {
      intercept_t_stats[[paste0(var, "_", coef_name, "_t_value")]] <- t_test_matrix
    }
  }
  
  # Extract R-squared values and organize into a matrix
  r_squared_matrix <- matrix(nrow = 5, ncol = 5)
  dimnames(r_squared_matrix) <- list(paste0("SIZE_", 1:5), paste0(var, "_", 1:5))
  for (i in 1:25) {
    r_squared_matrix[i] <- round(summary(reg_results[[i]])$r.squared, 2)
  }
  r_squared_results[[paste0(var, "_R_squared")]] <- r_squared_matrix
}

# Function to format column names for LaTeX
format_colnames <- function(names) {
  return(gsub("_", "\\\\_", names))
}

# Create a LaTeX file for each variable
for (var in variables) {
  output_file <- paste0("tables/data/table4vol_", var, "_regression_results.tex")
  file_conn <- file(output_file, open = "wt")
  
  for (coef_name in c("(Intercept)", "r_mkt", "SMB", "HML", "RMW", "CMA", "R_squared")) {
    if (coef_name == "R_squared") {
      table_matrix <- r_squared_results[[paste0(var, "_", coef_name)]]
    } else {
      table_matrix <- regression_results[[paste0(var, "_", coef_name)]]
    }
    
    # Convert the matrix to a data frame for xtable
    df_table <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
    
    # Format column names for LaTeX
    colnames(df_table) <- format_colnames(colnames(df_table))
    rownames(df_table) <- format_colnames(rownames(df_table))
    
    # Create xtable object
    xtable_obj <- xtable(df_table, caption = paste("Regression results for", var, "-", coef_name))
    
    # Write the LaTeX code to the file
    print(xtable_obj, file = file_conn, sanitize.text.function = identity, include.rownames = TRUE, caption.placement = "top")
    cat("\n\n", file = file_conn, append = TRUE)
  }
  
  close(file_conn)
  cat("Regression results for", var, "saved to", output_file, "\n")
}

# Create a LaTeX file for the intercept t-statistics
for (var in variables) {
  intercept_output_file <- paste0("tables/data/table4vol_", var, "_intercept_t_stats.tex")
  file_conn <- file(intercept_output_file, open = "wt")
  
  intercept_matrix <- intercept_t_stats[[paste0(var, "_(Intercept)_t_value")]]
  
  # Convert the matrix to a data frame for xtable
  df_intercept <- as.data.frame(intercept_matrix, stringsAsFactors = FALSE)
  
  # Format column names for LaTeX
  colnames(df_intercept) <- format_colnames(colnames(df_intercept))
  rownames(df_intercept) <- format_colnames(rownames(df_intercept))
  
  # Create xtable object
  xtable_obj <- xtable(df_intercept, caption = paste("Intercept t-statistics for", var))
  
  # Write the LaTeX code to the file
  print(xtable_obj, file = file_conn, sanitize.text.function = identity, include.rownames = TRUE, caption.placement = "top")
  cat("\n\n", file = file_conn, append = TRUE)
  
  close(file_conn)
  cat("Intercept t-statistics for", var, "saved to", intercept_output_file, "\n")
}
