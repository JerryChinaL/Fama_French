library(dplyr)
library(tidyr)

# Load your data
factor_df <- read.csv("data/four_factors_excret.csv")

factor_df <- factor_df %>%
  mutate(return_date = as.Date(return_date)) %>%
  filter(return_date >= as.Date("1963-07-01") & return_date <= as.Date("2013-12-31")) %>%
  filter(PRIMEXCH == "N" | PRIMEXCH == "Q" | PRIMEXCH == "A") %>%
  mutate(
    MTHRET = as.numeric(excess_return), # To fix a bug, MTHRET is actually excess_return, not raw return
    YYYYMM = format(as.Date(return_date), "%Y%m")
  )

# Ensure the necessary columns are numeric and remove NA rows for SIZE and bm
final_df <- factor_df %>%
  filter(!is.na(SIZE) & !is.na(bm) & !is.na(op) & !is.na(inv))

# Define the function to assign portfolios
assign_portfolio <- function(data, sorting_variable, percentiles) {
  breakpoints <- data %>%
    filter(PRIMEXCH == "N") %>%
    filter(!is.na({{ sorting_variable }})) %>%
    pull({{ sorting_variable }}) %>%
    quantile(probs = percentiles, na.rm = TRUE, names = FALSE)
  
  assigned_portfolios <- findInterval(data[[deparse(substitute(sorting_variable))]], breakpoints, all.inside = TRUE)
  
  return(assigned_portfolios)
}

# Apply the portfolio assignment
portfolio <- final_df %>%
  group_by(sort_date) %>%
  mutate(
    size_quantile = assign_portfolio(
      data = pick(everything()),
      sorting_variable = SIZE,
      percentiles = seq(0, 1, by = 0.2)
    ),
    bm_quantile = assign_portfolio(
      data = pick(everything()),
      sorting_variable = bm,
      percentiles = seq(0, 1, by = 0.2)
    ),
    op_quantile = assign_portfolio(
      data = pick(everything()),
      sorting_variable = op,
      percentiles = seq(0, 1, by = 0.2)
    ),
    inv_quantile = assign_portfolio(
      data = pick(everything()),
      sorting_variable = inv,
      percentiles = seq(0, 1, by = 0.2)
    )
  ) %>%
  ungroup() %>%
  mutate(YYYYMM = format(as.Date(return_date), "%Y%m"))


# List of variables to loop through
variables <- c("bm", "op", "inv")

for (var in variables) {
  # Calculate the average monthly return for each quantile pair for each month
  monthly_grid <- portfolio %>%
    group_by(YYYYMM, size_quantile, !!sym(paste0(var, "_quantile"))) %>%
    # summarize(avg_monthly_return = mean(MTHRET, na.rm = TRUE), .groups = "drop")
    summarize(avg_monthly_return = weighted.mean(MTHRET, SIZE, na.rm = TRUE), .groups = "drop")
  
  # Pivot the data to a wider format
  monthly_grid_wide <- monthly_grid %>%
    unite("quantile", size_quantile, !!sym(paste0(var, "_quantile")), sep = "") %>%
    pivot_wider(names_from = quantile, values_from = avg_monthly_return) %>%
    rename_with(~ paste0("f", .), -YYYYMM)
  
  # Sort the columns by name
  sorted_columns <- c("YYYYMM", sort(names(monthly_grid_wide)[-1]))
  monthly_grid_wide <- monthly_grid_wide %>% 
    select(all_of(sorted_columns))
  
  # Fill missing values with NA (if any)
  monthly_grid_wide[is.na(monthly_grid_wide)] <- NA
  
  # Calculate the average of each column to get the final grid
  final_grid <- monthly_grid_wide %>%
    summarize(across(-YYYYMM, mean, na.rm = TRUE))
  
  # Convert the final grid to a 5x5 matrix
  final_matrix <- matrix(
    as.numeric(final_grid[1, ]),
    nrow = 5, ncol = 5, byrow = TRUE,
    dimnames = list(paste0("SIZE_", 1:5), paste0(var, "_", 1:5))
  )
  
  print(100 * final_matrix)
  
  # # Save the files
  write.csv(format(as.data.frame(monthly_grid_wide), scientific = FALSE), paste0("data/table1/5x5_monthly_size_", var, ".csv"), row.names = FALSE)
  write.csv(format(as.data.frame(100 * final_matrix), scientific = FALSE), paste0("data/table1/5x5_size_", var, ".csv"), row.names = TRUE)
}
