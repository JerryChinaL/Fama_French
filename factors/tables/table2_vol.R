rm(list = ls())

library(dplyr)
library(tidyr)

min_date <- as.Date("1963-07-01")
max_date <- as.Date("2013-12-31")

max_date <- as.Date("2099-12-31")

# Load the data
factors <- readRDS("data/portfolios_w_return_vol.rds") %>%
  select(KYPERMNO, KYGVKEY, monthly_date = YYYYMM, PRIMEXCH, MTHRET, VOL, bm, op, inv)

mom_factors <- readRDS("data/mom_variables_vol.rds") %>%
  select(KYPERMNO = permno, YYYYMM = sort_date, mom = cum_ret, portfolio_mom = momentum_portfolio, MTHCAP)

volm_factors <- readRDS("../../ELM/data/ELM-portfolios_nordq2.rds") %>%
  select(KYPERMNO, YYYYMM = monthly_date, volm = VOLM, portfolio_volm) %>%
  filter(!is.na(volm) & !is.na(portfolio_volm)) %>%
  distinct()


# Define the winsorize function
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

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
    portfolio_VOL = assign_portfolio(
      data = pick(everything()),
      sorting_variable = VOL,
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
  select(KYPERMNO, YYYYMM = monthly_date, MTHRET, VOL, portfolio_VOL, portfolio_bm, portfolio_op, portfolio_inv)

# Convert YYYYMM to Date format
portfolios_5x5 <- portfolios_5x5 %>% mutate(YYYYMM = as.Date(paste0(YYYYMM, "01"), format = "%Y%m%d"))

portfolios_5x5 <- portfolios_5x5 %>%
  left_join(mom_factors, by = c("KYPERMNO", "YYYYMM")) %>%
  left_join(volm_factors, by = c("KYPERMNO", "YYYYMM"))

# Add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("data/monthly_rf.csv")
portfolios_5x5 <- portfolios_5x5 %>% 
  mutate(rf_date = format(as.Date(YYYYMM), "%Y%m")) %>%
  left_join(rf_data %>% mutate(rf_date = as.character(X)) %>% select(rf_date, RF), by = c("rf_date")) %>%
  select(-c(rf_date)) %>%
  mutate(MTHRET = MTHRET - (RF / 100))

# List of variables to loop through
variables <- c("bm", "op", "inv", "mom", "volm")

for (var in variables) {
  # Filter for non-NA values at the beginning of the loop
  portfolios_5x5_filtered <- portfolios_5x5 %>%
    filter(!is.na(!!sym(paste0("portfolio_", var))) & !is.na(VOL) & !is.na(MTHRET),
           YYYYMM >= min_date & YYYYMM <= max_date)
  
  # Conditionally apply winsorization outside mutate
  if (var == "volm") {
    portfolios_5x5_filtered <- portfolios_5x5_filtered %>%
      mutate(winsorized_VOL = winsorize(VOL))
  } else {
    portfolios_5x5_filtered <- portfolios_5x5_filtered %>%
      mutate(winsorized_VOL = VOL)
  }
  
  # Set the weights
  portfolios_5x5_filtered <- portfolios_5x5_filtered %>%
    mutate(VOL_weight = ifelse(is.na(winsorized_VOL), 0, winsorized_VOL))
  
  # Calculate the average monthly return for each quantile pair for each month
  monthly_grid <- portfolios_5x5_filtered %>%
    group_by(YYYYMM, portfolio_VOL, !!sym(paste0("portfolio_", var))) %>%
    summarize(avg_monthly_return = weighted.mean(MTHRET, VOL_weight, na.rm = TRUE), 
              .groups = "drop")
  # # Calculate the average monthly return for each quantile pair for each month
  # monthly_grid <- portfolios_5x5_filtered %>%
  #   group_by(YYYYMM, portfolio_VOL, !!sym(paste0("portfolio_", var))) %>%
  #   mutate(VOL_weight = ifelse(is.na(VOL), 0, VOL)) %>%
  #   summarize(avg_monthly_return = weighted.mean(MTHRET, VOL_weight, na.rm = TRUE), .groups = "drop")
  # 
  # Pivot the data to a wider format
  monthly_grid_wide <- monthly_grid %>%
    unite("quantile", portfolio_VOL, !!sym(paste0("portfolio_", var)), sep = "") %>%
    pivot_wider(names_from = quantile, values_from = avg_monthly_return) %>%
    rename_with(~ paste0("f", .), -YYYYMM)
  
  # Sort the columns by name
  sorted_columns <- c("YYYYMM", sort(names(monthly_grid_wide)[-1]))
  monthly_grid_wide <- monthly_grid_wide %>% 
    select(all_of(sorted_columns))
  
  # Calculate the average of each column to get the final grid
  final_grid <- monthly_grid_wide %>%
    summarize(across(-YYYYMM, \(x) mean(x, na.rm = TRUE)))
  
  # Calculate the t-statistics for each column
  t_stats <- monthly_grid_wide %>%
    summarize(across(-YYYYMM, ~ {
      if(length(na.omit(.x)) < 2) return(NA)
      t.test(.x)$statistic
    }))
  
  # Convert the final grid to a 5x5 matrix
  final_matrix <- matrix(
    as.numeric(final_grid[1, ]),
    nrow = 5, ncol = 5, byrow = TRUE,
    dimnames = list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
  )
  
  t_stat_matrix <- matrix(
    as.numeric(t_stats[1, ]),
    nrow = 5, ncol = 5, byrow = TRUE,
    dimnames = list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
  )
  
  print("5x5 grid:")
  print(100 * final_matrix)
  
  # Print t-statistics
  print("tstat:")
  print(t_stat_matrix)
  
  # Save the files
  write.csv(format(as.data.frame(monthly_grid_wide), scientific = FALSE), paste0("tables/data/table2_5x5_monthly_VOL_", var, ".csv"), row.names = FALSE)
  write.csv(format(as.data.frame(100 * final_matrix), scientific = FALSE), paste0("tables/data/table2_5x5_VOL_", var, ".csv"), row.names = TRUE)
  write.csv(format(as.data.frame(t_stat_matrix), scientific = FALSE), paste0("tables/data/table2_5x5_tstat_VOL_", var, ".csv"), row.names = TRUE)
}

# Function to mark bold if significant
mark_bold <- function(means, tstats, alpha) {
  result_means <- matrix("", nrow=nrow(means), ncol=ncol(means))
  result_tstats <- matrix("", nrow=nrow(means), ncol=ncol(means))
  for (i in 1:nrow(means)) {
    for (j in 1:ncol(means)) {
      if (!is.na(tstats[i, j]) && abs(tstats[i, j]) > 1.96) {
        result_means[i, j] <- paste0("\\textbf{", sprintf("%.2f", means[i, j]), "}")
        result_tstats[i, j] <- paste0("\\textbf{", sprintf("%.2f", tstats[i, j]), "}")
      } else {
        result_means[i, j] <- sprintf("%.2f", means[i, j])
        result_tstats[i, j] <- sprintf("%.2f", tstats[i, j])
      }
    }
  }
  return(list(means=result_means, tstats=result_tstats))
}

# Initialize storage for the LaTeX table sections
latex_sections <- ""

for (var in variables) {
  means <- read.csv(paste0("tables/data/table2_5x5_VOL_", var, ".csv"), row.names = 1)
  tstats <- read.csv(paste0("tables/data/table2_5x5_tstat_VOL_", var, ".csv"), row.names = 1)
  
  # Mark bold significant values
  panel <- mark_bold(as.matrix(means), as.matrix(tstats), alpha = 0.05)
  
  # Create the LaTeX section for the current variable
  latex_section <- paste0("
 \\multicolumn{4}{l}{Panel VOL - ", toupper(var), " Sorts} & & & & & & & \\\\
 Illiquid &", paste(panel$means[1,], collapse=" & "), "&", paste(panel$tstats[1,], collapse=" & "), "\\\\
 2 &", paste(panel$means[2,], collapse=" & "), "&", paste(panel$tstats[2,], collapse=" & "), "\\\\
 3 &", paste(panel$means[3,], collapse=" & "), "&", paste(panel$tstats[3,], collapse=" & "), "\\\\
 4 &", paste(panel$means[4,], collapse=" & "), "&", paste(panel$tstats[4,], collapse=" & "), "\\\\
 Liquid &", paste(panel$means[5,], collapse=" & "), "&", paste(panel$tstats[5,], collapse=" & "), "\\\\
")
  
  # Append the section to the overall LaTeX content
  latex_sections <- paste0(latex_sections, latex_section)
}

# Create the LaTeX code for the entire table
output <- paste0("


\\begin{tabular}{p{1.8cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1.2cm} p{1cm}}
 \\hline
  & Low & 2 & 3 & 4 & High & Low & 2 & 3 & 4 & High \\\\
 \\multicolumn{5}{c}{Arithmetic Mean} & \\multicolumn{5}{c}{T Stat} \\\\
 \\hline", latex_sections, "
 \\hline
\\end{tabular}

")

# Print the output
cat(output)
