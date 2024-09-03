rm(list = ls())

library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)

# Load your data
data <- readRDS("../../old data/sfz_agg_mth.rds") %>%
  filter(PRIMEXCH %in% c("N", "A", "Q"), SHRCD %in% c(10,11)) %>%
  mutate(MTHCAP = MTHVOL * MTHPRC) %>%
  select(KYPERMNO, MCALDT, YYYYMM, MTHCAP, MTHRET, PRIMEXCH) %>%
  mutate(year_month = floor_date(MCALDT, unit = "month")) %>%
  unique()

# Create a complete sequence of year_month for each stock
complete_dates <- data %>%
  select(KYPERMNO, year_month) %>%
  group_by(KYPERMNO) %>%
  complete(year_month = seq.Date(min(year_month), max(year_month), by = "month")) %>%
  ungroup()

# Merge the complete sequence with the original data to fill missing months with NA
data_complete <- complete_dates %>%
  left_join(data, by = c("KYPERMNO", "year_month")) %>%
  arrange(KYPERMNO, year_month)

# Add sort_date column as the floored year_month + 1 month
data_mthcap <- data_complete %>%
  select(KYPERMNO, year_month, MTHCAP) %>%
  mutate(year_month = year_month + months(1))

# Define a function to calculate 11-month cumulative return with valid date check
calc_cum_return <- function(returns) {
  cum_returns <- rollapply(returns, width = 11, 
                           FUN = function(x) {
                             if (sum(!is.na(x)) >= 10) {
                               return(prod(1 + na.omit(x)) - 1)
                             } else {
                               return(NA)
                             }
                           }, fill = NA, align = "right")
  return(cum_returns)
}

filtered_data_complete <- data_complete %>%
  filter(KYPERMNO == 10014)

# Apply the function to calculate cumulative returns
data_complete <- data_complete %>%
  group_by(KYPERMNO) %>%
  arrange(year_month) %>%
  mutate(
    cum_11_month_return = calc_cum_return(lag(MTHRET, n = 2))  # Lag the returns by 1 month before rolling
  ) %>%
  ungroup()

data_complete <- data_complete %>%
  select(KYPERMNO, year_month, cum_11_month_return, MTHRET, PRIMEXCH, YYYYMM) %>%
  left_join(data_mthcap, by = c("KYPERMNO", "year_month")) %>%
  mutate(sort_date = year_month)

# Assign stocks to momentum portfolios (High, Neutral, Low)
assign_momentum_portfolio <- function(data) {
  # Ensure the vector used for quantiles does not contain NAs and is sorted
  cum_returns_sorted <- data %>%
    filter(PRIMEXCH == "N") %>%
    pull(cum_11_month_return) %>%
    na.omit() %>%
    sort()
  
  breakpoints <- quantile(cum_returns_sorted, probs = c(0, 0.3, 0.7, 1), names = FALSE)
  
  assigned_portfolios <- findInterval(data$cum_11_month_return, breakpoints, all.inside = TRUE)
  
  return(assigned_portfolios)
}

# Assign stocks to size portfolios (Small, Big)
assign_size_portfolio <- function(data) {
  size_sorted <- data %>%
    filter(PRIMEXCH == "N") %>%
    pull(MTHCAP) %>%
    na.omit() %>%
    sort()
  
  breakpoint <- quantile(size_sorted, probs = 0.5, na.rm = TRUE)
  
  assigned_portfolios <- ifelse(data$MTHCAP <= breakpoint, "Small", "Big")
  
  return(assigned_portfolios)
}

# 
# filtered_data_complete <- data_complete %>%
#   filter(!is.na(cum_11_month_return))

# Apply the portfolio assignment using sort_date for monthly rebalancing
data_complete <- data_complete %>%
  filter(!is.na(cum_11_month_return) & !is.na(MTHCAP)) %>%
  group_by(sort_date) %>%
  mutate(
    momentum_portfolio = assign_momentum_portfolio(pick(everything())),
    size_portfolio = assign_size_portfolio(pick(everything()))
  ) %>%
  ungroup() %>%
  select(permno = KYPERMNO, sort_date, cum_ret = cum_11_month_return,  momentum_portfolio, size_portfolio, MTHRET, MTHCAP) %>%
  mutate(YYYYMM = format(sort_date, "%Y%m"))

saveRDS(data_complete, "data/mom_variables_vol.rds")

# Calculate the momentum factor according to the provided formula using weighted mean
momentum_factors_permno <- data_complete %>%
  group_by(YYYYMM) %>%
  summarize(
    Small_High = weighted.mean(MTHRET[momentum_portfolio == 3 & size_portfolio == "Small"], MTHCAP[momentum_portfolio == 3 & size_portfolio == "Small"], na.rm = TRUE),
    Big_High = weighted.mean(MTHRET[momentum_portfolio == 3 & size_portfolio == "Big"], MTHCAP[momentum_portfolio == 3 & size_portfolio == "Big"], na.rm = TRUE),
    Small_Low = weighted.mean(MTHRET[momentum_portfolio == 1 & size_portfolio == "Small"], MTHCAP[momentum_portfolio == 1 & size_portfolio == "Small"], na.rm = TRUE),
    Big_Low = weighted.mean(MTHRET[momentum_portfolio == 1 & size_portfolio == "Big"], MTHCAP[momentum_portfolio == 1 & size_portfolio == "Big"], na.rm = TRUE)
  ) %>%
  mutate(
    MOM = 0.5 * (Small_High + Big_High) - 0.5 * (Small_Low + Big_Low)
  ) %>%
  ungroup() %>%
  select(YYYYMM, MOM)

# View the momentum factor
print(head(momentum_factors_permno))

# Save the momentum factor to a CSV file if needed
write.csv(momentum_factors_permno, "data/momentum_factor_permno_vol.csv", row.names = FALSE)
