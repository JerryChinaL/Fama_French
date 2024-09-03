library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)

final_df <- read.csv("data/four_factors_excret.csv") %>% 
  select(KYPERMNO, KYGVKEY, MTHRET, PRIMEXCH, return_date) %>% 
  mutate(year_month = floor_date(as.Date(return_date), unit = "month")) %>%
  unique()

mktcap <- read.csv("../mkt_cap/mktcap_combined.csv") %>%
  mutate(SIZE = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)) %>%
  filter(!is.na(SIZE) & SIZE != 0) %>%
  select(KYGVKEY, YYYYMM, SIZE) %>%
  mutate(year_month = as.Date(paste0(YYYYMM, "01"), format = "%Y%m%d") %m+% months(1)) %>%
   group_by(KYGVKEY, year_month) %>%
  summarize(SIZE = mean(SIZE, na.rm = TRUE)) %>%
  ungroup()

# final_df <- final_df %>%
#   select(KYPERMNO, KYGVKEY, MTHRET, PRIMEXCH, return_date) %>%
#   unique() %>%
#   left_join(mktcap %>% select(KYGVKEY, YYYYMM, SIZE), by = c("KYPERMNO", "return_date")) %>%
#   filter(!is.na(SIZE) & !is.na(bm) & !is.na(op) & !is.na(inv))

# Create a complete sequence of year_month for each stock
complete_dates <- final_df %>%
  select(KYPERMNO, year_month) %>%
  group_by(KYPERMNO) %>%
  complete(year_month = seq.Date(min(year_month), max(year_month), by = "month")) %>%
  ungroup()

# Merge the complete sequence with the original data to fill missing months with NA
data_complete <- complete_dates %>%
  left_join(final_df, by = c("KYPERMNO", "year_month")) %>%
  arrange(KYPERMNO, year_month)

# Define a function to calculate 11-month cumulative return with valid date check
calc_cum_return <- function(returns) {
  cum_returns <- rollapply(returns, width = 11, 
                           FUN = function(x) {
                             if (sum(!is.na(x)) >= 9) {
                               return(prod(1 + na.omit(x)) - 1)
                             } else {
                               return(NA)
                             }
                           }, fill = NA, align = "right")
  return(cum_returns)
}


# Apply the function to calculate cumulative returns
data_cum <- data_complete %>%
  group_by(KYPERMNO) %>%
  arrange(year_month) %>%
  mutate(
    cum_11_month_return = calc_cum_return(lag(MTHRET, n = 2))  # Lag the returns by 1 month before rolling
  ) %>%
  ungroup()

data_merged <- data_cum %>%
  select(KYPERMNO, KYGVKEY, year_month, cum_11_month_return, MTHRET, PRIMEXCH) %>%
  left_join(mktcap, by = c("KYGVKEY", "year_month")) %>%
  mutate(sort_date = year_month)

####################################################################################################


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
    pull(SIZE) %>%
    na.omit() %>%
    sort()
  
  breakpoint <- quantile(size_sorted, probs = 0.5, na.rm = TRUE)
  
  assigned_portfolios <- ifelse(data$SIZE <= breakpoint, "Small", "Big")
  
  return(assigned_portfolios)
}

# filtered_data_complete <- data_complete %>%
#   filter(KYPERMNO == 10014)
# 
# filtered_data_complete <- data_complete %>%
#   filter(!is.na(cum_11_month_return))

# Apply the portfolio assignment using sort_date for monthly rebalancing
data_merged <- data_merged %>%
  filter(!is.na(cum_11_month_return) & !is.na(SIZE)) %>%
  group_by(sort_date) %>%
  mutate(
    momentum_portfolio = assign_momentum_portfolio(pick(everything())),
    size_portfolio = assign_size_portfolio(pick(everything()))
  ) %>%
  ungroup() %>%
  select(permno = KYPERMNO, sort_date, momentum_portfolio, size_portfolio, MTHRET, SIZE) %>%
  mutate(YYYYMM = format(sort_date, "%Y%m"))

# Calculate the momentum factor according to the provided formula using weighted mean
momentum_factors_gvkey <- data_merged %>%
  group_by(YYYYMM) %>%
  summarize(
    Small_High = weighted.mean(MTHRET[momentum_portfolio == 3 & size_portfolio == "Small"], SIZE[momentum_portfolio == 3 & size_portfolio == "Small"], na.rm = TRUE),
    Big_High = weighted.mean(MTHRET[momentum_portfolio == 3 & size_portfolio == "Big"], SIZE[momentum_portfolio == 3 & size_portfolio == "Big"], na.rm = TRUE),
    Small_Low = weighted.mean(MTHRET[momentum_portfolio == 1 & size_portfolio == "Small"], SIZE[momentum_portfolio == 1 & size_portfolio == "Small"], na.rm = TRUE),
    Big_Low = weighted.mean(MTHRET[momentum_portfolio == 1 & size_portfolio == "Big"], SIZE[momentum_portfolio == 1 & size_portfolio == "Big"], na.rm = TRUE)
  ) %>%
  mutate(
    MOM = 0.5 * (Small_High + Big_High) - 0.5 * (Small_Low + Big_Low)
  ) %>%
  ungroup() %>%
  select(YYYYMM, MOM)

# View the momentum factor
print(head(momentum_factors_gvkey))

# Save the momentum factor to a CSV file if needed
write.csv(momentum_factors_gvkey, "data/momentum_factor_gvkey.csv", row.names = FALSE)



