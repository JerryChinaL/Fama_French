

library(dplyr)
library(ggplot2)

# Load the data
factors <- readRDS("data/portfolios_w_return.rds") %>%
  select(KYPERMNO, KYGVKEY, monthly_date = return_date, PRIMEXCH, MTHRET, SIZE, bm, op, inv)

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
    portfolio_SIZE = assign_portfolio(
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
  select(KYPERMNO, YYYYMM = monthly_date, MTHRET, SIZE, portfolio_SIZE, portfolio_bm, portfolio_op, portfolio_inv)


# Add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("data/monthly_rf.csv")
portfolios_5x5 <- portfolios_5x5 %>% 
  mutate(rf_date = format(as.Date(YYYYMM), "%Y%m")) %>%
  left_join(rf_data %>% mutate(rf_date = as.character(X)) %>% select(rf_date, RF), by = c("rf_date")) %>%
  select(-c(rf_date)) %>%
  mutate(MTHRET = MTHRET - (RF / 100))

# Filter for the specific portfolios and calculate the weighted mean return for each month
filtered_returns <- portfolios_5x5 %>%
  filter(portfolio_SIZE == 1, portfolio_bm == 5) %>%
  mutate(SIZE = ifelse(is.na(SIZE), 0, SIZE)) %>%
  group_by(YYYYMM) %>%
  summarize(weighted_avg_monthly_return = weighted.mean(MTHRET, SIZE, na.rm = TRUE)) %>%
  ungroup()

# save to xlsx file
library(openxlsx)
write.xlsx(filtered_returns, "../../size1bm5_returns.xlsx")

# Calculate cumulative returns
filtered_returns <- filtered_returns %>%
  arrange(YYYYMM) %>%
  mutate(cumulative_return = cumprod(1 + weighted_avg_monthly_return))

# Plot the cumulative return time series
ggplot(filtered_returns, aes(x = YYYYMM, y = cumulative_return)) +
  geom_line() +
  labs(title = "Cumulative Return Time Series for Portfolio (SIZE = 1, bm = 5)",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal()

# Print the time series data
print(filtered_returns)
