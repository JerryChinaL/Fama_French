rm(list=ls())
library(dplyr)
library(readxl)
library(lubridate)

primiss <- readRDS("../four_factor_combined/data/primiss.rds") %>%
  mutate(monthly_date = floor_date(DATADATE, "month")) %>%
  filter(PRIMISS == "P") %>%
  distinct()

mkt_data <- readRDS("../../ELM/data/sfz_agg_mth_short.rds") %>%
  mutate(monthly_date = floor_date(MCALDT, "month"),
         VOL = MTHVOL * MTHPRC) %>%
  filter(PRIMEXCH %in% c("N", "A", "Q"))

# Return should be for this month, but VOL, as a factor, should be from last month
mkt_data <- mkt_data %>%
  select(VOL, KYPERMNO, monthly_date) %>%
  mutate(monthly_date = monthly_date + months(1)) %>%
  right_join(mkt_data %>% select(-VOL), by = c("KYPERMNO", "monthly_date")) %>%
  select(PRIMEXCH, VOL, MTHRET, monthly_date, KYPERMNO) %>%
  inner_join(primiss %>% select(monthly_date, KYGVKEY, KYPERMNO) %>% distinct(), by = c("monthly_date", "KYPERMNO"))

mkt_data <- mkt_data %>%
  mutate(
    year = year(monthly_date),
    month = month(monthly_date),
    sort_date = case_when(
      month <= 6 ~ paste0(year - 1, "-07-01"),
      month >= 7 ~ paste0(year, "-07-01")
    )
  ) %>%
  select(KYPERMNO, KYGVKEY, monthly_date, sort_date, MTHRET, PRIMEXCH, VOL)

mkt_data_july <- mkt_data %>%
  filter(month(monthly_date) == 7) %>%
  select(KYGVKEY, KYPERMNO, sort_date, PRIMEXCH, VOL, monthly_date)

# Load your data
final_df <- readRDS("data/four_factors.rds")

final_df_extended <- mkt_data_july %>%
  mutate(sort_date = as.Date(sort_date)) %>%
  left_join(final_df, by = c("KYGVKEY", "sort_date"))

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
portfolios <- final_df_extended %>%
  group_by(sort_date) %>%
  mutate(
    portfolio_size = assign_portfolio(
      data = pick(everything()),
      sorting_variable = SIZE,
      percentiles = c(0, 0.5, 1)
    ),
    portfolio_bm = assign_portfolio(
      data = pick(everything()),
      sorting_variable = bm,
      percentiles = c(0, 0.3, 0.7, 1)
    ),
    portfolio_op = assign_portfolio(
      data = pick(everything()),
      sorting_variable = op,
      percentiles = c(0, 0.3, 0.7, 1)
    ),
    portfolio_inv = assign_portfolio(
      data = pick(everything()),
      sorting_variable = inv,
      percentiles = c(0, 0.3, 0.7, 1)
    ),
    portfolio_vol = assign_portfolio(
      data = pick(everything()),
      sorting_variable = VOL,
      percentiles = c(0, 0.5, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, KYGVKEY, sort_date, portfolio_size, portfolio_bm, portfolio_op, portfolio_inv, portfolio_vol, bm, op, inv, VOL, SIZE)

portfolios_w_return <- mkt_data %>%
  select(-VOL) %>%
  mutate(sort_date = as.Date(sort_date)) %>%
  left_join(portfolios, by = c("KYPERMNO", "KYGVKEY", "sort_date"))

# add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("../monthly_rf.csv")
portfolios_w_return <- portfolios_w_return %>%
  left_join(rf_data %>% mutate(YYYYMM = ymd(paste0(X, "01"))) %>% select(YYYYMM, RF), by = c("monthly_date" = "YYYYMM")) %>%
  filter(!is.na(SIZE)) %>%
  # filter(!is.na(portfolio_bm) & !is.na(portfolio_size) & !is.na(portfolio_op) & !is.na(portfolio_inv)) %>%
  mutate(excess_return = MTHRET - (RF/100))

portfolios_w_return <- portfolios_w_return %>%
  mutate(SIZE = VOL, portfolio_size = portfolio_vol)

saveRDS(portfolios_w_return, "data/portfolios_w_return.rds")

# Calculate the factors according to the provided formula using weighted mean with SIZE
factors_replicated <- portfolios_w_return %>%
  group_by(monthly_date) %>%
  mutate(SIZE = ifelse(is.na(SIZE), 0, SIZE)) %>%
  summarize(
    SH = weighted.mean(excess_return[portfolio_size == 1 & portfolio_bm == 3], SIZE[portfolio_size == 1 & portfolio_bm == 3], na.rm = TRUE),  # Small size, High B/M
    SN_bm = weighted.mean(excess_return[portfolio_size == 1 & portfolio_bm == 2], SIZE[portfolio_size == 1 & portfolio_bm == 2], na.rm = TRUE),  # Small size, Neutral B/M
    SL = weighted.mean(excess_return[portfolio_size == 1 & portfolio_bm == 1], SIZE[portfolio_size == 1 & portfolio_bm == 1], na.rm = TRUE),  # Small size, Low B/M
    BH = weighted.mean(excess_return[portfolio_size == 2 & portfolio_bm == 3], SIZE[portfolio_size == 2 & portfolio_bm == 3], na.rm = TRUE),  # Big size, High B/M
    BN_bm = weighted.mean(excess_return[portfolio_size == 2 & portfolio_bm == 2], SIZE[portfolio_size == 2 & portfolio_bm == 2], na.rm = TRUE),  # Big size, Neutral B/M
    BL = weighted.mean(excess_return[portfolio_size == 2 & portfolio_bm == 1], SIZE[portfolio_size == 2 & portfolio_bm == 1], na.rm = TRUE),  # Big size, Low B/M
    
    SR = weighted.mean(excess_return[portfolio_size == 1 & portfolio_op == 3], SIZE[portfolio_size == 1 & portfolio_op == 3], na.rm = TRUE),  # Small size, High OP
    SN_op = weighted.mean(excess_return[portfolio_size == 1 & portfolio_op == 2], SIZE[portfolio_size == 1 & portfolio_op == 2], na.rm = TRUE),  # Small size, Neutral OP
    SW = weighted.mean(excess_return[portfolio_size == 1 & portfolio_op == 1], SIZE[portfolio_size == 1 & portfolio_op == 1], na.rm = TRUE),  # Small size, Low OP
    BR = weighted.mean(excess_return[portfolio_size == 2 & portfolio_op == 3], SIZE[portfolio_size == 2 & portfolio_op == 3], na.rm = TRUE),  # Big size, High OP
    BN_op = weighted.mean(excess_return[portfolio_size == 2 & portfolio_op == 2], SIZE[portfolio_size == 2 & portfolio_op == 2], na.rm = TRUE),  # Big size, Neutral OP
    BW = weighted.mean(excess_return[portfolio_size == 2 & portfolio_op == 1], SIZE[portfolio_size == 2 & portfolio_op == 1], na.rm = TRUE),  # Big size, Low OP
    
    SC = weighted.mean(excess_return[portfolio_size == 1 & portfolio_inv == 1], SIZE[portfolio_size == 1 & portfolio_inv == 1], na.rm = TRUE),  # Small size, Low INV
    SN_inv = weighted.mean(excess_return[portfolio_size == 1 & portfolio_inv == 2], SIZE[portfolio_size == 1 & portfolio_inv == 2], na.rm = TRUE),  # Small size, Neutral INV
    SA = weighted.mean(excess_return[portfolio_size == 1 & portfolio_inv == 3], SIZE[portfolio_size == 1 & portfolio_inv == 3], na.rm = TRUE),  # Small size, High INV
    BC = weighted.mean(excess_return[portfolio_size == 2 & portfolio_inv == 1], SIZE[portfolio_size == 2 & portfolio_inv == 1], na.rm = TRUE),  # Big size, Low INV
    BN_inv = weighted.mean(excess_return[portfolio_size == 2 & portfolio_inv == 2], SIZE[portfolio_size == 2 & portfolio_inv == 2], na.rm = TRUE),  # Big size, Neutral INV
    BA = weighted.mean(excess_return[portfolio_size == 2 & portfolio_inv == 3], SIZE[portfolio_size == 2 & portfolio_inv == 3], na.rm = TRUE)  # Big size, High INV
  ) %>%
  mutate(
    SMB_bm = (SH + SN_bm + SL)/3 - (BH + BN_bm + BL)/3,
    SMB_op = (SR + SN_op + SW)/3 - (BR + BN_op + BW)/3,
    SMB_inv = (SC + SN_inv + SA)/3 - (BC + BN_inv + BA)/3,
    SMB = (SMB_bm + SMB_op + SMB_inv)/3,
    HML = ((SH + BH)/2 - (SL + BL)/2),
    RMW = ((SR + BR)/2 - (SW + BW)/2),
    CMA = ((SC + BC)/2 - (SA + BA)/2)
  ) %>%
  ungroup() %>%
  mutate(
    SMB = SMB * 100,
    HML = HML * 100,
    RMW = RMW * 100,
    CMA = CMA * 100
  ) %>%
  select(YYYYMM = monthly_date, SMB, HML, RMW, CMA, SMB_bm, SMB_op, SMB_inv)

# View the replicated factors
print(head(factors_replicated))

# Save the factors to a CSV file if needed
write.csv(factors_replicated, "data/ff5_vol_fixed.csv", row.names = FALSE)
