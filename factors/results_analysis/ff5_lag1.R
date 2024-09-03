library(dplyr)

mthret <- read.csv("data/mthret.csv")
exch <- readRDS("../mkt_cap/data/sfz_agg_mth_short.rds") %>% 
  select(KYPERMNO, YYYYMM, PRIMEXCH)
primiss_files <- c("6070", "7078", "7885", "8590", "9095", "9500", "0004", "0408", "p0812", "p1216","p1620","p2023")
primiss_files <- paste0("primiss/", primiss_files, "_ms.xlsx")
primiss <- bind_rows(lapply(primiss_files, read_excel))

primiss <- primiss %>%
  mutate(
    DATADATE = as.Date(as.character(DATADATE), "%Y-%m-%d"),
    YYYYMM = as.numeric(format(DATADATE, "%Y%m"))
  ) %>% 
  filter(PRIMISS == 'P') %>% 
  select(KYPERMNO, YYYYMM) %>% 
  distinct()

mthret <- mthret %>%
  left_join(exch, by = c("KYPERMNO", "YYYYMM")) %>%
  filter(PRIMEXCH == "N" | PRIMEXCH == "A" | PRIMEXCH == "Q") %>%
  inner_join(primiss, by = c("KYPERMNO", "YYYYMM")) %>% # use primary stock
  mutate(
    year = floor(YYYYMM / 100),
    month = round(YYYYMM %% 100),
    sort_date = case_when(
      month <= 6 ~ paste0(year - 1, "-07-01"),
      month >= 7 ~ paste0(year, "-07-01")
    )
  ) %>%
  select(KYPERMNO, KYGVKEY, sort_date, MTHRET, return_date = MCALDT, PRIMEXCH, YYYYMM)

permno_data <- mthret %>%
  filter(round(YYYYMM %% 100) == 7)

# Load your data
final_df <- readRDS("data/four_factors.rds") %>%
  filter(!is.na(SIZE))

final_df_extended <- permno_data %>%
  select(-c("MTHRET")) %>%
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
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, KYGVKEY, sort_date, return_date, portfolio_size, portfolio_bm,portfolio_op, portfolio_inv, SIZE)

portfolios_w_return <- mthret %>%
  select(KYPERMNO, KYGVKEY, sort_date, MTHRET, YYYYMM) %>%
  left_join(portfolios, by = c("KYPERMNO", "KYGVKEY", "sort_date")) %>%
  select(KYPERMNO, YYYYMM, MTHRET, portfolio_size, portfolio_bm, portfolio_op, portfolio_inv, SIZE)

# add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("../monthly_rf.csv")
portfolios_w_return <- portfolios_w_return %>% 
  left_join(rf_data %>% mutate(YYYYMM = X) %>% select(YYYYMM, RF), by = c("YYYYMM")) %>%
  filter(!is.na(SIZE)) %>%
  filter(!is.na(portfolio_bm) & !is.na(portfolio_size) & !is.na(portfolio_op) & !is.na(portfolio_inv)) %>%
  mutate(excess_return = MTHRET - (RF/100))

# Calculate the factors according to the provided formula using weighted mean with SIZE
factors_replicated <- portfolios_w_return %>%
  group_by(YYYYMM) %>%
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
  select(YYYYMM, SMB, HML, RMW, CMA)

# View the replicated factors
print(head(factors_replicated))

# Save the factors to a CSV file if needed
write.csv(factors_replicated, "data/ff5.csv", row.names = FALSE)
