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

n_stocks <- mthret %>%
  select(KYPERMNO, KYGVKEY, YYYYMM) %>%
  distinct() %>%
  group_by(KYGVKEY, YYYYMM) %>%
  summarize(n_stocks = n()) %>%
  ungroup()

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

# Calculate the number of stocks (n) for each company
final_df <- final_df %>%
  group_by(KYGVKEY, sort_date) %>%
  mutate(n_stocks = n()) %>%
  ungroup()

# Adjust SIZE by dividing by the number of stocks (n)
final_df <- final_df %>%
  mutate(adjusted_SIZE = SIZE / n_stocks)

View(final_df %>% filter(n_stocks > 1))

View(final_df %>% filter(KYGVKEY == 2176))

final_df_extended <- permno_data %>%
  select(-c("MTHRET")) %>%
  left_join(final_df, by = c("KYGVKEY", "sort_date"))


assign_portfolio <- function(data, sorting_variable, percentiles) {
  # Calculate breakpoints at the company level
  breakpoints <- data %>%
    filter(PRIMEXCH == "N") %>%
    filter(!is.na({{ sorting_variable }})) %>%
    group_by(KYGVKEY) %>%
    summarize(mean_sorting_variable = mean({{ sorting_variable }}, na.rm = TRUE)) %>%
    ungroup() %>%
    pull(mean_sorting_variable) %>%
    quantile(probs = percentiles, na.rm = TRUE, names = FALSE)
  
  # Assign portfolios based on breakpoints
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
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, KYGVKEY, sort_date, return_date, portfolio_size, portfolio_bm, SIZE)

portfolios_w_return <- mthret %>%
  select(KYPERMNO, KYGVKEY, sort_date, MTHRET, YYYYMM) %>%
  left_join(portfolios, by = c("KYPERMNO", "KYGVKEY", "sort_date")) %>%
  select(KYPERMNO, KYGVKEY, YYYYMM, MTHRET, portfolio_size, portfolio_bm, SIZE)

# add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("../monthly_rf.csv")
portfolios_w_return <- portfolios_w_return %>% 
  left_join(rf_data %>% mutate(YYYYMM = X) %>% select(YYYYMM, RF), by = c("YYYYMM")) %>%
  filter(!is.na(SIZE)) %>%
  filter(!is.na(portfolio_bm) & !is.na(portfolio_size)) %>%
  mutate(excess_return = MTHRET - (RF/100))

portfolios_w_return <- portfolios_w_return %>%
  left_join(n_stocks, by = c("KYGVKEY", "YYYYMM")) %>%
  mutate(adjusted_SIZE = SIZE / n_stocks)

# Calculate the factors
factors_replicated <- portfolios_w_return %>%
  group_by(portfolio_size, portfolio_bm, YYYYMM) %>%
  summarize(
    ret = weighted.mean(excess_return, adjusted_SIZE, na.rm = TRUE), .groups = "drop"
  ) %>%
  group_by(YYYYMM) %>%
  summarize(
    smb_replicated = mean(ret[portfolio_size == 1], na.rm = TRUE) -
      mean(ret[portfolio_size == 2], na.rm = TRUE),
    hml_replicated = mean(ret[portfolio_bm == 3], na.rm = TRUE) -
      mean(ret[portfolio_bm == 1], na.rm = TRUE)
  ) %>%
  mutate(smb_replicated = smb_replicated * 100,
         hml_replicated = hml_replicated * 100)

# View the replicated factors
print(head(factors_replicated))

# Save the factors to a CSV file if needed
write.csv(factors_replicated, "data/ff3_d.csv", row.names = FALSE)
