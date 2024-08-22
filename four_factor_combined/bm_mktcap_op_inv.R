rm(list=ls())
##############
# This file joins the be_op_inv output with the mktcap data to calculate the SIZE and bm factor.
# It also joins with the mthret and rf-return data to calculate the excess return. 
#The final dataframe only contains rows with non-empty returns, stocks in NAQ exchanges, and primary stocks.
##############

library(readxl)
library(dplyr)
library(lubridate)

mktcap <- readRDS("../mkt_cap/mktcap_combined.rds")
book_equity_df <- readRDS("data/be_op_in.rds")

size <- mktcap %>%
  mutate(
    year = floor(YYYYMM / 100),
    month = round(YYYYMM %% 100)
  ) %>%
  filter(month == 6) %>%
  mutate(
    sort_date = paste0(year, "-07-01"),
    SIZE = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)
  ) %>%
  filter(!is.na(SIZE) & SIZE != 0) %>%
  select(KYGVKEY, YYYYMM, SIZE, sort_date) %>%
  group_by(KYGVKEY, YYYYMM, sort_date) %>%
  summarize(SIZE = mean(SIZE, na.rm = TRUE)) %>%
  ungroup()

mkt_equity <- mktcap %>%
  mutate(
    year = floor(YYYYMM / 100),
    month = round(YYYYMM %% 100)
  ) %>%
  filter(month == 12) %>%
  mutate(
    sort_date = paste0(year + 1, "-07-01"),
    mktcap = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)
  ) %>%
  filter(!is.na(mktcap) & mktcap != 0) %>%
  select(KYGVKEY, YYYYMM, me = mktcap, sort_date) %>%
  group_by(KYGVKEY, YYYYMM, sort_date) %>%
  summarize(me = mean(me, na.rm = TRUE)) %>%
  ungroup()

# # keep date > 2008
# book_equity_df <- book_equity_df %>% 
#   filter(FYYYY >= 2008)

# Full join (be, op, inv) and mktcap on KYGVKEY and YYYYMM
combined_df <- size %>% 
  mutate(sort_date = as.Date(sort_date)) %>%
  left_join(book_equity_df, by = c("KYGVKEY", "sort_date")) %>%
  left_join(mkt_equity %>% mutate(sort_date = as.Date(sort_date)), by = c("KYGVKEY", "sort_date")) %>%
  mutate(bm = be1 / me)

# filtered_combined_df <- combined_df %>%
#   filter(KYGVKEY == 2176)

fout_factor_tojoin <- combined_df %>% 
  select(KYGVKEY, sort_date, SIZE, bm, op1, op = op2, inv, at = AT, at_lag1, be1, FYYYY, FYRA)

saveRDS(fout_factor_tojoin, "data/four_factors.rds")

