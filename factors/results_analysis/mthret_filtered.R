library(readxl)
library(dplyr)
mthret <- read.csv("../FF5_Replciation/four_factor_combined/data/mthret.csv")
exch <- readRDS("../FF5_Replciation/mkt_cap/data/sfz_agg_mth_short.rds") %>% 
  select(KYPERMNO, YYYYMM, PRIMEXCH)
primiss_files <- c("6070", "7078", "7885", "8590", "9095", "9500", "0004", "0408", "p0812", "p1216","p1620","p2023")
primiss_files <- paste0("../FF5_Replciation/four_factor_combined/primiss/", primiss_files, "_ms.xlsx")
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

saveRDS(mthret, "data/mthret_filtered.rds")
saveRDS(n_stocks, "data/n_stocks.rds")