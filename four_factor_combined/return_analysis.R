library(readxl)
library(dplyr)

mthcap <- readRDS("../mkt_cap/data/sfz_agg_mth_short.rds")

start_date <- as.Date("1926-01-01")

ret <- mthcap %>% 
  select(KYPERMNO, YYYYMM, MCALDT, MTHRET) %>%
  mutate(MCALDT = as.Date(as.character(MCALDT), "%Y-%m-%d")) %>%
  filter(MCALDT > start_date & !is.na(MTHRET)) %>%
  distinct()

alllinks <- read_excel("../mkt_cap/data/alllinks_link.xlsx")

# Filter allinks based on end dates and existing PERMNOs
alllinks_filtered <- alllinks %>%
  filter(LINKENDDT >= as.numeric(format(start_date, "%Y%m%d")) & !is.na(LPERMNO)) %>%
  distinct() %>%
  mutate(
    LINKDT = as.Date(as.character(LINKDT), "%Y%m%d"),
    LINKENDDT = ifelse(LINKENDDT == 99999999, as.Date("2030-01-01"), as.Date(as.character(LINKENDDT), "%Y%m%d"))
  )

ret_joined <- ret %>%
  left_join(alllinks_filtered, by = c("KYPERMNO" = "LPERMNO")) %>%
  filter(MCALDT >= LINKDT & MCALDT <= LINKENDDT)

ret_filtered <- ret_joined %>%
  group_by(KYPERMNO, MCALDT, KYGVKEY) %>%
  summarize(
    MTHRET = first(MTHRET),
    YYYYMM = first(YYYYMM)
  ) %>%
  ungroup()


write.csv(ret_filtered, "data/mthret.csv", row.names = FALSE)
