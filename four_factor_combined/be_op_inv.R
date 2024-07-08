
###################

# This file is used to calculate the book equity, operating profitability, and investment factors

###################

library(readxl)
library(dplyr)
library(tidyr)

directory = "."

she <- read_excel(file.path(directory, "she_ai.xlsx"))
pef <- read_excel(file.path(directory, "pef_ai.xlsx"))
txditc <- read_excel(file.path(directory, "txditc_ai.xlsx"))
op1 <- read_excel(file.path(directory, "op1_ai.xlsx"))

pef <- pef %>%
  mutate(pef = coalesce(PSTKRV, PSTKL, PSTKR, PSTKN, 0))

she <- she %>%
  left_join(pef %>% select(KYGVKEY, FYYYY, FYRA, pef), by = c("KYGVKEY", "FYYYY", "FYRA")) %>%
  mutate(she = coalesce(SEQ, TEQ, AT - LT, CEQ + pef))

txditc <- txditc %>%
  mutate(txditc = coalesce(TXDITC, TXDB + ITCB, 0))

combined_df <- full_join(she, txditc, by = c("KYGVKEY", "FYYYY", "FYRA")) %>%
  full_join(op1, by = c("KYGVKEY", "FYYYY", "FYRA")) %>%
  mutate(
    txditc = replace_na(txditc, 0),
    pef = replace_na(pef, 0),
    be1 = ifelse((she + txditc - pef) > 0, (she + txditc - pef), NaN)
  )

combined_df <- combined_df %>%
  mutate(
    FYYYY = case_when(
      FYRA < 6 ~ FYYYY + 1,
      TRUE ~ FYYYY
      ),
    op1 = SALE - coalesce(COGS, 0) - coalesce(XSGA, 0) - coalesce(XINT, 0),
    op2 = op1 / be1
    ) %>%
  group_by(KYGVKEY, FYYYY) %>%
  slice_max(order_by = FYRA, with_ties = FALSE) %>%
  ungroup()

# Create a segment identifier using cumsum
combined_df <- combined_df %>%
  arrange(KYGVKEY, FYYYY, FYRA) %>%
  group_by(KYGVKEY) %>%
  mutate(segment = cumsum(FYRA != lag(FYRA, default = first(FYRA)))) %>%
  ungroup()

# Perform the lag operation within each segment
combined_df <- combined_df %>%
  group_by(KYGVKEY, segment) %>%
  mutate(at_lag1 = lag(AT),
         inv = ifelse(at_lag1 == 0, NA, AT / at_lag1)) %>%
  ungroup()

book_equity_df <- combined_df %>%
  select(KYGVKEY, KEYSET_TAG, FYYYY, FYRA, pef, she, txditc, be1, op1, op2, inv, AT, at_lag1) %>%
  filter(!(is.na(inv) & is.na(op1) & is.na(op2) & is.na(be1) & is.na(AT)))

book_equity_df <- book_equity_df %>%
  mutate(sort_date = as.Date(paste0(FYYYY + 1, "-07-01")))

duplicate_rows <- book_equity_df %>%
  group_by(KYGVKEY, sort_date) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(book_equity_df, "data/be_op_in.csv", row.names = FALSE)

# View(book_equity_df %>% filter(KYGVKEY == 2176))
# 
# op1 <- read_excel(file.path(directory, "op1_ai.xlsx"))
# 
# View(op1 %>% filter(KYGVKEY == 2176))

# anyNA(book_equity_df$FYRA)

