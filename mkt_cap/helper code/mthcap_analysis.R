library(readxl)
library(dplyr)
library(ggplot2)

# Load the data
mthcap <- readRDS("sfz_agg_mth_short.rds")
alllinks <- read_excel("alllinks_link.xlsx")

start_date <- as.Date("2008-01-01")

# Filter allinks based on end dates and existing PERMNOs
alllinks_filtered <- alllinks %>%
  filter(LINKENDDT >= as.numeric(format(start_date, "%Y%m%d")) & !is.na(LPERMNO)) %>%
  distinct() %>%
  mutate(
    LINKDT = as.Date(as.character(LINKDT), "%Y%m%d"),
    LINKENDDT = ifelse(LINKENDDT == 99999999, as.Date("2030-01-01"), as.Date(as.character(LINKENDDT), "%Y%m%d"))
  )

# Do some type conversion, and delete rows with MTHCAP = N/A
mthcap <- mthcap %>%
  mutate(
    MCALDT = as.Date(as.character(MCALDT), "%Y-%m-%d"),
    MTHCAP = MTHCAP / 1000
  ) %>%
  filter(MCALDT > start_date & !is.na(MTHCAP)) %>%
  distinct()

mthcap_joined <- mthcap %>%
  left_join(alllinks_filtered, by = c("KYPERMNO" = "LPERMNO")) %>%
  filter(MCALDT >= LINKDT & MCALDT <= LINKENDDT)

mthcap_filtered <- mthcap_joined %>%
  group_by(KYPERMNO, MCALDT, KYGVKEY) %>%
  summarize(
    MIN_LINKDT = min(LINKDT, na.rm = TRUE),
    MAX_LINKENDDT = max(LINKENDDT, na.rm = TRUE),
    MTHCAP = first(MTHCAP)  # Ensure MTHCAP remains consistent within each group
  ) %>%
  ungroup()

# Left join with link to add gvkey, only keep rows where dateeffective dates, 
# then handle overlapping effective dates
mthcap_filtered <- mthcap %>%
  left_join(alllinks_filtered, by = c("KYPERMNO" = "LPERMNO")) %>%
  filter(MCALDT >= LINKDT & MCALDT <= LINKENDDT) %>%
  group_by(KYPERMNO, MCALDT, KYGVKEY) %>%
  summarize(
    MIN_LINKDT = min(LINKDT),
    MAX_LINKENDDT = max(LINKENDDT),
    MTHCAP = first(MTHCAP)  # Ensure MTHCAP remains consistent within each group
  ) %>%
  ungroup()

# Identify rows that are lost during the join
lost_rows <- anti_join(mthcap, mthcap_joined, by = c("KYPERMNO", "MCALDT"))

# Print lost rows
print("Lost Rows:")
print(lost_rows)

# Check the row count before and after join
final_mthcap_row_count <- nrow(mthcap_filtered)

print(paste("Initial row count of mthcap:", initial_mthcap_row_count))
print(paste("Row count of mthcap after join:", final_mthcap_row_count))

# Check the first few rows of the joined data
print(head(mthcap_filtered))

# Save the filtered data to a CSV file
write.csv(mthcap_filtered, "mthcap_filtered.csv", row.names = FALSE)
