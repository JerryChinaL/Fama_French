library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
cshoq <- read_excel("cshoq0823_qi.xlsx")

#mkvalt <- read_excel("666_amkt.xlsx")
#mkvaltq <- read_excel("666_imkt.xlsx")
mkvaltq <- read_excel("mkvaltlong_imkt.xlsx")

#prccm <- read_excel("777_imkt.xlsx")
prccm0812 <- read_excel("ab0812_ms.xlsx")
prccm1216 <- read_excel("ab16_20_ms.xlsx")
prccm1620 <- read_excel("ab1620_true_ms.xlsx")
prccm2023 <- read_excel("ab10_23_ms.xlsx")
prccm <- bind_rows(prccm0812, prccm1216, prccm1620, prccm2023)

mthcap <- readRDS("../old data/sfz_agg_mth.rds") %>% filter(SHRCD == 10 | SHRCD == 11)
mthcap <- mthcap[,c('KYPERMNO','YYYYMM','MCALDT','MTHCAP', 'MTHPRC', 'MTHVOLFLG', 'MTHRET', 'MTHVOL', 'TICKER', 'PRIMEXCH')]
saveRDS(mthcap, "data/sfz_agg_mth_short.rds")

data <- mthcap %>%
  select(-c("MTHPRC", "MTHPREVPRC", "MTHRETX", "TICKER")) %>%
  mutate(year_month = floor_date(MCALDT, unit = "month")) %>%
  filter(PRIMEXCH %in% c("N", "A", "Q")) %>%
  unique()

# Filter the data frames based on the gvkey and permno values
# Berkshire - 2176 - 17778,83443; nvidia - 117768 - 86580; Apple - 1690 - 14593; 
# Best Buy - 2184 - 85914; microsoft - 12141 - 10107; walmart; target
# no alibaba, toyota, sony
gvkeys <- c(2176, 117768, 1690, 2184, 12141, 11259, 3813, 64768, 25283, 20530, 19661, 9818, 5165, 8087)
permnos <- c(17778, 86580, 14593, 85914, 10107, 55976, 49154, 84788, 77606, 14888, 76655, 51131, 39029, 67029)

calculate_yyyymm <- function(FYYYYQ, FYRQ) {
  year <- floor(FYYYYQ)
  quarter <- round((FYYYYQ - year) * 10)
  
  # Calculate the month based on the fiscal quarter and fiscal year-end month
  month <- case_when(
    quarter == 4 ~ FYRQ,
    quarter == 3 ~ FYRQ - 3,
    quarter == 2 ~ FYRQ - 6,
    quarter == 1 ~ FYRQ - 9
  )
  
  # Adjust for negative or zero months
  if (month < 1) {
    month <- month + 12
    year <- year - 1
  }
  
  as.numeric(sprintf("%04d%02d", year, month))
}

cshoq_filtered <- cshoq %>% 
  filter(KYGVKEY %in% gvkeys) %>%
  mutate(YYYYMM = mapply(calculate_yyyymm, FYYYYQ, FYRQ))

# mkvalt_filtered <- mkvalt %>% filter(KYGVKEY %in% gvkeys)%>%
#   mutate(
#     FYRA = sprintf("%02d", FYRA),
#     YYYYMM = paste0(FYYYY, FYRA)
#   )

prccm_filtered <- prccm %>% 
  filter(KYGVKEY %in% gvkeys, PRCCM < 10000 | KYGVKEY != 2176) %>%
  mutate(
    YYYYMM = as.numeric(format(DATADATE, "%Y%m"))
  )

mthcap_filtered <- mthcap %>%
  filter(KYPERMNO %in% permnos) %>%
  mutate(
    YYYYMM = as.numeric(format(MCALDT, "%Y%m")),
    MTHCAP = round(MTHCAP / 1000, 2)
  )

mkvaltq_filtered <- mkvaltq %>%
  filter(KYGVKEY %in% gvkeys) %>%
  mutate(YYYYMM = mapply(calculate_yyyymm, FYYYYQ, FYRQ))

# Simply adding gvkey column to mthcap, using the link mapping
permno_gvkey_mapping <- data.frame(KYPERMNO = permnos, KYGVKEY = gvkeys)
mthcap_filtered <- left_join(mthcap_filtered, permno_gvkey_mapping, by = "KYPERMNO")

#
#
#
# Making some plots to see how much they fit
merged_data <- left_join(mkvaltq_filtered, mthcap_filtered, by = c("KYGVKEY", "YYYYMM"))

# Merge and Multiply CSHOQ and PRCCM columns
cshoq_prccm_merged <- left_join(cshoq_filtered, prccm_filtered, by = c("KYGVKEY", "YYYYMM"))
cshoq_prccm_merged <- cshoq_prccm_merged %>%
  mutate(CSHOQ_PRCCM = CSHOQ * PRCCM)

final_merged_data <- left_join(merged_data, cshoq_prccm_merged %>% select(KYGVKEY, YYYYMM, CSHOQ_PRCCM), by = c("KYGVKEY", "YYYYMM"))

for (gvkey in gvkeys) {
  plot_data <- final_merged_data %>% filter(KYGVKEY == gvkey)
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = as.Date(paste0(YYYYMM, "01"), "%Y%m%d"))) +
    geom_line(aes(y = MKVALTQ, color = "MKVALTQ"), linewidth = 1) +
    geom_line(aes(y = MTHCAP, color = "MTHCAP"), linewidth = 1) +
    geom_line(aes(y = CSHOQ_PRCCM, color = "CSHOQ * PRCCM"), linewidth = 1) +
    labs(title = paste("MKVALTQ, MTHCAP, and CSHOQ * PRCCM for GVKEY", gvkey),
         x = "Date",
         y = "Value (in thousands)",
         color = "Legend") +
    theme_minimal()
  
  # Print the plot
  print(p)
}

mthcap <- readRDS("../old data/sfz_agg_mth.rds")

#write.csv(mthcap, file = "mthcap.csv", row.names = FALSE)

