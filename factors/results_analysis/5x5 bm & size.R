library(dplyr)
library(tidyr)

# Load your data
final_df <- read.csv("../data/four_factors_excret.csv")

# Ensure the necessary columns are numeric and remove NA rows for SIZE and bm
final_df <- final_df %>%
  mutate(
    bm = as.numeric(bm),
    SIZE = as.numeric(SIZE),
    MTHRET = as.numeric(MTHRET),
    YYYYMM = format(as.Date(return_date), "%Y%m")
  ) %>%
  filter(!is.na(SIZE) & !is.na(bm))

# Create quantiles for bm and SIZE
final_df <- final_df %>%
  group_by(sort_date) %>%
  mutate(
    bm_quantile = ntile(bm, 5),
    size_quantile = ntile(SIZE, 5)
  ) %>%
  ungroup()

# Calculate the average monthly return for each bm and size quantile pair for each month
monthly_grid <- final_df %>%
  group_by(YYYYMM, size_quantile, bm_quantile) %>%
  # summarize(avg_monthly_return = mean(MTHRET, na.rm = TRUE)) %>%
  summarize(avg_monthly_return = weighted.mean(MTHRET, SIZE, na.rm = TRUE)) %>%
  ungroup()

# Pivot the data to a wider format
monthly_grid_wide <- monthly_grid %>%
  unite("quantile", size_quantile, bm_quantile, sep = "") %>%
  pivot_wider(names_from = quantile, values_from = avg_monthly_return) %>%
  rename_with(~ paste0("f", .), -YYYYMM)

# Sort the columns by name
sorted_columns <- c("YYYYMM", sort(names(monthly_grid_wide)[-1]))
monthly_grid_wide <- monthly_grid_wide %>% 
  select(all_of(sorted_columns))

# Fill missing values with NA (if any)
monthly_grid_wide[is.na(monthly_grid_wide)] <- NA

write.csv(format(as.data.frame(monthly_grid_wide), scientific = FALSE), "data/monthly_grid_wide_weighted.csv", row.names = FALSE)

# Calculate the average of each column to get the final grid
final_grid <- monthly_grid_wide %>%
  summarize(across(-YYYYMM, mean, na.rm = TRUE))

# Convert the final grid to a 5x5 matrix
final_matrix <- matrix(
  as.numeric(final_grid[1, ]),
  nrow = 5, ncol = 5, byrow = TRUE,
  dimnames = list(paste0("SIZE_", 1:5), paste0("BM_", 1:5))
)

print(final_matrix)
write.csv(format(as.data.frame(final_matrix), scientific = FALSE), "data/final_5x5_table.csv", row.names = TRUE)
