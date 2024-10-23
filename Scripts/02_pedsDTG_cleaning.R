# PROJECT:    C:/Users/atowey/Documents/Github/TinyTitans_DTG10_Quest
# PURPOSE:    Clean visits data from DTG10 Kenya
# AUTHOR:     A. Towey | USAID
# REF ID:     1997974d
# LICENSE:    MIT
# DATE:       2024-10-09
# UPDATED:

# DEPENDENCIES ==================================================================
library(tidyverse)
library(glue)
library(gagglr)
library(scales, warn.conflicts = FALSE)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(readxl)
library(gt)
library(gtExtras)
library(anthroplus)
library(anthro)
library(summarytools)
library(naniar)
library(forcats)
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)

# Global variables
ref_id <- "1997974d"  # A reference to be placed in viz captions

# LOAD DATA ====================================================================
# Sheet 3: Visits_Data
visits_df_original <-
  read_excel("Data/Data_pDTG10.xlsx", skip = 1, 4) %>%
  janitor::clean_names()

visits_df <- visits_df_original
visits_df %>% glimpse()

# MUNGE ========================================================================
# 1 - Rename variables & create id2 based on id and facility
visits_df <- visits_df %>%
  rename(
    id = client_number_de_identified,
    dob = date_of_birth,
    mfl_code = facility_mfl_code,
    facility = facility_name,
    date_visit = last_actual_visit_dates,
    date_next_appt = next_appointment_date
  ) %>%
  mutate(id2 = paste0(mfl_code, "_", id))

# 2 - Create distinct clients based on ids
visits_df %>% distinct(id)  # 61,234 distinct clients based on client ids
visits_df %>% distinct(facility, id)  # 61,234 distinct clients based on client id and facility
visits_df %>% distinct(mfl_code, id)  # same

ref_clientlist <-
  visits_df %>% distinct(facility, mfl_code, id)  # 61,234 distinct clients based on client id and facility mfl code
sum(duplicated(ref_clientlist))  # no duplicates

multiple_id2s <- visits_df %>%
  group_by(id) %>%
  summarize(unique_id2_count = n_distinct(id2)) %>%
  filter(unique_id2_count > 1)

print(multiple_id2s)  # None? Is this expected?

# 3 - Add Derived Columns
# - num_visits
# - age_at_visit_in_years
# - age_at_visit_in_months
# - age_at_visit_in_days
# - days_since_last_visit
# - cumulative num visits
# - is_last_visit flag
# - bmi

# For under 5s Z-scores
# - bmi
# - length (height)
# - weight
# - length for weight
# - muac

# For over 5s
# - height for age (up to 228 months)
# - weight for age (up to 120 months)
# - bmi for age (up to 228 months)

visits_df <- visits_df %>%
  arrange(id, date_visit) %>%
  group_by(id) %>%
  mutate(
    num_visits = n(),
    days_since_last_visit = as.numeric(difftime(date_visit, lag(date_visit), units = "days")),
    cumulative_num_visits = row_number(),
    is_last_visit = ifelse(date_visit == max(date_visit), 1, 0)
  ) %>%
  ungroup()

visits_df <- visits_df %>%
  mutate(
    age_at_visit_days = as.integer(difftime(date_visit, dob, units = "days")),
    age_at_visit_months = interval(dob, date_visit) / months(1),
    age_at_visit_years = interval(dob, date_visit) / years(1)
  )

visits_0_5 <- visits_df %>%
  filter(age_at_visit_days < 1825)  # Filter for ages < 5 years

visits_5_plus <- visits_df %>%
  filter(age_at_visit_days >= 1825)  # Filter for ages >= 5 years

# Calculate Z-scores for clients aged 0-5
z_scores_0_5 <- anthro::anthro_zscores(
  sex = ifelse(visits_0_5$sex == "MALE", 1, 2),
  age = visits_0_5$age_at_visit_days,
  weight = visits_0_5$weight,
  lenhei = visits_0_5$height,
  armc = visits_0_5$muac  # armc = muac?
) %>%
  select(cbmi, zlen, zwei, zwfl, zbmi, zac) %>%
  rename(
    bmi = cbmi,
    z_length_0_5 = zlen,
    z_weight_0_5 = zwei,
    z_weight_for_length_0_5 = zwfl,
    z_bmi_0_5 = zbmi,
    z_muac_0_5 = zac
  )

visits_0_5 <- visits_0_5 %>%
  cbind(z_scores_0_5)

# Calculate Z-scores for clients aged 5 and above
z_scores_5_plus <- anthroplus::anthroplus_zscores(
  sex = ifelse(visits_5_plus$sex == "MALE", 1, 2),
  age_in_months = visits_5_plus$age_at_visit_months,
  weight_in_kg = visits_5_plus$weight,
  height_in_cm = visits_5_plus$height
) %>%
  select(cbmi, zhfa, zwfa, zbfa) %>%
  rename(
    bmi = cbmi,
    z_height_for_age_5plus = zhfa,
    # up to 228 months
    z_weight_for_age_plus = zwfa,
    # up to 120 months
    z_bmi_for_age_5plus = zbfa       # up to 228 months
  )

visits_5_plus <- visits_5_plus %>%
  cbind(z_scores_5_plus)

all_visits_df <- bind_rows(visits_0_5, visits_5_plus) %>%
  arrange(id2, date_visit)

View(all_visits_df)  # Join 0-5 and 5+

# Save
rds_file_path <- file.path("dataout", "02_visits_df.rds")
saveRDS(all_visits_df, rds_file_path)
cat("RDS file saved at:", rds_file_path, "\n")

# DATA QUALITY ----------------------------------------------------------------
## Explore All Data ---------------------------------------------------------
dfSummary(all_visits_df)

all_visits_df %>% summarise_all(~ sum(is.na(.)))

summary(all_visits_df$weight) # 3 to 80
summary(all_visits_df$height) # 30 to 180
summary(all_visits_df$muac) # 1 to 111111115 (seems incorrect)
summary(all_visits_df$num_visits) # 1 to 101
summary(all_visits_df$age_at_visit_days) # 0 to 5479 (0 to 15 years)

missing_analysis <- visits_df %>%
  summarise(
    sex_missing = sum(is.na(sex)),
    facility_missing = sum(is.na(facility)),
    mfl_code_missing = sum(is.na(mfl_code)),
    date_visit_missing = sum(is.na(date_visit)),
    dob_missing = sum(is.na(dob))
  )
print(missing_analysis)  # None

# We have large amounts of data missing. What do we have for height/weight/MUAC/nutritional status?
total_rows <- nrow(visits_df)
height_count <- sum(!is.na(visits_df$height))
weight_count <- sum(!is.na(visits_df$weight))
muac_count <- sum(!is.na(visits_df$muac))
nutritional_status_count <-
  sum(!is.na(visits_df$nutritional_status))

summary_stats <- data.frame(
  Metric = c(
    "Total Visits",
    "Height Measures",
    "Weight Measures",
    "MUAC Measures",
    "Nutritional Status Measures"
  ),
  Count = c(
    total_rows,
    height_count,
    weight_count,
    muac_count,
    nutritional_status_count
  )
)
print(summary_stats)
# Total Visits: 739351
# Height Measures: 502254
# Weight Measures: 519996
# MUAC Measures: 20811
# Nutritional Status Measures: 12124

# Connections between missing measurements?
visits_df_filtered <- visits_df %>% select(-days_since_last_visit)
gg_miss_upset(visits_df_filtered)

# Connection between missing height and weight?
height_but_not_weight <-
  visits_df %>% filter(!is.na(height) & is.na(weight))
weight_but_not_height <-
  visits_df %>% filter(!is.na(weight) & is.na(height))

nrow(height_but_not_weight)  # 8425
nrow(weight_but_not_height)  # 26167

### MUAC
visits_df <- visits_df %>%
  mutate(muac_missing = is.na(muac))  # 1 if missing, 0 otherwise

# Chi-Square Tests for MUAC missing randomness by partner and county
chisq_results <- lapply(list(
  county = visits_df$county,
  partner = visits_df$implementing_partner
), function(x)
  chisq.test(table(visits_df$muac_missing, x)))
print(chisq_results)  # Not random, very low p-values

# Grab MUAC counts by county and IP
muac_available <- visits_df %>% filter(!muac_missing)

muac_counts_by_ip_and_county <- muac_available %>%
  group_by(implementing_partner, county) %>%
  summarise(total_muac = n(), .groups = 'drop') %>%
  arrange(implementing_partner, county)

muac_counts_by_ip_and_county %>% View()

## Histograms for age and visits
visits_df %>%
  ggplot(aes(x = age_at_visit_years)) +
  geom_histogram(binwidth = 1, fill = midnight_blue) +
  labs(title = "Histogram of Age at Visit", x = "Age (Years)", y = "Count") +
  si_style()

visits_df %>%
  ggplot(aes(x = num_visits)) +
  geom_histogram(binwidth = 1, fill = viking) +
  labs(title = "Histogram of Number of Visits", x = "Number of Visits", y = "Count") +
  si_style()

## Find Potential Outliers

detect_outliers <- function(data, column) {
  q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  iqr <- IQR(data[[column]], na.rm = TRUE)
  
  lower_threshold <- q1 - 2 * iqr
  upper_threshold <- q3 + 2 * iqr
  
  outliers <- data %>%
    filter(data[[column]] < lower_threshold |
             data[[column]] > upper_threshold) %>%
    select(id2,
           age_at_visit_days,
           date_visit,
           height,
           weight,
           muac,
           bmi,!!sym(column))
  
  return(outliers)
}

numeric_columns <-
  c(
    "weight",
    "height",
    "muac",
    "bmi",
    "z_length_0_5",
    "z_weight_0_5",
    "z_weight_for_length_0_5",
    "z_bmi_0_5",
    "z_muac_0_5",
    "z_height_for_age_5plus",
    "z_weight_for_age_plus",
    "z_bmi_for_age_5plus"
  )

outlier_results <- list()

for (col in numeric_columns) {
  outliers <- detect_outliers(master_visits, col)
  outlier_results[[col]] <- outliers
  cat(paste("Outliers detected in:", col, "Count:", nrow(outliers), "\n"))
}

# 0-5s
#View(outlier_results[["z_length_0_5"]])
#View(outlier_results[["z_weight_0_5"]])
#View(outlier_results[["z_weight_for_length_0_5"]])
#View(outlier_results[["z_bmi_0_5"]])
#View(outlier_results[["z_muac_0_5"]])

#5 plus
#View(outlier_results[["z_height_for_age_5plus"]])
#View(outlier_results[["z_weight_for_age_plus"]])
#View(outlier_results[["z_bmi_for_age_5plus"]])

# Summary of outlier counts
summary_outliers <- data.frame(Measure = numeric_columns,
                               Outlier_Count = sapply(outlier_results, nrow))
print(summary_outliers)



## Explore 0-5 Data-----------------------------------------------------------
dfSummary(visits_0_5)

# MUAC
threshold <- quantile(visits_0_5$muac, 0.95, na.rm = TRUE)
top_5_percent_muac_0_5 <- visits_0_5 %>%
  filter(muac >= threshold) %>%
  arrange(desc(muac))
print(top_5_percent_muac_0_5$muac) # Some VERY high MUACs... hmmm. What is normal here?

# MUAC present but missing MUAC Z-score
muac_without_z <- visits_0_5 %>%
  filter(!is.na(muac) & is.na(z_muac_0_5))
View(muac_without_z) # There are 15 of them. Why would this be?

# Height and weight present but z_weight_for_length is missing
height_weight_no_z_weight_length <- visits_0_5 %>%
  filter(!is.na(height) &
           !is.na(weight) & is.na(z_weight_for_length_0_5))
View(height_weight_no_z_weight_length) # There are 555 of them. Why would this be?


## Explore 5+ Data-----------------------------------------------------------
dfSummary(visits_5_plus)

# MUAC
threshold <- quantile(visits_5_plus$muac, 0.95, na.rm = TRUE)

top_5_percent_muac_5_plus <- visits_5_plus %>%
  filter(muac >= threshold) %>%
  arrange(desc(muac))

print(top_5_percent_muac_5_plus$muac)# Again some very high MUACs (in the thousands +)
