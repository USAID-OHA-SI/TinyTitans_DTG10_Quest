# PROJECT:  C:/Users/atowey/Documents/Github/TinyTitans_DTG10_Quest
# PURPOSE:  Clean viral load data from DTG10 Kenya
# AUTHOR:   A. Towey | USAID
# REF ID:   2b388655
# LICENSE:  MIT
# DATE:     2024-10-15


# DEPENDENCIES ==================================================================

# Libraries
library(tidyverse)
library(gagglr)
library(scales, warn.conflicts = FALSE)
library(systemfonts)
library(readxl)
library(summarytools)
library(ggplot2)
library(dplyr)
library(lubridate)

# Global variables
ref_id <- "2b388655"

# LOAD DATA =====================================================================

# Sheet 4: Visits_Data
vl_df_original <-
  read_excel("Data/Data_pDTG10.xlsx", skip = 1, 5) %>%
  janitor::clean_names()
vl_df <- vl_df_original

# MUNGE =========================================================================

vl_df <- vl_df %>%
  rename(
    id = client_number_de_identified,
    dob = date_of_birth,
    mfl_code = facility_mfl_code,
    facility = facility_name,
    vl_test_result = viral_load_test_result
  ) %>%
  mutate(id2 = paste0(mfl_code, "_", id))

vl_df %>% distinct(id) #49,668
vl_df %>% distinct(facility, id) #49,668
vl_df %>% distinct(mfl_code, id) #49,668
vl_df %>% distinct(id2) #49,668
# Does this mean there were no clients who had tests at different sites?

# Check for duplicates
vl_df %>%
  group_by(id2, vl_test_date) %>%
  summarise(count = n()) %>%
  filter(count > 1) #none

# Format dates and calculate number of tests, days since last test and change since last test
vl_df <- vl_df %>%
  mutate(
    vl_test_date = as.Date(vl_test_date),
    dob = as.Date(dob),
    age_at_test_days = as.integer(difftime(vl_test_date, dob, units = "days"))
  ) %>%
  group_by(id) %>%
  mutate(
    num_tests = n(),
    days_since_last_test = as.numeric(difftime(vl_test_date, lag(vl_test_date),
                                               units = "days")),
    change_since_last_test = vl_test_result - lag(vl_test_result)
  ) %>%
  ungroup() %>%
  arrange(id)


# check for missing
vl_df %>% summarise_all(~ sum(is.na(.)))


# what is the range for viral load results?
summary(vl_df$vl_test_result) # 0 to 26,300,000. mean: 14025 median: 49

# What is the date range for tests?
summary(vl_df$vl_test_date) # 3/1/2021 to 9/30/2023

# What is the age range at day of test?
summary(vl_df$age_at_test_days) # 0 to 5479 (0 to 15 years)

# What is the range of numbers of tests per id?
summary(vl_df$num_tests) # 1 test to 16 tests

# Create categories for viral load
vl_df <- vl_df %>%
  mutate(
    vl_category = case_when(
      vl_test_result > 1000 ~ "unsuppressed",
      vl_test_result > 0 & vl_test_result <= 1000 ~ "suppressed",
      vl_test_result == 0 ~ "undetectable",
      TRUE ~ NA_character_
    )
  )

vl_category_summary <- vl_df %>%
  group_by(vl_category) %>%
  summarise(
    count = n(),
    percentage = (count / nrow(vl_df)) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

print(vl_category_summary)


View(vl_df)

dfSummary(vl_df)

# Specify the path for the RDS file in the 'dataout' folder
rds_file_path <- file.path("dataout", "03_vl_df.rds")
saveRDS(vl_df, rds_file_path)
cat("RDS file saved at:", rds_file_path, "\n")


# CHECK DATA ==================================================================

# Check viral load distribution
log_breaks <- c(0, 1, 50, 200, 1000, 10000, 50000, 100000, Inf)
labels <-
  c(
    "0",
    "1-49",
    "50-199",
    "200-999",
    "1,000-9,999",
    "10,000-49,999",
    "50,000-99,999",
    ">100,000"
  )

vl_df_histogram <- vl_df %>%
  mutate(
    vl_result_binned = cut(
      vl_test_result,
      breaks = log_breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    ),
    color = ifelse(vl_result_binned %in% labels[1:4], "suppressed",
                   "unsuppressed")
  )

ggplot(vl_df_histogram, aes(x = vl_result_binned, fill = color)) +
  geom_bar() +
  scale_fill_manual(values = c("suppressed" = midnight_blue,
                               "unsuppressed" = viking)) +
  labs(
    title = "Distribution of Viral Load Test Results",
    subtitle = "Vast majority of test results fall under 1000ml (virally suppressed).",
    x = "Viral Load (copies/ml)",
    y = "# Test Results"
  ) +
  si_style() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Check age distribution
age_bins <- seq(0, 15 * 365.25, by = 365.25)
age_labels <- c(
  "0-1 year",
  "1-2 years",
  "2-3 years",
  "3-4 years",
  "4-5 years",
  "5-6 years",
  "6-7 years",
  "7-8 years",
  "8-9 years",
  "9-10 years",
  "10-11 years",
  "11-12 years",
  "12-13 years",
  "13-14 years",
  "14-15 years"
)

vl_age_binned <- vl_df %>%
  mutate(
    age_bin = cut(
      age_at_test_days,
      breaks = age_bins,
      labels = age_labels,
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(age_bin) %>%
  summarise(
    count = n(),
    percentage = (count / nrow(vl_df)) * 100,
    .groups = 'drop'
  ) %>%
  arrange(age_bin)

print(vl_age_binned)

ggplot(vl_age_binned, aes(x = age_bin, y = count)) +
  geom_bar(stat = "identity", fill = midnight_blue) +
  si_style() +
  labs(title = "Count of Viral Load Tests by Age (Binned)",
       x = "Age at Test (Binned)",
       y = "Count of Tests") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
