# PROJECT: Pediatric DTG in Kenya 
# PURPOSE: Munge and Analysis of DTG cd4 Data 
# AUTHOR:  Nelly Maina | SI
# REF ID:  a98f405c
# LICENSE: MIT
# DATE:   2024-07-018
# NOTES:   https://github.com/USAID-OHA-SI/Aging/blob/main/Scripts/Ageing-up%20Analysis.R

# LOCALS & SETUP ============================================================================

# Libraries
library(gagglr)
library(grabr)
library(tidyverse)
library(scales)
library(sf)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(readxl)
library(glue)
library(gt)
library(gtExtras)

# REF ID for plots
ref_id <- "a98f405c"

# Functions  


# LOAD DATA ============================================================================  

#Sheet 2: cd4 
exits_df <- read_excel("Data/Data_pDTG10.xlsx",skip = 1, 7) %>% 
  janitor::clean_names()

exits_df %>% glimpse()




# MUNGE ============================================================================


# Newly Initiated  --------------------------------------------------------


#1 - Rename variables & create id2 based on id and facility 
exits_df <- exits_df %>% 
  rename(
    ip = implementing_partner,
    facility = facility_name,
    mfl_code = facility_mfl_code,
    county= county,
    id = client_number_de_identified,
    dob = date_of_birth,
    sex = sex,
    outcome = outcome,
    outcome_date = date_of_outcome,
  ) %>% 
  mutate(id2 = paste0(mfl_code, "_", id))

#2 - Create distinct clients based on ids 


exits_df %>% distinct(id) #7,850    distinct clients based on client ids 
exits_df %>% distinct(facility, id)  #7,850  distinct clients based on client id and facility



#3 - Creating visits_List based on necessary columns


#4 - check difference between master and reference lists (no difference)
#occurrence <- data.frame(table(master_clientlist$id2)) %>% rename(id2=Var1)

#master_clientlist <- master_clientlist %>% left_join(occurrence, by="id2") 

#master_clientlist %>% filter(Freq>1) %>% names() #0 records 


#5 - categorize period of ART initiation & DTG initiation 

#fix dates 
#note: some starting on DTG (control), others switch later on 

exits_df <- exits_df %>%
  mutate(
    dob = dmy(dob),  # If your dates are in 'dd/mm/yyyy' format
    outcome_date = dmy(outcome_date)  # Adjust if the format is similar
  )



# Convert specified columns to factors
exits_df <- exits_df %>%
  mutate(
    facility = as.factor(facility),
    ip = as.factor(ip),
    mfl_code = as.factor(mfl_code),
    county = as.factor(county),
    sex = as.factor(sex),
    outcome = as.factor(outcome)
  )

# check for duplicates 

# Check for duplicate entries by client ID and test date
exits_df %>%
  group_by(id2, outcome_date) %>%
  summarise(count = n()) %>%
  filter(count > 1)  # Identify duplicates



# Calculate age based on the current date, round to the nearest year, and create age_band based on age
exits_df <- exits_df %>%
  #filter(!is.na(dob)) %>%
  mutate(
    age = round(as.numeric(difftime(Sys.Date(), dob, units = "weeks")) / 52.25),
    age_band = case_when(
      age < 5 ~ "<5",
      age >= 5 & age < 10 ~ "5-9",
      age >= 10 & age < 15 ~ "10-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 ~ "25+",
      TRUE ~ "Unknown"
    )
  )

# check for missing 
exits_df %>% summarise_all(~ sum(is.na(.)))


# Create a frequency table for the 'outcome' variable
outcome_frequency <- exits_df %>%
  group_by(outcome) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Arrange in descending order



# Specify the path for the RDS file in the 'dataout' folder
rds_file_path <- file.path("dataout", "exits_df.rds")

# Save the CD4 dataset as an RDS file
saveRDS(exits_df, rds_file_path)

# Confirm the file has been saved
cat("RDS file saved at:", rds_file_path, "\n")









