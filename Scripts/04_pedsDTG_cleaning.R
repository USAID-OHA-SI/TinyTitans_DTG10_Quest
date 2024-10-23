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
cd4_df <- read_excel("Data/Data_pDTG10.xlsx",skip = 1, 6) %>% 
  janitor::clean_names()

cd4_df %>% glimpse()




# MUNGE ============================================================================


# Newly Initiated  --------------------------------------------------------


#1 - Rename variables & create id2 based on id and facility 
cd4_df <- cd4_df %>% 
  rename(
    id = client_number_de_identified,
    ip = implementing_partner,
    facility = facility_name,
    mfl_code = facility_mfl_code,
    county= county,
    dob = date_of_birth,
    sex = sex,
    cd4_test_date = cd4_test_date_8,                
    cd4_test_perc = cd4_test_result_9 ,
    cd4_test_date_2 = cd4_test_date_10,                
    cd4_test_result = cd4_test_result_11,
  ) %>% 
  mutate(id2 = paste0(mfl_code, "_", id))

#2 - Create distinct clients based on ids 


cd4_df %>% distinct(id) #6,084   distinct clients based on client ids 
cd4_df %>% distinct(facility, id)  #6,084   distinct clients based on client id and facility



#3 - Creating visits_List based on necessary columns


#4 - check difference between master and reference lists (no difference)
#occurrence <- data.frame(table(master_clientlist$id2)) %>% rename(id2=Var1)

#master_clientlist <- master_clientlist %>% left_join(occurrence, by="id2") 

#master_clientlist %>% filter(Freq>1) %>% names() #0 records 


#5 - categorize period of ART initiation & DTG initiation 

#fix dates 
#note: some starting on DTG (control), others switch later on 

cd4_df$dob <- ymd(cd4_df$dob)


# Convert specified columns to factors
cd4_df <- cd4_df %>%
  mutate(
    facility = as.factor(facility),
    ip = as.factor(ip),
    mfl_code = as.factor(mfl_code),
    county = as.factor(county),
    sex = as.factor(sex)
  )

# check for duplicates 

# Check for duplicate entries by client ID and test date
cd4_df %>%
  group_by(id2, cd4_test_date) %>%
  summarise(count = n()) %>%
  filter(count > 1)  # Identify duplicates



# Calculate age based on the current date, round to the nearest year, and create age_band based on age
cd4_df <- cd4_df %>%
  filter(!is.na(dob)) %>%
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

cd4_df %>% summarise_all(~ sum(is.na(.)))
 

# what is the range for cd4 results 

summary(cd4_df$cd4_test_result)
summary(cd4_df$cd4_test_perc)


# Assuming your dataset is named cd4_data, and the CD4 percentage column is named 'cd4_percentage'
# Create categories for CD4 percentage

cd4_df <- cd4_df %>%
  mutate(
    cd4_perc_cat = case_when(
      cd4_test_perc >= 25 ~ "Normal Immune Function",
      cd4_test_perc >= 15 & cd4_test_perc < 25 ~ "Mild Immunosuppression",
      cd4_test_perc >= 10 & cd4_test_perc < 15 ~ "Advanced Immunosuppression",
      cd4_test_perc < 10 ~ "Severe Immunosuppression",
      TRUE ~ NA_character_  # Handle missing values
    )
  )

# View the data with the new category column
print(cd4_df)


# Create categories for CD4 count based on pediatric age groups
cd4_df <- cd4_df %>%
  mutate(
    cd4_count_cat = case_when(
      age < 1 & cd4_test_result >= 1500 ~ "Normal (Infants)",
      age < 1 & cd4_test_result >= 750 & cd4_test_result < 1500 ~ "Moderate Immunosuppression (Infants)",
      age < 1 & cd4_test_result < 750 ~ "Severe Immunosuppression (Infants)",
      
      age >= 1 & age < 5 & cd4_test_result >= 1000 ~ "Normal (1-5 years)",
      age >= 1 & age < 5 & cd4_test_result >= 500 & cd4_test_result < 1000 ~ "Moderate Immunosuppression (1-5 years)",
      age >= 1 & age < 5 & cd4_test_result < 500 ~ "Severe Immunosuppression (1-5 years)",
      
      age >= 5 & cd4_test_result >= 500 ~ "Normal (5+ years)",
      age >= 5 & cd4_test_result >= 200 & cd4_test_result < 500 ~ "Moderate Immunosuppression (5+ years)",
      age >= 5 & cd4_test_result < 200 ~ "Severe Immunosuppression (5+ years)",
      
      TRUE ~ NA_character_  # Handle missing values
    )
  )

# View the data with the new category column
print(cd4_df)

# Specify the path for the RDS file in the 'dataout' folder
rds_file_path <- file.path("dataout", "cd4_df.rds")

# Save the CD4 dataset as an RDS file
saveRDS(cd4_df, rds_file_path)

# Confirm the file has been saved
cat("RDS file saved at:", rds_file_path, "\n")









