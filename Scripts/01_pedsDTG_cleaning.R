# PROJECT: Pediatric DTG in Kenya 
# PURPOSE: Munge and Analysis of DTG Data 
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  a98f405c
# LICENSE: MIT
# DATE:   2024-07-03
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

    #Sheet 2: Newly Initiated 
    newint_df <- read_excel("Data/Data_pDTG10.xlsx",skip = 1, 2) %>% 
      janitor::clean_names()
    
      newint_df %>% glimpse()
    
    #Sheet 3: On Treatment 
    ontreat_df <- read_excel("Data/Data_pDTG10.xlsx", skip =1 ,3) %>% 
      janitor::clean_names()
    
      ontreat_df %>% glimpse()
    

# MUNGE ============================================================================
  

# Newly Initiated  --------------------------------------------------------


      #1 - Rename variables & create id2 based on id and facility 
      newint_df <- newint_df %>% 
        rename(id = client_number_de_identified, dob = date_of_birth, school_status= in_school_status,
               mfl_code= facility_mfl_code, orphan=opharnhood_status, facility= facility_name,
               date_hiv_diag= date_of_hiv_diagnosis, who_stage= who_stage_at_hiv_diagnosis, date_art_init= date_of_art_initiation,
               reg_art_init= art_regimen_at_initiation, date_dtg_init = dtg10_initiation_date, reg_dtg_init= dtg10_initiation_regimen,
               reg_art_current= current_art_regimen, date_reg_current_start= date_of_current_regimen_start_date) %>% 
        mutate(id2 = paste0(mfl_code, "_", id))
      
      #2 - Create distinct clients based on ids 
      newint_df %>% distinct(id) #9,063 distinct clients based on client ids 
      newint_df %>% distinct(facility, id)  #9,063 distinct clients based on client id and facility
      newint_df %>% distinct(mfl_code, id) #same
      
      ref_clientlist <- newint_df %>% distinct(facility, mfl_code, id) #9,063 distinct clients based on client id and facility mfl code
      sum(duplicated(ref_clientlist))
      
      
      #3 - Creating Master Client List based on necessary columns
        #Leave out (3) implementing partner, date of current regiment start date, current art regimen
      
      master_clientlist <- newint_df %>% distinct(id, id2, sex, dob, facility, mfl_code, county, orphan, school_status, education_level, 
                                                  date_hiv_diag, date_art_init, who_stage, reg_art_init,
                                                  date_dtg_init, reg_dtg_init, ovc_enrollment, caregiver) 
      
      
    #4 - check difference between master and reference lists (no difference)
      #occurrence <- data.frame(table(master_clientlist$id2)) %>% rename(id2=Var1)
      
      #master_clientlist <- master_clientlist %>% left_join(occurrence, by="id2") 
      
      #master_clientlist %>% filter(Freq>1) %>% names() #0 records 
      
    
      #5 - categorize period of ART initiation & DTG initiation 
      
        #fix dates 
          #note: some starting on DTG (control), others switch later on 
        master_clientlist$date_art_init <- ymd(master_clientlist$date_art_init)
        master_clientlist$dob <- ymd(master_clientlist$dob)
        master_clientlist$date_hiv_diag <- ymd(master_clientlist$date_hiv_diag)
        master_clientlist$ovc_enrollment <- ymd(master_clientlist$ovc_enrollment)
        master_clientlist$date_dtg_init <- ymd(master_clientlist$date_dtg_init) #some kids not optimized from the start on dtg 
        
        #There are 3 FYs: 2021 - 2023
        master_clientlist <- master_clientlist %>% mutate(
          art_init_period = case_when(
            date_art_init >= "2020-10-01" & date_art_init <= "2021-09-30"  ~ "FY21",
            date_art_init >= "2021-10-01" & date_art_init <= "2022-09-30"  ~ "FY22",
            date_art_init >= "2022-10-01" & date_art_init <= "2023-09-30"  ~ "FY23",
            TRUE ~ "Other Years"))
      
        master_clientlist <- master_clientlist %>% mutate(
          dtg_init_period = case_when(
            date_dtg_init >= "2020-10-01" & date_dtg_init <= "2021-09-30"  ~ "FY21",
            date_dtg_init >= "2021-10-01" & date_dtg_init <= "2022-09-30"  ~ "FY22",
            date_dtg_init >= "2022-10-01" & date_dtg_init <= "2023-09-30"  ~ "FY23",
            TRUE ~ "Other Years"))
        
      #6 - Calculating age at ART initiation & DTG initiation
        master_clientlist <- master_clientlist %>% filter(!(is.na(date_art_init)) & !(is.na(dob)))
        master_clientlist <- master_clientlist %>% mutate(
          age_at_art_init = as.integer(difftime(date_art_init, dob, unit = "weeks")/52.25),
          age_group_art_init = if_else(age_at_art_init <15, "<15 years", "15+ years"))
        
        master_clientlist %>% group_by(art_init_period, age_group_art_init) %>% summarize(clients = n())
        master_clientlist %>%  group_by(art_init_period, age_group_art_init) %>% 
          filter(age_group_art_init == "<15 years") %>% count()
        #Total children <15 on ART: 6850 and of those
        #FY21: 445 children under 15, FY22: 2979 children under 15, FY23: 3197 children under 15, Other: 229 
        
        master_clientlist <- master_clientlist %>% filter(!(is.na(date_dtg_init)) & !(is.na(dob)))
        master_clientlist <- master_clientlist %>% mutate(
          age_at_dtg_init = as.integer(difftime(date_dtg_init, dob, unit = "weeks")/52.25),
          age_group_dtg_init = if_else(age_at_dtg_init <15, "<15 years", "15+ years"))
        

        
        master_clientlist %>% group_by(dtg_init_period, age_group_dtg_init) %>% summarize(clients = n())
        master_clientlist %>%  group_by(dtg_init_period, age_group_dtg_init) %>% 
          filter(age_group_dtg_init == "<15 years") %>% count()
        #Total children <15 on DTG: 6848 and of those
        #FY21: 348 children under 15, FY22: 2796 children under 15, FY23: 3444 children under 15, Other: 260
        
        

# On Treatment ------------------------------------------------------------

        #1 - Rename variables & create id2 based on id and facility 
        ontreat_df <- ontreat_df %>% 
          rename(id = client_number_de_identified, dob = date_of_birth, school_status= in_school_status,
                 mfl_code= facility_mfl_code, orphan=opharnhood_status, facility= facility_name,
                 date_hiv_diag= date_of_hiv_diagnosis, who_stage= who_stage_at_hiv_diagnosis,
                 date_art_init= date_of_art_initiation,reg_art_init= art_regimen_at_initiation,
                 date_dtg_init = dtg10_initiation_date, reg_dtg_init= dtg10_initiation_regimen,
                 reg_art_current= current_art_regimen, date_reg_current_start= date_of_current_regimen_start_date) %>% 
          mutate(id2 = paste0(mfl_code, "_", id))
        
        #2 - Create distinct clients based on ids 
        ontreat_df %>% distinct(id) #54,241 distinct clients based on client ids 
        ontreat_df %>% distinct(facility, id)  #54,241 distinct clients based on client id and facility
        ontreat_df %>% distinct(mfl_code, id) #same
        
        ref_clientlist <- ontreat_df %>% distinct(facility, mfl_code, id) #9,063 distinct clients based on client id and facility mfl code
        sum(duplicated(ref_clientlist)) #0 duplicates 
        
        #3 - Creating Master Client List based on necessary columns
        #Leave out (3) implementing partner, date of current regiment start date, current art regimen
        
        master_clientlist_2 <- ontreat_df %>% distinct(id, id2, sex, dob, facility, mfl_code, county,
                                                       orphan, school_status, education_level, 
                                                    date_hiv_diag, date_art_init, who_stage, reg_art_init,
                                                    date_dtg_init, reg_dtg_init, ovc_enrollment, caregiver) 
        
        #5 - categorize period of ART initiation & DTG initiation 
        
        #fix dates 
        master_clientlist_2$date_art_init <- ymd(master_clientlist_2$date_art_init)
        master_clientlist_2$dob <- ymd(master_clientlist_2$dob)
        master_clientlist_2$date_hiv_diag <- ymd(master_clientlist_2$date_hiv_diag)
        #master_clientlist_2$ovc_enrollment <- ymd(master_clientlist_2$ovc_enrollment)
        master_clientlist_2$date_dtg_init <- ymd(master_clientlist_2$date_dtg_init) 
        
        #There are 3 FYs: 2021 - 2023
        master_clientlist_2 <- master_clientlist_2 %>% mutate(
          art_init_period = case_when(
            date_art_init >= "2020-10-01" & date_art_init <= "2021-09-30"  ~ "FY21",
            date_art_init >= "2021-10-01" & date_art_init <= "2022-09-30"  ~ "FY22",
            date_art_init >= "2022-10-01" & date_art_init <= "2023-09-30"  ~ "FY23",
            TRUE ~ "Other Years"))
        
        master_clientlist_2 <- master_clientlist %>% mutate(
          dtg_init_period = case_when(
            date_dtg_init >= "2020-10-01" & date_dtg_init <= "2021-09-30"  ~ "FY21",
            date_dtg_init >= "2021-10-01" & date_dtg_init <= "2022-09-30"  ~ "FY22",
            date_dtg_init >= "2022-10-01" & date_dtg_init <= "2023-09-30"  ~ "FY23",
            TRUE ~ "Other Years"))
        
        #6 - Calculating age at ART initiation & DTG initiation
        master_clientlist_2 <- master_clientlist_2 %>% filter(!(is.na(date_art_init)) & !(is.na(dob)))
        master_clientlist_2 <- master_clientlist_2 %>% mutate(
          age_at_art_init = as.integer(difftime(date_art_init, dob, unit = "weeks")/52.25),
          age_group_art_init = if_else(age_at_art_init <15, "<15 years", "15+ years")) #only includes children 
        
        master_clientlist_2 %>% group_by(age_group_art_init) %>% summarize(clients = n())
        master_clientlist_2 %>%  group_by(art_init_period, age_group_art_init) %>% count()
          #filter(age_group_art_init == "<15 years")%>%
          count()
          #Total children <15 on ART: 6850 and of those
          #FY21: 445 children under 15, FY22: 2979 children under 15, FY23: 3197 children under 15, Other: 229 
        
        master_clientlist_2 <- master_clientlist_2 %>% filter(!(is.na(date_dtg_init)) & !(is.na(dob)))
        master_clientlist_2 <- master_clientlist_2 %>% mutate(
          age_at_dtg_init = as.integer(difftime(date_dtg_init, dob, unit = "weeks")/52.25),
          age_group_dtg_init = if_else(age_at_dtg_init <15, "<15 years", "15+ years"))
        
        master_clientlist_2 %>% group_by(age_group_dtg_init) %>% summarize(clients = n())
        master_clientlist_2 %>%  group_by(dtg_init_period, age_group_dtg_init) %>% 
          filter(age_group_dtg_init == "<15 years") %>% count()
        #Total children <15 on DTG: 6848 and of those
        #FY21: 348 children under 15, FY22: 2796 children under 15, FY23: 3444 children under 15, Other: 260
        
        
  
# VIZ ============================================================================

      

# SPINDOWN ============================================================================

