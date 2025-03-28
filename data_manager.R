

# Data pre-processing script

library(tidyverse)
library(lubridate)
library(here)
library(ggthemes)
library(scales)
library(haven)
library(survival)  # For survival analysis
library(survey)  # For survey-weighted analysis
library(ggfortify)
library(forcats)



data_2003 <- read_sas(here('Data','2003_BR.sas7bdat')) |> 
  dplyr::select(CASEID, V463A, BIDX, V005,M5, V106, ML101, V008, V501,V701, BORD,
                B0:B5, B7, B8, V024, V025, V190, V012, B11, M15, M4,  H10, H11) |> 
  #pick only singletons -- no multiple births- B=0
  #also pick only children below the age of five years prior to the interview date/year = 2003
  dplyr::filter(B0 == 0 & B2 >= 1999) |> 
  mutate(region = factor(V024, label = c('Nairobi','Central','Coast',
                                         'Eastern','Nyanza','Rift Valley',
                                         'Western','North Eastern'))) |>
  mutate(place_of_delivery = factor(M15, 
                                    label = c("Respondents home",
                                              "Other home",
                                            
                                              "Govt. hospital",
                                              "Govt. health center",
                                            
                                              "Govt. dispensary",
                                              "Other public",
                                             
                                              "Private hosp/clinic",
                                              "Mission Hospital/clinic",
                                              "Nursing/maternity home",
                                              "Other private medical",
                                              "OTHER"))) |> 
  dplyr::select(-c(V024, M15)) 


data_2008_2009 <- read_sas(here('Data','2008_2009_BR.sas7bdat')) |> 
  dplyr::select(CASEID, V463A, BIDX,V106, M5, V190,ML101, V005, V008, V501,V701,
                BORD,B0:B5, B7, B8, V024, V012, V025, B11, M15, M4,  H10, H11) |> 
  
  #pick only singletons -- no multiple births - B=0
  #also pick only children below the age of five years prior to the interview date/year = 2009
  dplyr::filter(B0 == 0 & B2 >= 2005) |> 
  mutate(region = factor(V024, label = c('Nairobi','Central','Coast',
                                         'Eastern','Nyanza','Rift Valley',
                                         'Western','North Eastern'))) |>
  mutate(place_of_delivery = factor(M15,
                                    label = c("Respondents home",
                                              "Other home",
                                              "Enroute to provider",
                                              
                                              "Govt. hospital",
                                              "Govt. health center",
                                              "Govt. dispensary",
                                               
                                              "Other public",
                                              "Mission Hospital/clinic",
                                              
                                              "Private hosp/clinic",
                                              
                                              "Nursing/maternity home",
                                              "Other private medical",
                                              "OTHER"))) |> 
  dplyr::select(-c(V024, M15))


data_2014 <- read_sas(here('Data','2014_BR.sas7bdat')) |> 
  dplyr::select(CASEID, V463A, BIDX, V190,M5, ML101, V106, V005, V008, V501,V701, BORD,
                B0:B5, B7,B8, V024, V012, V025, B11, M15, M4,  H10, H11) |> 
  
  #pick only singletons -- no multiple births - B=0
  #also pick only children below the age of five years prior to the interview date/year = 2014
  dplyr::filter(B0 == 0 & B2 >= 2010) |> 
  mutate(region_ = case_when(V024 == 7 ~ 6,
                             V024 == 8 ~ 7,
                             V024 == 9 ~ 8,
                            .default = V024),
         region = factor(region_, label = c('Coast','North Eastern','Eastern',
                                            'Central','Rift Valley','Western',
                                            'Nyanza','Nairobi'))) |> 
  mutate(place_of_delivery = factor(M15, 
                                    label = c("Respondents home",
                                              "Other home",
                                                  
                                              "Govt. hospital",
                                              "Govt. health center",
                                              "Govt. health post",
                                                   
                                              "Other public",
                                                    
                                              "Private hosp/clinic",
                                              "Mission Hospital/clinic",
                                              "Nursing/maternity home",
                                              "Other private medical",
                                              "OTHER"))) |> 
  dplyr::select(-c(V024, region_, M15))


data_2022 <- read_sas(here('Data','2022_BR.sas7bdat')) |> 
  dplyr::select(CASEID, V463A, BIDX,V106, M5,V190, ML101, V005, V008, V501,V701, BORD,
                B0:B5, B7, B8, B11, V012, V024, V025, M15, M4,  H10, H11) |> 
  
  # pick only singletons -- no multiple births - B=0
  #also pick only children below the age of five years prior to the interview date/year = 2022
  dplyr::filter(B0 == 0 & B2 >= 2018) |> 
  
  #Convert counties to former provinces
  mutate(region_ = case_when(V024 == 1          ~ 1,
                             V024 %in% c(18:22) ~ 2,
                             V024 %in% c(1:6)   ~ 3,
                             V024 %in% c(10:17) ~ 4,
                             V024 %in% c(41:46) ~ 5,
                             V024 %in% c(23:36) ~ 6,
                             V024 %in% c(37:40) ~ 7,
                             V024 %in% c(7:9)   ~ 8),
         region = factor(region_, 
                         label = c('Nairobi','Central','Coast',
                                   'Eastern','Nyanza','Rift Valley',
                                   'Western','North Eastern'))) |> 
  mutate(place_of_delivery = factor(M15, 
                                    label = c("Respondents home",
                                              "Other home",
                                                   
                                              "Govt. hospital",
                                              "Govt. health center",
                                              "Govt. health post",
                                                 
                                              "Private hosp/clinic",
                                              "Mission Hospital/clinic",
                                              "NGO hospital",
                                              "FBO/mission hospital",
                                              "FBO/mission clinic",
                                              "OTHER"))) |> 
  dplyr::select(-c(V024, region_, M15))


DHS_data <- bind_rows(data_2003, data_2008_2009, data_2014, data_2022)

## collapse some categories for place of delivery into one category

# table(DHS_data$place_of_delivery, exclude = T)

DHS_data = DHS_data |> 
  rename(place_of_delivery2 = place_of_delivery) |> 
  mutate(place_of_delivery = fct_collapse(place_of_delivery2,
        home = c("Respondents home","Other home"),
        public_facility = c("Govt. hospital","Govt. health center",
                            "Govt. dispensary","Govt. health post",
                            "Other public","Nursing/maternity home"),
        private_facility = c("Private hosp/clinic","Other private medical"),
        mission_NGO_facility = c("FBO/mission hospital","FBO/mission clinic",
                                 "NGO hospital","Mission Hospital/clinic"),
        other_facility = c("OTHER","Enroute to provider"))) |> 
  dplyr::select(-place_of_delivery2)

DHS_data$place_of_delivery[is.na(DHS_data$place_of_delivery)] = "other_facility" 

# table(DHS_data$place_of_delivery, exclude = T)


#Distribution of birth dates

summary(DHS_data$B2)
summary(data_2003$B2)
summary(data_2008_2009$B2)
summary(data_2014$B2)
summary(data_2022$B2)


# Distribution of survival times - age at death in months

summary(DHS_data$B7)
sum(is.na(DHS_data$B7))

#compute new columns and rename some columns for easier readability

# GENERATE AGE AT DEATH IN MONTHS (survival time)

### If the child is dead then age at death is captured in B7, otherwise if the 
### child is alive then their survival time is counted as the numbers of months
### since birth(B3) till the date of interview(V008)

DHS_data <- DHS_data %>% 
  mutate(B7 = ifelse(B5 == 1, V008 - B3, B7))

summary(DHS_data$B7)

# longest survival time is 57 months while shortest survival time is zero months


###########################################

#####  Survival analysis setup  ###########

###########################################

## perturb survival times a little bit by 0.01 to avoid having exactly zero survival periods

DHS_data <- DHS_data |> 
  mutate(B7 = ifelse(B7 == 0, B7 + 0.01, B7))

summary(DHS_data$B7)

## create the under-five mortality indicator

DHS_data <- DHS_data |> 
  mutate(u5m = ifelse(B5 == 0 & !is.na(B3), 1, 0))


table(DHS_data$u5m, exclude = T)

# Split survival data to create analysis categories

breaks <- seq(0, 60, by = 12)

DHS_data <- survSplit(Surv(B7, u5m) ~ ., data = DHS_data, 
                        cut = breaks,  episode = "category")


table(DHS_data5$category, exclude = T)


# create year of analysis each child is contributing

DHS_data <- DHS_data %>% 
  mutate(
    # event = as.numeric(B5 == 0),
    age_death_months = B7,
    died_under_five = ifelse(B7 < 60 & !is.na(B7), 1, 0),
    yob = B2, # Year of birth
    yoa = 1900 + as.integer(((B3 + tstart) - 1) / 12)  # Year of analysis
  ) |> 
  dplyr::select(-c(B5, B7))



# Define 5-year periods
# DHS_data <- DHS_data %>% 
#   mutate(period = case_when(
#     yob >= 2003 & yob < 2007 ~ 1,
#     yob >= 2008 & yob < 2012 ~ 2,
#     yob >= 2013 & yob < 2017 ~ 3,
#     yob >= 2018 & yob < 2022 ~ 4
#   ))

# we will compute mean mortality over these time periods

DHS_data <- DHS_data %>% 
  mutate(period = case_when(
    yoa >= 1999 & yoa <= 2003 ~ 1,
    yoa >= 2005 & yoa <= 2009 ~ 2,
    yoa >= 2010 & yoa <= 2014 ~ 3,
    yoa >= 2018 & yoa <= 2022 ~ 4
  ))

#check how the data is distributed across five-year periods

table(DHS_data$period,  exclude = T)


DHS_data$period <- factor(DHS_data$period, 
                  labels = c("1999-2003","2005-2009", "2010-2014", "2018-2022"))



# Rename some columns

DHS_data1 <- DHS_data |> 
  mutate(smoke_status   = factor(V463A, label = c('No','Yes')),
         marital_status = factor(V501,  label = c('Never married','Married',
                                                 'Living together','Widowed',
                                                 'Divorced','Not living together')),
         # husband_level_educ = factor(V701, label = c('No education','Primary',
         #                                             'Secondary','Higher',
         #                                             "Don't know")),
         mother_level_educ = factor(V106, label = c('No education','Primary',
                                                    'Secondary','Higher')),
         wealth_index = factor(V190, label = c('Poorest','Poorer',
                                               'Middle','Richer','Richest')),
         type_of_bednet = factor(ML101, label = c('No net','Only treated nets',
                                                  'Both treated and untreated nets',
                                                  'Only untreated nets')),
         birth_order_no    = BORD,
         age_of_the_mother = V012,
         type_of_residence = factor(V025, label = c('Urban','Rural')),
         # date_of_birth = B3,
         sex_of_child  = factor(B4, label = c('Male','Female')),
         # current_age_of_child_alive = B8,
         preced_birth_interval_months = B11,
         #duration_of_breastfeeding = M4,
         duration_of_breastfeeding = M5,
         vaccination_satus = factor(H10, label = c('No','Yes',"Don't know")),
         had_diarrhea_recently = factor(H11, label = c('No',
                                                       'Yes, last two weeks',
                                                       "Don't know"))) |> 
  dplyr::select(-c(V463A, V501, B2, V008,ML101, B11, V012, V025, V701, B0, B8,
                   B4, H10, H11,BORD, B3, M5, B1, V190, V106))


# table(DHS_data1$smoke_status)

## Check distribution of missingness

miss_na  <- function(x) {
  
  x |> is.na() |> sum()
  
}

DHS_data1 |> 
  sapply(miss_na)

# drop_cases = DHS_data1 |> 
#   drop_na()
# 
# dim(drop_cases)

#save to local file

#write.csv(DHS_data1, 'Data/DHS_final.csv', row.names = FALSE)

write_rds(DHS_data1, 'Data/DHS_final2.rds')




