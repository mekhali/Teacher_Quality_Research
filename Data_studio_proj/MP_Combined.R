library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(car)

#Import data and give each spreadsheet an easy data frame (df) name
#Note: Not all data from these sets was used in the final project, however, I did run various tests on them prior
#I think all the data is useful, and so I am keeping them in the final combined data set for anyone who wants to look into the other variables further.
#However, for those who only want to replicate what was in the final article, that datasets used for that were: 
#as, pe, tea, tq, inc, and pl (had school poverty level data)
ss <- read_csv("staffing_and_vacancy_report_statistics.csv")
as <- read_csv("assessment_statistics.csv")
isl <- read_csv("Inexperienced School Leaders.csv", skip = 3)
gs <- read_csv("On-time Graduation Rate.csv", skip = 3)
pe <- read_csv("Postsecondary Enrollment.csv", skip = 3)
pl <- read_csv("Provisionally Licensed.csv", skip = 3)
tea <- read_csv("Teacher Educational Attainment.csv", skip = 3)
tq <- read_csv("Teacher Quality.csv", skip = 3)
pps <- read_csv("Per Pupil Spending.csv", skip = 3)
inc <- read_csv("HDPulse_data_export_copy.csv")
brk <- read_csv("Breakfast Participation of Eligible Students.csv", skip = 3)


#Clean Data Sets, make sure numbers are listed as numeric and not characters, 
#give variables specific names so they don't get confused between data sets,
#and standardize variable names across data sets.
ss_clean <- clean_names(ss) %>%
  mutate(percent_unfilled = as.numeric(gsub("%", "", percent_unfilled))) %>% 
  rename(number_unfilled = number_of_unfilled_positions_by_fte)

as_clean <- clean_names(as) %>% 
  mutate(pass_rate = as.numeric(pass_rate)) %>% 
  mutate(subject2 = case_when(subject == "Science" ~ "Life and Physical Sciences", 
                              subject == "English:Reading" ~ "English Language and Literature", 
                              subject == "English:Writing" ~ "English Language and Literature", 
                              subject == "History and Social Science" ~ "Social Sciences and History",
                              TRUE ~ subject))

pps_clean <- clean_names(pps) %>% 
  rename(total_school_level_ppexp = school_level_expenditures_per_pupil_subtotal) %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

brk_clean <- clean_names(brk) %>% 
  rename(breakfast_percent = percent) %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

#for some data sets like this one, you want to use variable values across all schools 
#instead of a certain poverty level or title 1 code
#this helps avoid duplicate school values
isl_clean <- clean_names(isl) %>% 
  filter(poverty_level == "All Schools") %>% 
  filter(title1_code == "All Schools") %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

gs_clean <- clean_names(gs) %>% 
  filter(subgroup == "All Students") %>% 
  mutate(graduation_rate = as.numeric(virginia_on_time_graduation_rate)) %>% 
  mutate(completion_rate = as.numeric(completion_rate)) %>% 
  mutate(cohort = as.numeric(cohort)) %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

pe_clean <- clean_names(pe) %>% 
  filter(subgroup == "All Students") %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools")) %>% 
  mutate(ps_enrollment_percent = as.numeric(percent))

#assign poverty level as a dummy variable in this data set so it can be used for quantitative analysis
pl_clean <- clean_names(pl) %>% 
  filter(x10 == "All Schools") %>% 
  mutate(poverty_high = if_else(poverty_level == "High Poverty", 1, 0)) %>%
  mutate(poverty_low = if_else(poverty_level == "Low Poverty", 1, 0)) %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

bachelor_tea_clean <- clean_names(tea) %>% 
  filter(type == "Bachelor's Degree") %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  rename(bachelors_percent = year_percent) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

masters_tea_clean <- clean_names(tea) %>% 
  filter(type == "Master's Degree") %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  rename(masters_percent = year_percent) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

tq_clean <- clean_names(tq) %>% 
  filter(title1_code == "All Schools") %>% 
  rename(division_name = division) %>% 
  rename(school_name = school) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"))

#Have to rename Alleghany County and Williamsburg City for consistency across datasets and maps
inc_clean <- clean_names(inc) %>% 
  rename(division_name = county) %>% 
  rename(county_rank = rank_within_us_of_3141_counties) %>% 
  rename(median_income = value_dollars) %>% 
  mutate(division_name = case_when(division_name == "Alleghany County and Clifton Forge City" ~ "Alleghany Highlands",
                                   division_name == "Williamsburg City" ~ "Williamsburg-James City County",
                                   division_name == "Bedford City and County" ~ "Bedford County",
                                   division_name == "Halifax County with South Boston City" ~ "Halifax County",
                                   TRUE ~ division_name))


#Combine the Cleaned Data Sets
combined <- ss_clean %>%
  full_join(as_clean, by = c("school_name", "division_name", "group_type" = "subject2")) %>%
  full_join(pps_clean, by = c("school_name", "division_name")) %>% 
  full_join(brk_clean, by = c("school_name", "division_name")) %>% 
  full_join(isl_clean, by = c("school_name", "division_name")) %>%
  full_join(bachelor_tea_clean, by = c("school_name", "division_name")) %>% 
  full_join(gs_clean, by = c("school_name", "division_name")) %>%
  full_join(masters_tea_clean, by = c("school_name", "division_name")) %>% 
  full_join(pe_clean, by = c("school_name", "division_name")) %>%
  full_join(pl_clean, by = c("school_name", "division_name")) %>%
  full_join(tq_clean, by = c("school_name", "division_name")) %>%
  full_join(inc_clean, by = c("division_name")) %>% 
  select(division_name, school_name, group_type, number_unfilled, percent_unfilled,
         pass_rate, school_level_expenditures_per_pupil_federal, school_level_expenditures_per_pupil_state,
         total_school_level_ppexp, per_pupil_federal_funds, per_pupil_state_funds,
         breakfast_percent, percent_of_inexperienced_principals, percent_of_inexperienced_assistant_principals,
         bachelors_percent, masters_percent, graduation_rate, completion_rate, ps_enrollment_percent, 
         special_education_percent, poverty_high, poverty_low, poverty_level, percent_of_inexperienced_teachers, 
         percent_of_out_of_field_teachers, percent_of_out_of_field_and_inexperienced_teachers, median_income, county_rank)


#Export data into a CSV file
write.csv(combined, "combined_data_set.csv", row.names = FALSE)
