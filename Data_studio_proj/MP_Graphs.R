library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(car)

#Import combined data set
combined <- read_csv("combined_data_set.csv")

#Clean combined set
#Remove magnet, charter, and lab schools
#Adjust names of some schools to standardize them and make them easier to filter
combined_cleaned <- combined %>%
  filter(str_detect(school_name, "Elementary|Middle|High|Intermediate|Primary|Secondary")) %>%
  filter(school_name != "Thomas Jefferson High for Science and Technology") %>% 
  mutate(school_name = case_when(school_name == "Baker-Butler Elem" ~ "Baker-Butler Elementary",
                                 school_name == "Arlington Traditional" ~ "Arlington Traditional Elementary",
                                 school_name == "Claremont Immersion" ~ "Claremont Immersion Elementary",
                                 school_name == "Lake Taylor" ~ "Lake Taylor High",
                                 school_name == "Montessori Public School of Arlington" ~ "Montessori Public School of Arlington Elementary",
                                 school_name == "R.I.S.E Academy at the John M. Langston Campus" ~ "R.I.S.E Academy at the John M. Langston Campus 6-12",
                                 school_name == "Richmond Alternative" ~ "Richmond Alternative 6-12",
                                 school_name == "Ruffner School" ~ "Ruffner School Middle",
                                 TRUE ~ school_name))



#Isolate Math, Science, English Subjects (other subjects will be Null anyway because they don't filled in unique values)
#Filter for only middle and high schools: Not looking at elementary schools to keep it standardized. 
#Elementary schools are generally grouped as a different category, and when VDOE discusses any statistics for SOL exams, 
#it tends to group middle and high schools together. I am sticking to that so this data can always be mapped onto other things.
mes <- combined_cleaned %>% 
  filter(str_detect(group_type, "English Language and Literature|Mathematics|Life and Physical Sciences")) %>% 
  drop_na(school_name, pass_rate, number_unfilled, percent_unfilled) %>% 
  filter(str_detect(school_name, "Middle|Intermediate|Secondary|High|6-12")) %>% 
  filter(!str_detect(school_name, "Elementary|Primary"))

#Create a Seperate DF for Math because we isolate Math Test Scores in some graphs
#I mainly looked at math test pass rates because those are the most impacted by the independent variables
#But both English and Science test pass rates show similar results
math <- mes %>% 
  filter(str_detect(group_type, "Mathematics")) %>% 
  distinct()

#I used the math data set on all the below graphs because we don't want duplicate values of schools
#if science and english were added, there would be duplicate values, 
#This is because most of the variables are school-level variables, and not subject level
#but feel free to use the MES data set if you want to look at pass rates for each subject

#Scatterplots
ggplot(math, aes(x = masters_percent, y = ps_enrollment_percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "#b23fae") +
  labs(title = "Master's Percent on Post Secondary Enrollment Rate",
       x = "Percent of Teachers with Master's Degrees",
       y = "Post Secondary Enrollment Rate") +
  theme_minimal()

ggplot(math, aes(x = percent_of_out_of_field_teachers, y = pass_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "#b23fae") +
  labs(title = "Percent of Out-of-Field Teachers on Math SOL Pass Rate",
       x = "Percent of Out-of-Field Teachers",
       y = "Post Secondary Enrollment Rate") +
  theme_minimal()


#Bined Graphs
#Enrollment by Masters
masters_bins <- math %>% 
  mutate(unfilled_bins = cut(masters_percent,
                             breaks = seq(0, 100, by = 5),
                             include.lowest = TRUE,
                             right = FALSE)) %>% 
  group_by(unfilled_bins) %>%
  summarise(avg_percent = mean(ps_enrollment_percent, na.rm = TRUE)) %>% 
  drop_na(unfilled_bins, avg_percent)

ggplot(masters_bins, aes(x = unfilled_bins, y = avg_percent)) +
  geom_col(fill = "#e7b9dd") +
  labs(title = "Average Post Secondary Enrollment Rate by Masters Degree Percentage",
       x = "Masters Degree Percentage",
       y = "Avg Post Secondary Enrollment Rate") +
  theme_minimal()


#Pass Rate by Teacher Quality
#Filter for Math Pass Rates
teacher_qualification_bins <- math %>% 
  filter(group_type == "Mathematics") %>%
  mutate(unfilled_bins = cut(percent_of_out_of_field_teachers,
                             breaks = seq(0, 100, by = 10),
                             include.lowest = TRUE,
                             right = FALSE)) %>% 
  group_by(unfilled_bins) %>%
  summarise(avg_rate = mean(pass_rate, na.rm = TRUE)) %>% 
  drop_na(unfilled_bins, avg_rate)

ggplot(teacher_qualification_bins, aes(x = unfilled_bins, y = avg_rate)) +
  geom_col(fill = "#e7b9dd") +
  labs(title = "Average Math SOL Pass Rate by Out of Field Teachers",
       x = "Percent of Out of Field Teachers",
       y = "Avg Math SOL Pass Rate") +
  theme_minimal()


#By Poverty Level
#Academic Outcomes Stats: Post-Secondary Enrollment
avg_enrollment <- math %>%
  group_by(poverty_level) %>%
  summarise(avg_enrollment = mean(ps_enrollment_percent, na.rm = TRUE)) %>% 
  drop_na(poverty_level, avg_enrollment) %>% 
  mutate(poverty_level = factor(poverty_level, levels = c("Low Poverty", "Medium Poverty", "High Poverty")))

ggplot(avg_enrollment, aes(x = poverty_level, y = avg_enrollment, fill = poverty_level)) +
  geom_col() +
  scale_fill_manual(values = c("Low Poverty" = "#bfd3e6", 
                               "Medium Poverty" = "#8c96c6", 
                               "High Poverty" = "#8c6bb1")) +
  labs(title = "Average Post Secondary Enrollment Rate by Poverty Level",
       x = "Poverty Level",
       y = "Avg Post Secondary Enrollment Rate") +
  theme_minimal()


#Academic Outcomes Stats: Pass Rate
avg_pass <- math %>%
  filter(group_type == "Mathematics") %>% 
  group_by(poverty_level) %>%
  summarise(avg_rate = mean(pass_rate, na.rm = TRUE)) %>% 
  drop_na(poverty_level, avg_rate) %>% 
  mutate(poverty_level = factor(poverty_level, levels = c("Low Poverty", "Medium Poverty", "High Poverty")))

ggplot(avg_pass, aes(x = poverty_level, y = avg_rate, fill = poverty_level)) +
  geom_col() +
  scale_fill_manual(values = c("Low Poverty" = "#bfd3e6", 
                               "Medium Poverty" = "#8c96c6", 
                               "High Poverty" = "#8c6bb1")) +
  labs(title = "Average Post Math SOL Pass Rate by Poverty Level",
       x = "Poverty Level",
       y = "Avg Math SOL Pass Rate") +
  theme_minimal()


#Resource Stats: Out-of-Filed Teachers
avg_qualification <- math %>%
  group_by(poverty_level) %>%
  summarise(avg_out_field = mean(percent_of_out_of_field_teachers, na.rm = TRUE)) %>% 
  drop_na(poverty_level, avg_out_field) %>% 
  mutate(poverty_level = factor(poverty_level, levels = c("Low Poverty", "Medium Poverty", "High Poverty")))


ggplot(avg_qualification, aes(x = poverty_level, y = avg_out_field, fill = poverty_level)) +
  geom_col() +
  scale_fill_manual(values = c("Low Poverty" = "#f2e0f1", 
                               "Medium Poverty" = "#d48ccf", 
                               "High Poverty" = "#810f7c")) +
  labs(title = "Average Percent of Out of Field Teachers by Poverty Level",
       x = "Poverty Level",
       y = "Avg Percent of Out of Field Teachers") +
  theme_minimal()


#Resource Stats: Master's Degrees
avg_masters <- math %>%
  group_by(poverty_level) %>%
  summarise(avg_percent = mean(masters_percent, na.rm = TRUE)) %>% 
  drop_na(poverty_level, avg_percent) %>% 
  mutate(poverty_level = factor(poverty_level, levels = c("Low Poverty", "Medium Poverty", "High Poverty")))

ggplot(avg_masters, aes(x = poverty_level, y = avg_percent, fill = poverty_level)) +
  geom_col() +
  scale_fill_manual(values = c("Low Poverty" = "#f2e0f1", 
                               "Medium Poverty" = "#d48ccf", 
                               "High Poverty" = "#810f7c")) +
  labs(title = "Average Percent of Teachers with Master's Degrees by Poverty Level",
       x = "Poverty Level",
       y = "Avg Percent of Teachers with Master's Degrees") +
  theme_minimal()


#Create a CSV file that uploads properly to Datawrapper in order to create maps
#Group masters percent by district because Datawrapper provides maps by VA school district
#Median Income is already by District, 
#but master's percentage is not, so that will have to be averaged by district
#use "distinct" to remove duplicates (there will be duplicate school_name rows because of the multiple subjects)
#Note: When you upload this into data wrapper, some names will have to be adjusted in the website to match their database
datawrapper_df <- combined_cleaned %>%
  group_by(division_name) %>%
  mutate(district_masters_avg = mean(masters_percent, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(division_name, median_income, district_masters_avg) %>% 
  mutate(division_name = paste0(division_name, " Public Schools")) %>% 
  distinct()

write.csv(datawrapper_df, "datawrapper_df.csv", row.names = FALSE)




