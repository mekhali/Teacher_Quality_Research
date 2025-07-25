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
#Filter for only middle and high schools: Not looking at elementary schools to keep it standardized. 
#Elementary schools are generally grouped as a different category, and when VDOE discusses any statistics for SOL exams, 
#it tends to group middle and high schools together. I am sticking to that so this data can always be mapped onto other things.
#Since we also want to know the impact of unfilled positions in this regression, we removed schools that have 0 positions unfilled
#since there are many and they hide the impact of what having even a single unfilled postion has. They skew the data.

combined_cleaned <- combined %>%
  filter(percent_unfilled != 0) %>%
  filter(school_name != "Thomas Jefferson High for Science and Technology") %>% 
  mutate(school_name = case_when(school_name == "Baker-Butler Elem" ~ "Baker-Butler Elementary",
                                 school_name == "Arlington Traditional" ~ "Arlington Traditional Elementary",
                                 school_name == "Claremont Immersion" ~ "Claremont Immersion Elementary",
                                 school_name == "Lake Taylor" ~ "Lake Taylor High",
                                 school_name == "Montessori Public School of Arlington" ~ "Montessori Public School of Arlington Elementary",
                                 school_name == "R.I.S.E Academy at the John M. Langston Campus" ~ "R.I.S.E Academy at the John M. Langston Campus 6-12",
                                 school_name == "Richmond Alternative" ~ "Richmond Alternative 6-12",
                                 school_name == "Ruffner School" ~ "Ruffner School Middle",
                                 TRUE ~ school_name)) %>% 
  filter(str_detect(school_name, "Middle|Intermediate|Secondary|High|6-12")) %>% 
  filter(!str_detect(school_name, "Elementary|Primary")) %>% 

#Note: When determining which non-traditional schools to remove, in addition to combing through the data set, and searching for lists of charter, magnet, and lab schools online
#I also used this filter function to find schools that didn't fit the traditional elementary, middle, high
#The entire list of removed schools can be found on my git.hub, 
#most of the schools on that list got filtered out because they do not hold include the taglines below
#filter for non-traditional schools
charter <- combined %>% 
  filter(!str_detect(school_name, "Elementary|Middle|High|Intermediate|Primary|Secondary")) %>% 
  group_by(school_name) %>% 
  summarize(count=n(), na.rm = TRUE)


#Post Secondary Enrollment Regression
ps_enrollment_model <- lm(ps_enrollment_percent ~  percent_of_out_of_field_teachers + 
                            masters_percent + percent_unfilled +
                            percent_of_inexperienced_teachers +
                            median_income + poverty_low, , data = combined_cleaned)
summary(ps_enrollment_model)

#turn off scientific notation to make it easier to read/screenshot
options(scipen=999)
summary(ps_enrollment_model)

#turn scientific notation back on
options(scipen=0)

#Check for Multicollinearity in the model, if VIF = 1 then there is no multicollinearity
#if VIF is between 1-5 that is moderate multicollinearity and acceptable in most models
#if VIF is greater than 5, then there is high multicollinearity and corrective measures are required
vif(ps_enrollment_model)

#This model suggests there is slight multicollinearity between masters_percent and median_income,
#which is as expected, but it is not large enough to inflate standard errors or require corrective measures


