#Script made by Mathew Chan, supervised by Richard Latham
#This code was used for an experimental purpose.


# Retrieving Data ---------------------------------------------------------


#Loads in installed packages
library(tidyverse)
library(tidycensus)
library(plyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

#Retrieves census data through use of requested API key
Sys.getenv("CENSUS_API_KEY") 

#Loads census data that was retrieved from API key
var_combo = load_variables(2019,"acs5",cache=TRUE) 

#Filters out data based on census concepts
vars_combo = var_combo %>% dplyr::filter(grepl("B08006", name, label)) %>% 
  rbind(var_combo %>% dplyr::filter(grepl("B08203", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B08141", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B25044", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B05011", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B08016", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B28006", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B25034", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B25121", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B28011", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B16001", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B23007", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B02001", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B14007", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B18103", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B08302", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B08303", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B16001", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B25024", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B19001", name, label))) %>%
  rbind(var_combo %>% dplyr::filter(grepl("B01001", name, label)))  

#Retrieves variables based on 1 year of American Community Survey data from 2016 to 2019
df_combo = get_acs(geography="county", state = c("PA","MD"),variables = vars_combo %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2019)
df2_combo = get_acs(geography="county", state = c("PA","MD"),variables = vars_combo %>% select(name) %>% unlist() %>% unname(),year=2018,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2018)
df3_combo = get_acs(geography="county", state = c("PA","MD"),variables = vars_combo %>% select(name) %>% unlist() %>% unname(),year=2017,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2017)
df4_combo = get_acs(geography="county", state = c("PA","MD"),variables = vars_combo %>% select(name) %>% unlist() %>% unname(),year=2016,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2016)


# Manipulating Data -------------------------------------------------------


#Row binds all years of data from 2016 to 2019
all_df_combo = df_combo %>% 
  rbind(df2_combo) %>%
  rbind(df3_combo) %>%
  rbind(df4_combo) 

#Reorganizes data from row-rise to column-wise
all_county_df_combo = all_df_combo %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate")

#Turns absolute numbers into proportions by dividing total population of the group by the total population of the county
multi_variable_data_combo = all_county_df_combo %>% select(GEOID, NAME, year, B01001_001, B25044_009, B08203_007, B25034_011, B28011_002, B02001_002, B14007_017, B18103_001, B08302_005, B08303_008, B25024_003, B08203_003 ,B08203_004 ,B08203_005 ,B08203_006, B19001_012, B08203_013) %>% 
  mutate(Renter_housing_prop_combo = B25044_009/B01001_001) %>%
  mutate(No_home_workers_prop_combo = B08203_007/B01001_001) %>%
  mutate(Structure_built_1939_earlier_prop_combo = B25034_011/B01001_001) %>%
  mutate(Household_income_60k_to_70k_prop_combo = B19001_012/B01001_001) %>%
  mutate(Internet_subscription_prop_combo = B28011_002/B01001_001) %>%
  mutate(White_race_prop_combo = B02001_002/B01001_001) %>%
  mutate(College_undergraduate_prop_combo = B14007_017/B01001_001) %>%
  mutate(Vision_difficulty_prop_combo = B18103_001/B01001_001) %>%
  mutate(Depart_work_6AM_629AM_prop_combo = B08302_005/B01001_001) %>%
  mutate(Travel_time_work_30_to_34_mins_prop_combo = B08303_008/B01001_001) %>%
  mutate(Attached_1_unit_building_prop_combo = B25024_003/B01001_001) %>%
  mutate(One_worker_at_home_prop_combo = B08203_013/B01001_001) %>%
  mutate(vehicle_ownership_prop_combo = (B08203_003 + B08203_004 + B08203_005 + B08203_006)/ B01001_001)

#Plots graph vs. graph (scatter plot matrix) of all comparison graphs
pairs(multi_variable_data_combo %>% select(-c(NAME, GEOID)) %>% select(c(College_undergraduate_prop_combo, Travel_time_work_30_to_34_mins_prop_combo, Structure_built_1939_earlier_prop_combo, Vision_difficulty_prop_combo, White_race_prop_combo, Internet_subscription_prop_combo, Attached_1_unit_building_prop_combo)))


#Plotting histograms against each other to compare data of both states. x can equal any variable from the census
p1 = multi_variable_data_combo %>% dplyr::filter(str_detect(NAME,"Maryland")) %>%
  ggplot() + geom_histogram(aes(x = College_undergraduate_prop_combo))
  
p2 = multi_variable_data_combo %>% dplyr::filter(str_detect(NAME,"Pennsylvania")) %>%
  ggplot() + geom_histogram(aes(x = College_undergraduate_prop_combo))

#Displays histograms
p1 / p2

#Creates the linear model
model_combo = lm(data = multi_variable_data_combo, formula = vehicle_ownership_prop_combo ~ College_undergraduate_prop_combo + Travel_time_work_30_to_34_mins_prop_combo + Structure_built_1939_earlier_prop_combo + Vision_difficulty_prop_combo + White_race_prop_combo + Internet_subscription_prop_combo + Attached_1_unit_building_prop_combo)


# Summarizing Data --------------------------------------------------------


#Summary of the model
summary(model_combo)

#Uses backwards selection/elimination in step-wise function to finds fit of variables into model
step(model_combo)

#Residual plots of model
plot(model_combo)

#Plots the combined data as x-axis and vehicle ownership as y-axis (graph not used currently)
#multi_variable_data_combo %>%
  #ggplot() + theme_bw() +
  #geom_point(aes(x=data_model_combo,y=vehicle_ownership_prop_combo)) + # Plots predicted vs actual
  #geom_line(aes(x=data_model_combo,y=vehicle_ownership_prop_combo),color="blue",linetype=1) + 
  # Plots actual data
  #scale_x_continuous(limits=c(4,6)) +
  #scale_y_continuous(limits=c(4,6)) +
  #ggtitle("Model of combined states") + # Title of plot
  #xlab("Variable Proportions") + # x axis title
  #ylab("Proportion of Household Vehicle Ownership") # y axis title
