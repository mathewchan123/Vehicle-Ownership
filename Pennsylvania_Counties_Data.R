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
var_pa = load_variables(2019,"acs5",cache=TRUE) 

#Filters out data based on census concepts
vars_pa = var_pa %>% dplyr::filter(grepl("B08006", name, label)) %>% 
  rbind(var_pa %>% dplyr::filter(grepl("B08203", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B08141", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B25044", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B05011", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B08016", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B28006", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B25034", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B25121", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B28011", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B16001", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B23007", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B02001", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B14007", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B18103", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B08302", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B08303", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B16001", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B25024", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B19001", name, label))) %>%
  rbind(var_pa %>% dplyr::filter(grepl("B01001", name, label)))  

#Retrieves variables based on 1 year of American Community Survey data from 2016 to 2019
df_pa = get_acs(geography="county", state = "PA",variables = vars_pa %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2019)
df2_pa = get_acs(geography="county", state = "PA",variables = vars_pa %>% select(name) %>% unlist() %>% unname(),year=2018,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2018)
df3_pa = get_acs(geography="county", state = "PA",variables = vars_pa %>% select(name) %>% unlist() %>% unname(),year=2017,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2017)
df4_pa = get_acs(geography="county", state = "PA",variables = vars_pa %>% select(name) %>% unlist() %>% unname(),year=2016,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2016)


# Manipulating Data -------------------------------------------------------


#Row binds all years of data from 2016 to 2019
all_df_pa = df_pa %>% 
  rbind(df2_pa) %>%
  rbind(df3_pa) %>%
  rbind(df4_pa) 

#Reorganizes data from row-rise to column-wise
all_county_df_pa = all_df_pa %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate")

#Turns absolute numbers into proportions by dividing total population of the group by the total population of the county
multi_variable_data_pa = all_county_df_pa %>% select(GEOID, NAME, year, B01001_001, B25044_009, B08203_007, B25034_011, B28011_002, B02001_002, B14007_017, B18103_001, B08302_005, B08303_008, B25024_003, B08203_003 ,B08203_004 ,B08203_005 ,B08203_006, B19001_012, B08203_013) %>% 
  mutate(Renter_housing_prop_pa = B25044_009/B01001_001) %>%
  mutate(No_home_workers_prop_pa = B08203_007/B01001_001) %>%
  mutate(Structure_built_1939_earlier_prop_pa = B25034_011/B01001_001) %>%
  mutate(Household_income_60k_to_70k_prop_pa = B19001_012/B01001_001) %>%
  mutate(Internet_subscription_prop_pa = B28011_002/B01001_001) %>%
  mutate(White_race_prop_pa = B02001_002/B01001_001) %>%
  mutate(College_undergraduate_prop_pa = B14007_017/B01001_001) %>%
  mutate(Vision_difficulty_prop_pa = B18103_001/B01001_001) %>%
  mutate(Depart_work_6AM_629AM_prop_pa = B08302_005/B01001_001) %>%
  mutate(Travel_time_work_30_to_34_mins_prop_pa = B08303_008/B01001_001) %>%
  mutate(Attached_1_unit_building_prop_pa = B25024_003/B01001_001) %>%
  mutate(One_worker_at_home_prop_pa = B08203_013/B01001_001) %>%
  mutate(vehicle_ownership_prop_pa = (B08203_003 + B08203_004 + B08203_005 + B08203_006)/ B01001_001)

#Plots graph vs. graph (scatter plot matrix) of all comparison graphs
pairs(multi_variable_data_pa %>% select(-c(NAME, GEOID)) %>% select(c(College_undergraduate_prop_pa, Travel_time_work_30_to_34_mins_prop_pa, Structure_built_1939_earlier_prop_pa, Vision_difficulty_prop_pa, White_race_prop_pa, Internet_subscription_prop_pa, Attached_1_unit_building_prop_pa)))

#Creates the linear model
model_pa = lm(data=multi_variable_data_pa, formula = vehicle_ownership_prop_pa ~ College_undergraduate_prop_pa + Travel_time_work_30_to_34_mins_prop_pa + Structure_built_1939_earlier_prop_pa + Vision_difficulty_prop_pa + White_race_prop_pa + Internet_subscription_prop_pa + Attached_1_unit_building_prop_pa)


# Summarizing Data ---------------------------------------------------------


#Summary of the model
summary(model_pa)

#Uses backwards selection/elimination in step-wise function to finds fit of variables into model
step(model_pa)

#Residual plots of model
plot(model_pa)

#Plots the Pennsylvania data as x-axis and vehicle ownership as y-axis (graph not used currently)
#multi_variable_data_pa %>%
#ggplot() + theme_bw() +
#geom_point(aes(x=data_model_pa,y=vehicle_ownership_prop_pa)) + # Plots predicted vs actual
#geom_line(aes(x=data_model_pa,y=vehicle_ownership_prop_pa),color="blue",linetype=1) + 
# Plots actual data
#scale_x_continuous(limits=c(4,6)) +
#scale_y_continuous(limits=c(4,6)) +
#ggtitle("Model of Pennsylvania") + # Title of plot
#xlab("Variable Proportions") + # x axis title
#ylab("Proportion of Household Vehicle Ownership") # y axis title