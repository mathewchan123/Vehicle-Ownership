#Script made by Mathew Chan, supervised by Richard Latham


# Retrieving data -----------------------------------------------------


#Loads in installed packages
library(tidyverse)
library(tidycensus)
library(plyr)
library(ggplot2)
library(patchwork)

#Retrieves census data through use of requested API key
Sys.getenv("CENSUS_API_KEY") 

#Loads census data that was retrieved from API key
var = load_variables(2019,"acs5",cache=TRUE) 

#Filtering out data based on census concepts
vars = var %>% dplyr::filter(grepl("B08006", name, label)) %>% 
  rbind(var %>% dplyr::filter(grepl("B08203", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08141", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25044", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B05011", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08016", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B28006", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25034", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25121", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B28011", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B16001", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B23007", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B02001", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B14007", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B18103", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08302", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08303", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B16001", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25024", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B19001", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B01001", name, label)))  

#Retrieves variables based on 1 year of American Community Survey data from 2016 to 2019
df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2019)
df2 = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2018,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2018)
df3 = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2017,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2017)
df4 = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2016,keep_geo_vars=TRUE, survey = "acs1") %>% mutate(year = 2016)


# Manipulating data -------------------------------------------------------


#row binds all years of data from 2016 to 2019
all_df = df %>% 
  rbind(df2) %>%
  rbind(df3) %>%
  rbind(df4) 

#reorganizing data from row-rise to column-wise 
all_county_df = all_df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate")

#Turning absolute numbers into proportions by dividing total population of the group by the total population of the county
multi_variable_data = all_county_df %>% select(GEOID, NAME, year, B01001_001, B25044_009, B08203_007, B25034_011, B28011_002, B02001_002, B14007_017, B18103_001, B08302_005, B08303_008, B25024_003, B08203_003 ,B08203_004 ,B08203_005 ,B08203_006, B19001_012, B08203_013) %>% 
  mutate(Renter_housing_prop = B25044_009/B01001_001) %>%
  mutate(No_home_workers_prop = B08203_007/B01001_001) %>%
  mutate(Structure_built_1939_earlier_prop = B25034_011/B01001_001) %>%
  mutate(Household_income_60k_to_70k_prop = B19001_012/B01001_001) %>%
  mutate(Internet_subscription_prop = B28011_002/B01001_001) %>%
  mutate(White_race_prop = B02001_002/B01001_001) %>%
  mutate(College_undergraduate_prop = B14007_017/B01001_001) %>%
  mutate(Vision_difficulty_prop = B18103_001/B01001_001) %>%
  mutate(Depart_work_6AM_629AM_prop = B08302_005/B01001_001) %>%
  mutate(Travel_time_work_30_to_34_mins_prop = B08303_008/B01001_001) %>%
  mutate(Attached_1_unit_building_prop = B25024_003/B01001_001) %>%
  mutate(One_worker_at_home_prop = B08203_013/B01001_001) %>%
  mutate(vehicle_ownership_prop = (B08203_003 + B08203_004 + B08203_005 + B08203_006)/ B01001_001)

#plotting graph vs. graph (scatterplot matrix) of all comparison graphs
#pairs(multi_variable_data %>% select(-c(NAME, GEOID)) %>% select(c(Depart_work_6AM_629AM_prop , Travel_time_work_30_to_34_mins_prop , Attached_1_unit_building_prop , Household_income_60k_to_70k_prop , Lives_outside_principal_city_prop , Public_transit_work_prop , Renter_housing_prop , Structure_built_1939_earlier_prop , One_worker_at_home_prop)))

#training data
df_train =  multi_variable_data %>% subset(year<2019) %>% select(-year)
df_test = multi_variable_data %>% subset(year>=2019) %>% select(-year)

#this line will say what variables are to be removed because of NA
model = lm(data=df_train, vehicle_ownership_prop ~ Depart_work_6AM_629AM_prop  + Household_income_60k_to_70k_prop + Renter_housing_prop + Structure_built_1939_earlier_prop + One_worker_at_home_prop + College_undergraduate_prop + Travel_time_work_30_to_34_mins_prop + Structure_built_1939_earlier_prop + Vision_difficulty_prop + White_race_prop + Internet_subscription_prop)


# Summarizing data --------------------------------------------------------


#summary of the model
summary(model)

#tells total number of data that is not available in the data set (NA)
is.na(df_test) %>% sum()

#uses backwards selection in step-wise function to finds fit of variables into model
step(model)

#residual plots of model
plot(model)

#variable for prediction
predict(model,newdata = df_test)

#predicting the years from 2019 onward
df_test$pred = predict(model,newdata = df_test)
df_test %>% select(pred, vehicle_ownership_prop, everything())

#graph/plot predictions
df_test %>%
  ggplot() + theme_bw() +
  geom_point(aes(x=pred,y=vehicle_ownership_prop)) + # plotting predicted vs actual
  geom_line(aes(x=vehicle_ownership_prop,y=vehicle_ownership_prop),color="blue",linetype=2) + # plotting actual data
  #geom_smooth(aes(x=pred,y=vehicle_ownership_prop),color="black") + # plotting predicted vs actual
  #scale_x_continuous(limits=c(4,6),name = "Predicted") +
  #scale_y_continuous(limits=c(4,6),name = "Actual") +
  ggtitle("Model Performance - Predicted vs. Actual") + # title of plot
  xlab("Prediction") + # x axis title
  ylab("Proportion of Household Vehicle Ownership") # y axis title

