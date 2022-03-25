#This code was being used for an experimental purpose. 

library(tidyverse)
library(tidycensus)
library(plyr)
library(ggplot2)
library(patchwork)

#Retrieves census data through use of requested API key
Sys.getenv("CENSUS_API_KEY") 

#loads census data that was retrieved from API key
var = load_variables(2019,"acs5",cache=TRUE) 

#B08203_007 _ no workers in house

#B25044_002 _ owner occupied housing

#B08006_033 _ Other transit means 

#	B08006_014 _ biked

#	B08006_015 _ walked

#B08006_008 _ public transportation

#B08203_007 _ workers in household

#B08141_031 _ worked from home 

#B25044_009 _ renter occupied <- not good indicator

#B25034_011 _ Structure built 1939 or earlier

#	B08006_002 _ car, truck, van
#B08303 = TRAVEL TIME TO WORK
#B08302 = TIME OF DEPARTURE TO GO TO WORK
#B08016 = PLACE OF WORK FOR WORKERS 16 YEARS AND OVER--METROPOLITAN STATISTICAL AREA LEVEL
#B08017 = PLACE OF WORK FOR WORKERS 16 YEARS AND OVER--MICROPOLITAN STATISTICAL AREA LEVEL
#B04007 = ANCESTRY
#B28006 = EDUCATIONAL ATTAINMENT BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
#B25034 = YEAR STRUCTURE BUILT

vars_1 = var %>% dplyr::filter(grepl("B08006", name, label)) %>% 
  rbind(var %>% dplyr::filter(grepl("B08203", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08141", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25044", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B05011", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08016", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B28006", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25034", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("C27021", name, label))) %>%
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

df = get_acs(geography="county", state = "PA" ,variables = vars %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE)
df2 = get_acs(geography="county", state = "PA" ,variables = vars %>% select(name) %>% unlist() %>% unname(),year=2018,keep_geo_vars=TRUE)

all_county_df = df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate")

all_county_df %>% dplyr::rename("Renter_occupied_housing"="B25044_009")

all_county_df %>% dplyr::rename("Owner_occupied_housing"="B25044_002")

all_county_df %>% dplyr::rename("Public transit to work"="B08006_008")

all_county_df %>% dplyr::rename("no workers in household"="B08203_007")

all_county_df %>% dplyr::rename("Naturalized citizens"="B08016_002")

all_county_df %>% dplyr::rename("Bachelor degree earned in computer present environment" = "B28006_014")

all_county_df %>% dplyr::rename("1939 earlier strucutre built" = "B25034_011")

all_county_df %>% dplyr::rename("Household income more than 100k, net worth more than 500k" = "B2 5121_106")

all_county_df %>% dplyr::rename("Internet Subscription" = "B28011_002")

all_county_df %>% dplyr::rename("Health Insurance Coverage" = "C27021_001")

all_county_df %>% dplyr::rename("English only speakers" = "B16001_002")

all_county_df %>% dplyr::rename("Families with children under 18" = "B23007_002")

all_county_df %>% dplyr::rename("White people" = "B02001_002")

all_county_df %>% dplyr::rename("College undergraduate" = "B14007_017")

all_county_df %>% dplyr::rename("Vision difficulty" = "B18103_001")

all_county_df %>% dplyr::rename("Depart to work 6:00AM to 6:29AM" = "B08302_005")

all_county_df %>% dplyr::rename("Travel time 30 to 34 min to work" = "B08303_008")

all_county_df %>% dplyr::rename("Attached 1 unit building" = "B25024_002")

all_county_df %>% dplyr::rename("Live outside principal city" = "B08016_013")

all_county_df %>% dplyr::rename("Total Population" = "B01001_001")

multi_variable_data = all_county_df %>% select(GEOID, NAME, B01001_001, B25044_009, B08006_008, B08203_007, B25034_011, B25121_106, B28011_002, C27021_001, B16001_003, B23007_002, B02001_002, B14007_017, B18103_001, B08302_005, B08303_008, B25024_003, B08203_003 ,B08203_004 ,B08203_005 ,B08203_006, B19001_012, B08016_013, B08203_013) %>% 
  mutate(Renter_housing_prop = B25044_009/B01001_001) %>%
  mutate(Public_transit_work_prop = B08006_008/B01001_001) %>%
  mutate(No_home_workers_prop = B08203_007/B01001_001) %>%
  mutate(Structure_built_1939_earlier_prop = B25034_011/B01001_001) %>%
  mutate(Household_income_60k_to_70k_prop = B19001_012/B01001_001) %>%
  mutate(Internet_subscription_prop = B28011_002/B01001_001) %>%
  mutate(Health_insurance_coverage_prop = C27021_001/B01001_001) %>%
  mutate(Spanish_speakers_prop = B16001_003/B01001_001) %>%
  mutate(Families_children_under_eighteen_prop = B23007_002/B01001_001) %>%
  mutate(White_race_prop = B02001_002/B01001_001) %>%
  mutate(College_undergraduate_prop = B14007_017/B01001_001) %>%
  mutate(Vision_difficulty_prop = B18103_001/B01001_001) %>%
  mutate(Depart_work_6AM_629AM_prop = B08302_005/B01001_001) %>%
  mutate(Travel_time_work_30_to_34_mins_prop = B08303_008/B01001_001) %>%
  mutate(Attached_1_unit_building_prop = B25024_003/B01001_001) %>%
  mutate(Lives_outside_principal_city_prop = B08016_013/B01001_001) %>%
  mutate(One_worker_at_home_prop = B08203_013/B01001_001) %>%
  mutate(vehicle_ownership_prop = (B08203_003 + B08203_004 + B08203_005 + B08203_006)/ B01001_001)

#multi_variable_data %>% ggplot() + geom_point(aes(x = Public_transit_work_prop , y= Structure_built_1939_earlier_prop)) #+
#ggrepel::geom_label_repel(aes(x = Public_transit_work_prop  , y=No_home_workers_prop , label = NAME))

#plotting graph vs. graph (scatterplot matrix) of all comparison graphs

model = lm(data = multi_variable_data, formula = vehicle_ownership_prop ~ Depart_work_6AM_629AM_prop + Household_income_60k_to_70k_prop + Renter_housing_prop + Structure_built_1939_earlier_prop + One_worker_at_home_prop + College_undergraduate_prop + Travel_time_work_30_to_34_mins_prop + Public_transit_work_prop)

summary(model)

step(model)

data_model = step(model)

plot(model)
multi_variable_data$predict = predict(model)


#all_county_data %>%
#ggplot() +
#geom_point(aes(x=B08016_002,y=B08203_001),colour="red") +
#geom_line(data=,aes(x=B08016_002,y=predict),size=1,alpha=0.3,colour="blue") +
#theme_bw()
#plot(data=harford_dff,x=B08016_002,y=B08203_001)
#abline(model)

