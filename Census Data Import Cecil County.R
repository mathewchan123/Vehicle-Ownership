library(tidyverse)
library(tidycensus)

#choose our variables
var = load_variables(2019,"acs5",cache=TRUE) 

#B08006_008 _ public transportation

#B08203_007 _ workers in household

#B08141_031 _ worked from home 

#	B25044_009 _ renter occupied

#	B08006_002 _ car, truck, van

vars = var %>% dplyr::filter(grepl("B08006", name)) %>% 
  rbind(var %>% dplyr::filter(grepl("B08203", name))) %>%
  rbind(var %>% dplyr::filter(grepl("B08141", name))) %>%
  rbind(var %>% dplyr::filter(grepl("B25044", name))) 


#import Cecil County

census_api_key("bb637a985d9b9b2dcdf2ea179ed31ae79eb077b0")

df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist(),year=2019)

#Name is what changes one dataset from another.

df2 = df %>% dplyr::filter(NAME == "Cecil County, Maryland") %>% 
  cbind(vars)

df2 %>% write_csv(file = "Cecil_County_Census_Data.csv")
