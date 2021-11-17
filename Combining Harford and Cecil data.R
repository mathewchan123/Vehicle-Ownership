library(tidyverse)
library(tidycensus)

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


census_api_key("bb637a985d9b9b2dcdf2ea179ed31ae79eb077b0")

df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist(),year=2019)

cecil_df = df %>% dplyr::filter(NAME == "Cecil County, Maryland") %>% 
  cbind(vars) %>% select(NAME, concept, name, label, estimate, moe)

harford_df = df %>% dplyr::filter(NAME == "Harford County, Maryland") %>% 
  cbind(vars) %>% select(NAME, name, estimate, moe)

ult_df = cecil_df %>% left_join(harford_df,by= "name")

ult_df = ult_df %>% rename(
  "cecil_moe" = "moe.x",
  "cecil_estimate" = "estimate.x",
  "harford_moe" = "moe.y",
  "harford_estimate" = "estimate.y"
) %>% select(-starts_with("NAME", ignore.case = FALSE))

ult_df = ult_df %>% mutate(diff = cecil_estimate - harford_estimate)

ult_df %>% write_csv(file = "Harford_and_Cecil_County_Census_Data.csv")

