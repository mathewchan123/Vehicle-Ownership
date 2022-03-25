#This code is no longer being used. 

library(tidyverse)
library(tidycensus)

Sys.getenv("CENSUS_API_KEY") #census_api_key("CENSUS_API_KEY",install=TRUE)
# census_api_key("CENSUS_API_KEY")

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

<<<<<<< HEAD
Sys.getenv("CENSUS_API_KEY") #census_api_key("CENSUS_API_KEY",install=TRUE)

var = load_variables(2019,"acs5",cache=TRUE) 

=======
>>>>>>> b26dddb9a5f67f4ba1baa92864c438a3de1aa2cb
df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE)

#Name is what changes one dataset from another.

df2 = df %>% dplyr::filter(NAME == "Cecil County, Maryland") %>% 
  left_join(vars,by=c("variable"="name"))
<<<<<<< HEAD
=======
  #cbind(vars) - this method doesn't work because get_acs() may change the order on return
>>>>>>> b26dddb9a5f67f4ba1baa92864c438a3de1aa2cb

df2 %>% write_csv(file = "Cecil_County_Census_Data.csv")
