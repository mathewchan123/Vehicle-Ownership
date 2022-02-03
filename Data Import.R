#rename (function rename) columns to meaningful name.
#download data over multiple years and combine it(rbind).
#Make bar charts of "filter by certain state" and make chart of distribution
#(dplyr::filter,ggplot2)
#geom_bar
#stretch goal: look at other columns that are relevant to problem 
#(load_variables); find related features 
#ACS 1 instead of ACS 5 when downloading
# find 5 acs variables to play with; EDA, ggplot for each of the 5 variables.

library(tidyverse)
library(tidycensus)
census_api_key("bb637a985d9b9b2dcdf2ea179ed31ae79eb077b0")

load_variables(2019,"acs5",cache=TRUE) %>% View()

df = get_acs(geography="county",variables =c ("B08141_003","B08141_004"),year=2019)

get_acs(state = "MD",year=2019, geography = "county",variables =c ("B08141_003","B08141_004"))

#read.csv("Display_Data.csv")

census_api_key("bb637a985d9b9b2dcdf2ea179ed31ae79eb077b0", install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
  readRenviron("~/.Renviron")
  # You can check it with:
  Sys.getenv("CENSUS_API_KEY")


load_variables(2019,"acs5",cache=TRUE) %>% View()

df = get_acs(geography="county",variables =c ("B08141_003","B08141_004"),year=2019)


get_acs(
  geography,
  variables = NULL,
  table = NULL,
  cache_table = FALSE,
  year = 2019,
  endyear = NULL,
  output = "tidy",
  state = NULL,
  county = NULL,
  zcta = NULL,
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  summary_var = NULL,
  key = NULL,
  moe_level = 90,
  survey = "acs5",
  show_call = FALSE,
)






  