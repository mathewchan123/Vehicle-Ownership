#This code is no longer being used. 

library(tidyverse)
library(tidycensus)
library(plyr)
library(ggplot2)


Sys.getenv("CENSUS_API_KEY") #census_api_key("CENSUS_API_KEY",install=TRUE)
# census_api_key("CENSUS_API_KEY")

var = load_variables(2019,"acs5",cache=TRUE) 

#B08203_007 _ no workers in house

#B25044_002 _ owner occupied housing

#B08006_033 _ Other transit means 

#	B08006_014 _ biked

#	B08006_015 _ walked
vars = var %>% dplyr::filter(grepl("B08006", name, label)) %>% 
  rbind(var %>% dplyr::filter(grepl("B08203", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B08141", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B25044", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("B05011", name, label))) %>%
  rbind(var %>% dplyr::filter(grepl("Estimate!!Total:!!Male:!!",name)))

#B05011 = PERIOD OF NATURALIZATION

#census_api_key("bb637a985d9b9b2dcdf2ea179ed31ae79eb077b0")

#B01001_001 = total overall population?
df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE)

all_county_df = df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate") 


#all_county_df %>% write_csv(file = "all_counties.csv")
model = lm(data = all_county_df, formula = B08203_003~ B08006_014 + B08006_015 + B08006_033 + B25044_002 + B08203_007 + B05011_002)
#12-8-21
#run ggplot (very optional, not very helpful for context)
#make sure variable being predicted is actually right
#find out demographic variable (population of adult age)
#exercise for next time: add up B08203 variables (003-006)
#divide response variable & predictor variables by population/demographic variable
#rerun model with new variables
#Write out a hypothesis; write out what the perfect variables would be to be predict behavior
#Can we get these variables? <- for conclusion; consider if other variables could have predictive value

summary(model)

all_county_df$predict = predict(model)

all_county_df %>%
  ggplot() +
  geom_point(aes(x=B05011_002,y=B08203_001),colour="red") +
  geom_line(data=,aes(x=B05011_002,y=predict),size=1,alpha=0.3,colour="blue") +
  theme_bw()
plot(data=harford_dff,x=B05011_002,y=B08203_001)
abline(model)

#12-8-21
#run ggplot (very optional, not very helpful for context)
#make sure variable being predicted is actually right
#find out demographic variable (population of adult age)
#exercise for next time: add up B08203 variables (003-006)
#divide response variable & predictor variables by population/demographic variable
#rerun model with new variables
#Write out a hypothesis; write out what the perfect variables would be to be predict behavior
#Can we get these variables? <- for conclusion; consider if other variables could have predictive value

#12-16-21 Goals:
#Find answer to relation between labels and the 10 concepts.
#Run in a linear model between labels and concepts. 
#^ after this: divide by population of county (variable "B01001_001")
#mutate by divide : rename every variable
#mutate ex. 
mutate_ex = all_county_df %>% select(GEOID, NAME, B05011_001, B05011_002, B05011_003) %>% 
  # assuming B05011_001 is the population of the entire county,
  mutate(B05011_002_prop = B05011_002/B05011_001) %>%
  mutate(B05011_003_prop = B05011_003/B05011_001)
mutate_ex %>% ggplot() + geom_point(aes(x = B05011_002_prop, y=B05011_003_prop)) + 
  ggrepel::geom_label_repel(aes(x = B05011_002_prop, y=B05011_003_prop, label = NAME))
#good conclusion statement, non census data variables researched 