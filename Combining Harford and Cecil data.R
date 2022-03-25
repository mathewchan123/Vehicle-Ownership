#This code is no longer being used. 

library(tidyverse)
library(tidycensus)
library(plyr)
library(ggplot2)


Sys.getenv("CENSUS_API_KEY") #census_api_key("CENSUS_API_KEY",install=TRUE)
# census_api_key("CENSUS_API_KEY")

Sys.getenv("CENSUS_API_KEY") #census_api_key("CENSUS_API_KEY",install=TRUE)
# census_api_key("CENSUS_API_KEY")

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

<<<<<<< HEAD
rbind(var %>% dplyr::filter(grepl("B08141", name))) %>%
  rbind(var %>% dplyr::filter(grepl("B25044", name))) 


#census_api_key("bb637a985d9b9b2dcdf2ea179ed31ae79eb077b0")

df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE)

df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate") 
=======
df = get_acs(geography="county", state = "MD",variables = vars %>% select(name) %>% unlist() %>% unname(),year=2019,keep_geo_vars=TRUE)
>>>>>>> b26dddb9a5f67f4ba1baa92864c438a3de1aa2cb

#cbind(vars) - this method doesn't work because get_acs() may change the order on return
cecil_df = df %>% dplyr::filter(NAME == "Cecil County, Maryland") %>% 
  left_join(vars,by=c("variable"="name")) %>% select(NAME, concept, variable, label, estimate, moe)

harford_df = df %>% dplyr::filter(NAME == "Harford County, Maryland") %>% 
<<<<<<< HEAD
  left_join(vars,by=c("variable"="name")) %>% select(NAME, variable, estimate, moe) 

baltimore_df = df %>% dplyr::filter(NAME == "Baltimore County, Maryland") %>% 
  left_join(vars,by=c("variable"="name")) %>% select(NAME, variable, estimate, moe)


ult_df = cecil_df %>% left_join(harford_df,by= "variable") %>% left_join(baltimore_df,by= "variable") 
=======
  left_join(vars,by=c("variable"="name")) %>% select(NAME, variable, estimate, moe)

baltimore_df = df %>% dplyr::filter(NAME == "Baltimore County, Maryland") %>% 
  left_join(vars,by=c("variable"="name")) %>% select(NAME, variable, estimate, moe)

ult_df = cecil_df %>% left_join(harford_df,by= "variable")
>>>>>>> b26dddb9a5f67f4ba1baa92864c438a3de1aa2cb

ult_df = ult_df %>% rename(
  "cecil_moe" = "moe.x",
  "cecil_estimate" = "estimate.x",
  "harford_moe" = "moe.y",
  "harford_estimate" = "estimate.y"
) %>% select(-starts_with("NAME", ignore.case = FALSE))

ult_df = ult_df %>% mutate(diff = estimate.x - estimate.y) 

ult_df %>% write_csv(file = "Harford_and_Cecil_County_Census_Data.csv")

#combining the following:


#Estimate!!Total:!!Car, truck, or van: 

#Estimate!!Total:!!Public transportation (excluding taxicab):

#Estimate!!Total:!!Worked from home

#Estimate!!Total: (vehicles)

#harford_dff = harford_df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate") %>% rbind(cecil_df %>% select(-c(moe, concept, label)) %>% pivot_wider(names_from = "variable", values_from = "estimate"))

<<<<<<< HEAD
#harford_df %>% select(-moe) %>% pivot_wider(names_from = "name", values_from = "estimate") %>% plyr::rbind.fill(cecil_df %>% select(-moe) %>% pivot_wider(names_from = "name", values_from = "estimate"))
=======
#harford_df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate") %>% plyr::rbind.fill(cecil_df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate"))
harford_dff = harford_df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate") %>% rbind(cecil_df %>% select(-c(moe, concept, label)) %>% pivot_wider(names_from = "variable", values_from = "estimate")) %>% rbind(baltimore_df %>% select(-c(moe)) %>% pivot_wider(names_from = "variable", values_from = "estimate"))
>>>>>>> b26dddb9a5f67f4ba1baa92864c438a3de1aa2cb

harford_dff = harford_df %>% select(-moe) %>% pivot_wider(names_from = "variable", values_from = "estimate") %>% rbind(cecil_df %>% select(-c(moe, concept, label)) %>% pivot_wider(names_from = "variable", values_from = "estimate")) %>% rbind(baltimore_df %>% select(-c(moe)) %>% pivot_wider(names_from = "variable", values_from = "estimate"))

# model
model = lm(data = harford_dff, formula = B08203_001~ B08203_027)
summary(model)

# write new column based on model
harford_dff$predict = predict(model)

# example plot
harford_dff %>%
<<<<<<< HEAD
  ggplot() +
  geom_point(aes(x=B08203_027,y=B08203_001),colour="red") +
  geom_line(data=,aes(x=B08203_027,y=predict),size=1,alpha=0.3,colour="blue") +
  theme_bw()
plot(data=harford_dff,x=B08203_027,y=B08203_001)
abline(model)

#11-17
#get data to be not messed up
=======
ggplot() +
  geom_point(aes(x=B08203_027,y=B08203_001),colour="red") +
  geom_line(data=,aes(x=B08203_027,y=predict),size=1,alpha=0.3,colour="blue") +
  theme_bw()

#11-17
>>>>>>> b26dddb9a5f67f4ba1baa92864c438a3de1aa2cb
#get data to be not messed up (done)
#pull in ALL counties
#select features; in depth analysis; ask questions
#add own features (for further future) 


#lm(data=df, Response_Variable ~ .)

