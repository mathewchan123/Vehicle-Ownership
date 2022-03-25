#Code that is no longer being used.

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

all_county_df %>% dplyr::rename("Attached 1 unit building" = "B25024_003")

all_county_df %>% dplyr::rename("Live outside principal city" = "B08016_013")

all_county_df %>% dplyr::rename("Total Population" = "B01001_001")

dfmutate = function(df){
  df_adj = df %>% mutate(name="year") 
  df_adj # acts like a print statement
  return(df_adj) # returns the dataframe after change
}
dfmutate(df_train)
dfmutate(df_test)

df1_adj = dfmutate(df_train)

#dfmutate2 = function(df){
df_adj = df %>% mutate(name=predictor(name,"thing")) # we add a column name and apply "fish" to every cell (row) in that column
df_adj # acts like a print statement
return(df_adj) # returns the dataframe after change
#}

#dfmutate2(df1_adj)


#dfmutate2(df_train)

#multi_variable_data %>% ggplot() + geom_point(aes(x = College_undergraduate_prop, y=Travel_time_work_30_to_34_mins_prop)) #+
#ggrepel::geom_label_repel(aes(x = Public_transit_work_prop  , y=Depart_work_6AM_629AM_prop , label = NAME))

#multi_variable_data$predict = predict(model)

#df = tribble(~x,~y,1,2,5,8,12,9)
#df %>%
ggplot() +
  geom_point(aes(x,y)) +
  geom_line(aes(x,y)) +
  geom_line(aes(x,y),colour="red") +
  theme_bw() +
  ggtitle("Tester plot") +
  scale_x_continuous(breaks=c(2,5,12)) +
  scale_y_continuous(breaks=c(2,6,8,10)) +
  ggrepel::geom_text_repel(data=df %>% dplyr::filter(x==1),aes(x=x,y=y),label="Label #1")

mutate(Public_transit_work_prop = B08006_008/B01001_001) %>%
mutate(Spanish_speakers_prop = B16001_003/B01001_001) %>%
mutate(Families_children_under_eighteen_prop = B23007_002/B01001_001) %>%
mutate(Lives_outside_principal_city_prop = B08016_013/B01001_001) %>%
  
#all_county_data %>%
#ggplot() +
#geom_point(aes(x=B08016_002,y=B08203_001),colour="red") +
#geom_line(data=,aes(x=B08016_002,y=predict),size=1,alpha=0.3,colour="blue") +
#theme_bw()
#plot(data=harford_dff,x=B08016_002,y=B08203_001)
#abline(model)

model = lm(data = multi_variable_data, formula = vehicle_ownership_prop ~ Depart_work_6AM_629AM_prop  + Household_income_60k_to_70k_prop + Renter_housing_prop + Structure_built_1939_earlier_prop + One_worker_at_home_prop + College_undergraduate_prop + Travel_time_work_30_to_34_mins_prop + Public_transit_work_prop)