### The first section wrangles the files from the Special educational needs in England publication to get them into a tidy long format
## Note there are 5 tables in the publication
## We have not imported the Pupils in all schools, by type of SEN provision - 2016 to 2025 (sen_allpupils) table
## We have not imported the percentages
## The processed files have the _long suffix



## pivot for age & sex

sen_age_sex_long <- sen_age_sex %>%
  pivot_longer(
    # Only pivot columns that start with male_age_ or female_age_
    cols = matches("^(male|female)_age_"),
    names_to = "variable",
    values_to = "number_of_pupils"
  ) %>%
  mutate(
    sex = case_when(
      str_detect(variable, "^male_") ~ "male",
      str_detect(variable, "^female_") ~ "female"
    ),
    age = case_when(
      str_starts(variable, "male_age_") ~ str_remove(variable, "^male_age_"),
      str_starts(variable, "female_age_") ~ str_remove(variable, "^female_age_"),
      TRUE ~ NA_character_
    )  
  ) %>%
  select(time_period:pupil_sex_female, sex, age, total)




## Pivot for ethnicity

# Select relevant columns
cols_to_pivot <- c(
  names(sen_fsm_eth_lang)[str_detect(names(sen_fsm_eth_lang), "^(fsm|ethnicity|language)_") & 
                            !str_detect(names(sen_fsm_eth_lang), "_percent$")],
  "Total"
)

# Pivot longer
sen_fsm_eth_lang_long <- sen_fsm_eth_lang %>%
  rename("Total" = "number_of_pupils") %>%
  select(-ends_with("_percent")) %>%
  pivot_longer(
    cols = all_of(cols_to_pivot),
    names_to = c("category"),
    values_to = "number_of_pupils"
  ) 


## Pivot for ncyear

sen_year_long <- sen_year %>%
  select(time_period:nc_not_followed) %>%
  rename("nc_Total" = "number_of_pupils") %>%
  pivot_longer(cols = starts_with("nc_"), 
               names_to = "nc_year", 
               values_to = "number_of_pupils") %>%
  mutate(nc_year = gsub("nc_", "", nc_year)) 

## pivot for secondary_need


sen_secondaryneed_long <- sen_secondaryneed %>%
  select(time_period:secondary_need_nsa) %>%
  rename("secondary_need_total" = "number_of_pupils") %>%
  pivot_longer(cols = starts_with("secondary_need_"), 
               names_to = "type_secondary_need", 
               values_to = "number_of_pupils") %>%
  mutate(type_secondary_need = gsub("secondary_need_", "", type_secondary_need)) 




##unpivot the sen_age_sex file into one file [by Matt Rolfe]

# note that the percentage columns are not being imported

sen_age_sex_1 <- sen_age_sex %>% 
  select(time_period,time_identifier,geographic_level,country_code,country_name,region_name,region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,sen_primary_need,number_of_pupils) %>% 
  mutate(sex = 'Total', age = 'Total', percent = NA) %>% 
  relocate(sex, .after = sen_primary_need) %>% 
  relocate(age, .after = sex)  %>% 
  relocate(number_of_pupils, .after = age)  %>% 
  relocate(percent, .after = number_of_pupils)                            

sen_age_sex_2 <- sen_age_sex %>% 
  select(time_period,time_identifier,geographic_level,country_code,country_name,region_name,region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,sen_primary_need,pupil_sex_male,pupil_sex_female) %>% 
  mutate(age = 'Total', percent = NA) %>% 
  pivot_longer(cols = starts_with("pupil_sex_"), 
               names_to = "sex", 
               values_to = "number_of_pupils") %>% 
  relocate(age, .after = sex)  %>% 
  relocate(number_of_pupils, .after = age)  %>% 
  relocate(percent, .after = number_of_pupils) %>% 
  mutate(sex = gsub('$pupil_sex_','',age))

sen_age_sex_3 <- sen_age_sex %>% 
  select(time_period,time_identifier,geographic_level,country_code,country_name,region_name,region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,sen_primary_need,age_2_and_under,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10, age_11,age_12,age_13,age_14,age_15,age_16,age_17,age_18,age_19_and_over) %>% 
  mutate(sex = 'Total', percent = NA) %>% 
  pivot_longer(cols = starts_with("age_"), 
               names_to = "age", 
               values_to = "number_of_pupils") %>% 
  relocate(age, .after = sex)  %>% 
  relocate(number_of_pupils, .after = age)  %>% 
  relocate(percent, .after = number_of_pupils) %>% 
  mutate(age = gsub('age_','',age),
         age = gsub('female_','',age),
         age = gsub('male_','',age))      


sen_age_sex_4 <- sen_age_sex %>% 
  select(time_period,time_identifier,geographic_level,country_code,country_name,region_name,region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,sen_primary_need,male_age_2_and_under,male_age_3_lower,male_age_3_middle,male_age_3_upper,male_age_4_lower,male_age_4_middle,male_age_4_upper,male_age_5,male_age_6,male_age_7,male_age_8,male_age_9,
         male_age_10, male_age_11,male_age_12,male_age_13,male_age_14,male_age_15,male_age_16,male_age_17,male_age_18,male_age_19_and_over) %>% 
  mutate(sex = 'Male', percent = NA) %>% 
  pivot_longer(cols = starts_with("male_age_"), 
               names_to = "age", 
               values_to = "number_of_pupils") %>% 
  relocate(age, .after = sex)  %>% 
  relocate(number_of_pupils, .after = age)  %>% 
  relocate(percent, .after = number_of_pupils) %>% 
  mutate(age = gsub('age_','',age),
         age = gsub('female_','',age),
         age = gsub('male_','',age))

sen_age_sex_5 <- sen_age_sex %>% 
  select(time_period,time_identifier,geographic_level,country_code,country_name,region_name,region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,sen_primary_need,female_age_2_and_under,female_age_3_lower,female_age_3_middle,female_age_3_upper,female_age_4_lower,female_age_4_middle,female_age_4_upper,female_age_5,female_age_6,female_age_7,female_age_8,female_age_9,
         female_age_10, female_age_11,female_age_12,female_age_13,female_age_14,female_age_15,female_age_16,female_age_17,female_age_18,female_age_19_and_over) %>% 
  mutate(sex = 'Female', percent = NA) %>% 
  pivot_longer(cols = starts_with("female_age_"), 
               names_to = "age", 
               values_to = "number_of_pupils") %>% 
  relocate(age, .after = sex)  %>% 
  relocate(number_of_pupils, .after = age)  %>% 
  relocate(percent, .after = number_of_pupils) %>% 
  mutate(age = gsub('age_','',age),
         age = gsub('female_','',age),
         age = gsub('male_','',age))

# Combine back into the one file and remove the partial files

sen_age_sex_long_MR <- rbind(sen_age_sex_1,sen_age_sex_2,sen_age_sex_3,sen_age_sex_4,sen_age_sex_5)
rm(sen_age_sex_1,sen_age_sex_2,sen_age_sex_3,sen_age_sex_4,sen_age_sex_5)



## unpivot the sen_secondary_need file into one file [by Matt Rolfe]

# note that the percentage columns are not being imported

sen_secondaryneed_long_MR <- sen_secondaryneed %>% 
  select (-c(secondary_need_spld_percent, secondary_need_mld_percent,
             secondary_need_sld_percent, secondary_need_pmld_percent,
             secondary_need_semh_percent, secondary_need_slcn_percent,
             secondary_need_hi_percent, secondary_need_vi_percent,
             secondary_need_msi_percent, secondary_need_pd_percent,
             secondary_need_asd_percent, secondary_need_oth_percent,
             secondary_need_nsa_percent))


# get the overall totals 


sen_secondaryneed_1 <- sen_secondaryneed_long_MR %>% 
  select(time_period,time_identifier,geographic_level,
         country_code,country_name,region_name,
         region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,
         sen_primary_need,number_of_pupils) %>% 
  mutate(secondary_need = 'Total') %>% 
  relocate(secondary_need, .after = sen_primary_need)


#Get the secondaryneed total by pivot_longer

sen_secondaryneed_2  <- sen_secondaryneed_long_MR %>% 
  select(-number_of_pupils) %>% 
  pivot_longer(
    cols = starts_with("secondary_need_"),
    names_to = "secondary_need",
    values_to = "number_of_pupils"
  )

# Combine back into the one file and remove the partial files

sen_secondaryneed_long_MR <- rbind(sen_secondaryneed_1, sen_secondaryneed_2)

rm(sen_secondaryneed_1, sen_secondaryneed_2)








# Calculates totals for the SEN publication data files


test_summarise = function(a, b, c, d,e) {
  
  if( b=="") {
    
    
    #Alternative provision placements
    
    national_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "National") %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      summarise(national_total = sum(!!sym(d), na.rm = T))
    
    regional_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "Regional") %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      group_by(region_name) %>%
      summarise(region_total = sum(!!sym(d), na.rm = T))
    
    la_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "Local authority") %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      group_by(la_name) %>%
      summarise(la_total = sum(!!sym(d), na.rm = T))
    
    la_regional_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "Local authority") %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      group_by(region_name) %>%
      summarise(la_total = sum(!!sym(d), na.rm = T))
    
    #checks all geographic breakdowns sum to country
    all_nation_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      group_by(geographic_level) %>%
      summarise(count = sum(!!sym(d), na.rm = T)) %>%
      pivot_wider(names_from = geographic_level, values_from = count) %>%
      mutate(geographic_level = "National")
    
    #checks regional and la breakdowns sum to region
    regional_totals_comparison <- regional_totals %>%
      left_join(la_regional_totals, by = "region_name")
    
    # 
    # 
    # individual_region_totals <- a %>%
    #   filter(time_period == !!time_identifier) %>%
    #   filter(region_name != "", la_name != "") %>%
    #   filter(if_all(e, ~ .x == "Total")) %>%
    #   group_by(region_name) %>%
    #   summarise(count = sum(!!sym(d), na.rm = T))
    # 
    # individual_la_totals <- a %>%
    #   filter(time_period == !!time_identifier) %>%
    #   filter(region_name != "", la_name != "") %>%
    #   filter(if_all(e, ~ .x == "Total")) %>%
    #   group_by(region_name,la_name) %>%
    #   summarise(count = sum(!!sym(d), na.rm = T)) %>%
    #   group_by(region_name) %>%
    #   summarise(la_region_total = sum(count))
    
    # region_comparison_totals <- individual_region_totals %>%
    #   left_join(individual_la_totals, by = c("region_name"))
    # 
    # 
    # national_comparison_totals <- geographic_totals %>%
    #   left_join(regional_totals, by = "geographic_level") %>%
    #   left_join(la_totals, by = "geographic_level")
    
  }
  else {
    
    #Alternative provision placements
    national_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "National") %>%
      filter(!!sym(b) !="Total") %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      group_by(!!sym(b)) %>%
      summarise(national_total = sum(!!sym(d), na.rm = T))
    
    regional_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "Regional") %>%
      filter(if_all(e, ~ .x == "Total"),
             !!sym(b) != "Total") %>%
      group_by(region_name, !!sym(b)) %>%
      summarise(region_total = sum(!!sym(d), na.rm = T))
    
    la_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "Local authority") %>%
      filter(if_all(e, ~ .x == "Total"),
             !!sym(b) != "Total") %>%
      group_by(la_name, !!sym(b)) %>%
      summarise(la_total = sum(!!sym(d), na.rm = T))
    
    la_regional_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      filter(geographic_level == "Local authority") %>%
      filter(if_all(e, ~ .x == "Total"),
             !!sym(b) != "Total") %>%
      group_by(region_name, !!sym(b)) %>%
      summarise(la_total = sum(!!sym(d), na.rm = T))
    
    #checks all geographic breakdowns sum to country
    all_nation_totals <- a %>%
      filter(time_period == !!time_identifier) %>%
      group_by(geographic_level, !!sym(b)) %>%
      filter(!!sym(b) !="Total") %>%
      filter(if_all(e, ~ .x == "Total")) %>%
      summarise(count = sum(!!sym(d), na.rm = T)) %>%
      pivot_wider(names_from = geographic_level, values_from = count) %>%
      mutate(geographic_level = "National")
    
    #checks regional and la breakdowns sum to region
    regional_totals_comparison <- regional_totals %>%
      left_join(la_regional_totals, by = c("region_name",b))
    
    # 
    # individual_region_totals <- a %>%
    #   filter(time_period == !!time_identifier) %>%
    #   filter(region_name != "", la_name != "") %>%
    #   filter(if_all(e, ~ .x == "Total"),
    #          b != "Total") %>%
    #   group_by(region_name, !!sym(b)) %>%
    #   summarise(count = sum(!!sym(d), na.rm = T))
    # 
    # individual_la_totals <- a %>%
    #   filter(time_period == !!time_identifier) %>%
    #   filter(region_name != "", la_name != "") %>%
    #   filter(if_all(e, ~ .x == "Total"),
    #          b != "Total") %>%
    #   group_by(region_name,la_name, !!sym(b)) %>%
    #   summarise(count = sum(!!sym(d), na.rm = T)) %>%
    #   group_by(region_name, !!sym(b)) %>%
    #   summarise(la_region_total = sum(count))
    
    
    
    
    
    # region_comparison_totals <- individual_region_totals %>%
    #   left_join(individual_la_totals, by = c("region_name", b))
    #   
    # 
    # national_comparison_totals <- geographic_totals %>%
    #   left_join(regional_totals, by = c("geographic_level", b)) %>%
    #   left_join(la_totals, by = c("geographic_level", b))
    
  }
  
  #write out
  #total for each breakdown across la, region, nation
  #percentage match across la, region, nation
  #Y/N flag for 100% match
  assign(paste0("national_comparison_",c), all_nation_totals, envir = .GlobalEnv)
  assign(paste0("regional_comparison_", c), regional_totals_comparison, envir = .GlobalEnv)
  assign(paste0("national_total_", c), national_totals, envir = .GlobalEnv)
  assign(paste0("regional_total_", c), regional_totals, envir = .GlobalEnv)
  assign(paste0("la_total_", c), la_totals, envir = .GlobalEnv)
  
}

#sen year

dataset = sen_year_long
variables = c("phase_type_grouping","sen_status","sen_primary_need","nc_year")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("sen_year_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset,"", "sen_year_total", "number_of_pupils", variables)


# age sex 

dataset = sen_age_sex_long_MR 
variables = c("phase_type_grouping","sen_status","sen_primary_need","sex", "age")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("sex_age_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "sex_age_total", "number_of_pupils", variables)

# age sex 2

dataset = sen_age_sex_long
variables = c("phase_type_grouping","sen_status","sen_primary_need")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("sex_age_2_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "sex_age_total", "number_of_pupils", variables)

# eth lang

dataset = sen_fsm_eth_lang_long
variables = c("phase_type_grouping","sen_status","sen_primary_need", "category")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("fsm_eth_lang_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "fsm_eth_lang_total", "number_of_pupils", variables)

