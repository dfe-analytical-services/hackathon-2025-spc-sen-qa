## pivot for age & sex

sen_age_sex_long <- sen_age_sex %>%
  pivot_longer(
    # Only pivot columns that start with male_age_ or female_age_
    cols = matches("^(male|female)_age_"),
    names_to = "variable",
    values_to = "total"
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


test <- long_df %>% 
  filter(time_period == 202324, sen_status == 'Total', sen_primary_need == 'Total', geographic_level == 'National', phase_type_grouping == 'Total') 
View(test)

## Pivot for ethnicity

# Select relevant columns
cols_to_pivot <- names(sen_fsm_eth_lang)[str_detect(names(sen_fsm_eth_lang), "^(fsm|ethnicity|language)_") & 
                             !str_detect(names(sen_fsm_eth_lang), "_percent$")]

# Pivot longer
sen_fsm_eth_lang_long <- sen_fsm_eth_lang %>%
  pivot_longer(
    cols = all_of(cols_to_pivot),
    names_to = c("category", "sub_category"),
    names_sep = "_",
    values_to = "value"
  ) 


## Pivot for ncyear

sen_year_long <- sen_year %>%
  select(time_period:nc_not_followed) %>%
  rename("nc_total" = "number_of_pupils") %>%
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


