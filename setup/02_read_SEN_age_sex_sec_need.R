#load in data
sen_age_sex <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/debed9ee-9dcf-4f07-9284-63d7e084647d/csv")

sen_fsm_eth_lang <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/fab0ef04-6a54-4c5b-9099-a669e111c116/csv")

sen_year <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/ba8c091f-b4d7-4fb7-b3f4-a627da11bbbc/csv")

sen_allpupils <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/bd752797-670b-49e8-8d28-ee50ee977d8f/csv")

sen_secondaryneed <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/67b6e56b-8e4b-41d6-843c-d21b1c73eb4b/csv")


# clean column names 
sen_age_sex <- clean_names(sen_age_sex)
sen_fsm_eth_lang <- clean_names(sen_fsm_eth_lang)
sen_year <- clean_names(sen_year)
sen_allpupils <- clean_names(sen_allpupils)
sen_secondaryneed <- clean_names(sen_secondaryneed)






# process sen_age_sex

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

sen_age_sex <- rbind(sen_age_sex_1,sen_age_sex_2,sen_age_sex_3,sen_age_sex_4,sen_age_sex_5)
rm(sen_age_sex_1,sen_age_sex_2,sen_age_sex_3,sen_age_sex_4,sen_age_sex_5)




## process sen_secondary_need

# note that the percentage columns are not being imported

sen_secondaryneed <- sen_secondaryneed %>% 
  select (-c(secondary_need_spld_percent, secondary_need_mld_percent,
             secondary_need_sld_percent, secondary_need_pmld_percent,
             secondary_need_semh_percent, secondary_need_slcn_percent,
             secondary_need_hi_percent, secondary_need_vi_percent,
             secondary_need_msi_percent, secondary_need_pd_percent,
             secondary_need_asd_percent, secondary_need_oth_percent,
             secondary_need_nsa_percent))


# get the overall totals 


sen_secondaryneed_1 <- sen_secondaryneed %>% 
  select(time_period,time_identifier,geographic_level,
         country_code,country_name,region_name,
         region_code,old_la_code,la_name,
         new_la_code,phase_type_grouping,sen_status,
         sen_primary_need,number_of_pupils) %>% 
  mutate(secondary_need = 'Total') %>% 
  relocate(secondary_need, .after = sen_primary_need)


#Get the secondaryneed total by pivot_longer

sen_secondaryneed_2  <- sen_secondaryneed %>% 
  select(-number_of_pupils) %>% 
  pivot_longer(
    cols = starts_with("secondary_need_"),
    names_to = "secondary_need",
    values_to = "number_of_pupils"
  )


# Combine back into the one file and remove the partial files

sen_secondaryneed <- rbind(sen_secondaryneed_1, sen_secondaryneed_2)

rm(sen_secondaryneed_1, sen_secondaryneed_2, sen_secondaryneed)






