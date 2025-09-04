# Reads in the SPC publication data files

#Mark
#spc_pupils_age_and_sex.csv
#spc_pupils_yeargroup_and_sex.csv
#spc_pupils_yeargroup_and_sex.csv
#spc_pupils_fsm.csv
#spc_class_size.csv
#spc_ap_setting.csv
#ap_placement_reason_ap_census.csv

#Nathan

#ap_placements_ap_census.csv

spc_ap_placement_census <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/ad663ad5-f2e8-4ad1-a119-69bd425ab1fc/csv") %>%
  clean_names()


#spc_pupils_fsm_ethnicity_yrgp
spc_pupil_fsm <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/f0bc12b2-0fcf-43fa-bb8a-f5b852343568/csv") %>%
  clean_names()

#create total rows for fsm eligibility
spc_pupil_fsm_totals <- spc_pupil_fsm %>%
  group_by(across(-c(fsm_eligibility, number_of_pupils, denominator, percent_of_pupils))) %>%
  summarise(number_of_pupils = sum(number_of_pupils, na.rm = T)) %>%
  mutate(fsm_eligibility = "Total",
         denominator = NA,
         percent_of_pupils = NA)

spc_pupil_fsm_full <- spc_pupil_fsm %>%
  rbind(spc_pupil_fsm_totals) %>%
  mutate(characteristic = ifelse(characteristic == "Total" | characteristic_group == "Total", "Total", paste0(characteristic_group, " ", characteristic)))


#spc_uifsm.csv
spc_uifsm <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/ff8be58f-9812-4561-95b7-83216c1c202a/csv") %>%
  clean_names()

#create total rows for lunch taken
spc_uifsm_totals <- spc_uifsm %>%
  group_by(across(-c(lunch_taken, pupils, denominator, percentage))) %>%
  summarise(pupils = sum(pupils, na.rm = T)) %>%
  mutate(lunch_taken = "Total",
         denominator = NA,
         percentage = NA)

spc_uifsm_full <- spc_uifsm %>%
  rbind(spc_uifsm_totals)


#spc_pupils_young_carers_yrgp.csv
spc_young_carers <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/e111a1a2-523b-4eff-a139-a69a1946b117/csv") %>%
  clean_names()

#create total rows for lunch taken
spc_young_carers_totals <- spc_young_carers %>%
  group_by(across(-c(young_carer, number_of_pupils, denominator, percent_of_pupils))) %>%
  summarise(number_of_pupils = sum(number_of_pupils, na.rm = T)) %>%
  mutate(young_carer = "Total",
         denominator = NA,
         percent_of_pupils = NA)

spc_young_carers_full <- spc_young_carers %>%
  rbind(spc_young_carers_totals)


#spc_school_ap_characteristics.csv
spc_ap_chars <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/f045364a-dabe-446d-9350-019c74116b1c/csv") %>%
  clean_names() %>%
  mutate(pupil_characteristic = ifelse(pupil_characteristic == "Total" | characteristic_grouping == "Total", "Total", paste0(characteristic_grouping, " ", pupil_characteristic)))


#spc_school_ap_placement.csv
spc_ap_placement <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/320d1b0f-1fa5-4dda-a38d-c5bb25369f9e/csv") %>%
  clean_names() %>%
  mutate(pupil_characteristic = ifelse(pupil_characteristic == "Total" | characteristic_grouping == "Total", "Total", paste0(characteristic_grouping, " ", pupil_characteristic)))


#spc_school_characteristics.csv
spc_school_chars <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/9556adc9-c5f3-4b9c-af2e-01e2a5e5901e/csv") %>%
  clean_names()

#spc_pupils_age_and_sex.csv
spc_age_and_sex <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/77d37133-ab0e-4428-aa62-9aae5bfdfd73/csv") %>% 
  clean_names()

#spc_pupils_yeargroup_and_sex.csv
spc_yeargroup_and_sex <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/dd0a61a1-6db7-4014-a302-1233b7f32910/csv") %>% 
  clean_names()

#spc_pupils_fsm.csv
spc_pupils_fsm <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/9b265020-e071-43c5-beef-f345b41c20f8/csv") %>% 
  clean_names()

#spc_class_size.csv
spc_class_size <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/e6b28eda-f777-46e4-bb54-6bcc603cd154/csv") %>% 
  clean_names()

#create total rows for classtype
spc_class_size_totals <- spc_class_size %>%
  group_by(across(-c(classtype, number_of_classes, number_of_pupils, average_class_size))) %>%
  summarise(number_of_pupils = sum(number_of_pupils, na.rm = T),
            number_of_classes = sum(number_of_classes, na.rm = T)) %>%
  mutate(classtype = "Total",
         average_class_size = NA)

spc_class_size_full <- spc_class_size %>%
  rbind(spc_class_size_totals)

#spc_ap_setting.csv
spc_ap_setting <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/7bae73de-e66f-4d21-9569-44c691befdbd/csv") %>% 
  clean_names()


#ap_placement_reason_ap_census.csv
spc_ap_reason <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/e3fc0998-7fc1-48fa-91e6-bf9abca19fb4/csv") %>% 
  clean_names()

  
#spc_pupils_ethnicity_and_language.csv
spc_ethnicity_language <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/04ead355-2db9-48c8-87ab-fcc2477bda73/csv") %>% 
  clean_names()