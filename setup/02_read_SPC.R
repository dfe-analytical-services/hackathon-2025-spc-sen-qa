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


  