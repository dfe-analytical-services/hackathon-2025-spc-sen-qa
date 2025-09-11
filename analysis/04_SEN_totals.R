## using the function from 04_SPC_totals_Nathan.R

# On the following files
# sen_age_sex_long
# sen_age_sex_long_MR
# sen_fsm_eth_lang_long
# sen_year_long
# sen_secondaryneed_long
# sen_secondaryneed_long_MR

# sen_age_sex_long [Josie version] - age and sex do not have Totals
dataset = sen_age_sex_long
variables = c("phase_type_grouping","sen_status","sen_primary_need")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("sen_age_sex_",i), "number_of_pupils", grouping_vars)
}

# sen_age_sex_long_mr [Matt version] - sex is not working
dataset = sen_age_sex_long_MR
variables = c("phase_type_grouping","sen_status","sen_primary_need","age")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("sen_age_sex_MR_",i), "number_of_pupils", grouping_vars)
}

# sen_year - nc_year is not working
dataset = sen_year_long
variables = c("phase_type_grouping","sen_status","sen_primary_need")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("sen_year",i), "number_of_pupils", grouping_vars)
  
  
  # sen_secondaryneed_long [Josie version] - "type_secondary_need" is not working
  dataset = sen_secondaryneed_long
  variables = c("phase_type_grouping","sen_status")
  
  for (i in variables) {
    grouping_vars <- setdiff(variables, i)
    test_summarise(dataset, i, paste0("sen_secondaryneed_",i), "number_of_pupils", grouping_vars)
  }
  
  # sen_secondaryneed_long [Matt version] - WORKS
  dataset = sen_secondaryneed_long_MR
  variables = c("phase_type_grouping","sen_status","sen_primary_need","secondary_need")
  
  for (i in variables) {
    grouping_vars <- setdiff(variables, i)
    test_summarise(dataset, i, paste0("sen_secondaryneed_MR_",i), "number_of_pupils", grouping_vars)
  }
  
  # sen_fsm_eth_lang_long   - Not working too many columns
  dataset = sen_fsm_eth_lang_long
  variables = c("phase_type_grouping","sen_status","sen_primary_need","secondary_need")
  
  for (i in variables) {
    grouping_vars <- setdiff(variables, i)
    test_summarise(dataset, i, paste0("sen_secondaryneed_MR_",i), "number_of_pupils", grouping_vars)
  }
  
  
                    
                    
                    