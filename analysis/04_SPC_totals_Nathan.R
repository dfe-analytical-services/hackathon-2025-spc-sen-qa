# Calculates totals for the SPC publication data files
test_summarise = function(a, b, c, d,e) {
#Nathan
  
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


#SPC ap placement comparisons
dataset = spc_ap_placement
variables = c("age","fsm","sex","ethnicity_minor")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ap_placement_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset,"", "spc_ap_placement_total", "number_of_pupils", variables)


#SPC pupil fsm ethnicity yrgp comparisons
dataset = spc_pupil_fsm_full
variables = c("phase_type_grouping","characteristic","fsm_eligibility")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_pupil_fsm_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_pupil_fsm_total", "number_of_pupils", variables)


#SPC universal infant free school meals comparison
dataset = spc_uifsm_full
variables = c("phase_type_grouping","characteristic","lunch_taken")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_uifsm_",i), "pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_uifsm_total", "pupils", variables)


#SPC young carers comparison
dataset = spc_young_carers_full
variables = c("phase_type_grouping","characteristic","young_carer")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_young_carers_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_young_carers_total", "number_of_pupils", variables)


#SPC AP characteristics
dataset = spc_ap_chars
variables = c("pupil_characteristic","setting_type")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ap_chars_",i), "pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_ap_chars_total", "pupils", variables)


#SPC AP placement
dataset = sp_ap_placement
variables = c("pupil_characteristic","setting_type")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ap_placement_",i), "pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_ap_placement_total", "pupils", variables)

#SPC school characteristics
dataset = spc_school_chars
variables = c("sex_of_school_description","phase_type_grouping","type_of_establishment","denomination","admissions_policy","urban_rural","academy_flag")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_school_chars_",i), "headcount_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_school_chars_total", "headcount_of_pupils", variables)



