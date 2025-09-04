# Calculates totals for the SPC publication data files
test_summarise = function(a, b, c, d, e) {
#Nathan
  
if( b=="") { # if no grouping variable is provided it calculates the overal total across all variables

  #calculates national totals from national geographic grouping
  national_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(geographic_level == "National") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    summarise(national_total = sum(!!sym(d), na.rm = T))

  #calculates regional totals from regional geographic grouping
  regional_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(geographic_level == "Regional") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(region_name) %>%
    summarise(region_total = sum(!!sym(d), na.rm = T))
  
  #calculates LA totals from LA geographic grouping
  la_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(geographic_level == "Local authority") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(la_name) %>%
    summarise(la_total = sum(!!sym(d), na.rm = T))
  
  #calculates regional totals from LA geographic grouping
  la_regional_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(geographic_level == "Local authority") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(region_name) %>%
    summarise(la_total = sum(!!sym(d), na.rm = T))
  
  #compares the sum of la, regional and national at a national level
  all_nation_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(geographic_level) %>%
    summarise(count = sum(!!sym(d), na.rm = T)) %>%
    pivot_wider(names_from = geographic_level, values_from = count) %>%
    clean_names() %>%
    mutate(geographic_level = "National",
           matching_flag = ifelse(local_authority == regional & regional == national, 1, 0))
  
  #compares the sum of regional at a regional level and la at a regional level
  regional_totals_comparison <- regional_totals %>%
    left_join(la_regional_totals, by = "region_name") %>%
    mutate(matching_flag = ifelse(region_total == la_total, 1, 0))
  
  
  
  # Extract the flags
  regional_flag <- all(regional_totals_comparison$matching_flag == 1)
  national_flag <- all(all_nation_totals$matching_flag == 1)
  
  # Use case_when on scalar logicals
  comparison_summary <- case_when(
    regional_flag & national_flag ~ "All match",
    !regional_flag & national_flag ~ "regional mismatches",
    regional_flag & !national_flag ~ "national mismatches",
    !regional_flag & !national_flag ~ "regional & national mismatches",
    TRUE ~ NA_character_
  )
  
  
  
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
  else {  # if a grouping variable is provided it calculates the total for each value in that grouping variable
  
    #calculates national totals from national geographic grouping
national_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(geographic_level == "National") %>%
  filter(!!sym(b) !="Total") %>%
  filter(if_all(e, ~ .x == "Total")) %>%
  group_by(!!sym(b)) %>%
  summarise(national_total = sum(!!sym(d), na.rm = T))

#calculates regional totals from regional geographic grouping
regional_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(geographic_level == "Regional") %>%
  filter(if_all(e, ~ .x == "Total"),
         !!sym(b) != "Total") %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(region_total = sum(!!sym(d), na.rm = T))

#calculates LA totals from LA geographic grouping
la_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(geographic_level == "Local authority") %>%
  filter(if_all(e, ~ .x == "Total"),
         !!sym(b) != "Total") %>%
  group_by(la_name, !!sym(b)) %>%
  summarise(la_total = sum(!!sym(d), na.rm = T))

#calculates regional totals from LA geographic grouping
la_regional_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(geographic_level == "Local authority") %>%
  filter(if_all(e, ~ .x == "Total"),
         !!sym(b) != "Total") %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(la_total = sum(!!sym(d), na.rm = T))

#compares the sum of la, regional and national at a national level
all_nation_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  group_by(geographic_level, !!sym(b)) %>%
  filter(!!sym(b) !="Total") %>%
  filter(if_all(e, ~ .x == "Total")) %>%
  summarise(count = sum(!!sym(d), na.rm = T)) %>%
  pivot_wider(names_from = geographic_level, values_from = count) %>%
  clean_names() %>%
  mutate(geographic_level = "National",
         matching_flag = ifelse(local_authority == regional & regional == national, 1, 0))

#compares the sum of regional at a regional level and la at a regional level
regional_totals_comparison <- regional_totals %>%
  left_join(la_regional_totals, by = c("region_name",b)) %>%
  mutate(matching_flag = ifelse(region_total == la_total, 1, 0))


# Extract the flags
regional_flag <- all(regional_totals_comparison$matching_flag == 1)
national_flag <- all(all_nation_totals$matching_flag == 1)

# Use case_when on scalar logicals
comparison_summary <- case_when(
  regional_flag & national_flag ~ "All match",
  !regional_flag & national_flag ~ "regional mismatches",
  regional_flag & !national_flag ~ "national mismatches",
  !regional_flag & !national_flag ~ "regional & national mismatches",
  TRUE ~ NA_character_
)



  
  
 
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
  
  # output check of data matching at national level across national, regional and local authority groupings
  assign(paste0("national_comparison_",c), all_nation_totals, envir = .GlobalEnv) 
  
  # output check of data matching at regional level across regional and local authority groupings
  assign(paste0("regional_comparison_", c), regional_totals_comparison, envir = .GlobalEnv)
  
  # output national totals for overall and grouped vars for comparisons between SPC & SEN
  assign(paste0("national_total_", c), national_totals, envir = .GlobalEnv)
  
  # output regional totals for overall and grouped vars for comparisons between SPC & SEN
  assign(paste0("regional_total_", c), regional_totals, envir = .GlobalEnv)
  
  # output local authority totals for overall and grouped vars for comparisons between SPC & SEN
  assign(paste0("la_total_", c), la_totals, envir = .GlobalEnv)
  
  assign(paste0("internal_summary_", c), comparison_summary, envir = .GlobalEnv)

}


#SPC ap placement census comparisons
dataset = spc_ap_placement_census
variables = c("age","fsm","sex","ethnicity_minor")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ap_census_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset,"", "spc_ap_census_total", "number_of_pupils", variables)




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
dataset = spc_ap_placement
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
  test_summarise(dataset, i, paste0("spc_pupil_school_chars_",i), "headcount_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_pupil_school_chars_total", "headcount_of_pupils", variables)

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_schools_school_chars_",i), "number_of_schools", grouping_vars)
}
test_summarise(dataset, "", "spc_schools_school_chars_total", "number_of_schools", variables)



#SPC age and sex
dataset = spc_age_and_sex
variables = c("phase_type_grouping","sex","age")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_age_and_sex_",i), "headcount", grouping_vars)
}
test_summarise(dataset, "", "spc_age_and_sex_total", "headcount", variables)


#SPC yeargroup and sex
dataset = spc_yeargroup_and_sex
variables = c("phase_type_grouping","sex","ncyear")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_yeargroup_and_sex_",i), "headcount", grouping_vars)
}
test_summarise(dataset, "", "spc_yeargroup_and_sex_total", "headcount", variables)


#SPC pupils fsm
dataset = spc_pupils_fsm
variables = c("phase_type_grouping","fsm")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_pupils_fsm_",i), "headcount", grouping_vars)
}
test_summarise(dataset, "", "spc_pupils_fsm_total", "headcount", variables)

#SPC class size
dataset = spc_class_size_full
variables = c("classtype","size")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_class_size_pupils_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "spc_class_size_pupils_total", "number_of_pupils", variables)

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_class_size_classes_",i), "number_of_classes", grouping_vars)
}
test_summarise(dataset, "", "spc_class_size_classes_total", "number_of_classes", variables)


#SPC AP setting
dataset = spc_ap_setting
variables = c("setting_type")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ap_setting_",i), "placements", grouping_vars)
}
test_summarise(dataset, "", "spc_ap_setting_total", "placements", variables)


#SPC AP reason
dataset = spc_ap_reason
variables = c("setting_type","placement_reason")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ap_reason_",i), "number_of_placements", grouping_vars)
}
test_summarise(dataset, "", "spc_ap_reason_total", "number_of_placements", variables)

#SPC ethnicity language
dataset = spc_ethnicity_language
variables = c("phase_type_grouping","ethnicity_minor","language")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("spc_ethnicity_language_",i), "headcount", grouping_vars)
}
test_summarise(dataset, "", "spc_ethnicity_language_total", "headcount", variables)
