# Calculates totals for the SPC publication data files
test_summarise = function(a, b, c, d,e) {
#Nathan
  
if( b=="") {

  
  #Alternative provision placements
  geographic_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(geographic_level) %>%
    summarise(count = sum(!!sym(d), na.rm = T))
  
  regional_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(region_name != "",la_name != "") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(region_name) %>%
    summarise(count = sum(!!sym(d), na.rm = T)) %>%
    summarise(region_total = sum(count)) %>%
    mutate(geographic_level = "Regional")
  
  la_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(region_name != "",la_name != "") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(la_name) %>%
    summarise(count = sum(!!sym(d), na.rm = T)) %>%
    summarise(la_total = sum(count)) %>%
    mutate(geographic_level = "Local authority")
  
  individual_region_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(region_name != "", la_name != "") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(region_name) %>%
    summarise(count = sum(!!sym(d), na.rm = T))
  
  individual_la_totals <- a %>%
    filter(time_period == !!time_identifier) %>%
    filter(region_name != "", la_name != "") %>%
    filter(if_all(e, ~ .x == "Total")) %>%
    group_by(region_name,la_name) %>%
    summarise(count = sum(!!sym(d), na.rm = T)) %>%
    group_by(region_name) %>%
    summarise(la_region_total = sum(count))
  
  region_comparison_totals <- individual_region_totals %>%
    left_join(individual_la_totals, by = c("region_name"))
  
  
  national_comparison_totals <- geographic_totals %>%
    left_join(regional_totals, by = "geographic_level") %>%
    left_join(la_totals, by = "geographic_level")
  
}
  else {
  
#Alternative provision placements
geographic_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  group_by(geographic_level, !!sym(b)) %>%
  filter(b !="Total") %>%
  filter(if_all(e, ~ .x == "Total")) %>%
  summarise(count = sum(!!sym(d), na.rm = T))

regional_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  filter(if_all(e, ~ .x == "Total"),
         b != "Total") %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(count = sum(!!sym(d), na.rm = T)) %>%
  group_by(!!sym(b)) %>%
  summarise(region_total = sum(count)) %>%
  mutate(geographic_level = "Regional")

la_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  filter(if_all(e, ~ .x == "Total"),
         b != "Total") %>%
  group_by(la_name, !!sym(b)) %>%
  summarise(count = sum(!!sym(d), na.rm = T)) %>%
  group_by(!!sym(b)) %>%
  summarise(la_total = sum(count)) %>%
  mutate(geographic_level = "Local authority")

individual_region_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  filter(if_all(e, ~ .x == "Total"),
         b != "Total") %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(count = sum(!!sym(d), na.rm = T))

individual_la_totals <- a %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  filter(if_all(e, ~ .x == "Total"),
         b != "Total") %>%
  group_by(region_name,la_name, !!sym(b)) %>%
  summarise(count = sum(!!sym(d), na.rm = T)) %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(la_region_total = sum(count))

region_comparison_totals <- individual_region_totals %>%
  left_join(individual_la_totals, by = c("region_name", b))
  

national_comparison_totals <- geographic_totals %>%
  left_join(regional_totals, by = c("geographic_level", b)) %>%
  left_join(la_totals, by = c("geographic_level", b))

}


assign(paste0("regional_comparison_",c), region_comparison_totals, envir = .GlobalEnv)
assign(paste0("national_comparison_", c), national_comparison_totals, envir = .GlobalEnv)

}

#ap placement comparisons
dataset = spc_ap_placement
variables = c("age","fsm","sex","ethnicity_minor")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("ap_placement_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset,"", "ap_placement_total", "number_of_pupils", variables)


#pupil fsm comparisons
names(spc_pupil_fsm)
dataset = spc_pupil_fsm
variables = c("phase_type_grouping","characteristic_group","characteristic")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("pupil_fsm_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "pupil_fsm_total", "number_of_pupils", variables)




#Alternative provision placements
geographic_totals <- spc_pupil_fsm %>%
  filter(time_period == !!time_identifier) %>%
  filter(if_all(variables, ~ .x == "Total")) %>%
  group_by(geographic_level) %>%
  summarise(count = sum(number_of_pupils, na.rm = T))

regional_totals <- spc_pupil_fsm %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  filter(if_all(variables, ~ .x == "Total")) %>%
  group_by(region_name) %>%
  summarise(count = sum(number_of_pupils, na.rm = T)) %>%
  summarise(region_total = sum(count)) %>%
  mutate(geographic_level = "Regional")

la_totals <- spc_pupil_fsm %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  filter(if_all(variables, ~ .x == "Total")) %>%
  group_by(la_name) %>%
  summarise(count = sum(number_of_pupils, na.rm = T)) %>%
  summarise(la_total = sum(count)) %>%
  mutate(geographic_level = "Local authority")

individual_region_totals <- spc_pupil_fsm %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  filter(if_all(variables, ~ .x == "Total")) %>%
  group_by(region_name) %>%
  summarise(count = sum(number_of_pupils, na.rm = T))

individual_la_totals <- spc_pupil_fsm %>%
  filter(time_period == !!time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  filter(if_all(variables, ~ .x == "Total")) %>%
  group_by(region_name,la_name) %>%
  summarise(count = sum(number_of_pupils, na.rm = T)) %>%
  group_by(region_name) %>%
  summarise(la_region_total = sum(count))

region_comparison_totals <- individual_region_totals %>%
  left_join(individual_la_totals, by = c("region_name"))


national_comparison_totals <- geographic_totals %>%
  left_join(regional_totals, by = "geographic_level") %>%
  left_join(la_totals, by = "geographic_level")


