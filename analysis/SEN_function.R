# Calculates totals for the SPC publication data files
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


#pupil fsm comparisons
names(spc_pupil_fsm)
dataset = spc_pupil_fsm
variables = c("phase_type_grouping","characteristic_group","characteristic")

for (i in variables) {
  grouping_vars <- setdiff(variables, i)
  test_summarise(dataset, i, paste0("pupil_fsm_",i), "number_of_pupils", grouping_vars)
}
test_summarise(dataset, "", "pupil_fsm_total", "number_of_pupils", variables)

