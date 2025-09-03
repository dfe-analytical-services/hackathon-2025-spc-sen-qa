# Calculates totals for the SPC publication data files
test_summarise = function(a, b, c) {
#Nathan
  
if( b=="") {
  
  #Alternative provision placements
  spc_ap_placement_totals <- a %>%
    #filter(time_period == time_identifier) %>%
    group_by(geographic_level) %>%
    summarise(pupil_count = sum(number_of_pupils, na.rm = T))
  
  spc_ap_placement_totals_regions <- a %>%
    #filter(time_period == time_identifier) %>%
    filter(region_name != "",la_name != "") %>%
    group_by(region_name) %>%
    summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
    summarise(region_total = sum(pupil_count)) %>%
    mutate(geographic_level = "Regional")
  
  spc_ap_placement_totals_la <- a %>%
    #filter(time_period == time_identifier) %>%
    filter(region_name != "",la_name != "") %>%
    group_by(la_name) %>%
    summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
    summarise(la_total = sum(pupil_count)) %>%
    mutate(geographic_level = "Local authority")
  
  spc_ap_placement_totals_region_ind <- a %>%
    #filter(time_period == time_identifier) %>%
    filter(region_name != "", la_name != "") %>%
    group_by(region_name) %>%
    summarise(pupil_count = sum(number_of_pupils, na.rm = T))
  
  spc_ap_placement_totals_la_ind <- a %>%
    #filter(time_period == time_identifier) %>%
    filter(region_name != "", la_name != "") %>%
    group_by(region_name,la_name) %>%
    summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
    group_by(region_name) %>%
    summarise(la_region_total = sum(pupil_count))
  
  spc_ap_placement_totals_region_comparison <- spc_ap_placement_totals_region_ind %>%
    left_join(spc_ap_placement_totals_la_ind, by = c("region_name"))
  
  
  spc_ap_placement_totals_national_comparison <- spc_ap_placement_totals %>%
    left_join(spc_ap_placement_totals_regions, by = "geographic_level") %>%
    left_join(spc_ap_placement_totals_la, by = "geographic_level")
  
}
  else {
  
#Alternative provision placements
spc_ap_placement_totals <- a %>%
  #filter(time_period == time_identifier) %>%
  group_by(geographic_level, !!sym(b)) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T))

spc_ap_placement_totals_regions <- a %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
  group_by(!!sym(b)) %>%
  summarise(region_total = sum(pupil_count)) %>%
  mutate(geographic_level = "Regional")

spc_ap_placement_totals_la <- a %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  group_by(la_name, !!sym(b)) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
  group_by(!!sym(b)) %>%
  summarise(la_total = sum(pupil_count)) %>%
  mutate(geographic_level = "Local authority")

spc_ap_placement_totals_region_ind <- a %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T))

spc_ap_placement_totals_la_ind <- a %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  group_by(region_name,la_name, !!sym(b)) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
  group_by(region_name, !!sym(b)) %>%
  summarise(la_region_total = sum(pupil_count))

spc_ap_placement_totals_region_comparison <- spc_ap_placement_totals_region_ind %>%
  left_join(spc_ap_placement_totals_la_ind, by = c("region_name", as.character(!!sym(b))))
  

spc_ap_placement_totals_national_comparison <- spc_ap_placement_totals %>%
  left_join(spc_ap_placement_totals_regions, by = c("geographic_level", as.character(!!sym(b)))) %>%
  left_join(spc_ap_placement_totals_la, by = c("geographic_level", as.character(!!sym(b))))

}


assign(paste0("regional_comparison",c), spc_ap_placement_totals_region_comparison, envir = .GlobalEnv)
assign(paste0("national_comparison", c), spc_ap_placement_totals_national_comparison, envir = .GlobalEnv)

}

test_summarise(spc_ap_placement,"", "ap_placement_total")
test_summarise(spc_ap_placement, "sex", "ap_placement_sex")


#Free school meals - ethnitiy & year group
spc_pupil_fsm 

#Universal infanct free school meals
spc_uifsm 

#Young carers
spc_young_carers 

#Alternative provision characteristics
spc_ap_chars 

#Alternative provision placement
sp_ap_placement 

#School characteristics
spc_school_chars 


#Alternative provision placements
spc_ap_placement_totals <- spc_ap_placement %>%
  #filter(time_period == time_identifier) %>%
  group_by(geographic_level, sex) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T))

spc_ap_placement_totals_regions <- spc_ap_placement %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  group_by(region_name, sex) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
  summarise(region_total = sum(pupil_count)) %>%
  mutate(geographic_level = "Regional")

spc_ap_placement_totals_la <- spc_ap_placement %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "",la_name != "") %>%
  group_by(la_name, sex) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%

  summarise(la_total = sum(pupil_count)) %>%
  mutate(geographic_level = "Local authority")

spc_ap_placement_totals_region_ind <- spc_ap_placement %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  group_by(region_name, sex) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T))

spc_ap_placement_totals_la_ind <- spc_ap_placement %>%
  #filter(time_period == time_identifier) %>%
  filter(region_name != "", la_name != "") %>%
  group_by(region_name,la_name, sex) %>%
  summarise(pupil_count = sum(number_of_pupils, na.rm = T)) %>%
  group_by(region_name, sex) %>%
  summarise(la_region_total = sum(pupil_count))

spc_ap_placement_totals_region_comparison <- spc_ap_placement_totals_region_ind %>%
  left_join(spc_ap_placement_totals_la_ind, by = c("region_name", "sex"))


spc_ap_placement_totals_national_comparison <- spc_ap_placement_totals %>%
  left_join(spc_ap_placement_totals_regions, by = c("geographic_level", "sex")) %>%
  left_join(spc_ap_placement_totals_la, by = c("geographic_level", "sex"))

