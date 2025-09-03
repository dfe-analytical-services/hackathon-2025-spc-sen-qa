time_filter <- 202425

# SPC AP setting ----
spc_ap_setting_totals <- spc_ap_setting |> 
  filter(time_period == time_filter) |> 
  filter(region_name != "") |> 
  filter(la_name != "") |> 
  filter(setting_type != "Total") |> 
  group_by(geographic_level) |> 
  summarise(total_placements = sum(placements, na.rm = TRUE))


spc_ap_region_totals <- spc_ap_setting |> 
  filter(time_period == time_filter) |> 
  filter(region_name != "") |> 
  filter(la_name != "") |> 
  filter(setting_type != "Total") |> 
  group_by(region_name) |> 
  summarise(total_placements = sum(placements, na.rm = TRUE)) |> 
  adorn_totals()


spc_ap_la_totals <- spc_ap_setting |> 
  filter(time_period == time_filter) |> 
  filter(region_name != "") |> 
  filter(la_name != "") |> 
  filter(setting_type == "Total") |> 
  group_by(la_name) |> 
  mutate(national_total = sum(placements)) |> 
  select(la_name, national_total)
