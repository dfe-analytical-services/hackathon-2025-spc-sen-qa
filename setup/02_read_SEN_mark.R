#spc_pupils_age_and_sex.csv
#spc_pupils_yeargroup_and_sex.csv
#spc_pupils_yeargroup_and_sex.csv
#spc_pupils_fsm.csv
#spc_class_size.csv
#spc_ap_setting.csv
#ap_placement_reason_ap_census.csv

spc_age_and_sex <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/77d37133-ab0e-4428-aa62-9aae5bfdfd73/csv") |> 
  clean_names()

spc_yeargroup_and_sex <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/dd0a61a1-6db7-4014-a302-1233b7f32910/csv") |> 
  clean_names()

spc_pupils_fsm <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/9b265020-e071-43c5-beef-f345b41c20f8/csv") |> 
  clean_names()

spc_class_size <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/e6b28eda-f777-46e4-bb54-6bcc603cd154/csv") |> 
  clean_names()

spc_ap_setting <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/7bae73de-e66f-4d21-9569-44c691befdbd/csv") |> 
  clean_names()

spc_ap_reason <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/e3fc0998-7fc1-48fa-91e6-bf9abca19fb4/csv") |> 
  clean_names()

