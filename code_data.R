library(tidyverse)
library(RODBC)
library(RMySQL)
library(lubridate)
library(tidymodels)

Sys.setenv(TZ = "UTC")

# Database connection
con <- switch(
  .Platform$OS.type, 
  windows = RODBC::odbcConnect(dsn = "xsw"), 
  unix    = "/root/sql/sql_connect_string_linux" |> readr::read_lines() |> RODBC::odbcDriverConnect()
)
on.exit(RODBC::odbcClose(con), add = TRUE)

# Max date from ECDS only
max_date <- RODBC::sqlQuery(
  con, 
  "SELECT CAST(MAX(Arrival_Date) AS DATE) AS max_date FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS;"
) %>% 
  pull(max_date) %>% 
  ymd()

report_end <- max_date
report_start <- report_end - ddays(69) # 70 days inclusive
n_hour <- 2
pre_window_days <- 60

# Site normalisation data
site_resus_maj <- tibble(
  site = c("BRISTOL ROYAL INFIRMARY", "WESTON GENERAL HOSPITAL", "SOUTHMEAD HOSPITAL"), 
  maj = c(16, 9, 14), 
  resus = c(8, 4, 6)
) %>% 
  mutate(total = maj + resus)

# Load statistical models
mort_mod       <- readRDS("data/mort_effects_output_multilvl.RDS")[["mort_mod_raw"]]
readm_mod      <- readRDS("data/readm_effects_output_multilvl.RDS")[["readm_mod_raw"]]
los_mod        <- readRDS("data/los_outputs_reformulated_multilvl.RDS")[["mod_full"]]
vw_impute_rec  <- readRDS("data/vw_impute_rec.RDS")
ed_time_coef   <- readRDS("data/ed_time_coef_multilvl.RDS")

# NCTR Data
nctr_df <- RODBC::sqlQuery(
  con, 
  glue::glue(
    "SELECT [RN] ,[Organisation_Code_Provider] ,[Organisation_Code_Commissioner] ,[Census_Date] ,[Month_Date] ,[Month] ,[Day_Of_Week] ,[Week_Start_Date] ,[NHS_Number] ,[Person_Stated_Gender_Code] ,[Person_Age] ,[CDS_Unique_Identifier] ,[Sub_ICB_Location] ,[Organisation_Site_Code] ,[Current_Ward] ,[Specialty_Code] ,[Bed_Type] ,[Date_Of_Admission] ,[BNSSG] ,[Local_Authority] ,[Criteria_To_Reside] ,[Date_NCTR] ,[Current_LOS] ,[Days_NCTR] ,[Days_NCTR_On_Current_Code] ,[Current_Delay_Code] ,[Local_Authority_grouped] ,[Site_Name] ,[Current_Delay_Code_Standard] ,[Current_Delay_Code_Detailed] ,[Acute Community split] ,[Current_Covid_Status] ,[Planned_Date_Of_Discharge] ,[Date_Toc_Form_Completed] ,[Toc_Form_Status] ,[Discharge_Pathway] ,[DER_File_Name] ,[DER_Load_Timestamp] 
     FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI 
     WHERE Census_Date = '{report_start}'"
  )
) %>% 
  slice(1, .by = CDS_Unique_Identifier) %>% 
  filter(Criteria_To_Reside == "N", Bed_Type == "Medicine") %>% 
  mutate(Site_Name = stringr::str_to_upper(Site_Name)) %>% 
  summarise(nctr = n(), .by = Site_Name)

get_impact_data <- function(report_start, report_end) {
  
  snapshot_seq <- seq.POSIXt(from = as.POSIXct(ymd(report_start)), to = as.POSIXct(ymd(report_end)), by = glue::glue("{n_hour} hours"))
  snapshot_int <- map(snapshot_seq, ~ interval(.x - dminutes((60 * n_hour)/2), .x + dminutes((60 * n_hour)/2)))
  
  # --- DOWNSTREAM IMPACTS ---
  dat_downstream <- local({
    string_ecds <- glue::glue("
      SELECT nhs_number, Index_Of_Multiple_Deprivation, Stated_Gender, HES_Age_At_Arrival, Attendance_Unique_Identifier, Treatment_Function_Code, Arrival_Date, Arrival_Time, Initial_Assessment_Date, Initial_Assessment_Time, Seen_For_Treatment_Date, Seen_For_Treatment_Time, Decision_To_Admit_Date, Decision_To_Admit_Time, Organisation_Code_Commissioner, Departure_Date, Departure_Time, Site_Name, Acuity_Desc 
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS 
      WHERE Site_Name IN ('BRISTOL ROYAL INFIRMARY', 'BFT SOUTHMEAD SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
        AND Arrival_Date BETWEEN '{report_start}' AND '{report_end}'
    ")
    dat_ecds_raw <- sqlQuery(con, string_ecds)
    
    
    dat_ecds <- dat_ecds_raw %>%
      mutate(Site_Name = recode(Site_Name, "BFT SOUTHMEAD SOUTHMEAD HOSPITAL" = "SOUTMEAD HOSPITAL")) %>%
      mutate(nhs_number = as.character(nhs_number)) %>%
      mutate(arrival_dt = as.POSIXct(paste0(Arrival_Date, " ", substr(Arrival_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(initial_dt = as.POSIXct(paste0(Initial_Assessment_Date, " ", substr(Initial_Assessment_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(treatment_dt = as.POSIXct(paste0(Seen_For_Treatment_Date, " ", substr(Seen_For_Treatment_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(dta_dt = paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))) %>%
      mutate(dta_dt = as.POSIXct(ifelse(grepl("NA", dta_dt), NA, dta_dt), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(departure_dt = as.POSIXct(paste0(Departure_Date, " ", substr(Departure_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      select(nhs_number, imd = Index_Of_Multiple_Deprivation, sex = Stated_Gender, age = HES_Age_At_Arrival,
             Attendance_Unique_Identifier, tfc = Treatment_Function_Code, Organisation_Code_Commissioner,
             arrival_dt, initial_dt, treatment_dt, dta_dt, departure_dt, site = Site_Name, Acuity_Desc) %>%
      rowwise() %>%
      mutate(departure_dt = max(dta_dt, departure_dt, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(dta_int = interval(dta_dt, departure_dt),
             dta_time = dta_int / dminutes(1),
             time_to_dta = interval(arrival_dt, dta_dt) / dminutes(1),
             trt_time = interval(arrival_dt, treatment_dt) / dminutes(1),
             date = floor_date(departure_dt, "day"),
             ed_category = if_else(Acuity_Desc %in% c("Non-urgent level emergency care", "Standard level emergency care"), "minor", "major"),
             arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt))
    
    string_ecds_full <- glue::glue("
      SELECT NHS_Number, Arrival_Date, Arrival_Time, Initial_Assessment_Date, Initial_Assessment_Time, Seen_For_Treatment_Date, Seen_For_Treatment_Time, Decision_To_Admit_Date, Decision_To_Admit_Time, Departure_Date, Departure_Time, Conclusion_Date, Conclusion_Time, Site_Name, Acuity_Desc, Index_Of_Multiple_Deprivation, Stated_Gender, HES_Age_At_Arrival, Organisation_Code_Commissioner, Arrival_Mode_Desc, Arrival_Category_Desc, Attendance_Source_Desc, destination_desc, Chief_Complaint_Desc, Diagnoses_Code_1_Desc, Investigation_Code_1_Desc, Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      WHERE Site_Name IN ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
        AND Arrival_Date BETWEEN '{ymd(report_start) - ddays(pre_window_days)}' AND '{report_end}'
    ")
    dat_ecds_full_raw <- sqlQuery(con, string_ecds_full)
    
    dat_ecds_full <- dat_ecds_full_raw %>%
      filter(Organisation_Code_Commissioner == '15C') %>%
      mutate(NHS_Number = as.character(NHS_Number),
             Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code), "Missing"),
             arrival_dt = as.POSIXct(paste0(Arrival_Date, " ", substr(Arrival_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             initial_dt = as.POSIXct(paste0(Initial_Assessment_Date, " ", substr(Initial_Assessment_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             treatment_dt = as.POSIXct(paste0(Seen_For_Treatment_Date, " ", substr(Seen_For_Treatment_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             dta_dt = as.POSIXct(ifelse(grepl("NA", paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))), NA, paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))), format = "%Y-%m-%d %H:%M:%S"),
             departure_dt = as.POSIXct(paste0(Departure_Date, " ", substr(Departure_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             conclusion_dt = as.POSIXct(paste0(Conclusion_Date, " ", substr(Conclusion_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      select(nhs_number = NHS_Number, tfc = Treatment_Function_Code, imd = Index_Of_Multiple_Deprivation, sex = Stated_Gender, age = HES_Age_At_Arrival, arrival_dt, initial_dt, treatment_dt, dta_dt, departure_dt, conclusion_dt, site = Site_Name, Acuity_Desc, arrival_mode = Arrival_Mode_Desc, arrival_cat = Arrival_Category_Desc, diagnoses = Diagnoses_Code_1_Desc, investigation = Investigation_Code_1_Desc) %>%
      rowwise() %>%
      mutate(departure_dt = max(dta_dt, departure_dt, conclusion_dt, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(dta_time = interval(dta_dt, departure_dt) / dminutes(1),
             time_to_dta = interval(arrival_dt, dta_dt) / dminutes(1),
             ed_time = interval(arrival_dt, departure_dt) / dminutes(1),
             breach = ed_time > 4 * 60,
             date = floor_date(departure_dt, "day"),
             ed_category = if_else(Acuity_Desc %in% c("Non-urgent level emergency care", "Standard level emergency care"), "minor", "major")) %>%
      group_by(nhs_number, arrival_dt) %>%
      mutate(arr_id = cur_group_id()) %>%
      ungroup()
    
    arrivals <- dat_ecds %>%
      filter(!is.na(dta_time), arrival_dt >= report_start, !is.na(nhs_number)) %>%
      select(nhs_number, arr_id, tfc, arrival_dt, departure_dt) %>%
      distinct() %>%
      group_by(nhs_number, arr_id) %>%
      summarise(tfc = tfc[1], arrival_dt = arrival_dt, departure_dt = as.Date(departure_dt)[1], .groups = "drop")
    
    dat_spec <- arrivals %>%
      select(nhs_number, arr_id, tfc) %>%
      mutate(spec = if_else(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
      select(nhs_number, arr_id, spec)
    
    dat_dtas <- dat_ecds %>%
      select(nhs_number, arrival_dt, dta_dt) %>%
      mutate(arrival_day = floor_date(arrival_dt, "days"), dta_day = floor_date(dta_dt, "days")) %>%
      distinct() %>%
      filter(!is.na(nhs_number)) %>%
      arrange(nhs_number, arrival_day) %>%
      group_by(nhs_number, arrival_day) %>%
      slice_max(arrival_dt, na_rm = TRUE) %>%
      group_by(nhs_number) %>%
      filter(!all(is.na(dta_dt))) %>%
      slice_min(dta_dt) %>%
      group_by(nhs_number, dta_day) %>%
      mutate(id = cur_group_id()) %>%
      ungroup()
    
    dat_ed_attends <- dat_ecds_full %>%
      select(nhs_number, pre_arrival_dt = arrival_dt) %>%
      distinct()
    
    dat_pre_attends <- dat_dtas %>%
      left_join(dat_ed_attends, by = "nhs_number") %>%
      filter(pre_arrival_dt < arrival_dt, between(pre_arrival_dt, arrival_dt - ddays(pre_window_days), arrival_dt)) %>%
      group_by(id) %>%
      count(name = "n_ed")
    
    dat_ecds_n_ed <- dat_dtas %>%
      left_join(dat_pre_attends, by = "id") %>%
      mutate(n_ed = coalesce(n_ed, 0)) %>%
      select(nhs_number, n_ed) %>%
      distinct()
    
    dat_ecds %>%
      left_join(dat_spec, by = c("nhs_number", "arr_id")) %>%
      filter(spec == "medical", !is.na(dta_dt)) %>%
      mutate(dta_time = dta_time / 60, time_to_dta = time_to_dta / 60) %>%
      mutate(age = cut(age, breaks = c(0, seq(30, 100, 10), Inf), right = FALSE, 
                       labels = c("29 years old or under", "30-39 years old", "40-49 years old", "50-59 years old", "60-69 years old", "70-79 years old", "80-89 years old", "90-99 years old", "100 years old or over"))) %>%
      filter(sex %in% 1:2) %>%
      mutate(sex = factor(sex, labels = c("Male", "Female")),
             imd = factor(imd, levels = 1:10, labels = c("1 (most deprived)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (least deprived)")),
             week = ceiling_date(as.Date(arrival_dt), unit = "week", week_start = wday(report_end, week_start = 1), change_on_boundary = FALSE),
             wkend = factor(if_else(wday(arrival_dt, week_start = 1) < 6, "No", "Yes")),
             arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour"))),
             arrival_time = forcats::fct_recode(arrival_time, "00:00-05:59" = "00:00:00", "06:00-11:59" = "06:00:00", "12:00-17:59" = "12:00:00", "18:00-23:59" = "18:00:00"),
             arrival_month = factor(month.name[month(arrival_dt)], levels = month.name),
             arrival_hour = ceiling_date(arrival_dt, "4 hour")) %>%
      left_join(dat_ecds_n_ed, by = "nhs_number") %>%
      left_join(nctr_df, by = join_by(site == Site_Name)) %>%
      left_join(site_resus_maj, by = "site") %>%
      mutate(nctr = nctr / total) %>%
      select(week, nhs_number, wkend, arrival_time, arrival_month, n_ed, age, sex, imd, dta_time, time_to_dta, nctr, site)
  })
  
  downstream_impacts <- dat_downstream %>%
    bind_rows(mutate(., site = "System total")) %>%
    nest(.by = c(site, week)) %>%
    mutate(data = map(data, ~ bake(vw_impute_rec, new_data = mutate(.x, vw_score = NA_real_))),
           n_exposure = map_dbl(data, nrow),
           pred_mort = map(data, ~ predict(mort_mod, newdata = .x, type = "response", re.form = NA)),
           pred_mort_cf = map(data, ~ predict(mort_mod, newdata = mutate(.x, dta_time = 1), type = "response", re.form = NA)),
           pred_mort_full_cf = map(data, ~ predict(mort_mod, newdata = mutate(.x, dta_time = 0), type = "response", re.form = NA)),
           pred_readm = map(data, ~ predict(readm_mod, newdata = .x, type = "response", re.form = NA)),
           pred_readm_cf = map(data, ~ predict(readm_mod, newdata = mutate(.x, dta_time = 1), type = "response", re.form = NA)),
           pred_readm_full_cf = map(data, ~ predict(readm_mod, newdata = mutate(.x, dta_time = 0), type = "response", re.form = NA)),
           pred_los = map(data, ~ predict(los_mod, newdata = .x, re.form = NA)),
           pred_los_cf = map(data, ~ predict(los_mod, newdata = mutate(.x, dta_time = 1), re.form = NA)),
           pred_los_full_cf = map(data, ~ predict(los_mod, newdata = mutate(.x, dta_time = 0), re.form = NA)),
           om6_impact = map_dbl(pred_mort, ~ sum(.x, na.rm = TRUE)),
           om6_impact_cf = map_dbl(pred_mort_cf, ~ sum(.x, na.rm = TRUE)),
           om6_impact_full_cf = map_dbl(pred_mort_full_cf, ~ sum(.x, na.rm = TRUE)),
           om5_impact = map_dbl(pred_readm, ~ sum(.x, na.rm = TRUE)),
           om5_impact_cf = map_dbl(pred_readm_cf, ~ sum(.x, na.rm = TRUE)),
           om5_impact_full_cf = map_dbl(pred_readm_full_cf, ~ sum(.x, na.rm = TRUE)),
           om4_impact = map_dbl(pred_los, ~ sum(.x, na.rm = TRUE)),
           om4_impact_cf = map_dbl(pred_los_cf, ~ sum(.x, na.rm = TRUE)),
           om4_impact_full_cf = map_dbl(pred_los_full_cf, ~ sum(.x, na.rm = TRUE))) %>%
    mutate(across(matches("om4_impact"), ~ .x / (interval(report_start, report_end) / ddays(1)))) %>%
    select(grp = site, week, matches("om"), om4_n_exposure = n_exposure, om5_n_exposure = n_exposure, om6_n_exposure = n_exposure) %>%
    pivot_longer(-c(grp, week), names_to = "impact") %>%
    mutate(target = NA_real_,
           label = case_when(
             str_detect(impact, "n_exposure") ~ "number impacted",
             str_detect(impact, "om4") ~ "Medical beds occupied",
             str_detect(impact, "om5") ~ "readmissions",
             str_detect(impact, "om6") ~ "deaths"
           ),
           emph_text = number(round(value), big.mark = ","),
           sub_text = str_to_title(label))
  
  # --- DTA TOP STATS ---
  dat_dtas_res <- local({
    string_ecds <- glue::glue("
      SELECT nhs_number, Attendance_Unique_Identifier, Treatment_Function_Code, Arrival_Date, Arrival_Time, Initial_Assessment_Date, Initial_Assessment_Time, Seen_For_Treatment_Date, Seen_For_Treatment_Time, Decision_To_Admit_Date, Decision_To_Admit_Time, Organisation_Code_Commissioner, Departure_Date, Departure_Time, Site_Name, Acuity_Desc 
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS 
      WHERE Site_Name IN ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') 
        AND Arrival_Date BETWEEN '{report_start}' AND '{report_end}'
    ")
    dat_ecds <- sqlQuery(con, string_ecds) %>%
      mutate(nhs_number = as.character(nhs_number),
             arrival_dt = as.POSIXct(paste0(Arrival_Date, " ", substr(Arrival_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             dta_dt = as.POSIXct(ifelse(grepl("NA", paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))), NA, paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))), format = "%Y-%m-%d %H:%M:%S"),
             departure_dt = as.POSIXct(paste0(Departure_Date, " ", substr(Departure_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      select(nhs_number, Attendance_Unique_Identifier, tfc = Treatment_Function_Code, Organisation_Code_Commissioner, arrival_dt, dta_dt, departure_dt, Site_Name, Acuity_Desc) %>%
      rowwise() %>%
      mutate(departure_dt = max(dta_dt, departure_dt, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(dta_int = interval(dta_dt, departure_dt), dta_time = dta_int / dminutes(1), arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt))
    
    arrivals <- dat_ecds %>%
      filter(!is.na(dta_time), arrival_dt >= report_start, !is.na(nhs_number)) %>%
      group_by(nhs_number, arr_id) %>%
      summarise(tfc = tfc[1], arrival_dt = arrival_dt, departure_dt = as.Date(departure_dt)[1], .groups = "drop")
    
    arrivals_full <- dat_ecds %>%
      filter(arrival_dt >= report_start) %>%
      select(id = Attendance_Unique_Identifier, site = Site_Name, arrival_dt) %>%
      distinct()
    
    dat_spec <- arrivals %>%
      select(nhs_number, arr_id, tfc) %>%
      mutate(spec = if_else(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
      select(nhs_number, arr_id, spec)
    
    dat_dtas <- dat_ecds %>%
      left_join(dat_spec, by = c("nhs_number", "arr_id")) %>%
      filter(spec == "medical") %>%
      group_by(site = Site_Name) %>%
      nest() %>%
      mutate(dtas = list(map2_dbl(snapshot_seq, data, \(snapshot, d) sum(snapshot %within% na.omit(d$dta_int)))),
             arrivals = list(map2_dbl(snapshot_int, site, \(snip, s_id) nrow(filter(arrivals_full, arrival_dt %within% snip, site == s_id)))),
             dta_df = map(dtas, ~ map2_df(.x, snapshot_seq, ~ tibble(dt = .y, n_dta = .x))),
             arrival_df = map(arrivals, ~ map_df(.x, ~ tibble(n_arrival_all = .x)))) %>%
      select(site, dta_df, arrival_df) %>%
      unnest(cols = c(dta_df, arrival_df)) %>%
      left_join(site_resus_maj, by = "site") %>%
      mutate(n_dta_norm = n_dta / total) %>%
      ungroup()
    
    dat_top_stats <- dat_ecds %>%
      left_join(dat_spec, by = c("nhs_number", "arr_id")) %>%
      filter(spec == "medical", !is.na(dta_dt)) %>%
      bind_rows(mutate(., Site_Name = "System total")) %>%
      summarise(n_dta = n(), mean_dta_time = mean(dta_time), dta_time = sum(dta_time), .by = Site_Name) %>%
      left_join(
        dat_dtas %>% bind_rows(mutate(., site = "System total")) %>% summarise(n_dta_norm = mean(n_dta_norm), .by = site) %>% rename(Site_Name = site),
        by = "Site_Name"
      )
    
    list(dta_df = dat_dtas, dta_top_stats = dat_top_stats)
  })
  
  dta_df <- dat_dtas_res$dta_df
  dta_top_stats <- dat_dtas_res$dta_top_stats %>%
    rename(grp = Site_Name) %>%
    pivot_longer(-grp, names_to = "impact") %>%
    mutate(label = case_when(
      impact == "n_dta" ~ "emergency medical admissions",
      impact == "dta_time" ~ "hours waited for admission",
      impact == "mean_dta_time" ~ "average hours waited for admission",
      impact == "n_dta_norm" ~ "average number in DTA queue as proportion of ED capacity"
    ),
    target = NA_real_,
    emph_text = case_when(
      impact == "n_dta" ~ as.character(value),
      impact == "dta_time" ~ as.character(round(value / (60 * 24))),
      impact == "n_dta_norm" ~ scales::percent(value, accuracy = 1),
      impact == "mean_dta_time" ~ as.character(round(value / 60))
    ),
    sub_text = "days")
  
  # --- ED ATTEND IMPACT ---
  dat_ed_time <- local({
    string_ecds_full <- glue::glue("
      SELECT nhs_number, Attendance_Unique_Identifier, Treatment_Function_Code, Arrival_Date, Arrival_Time, Initial_Assessment_Date, Initial_Assessment_Time, Seen_For_Treatment_Date, Seen_For_Treatment_Time, Decision_To_Admit_Date, Decision_To_Admit_Time, Organisation_Code_Commissioner, Departure_Date, Departure_Time, Site_Name, Acuity_Desc 
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS 
      WHERE Site_Name IN ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') 
        AND Arrival_Date BETWEEN '{report_start}' AND '{report_end}'
    ")
    sqlQuery(con, string_ecds_full) %>%
      filter(is.na(Decision_To_Admit_Date)) %>%
      mutate(arrival_dt = as.POSIXct(paste0(Arrival_Date, " ", substr(Arrival_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             treatment_dt = as.POSIXct(paste0(Seen_For_Treatment_Date, " ", substr(Seen_For_Treatment_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S"),
             dta_dt = as.POSIXct(ifelse(grepl("NA", paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))), NA, paste0(Decision_To_Admit_Date, " ", substr(Decision_To_Admit_Time, 1, 8))), format = "%Y-%m-%d %H:%M:%S"),
             departure_dt = as.POSIXct(paste0(Departure_Date, " ", substr(Departure_Time, 1, 8)), format = "%Y-%m-%d %H:%M:%S")) %>%
      rowwise() %>%
      mutate(departure_dt = max(dta_dt, departure_dt, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(treatment_dt = pmin(treatment_dt, dta_dt, departure_dt, na.rm = TRUE),
             trt_time = interval(arrival_dt, treatment_dt) / dminutes(1),
             dt = floor_date(arrival_dt, glue::glue("{n_hour} hours"))) %>%
      select(dt, site = Site_Name, trt_time)
  })
  
  ed_attend_impact <- dat_ed_time %>%
    left_join(dta_df %>% select(site, dt, n_dta_norm), by = c("site", "dt")) %>%
    mutate(week = ceiling_date(as.Date(dt), unit = "week", week_start = wday(report_end, week_start = 1), change_on_boundary = FALSE),
           ed_time_impact = n_dta_norm * ed_time_coef,
           ed_time_impact_cf = pmin(n_dta_norm, 0.25) * ed_time_coef,
           ed_time_impact_full_cf = 0,
           diff = ed_time_impact - ed_time_impact_cf,
           diff_full = ed_time_impact - ed_time_impact_full_cf,
           trt_time_cf = trt_time - diff,
           trt_time_full_cf = trt_time - diff_full) %>%
    bind_rows(mutate(., site = "System total")) %>%
    summarise(across(c(trt_time, trt_time_cf, trt_time_full_cf), ~ mean(.x < 4 * 60, na.rm = TRUE)),
              diff = sum(diff, na.rm = TRUE),
              diff_full = sum(diff_full, na.rm = TRUE),
              om3_n_exposure = n(), .by = c(week, site)) %>%
    rename(grp = site, om3_impact = trt_time, om3_impact_cf = trt_time_cf, om3_raw = diff, om3_full_raw = diff_full) %>%
    pivot_longer(-c(week, grp), names_to = "impact") %>%
    mutate(target = 0.78,
           label = case_when(
             impact %in% c("om3_impact", "om3_impact_cf", "om3_impact_full_cf") ~ "4-hour seen for treatment performance",
             impact == "om3_raw" ~ "Hours additional waiting to be seen for treatment",
             impact == "om3_n_exposure" ~ "Number ED attends"
           ),
           emph_text = case_when(
             impact %in% c("om3_impact", "om3_impact_cf", "om3_impact_full_cf") ~ percent(value, accuracy = 0.1),
             impact %in% c("om3_raw", "om3_raw_full") ~ number(round(value), big.mark = ","),
             impact == "om3_n_exposure" ~ number(round(value), big.mark = ",")
           ),
           sub_text = case_when(
             impact %in% c("om3_impact", "om3_impact_cf", "om3_impact_full_cf") ~ "4-hour performance",
             impact %in% c("om3_raw", "om3_raw_full") ~ "Time to be seen, hours",
             impact == "om3_n_exposure" ~ label
           ))
  
  # Combine remaining impacts
  bind_rows(dta_top_stats, downstream_impacts, ed_attend_impact) %>%
    mutate(report_date = report_end)
}

# Run function and write output
out <- get_impact_data(report_start = report_start, report_end = report_end)

# Database export
host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")

conn <- dbConnect(dbDriver("MySQL"), dbname = dbname, host = host, port = 3306, user = user, password = password)
on.exit(dbDisconnect(conn), add = TRUE)

DBI::dbGetQuery(conn, "DELETE FROM excess_impacts_dev")
dbWriteTable(conn, "excess_impacts_dev", value = out, overwrite = TRUE)