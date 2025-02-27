require(tidyverse)
require(RODBC)
require(lubridate)
library(tidymodels)
library(patchwork)
library(ggtext)
library(marquee)

# upstream data
con_string <- "driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)

report_end <- today() - ddays(3)
report_start <- report_end - ddays(6)

n_hour <- 2
pre_window_days <- 60

bin_acc <- 0.01 # this is a legacy variable which is needed because it's baked into plot outputs (silly really..)
# Site normalisation data
site_resus_maj <- tibble(site = c("BRISTOL ROYAL INFIRMARY", "WESTON GENERAL HOSPITAL", "SOUTHMEAD HOSPITAL"),
                         maj = c(16, 9, 14),
                         resus = c(8, 4, 6)) %>%
  mutate(total = maj + resus)


mort_mod <- readRDS("data/mort_effects_output.RDS") %>% `[[`("mort_mod_raw")
readm_mod <- readRDS("data/readm_effects_output.RDS") %>% `[[`("readm_mod_raw")
los_mod <- readRDS("data/los_effect_output_reformulated.RDS") %>% `[[`("los_mod")
vw_impute_rec <- readRDS("data/vw_imput_rec.RDS")

handover_coef <- readRDS("data/avoidable_impacts_handover_coef.RDS")
ed_time_coef <- readRDS("data/avoidable_impacts_ed_time_coef.RDS")
resp_coef <- readRDS("data/avoidable_impacts_resp_coef.RDS")

get_impact_data <- function(report_start, report_end){
  
  # sequence of time snapshots (i.e. when we measure DTA queue)
  snapshot_seq <- seq.POSIXt(from = as.POSIXct(lubridate::ymd(report_start)),
                             to = as.POSIXct(lubridate::ymd(report_end)),
                             by = glue::glue("{n_hour} hours"))
  
  # sequence of time intervals (i.e. over which we measure mean handover delay)
  snapshot_int <- map(snapshot_seq, ~lubridate::interval(.x - dminutes((60*n_hour)/2),
                                                         .x + dminutes((60*n_hour)/2)))
  
  
  ## DOWNSTREAM IMPACTS
  
  dat_downstream <- local({
    string_ecds<-glue::glue("select 
                        nhs_number,
                        Index_Of_Multiple_Deprivation,
                        Stated_Gender,
                        HES_Age_At_Arrival,
                        Attendance_Unique_Identifier,
                        Treatment_Function_Code,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Organisation_Code_Commissioner,
                        Departure_Date,
                        Departure_Time,
                        Site_Name,
                        Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      --and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      --and destination_desc = 'Discharge to ward'
      and coalesce(destination_desc, 'Missing') not in ('Emergency department discharge to emergency department short stay ward',
      'Emergency department discharge to same day emergency care service',
      'Emergency department discharge to ambulatory emergency care service',
      'Emergency department discharge to neonatal intensive care unit',
      'Emergency department discharge to special care baby unit',
      'Admission to the mortuary',
      'Discharge to home',
      'Discharge to hospital at home service',
      'Discharge to nursing home',
      'Discharge to residential home',
      'Discharge to police custody',
      'Patient discharge, to legal custody',
      'Urgent admission to hospice'
      )
      ")
    dat_ecds_raw<-sqlQuery(con,string_ecds)
    
    dat_ecds<-dat_ecds_raw %>%
      mutate(nhs_number = as.character(nhs_number)) %>%
      mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
      mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      select(nhs_number,
             imd = Index_Of_Multiple_Deprivation,
             sex = Stated_Gender,
             age = HES_Age_At_Arrival,
             Attendance_Unique_Identifier,
             tfc = Treatment_Function_Code,
             Organisation_Code_Commissioner,
             arrival_dt,
             initial_dt,
             treatment_dt,
             dta_dt,
             departure_dt,
             site = Site_Name,
             Acuity_Desc) %>%
      rowwise() %>%
      mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(dta_int = interval(dta_dt, departure_dt)) %>%
      mutate(dta_time = dta_int/dminutes(1)) %>%
      mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
      mutate(date=floor_date(departure_dt,"day")) %>%
      mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                   TRUE~"major")) %>%
      mutate(arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt)) 
    
    string_ecds_full <-glue::glue("select 
                        NHS_Number,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Departure_Date,
                        Departure_Time,
                        Conclusion_Date,
                        Conclusion_Time,
                        Site_Name,
                        Acuity_Desc,
                        Index_Of_Multiple_Deprivation,
                        Stated_Gender,
                        HES_Age_At_Arrival,
                        Organisation_Code_Commissioner,
                        Arrival_Mode_Desc,
                        Arrival_Category_Desc,
                        Attendance_Source_Desc,
                        destination_desc,
                        Chief_Complaint_Desc,
                        Acuity_Desc,
                        Diagnoses_Code_1_Desc,
                        Investigation_Code_1_Desc,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{ymd(report_start) - ddays(pre_window_days)}' and '{report_end}'
      ")
    
    dat_ecds_full_raw<-sqlQuery(con,string_ecds_full)
    
    dat_ecds_full <- dat_ecds_full_raw %>%
      filter(Organisation_Code_Commissioner == '15C') %>% # filter for 15C as we can not pick up deaths for non BNSSG patients
      mutate(NHS_Number = as.character(NHS_Number)) %>%
      mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code, "Missing"))) %>%
      mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
      mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(conclusion_dt=as.POSIXct(paste0(Conclusion_Date," ",substr(Conclusion_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      select(
        nhs_number = NHS_Number,
        tfc = Treatment_Function_Code,
        imd = Index_Of_Multiple_Deprivation,
        sex = Stated_Gender,
        age = HES_Age_At_Arrival,
        arrival_dt,
        initial_dt,
        treatment_dt,
        dta_dt,
        departure_dt,
        conclusion_dt,
        site = Site_Name,
        Acuity_Desc,
        arrival_mode = Arrival_Mode_Desc,
        arrival_cat = Arrival_Category_Desc,
        diagnoses = Diagnoses_Code_1_Desc,
        investigation = Investigation_Code_1_Desc
      ) %>%
      rowwise() %>%
      mutate(departure_dt=max(dta_dt,departure_dt, conclusion_dt, na.rm=TRUE)) %>%
      # mutate(departure_dt=max(departure_dt, conclusion_dt, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(dta_time = interval(dta_dt, departure_dt)/dminutes(1)) %>%
      mutate(ed_time = interval(arrival_dt, departure_dt)/dminutes(1)) %>%
      mutate(breach = (interval(arrival_dt, departure_dt)/dminutes(1)) > 4*60) %>%
      mutate(date=floor_date(departure_dt,"day")) %>%
      mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                   TRUE~"major")) %>%
      group_by(nhs_number, arrival_dt) %>%
      mutate(arr_id = cur_group_id()) %>%
      ungroup()
    
    # arrivals that become dta
    arrivals <- 
      dat_ecds %>%
      filter(!is.na(dta_time),
             arrival_dt >= report_start,
             # nhs_number %in% unique(dat_attr$nhs_number),
             !is.na(nhs_number)) %>%
      select(nhs_number, arr_id, tfc, arrival_dt, departure_dt) %>%
      distinct() %>%
      group_by(nhs_number, arr_id) %>%
      summarise(
        tfc = tfc[1],
        arrival_dt = arrival_dt, 
        departure_dt = as.Date(departure_dt)[1]
      ) %>%
      ungroup()
    
    dat_spec <- arrivals %>%
      select(nhs_number, arr_id, tfc) %>%
      mutate(spec = ifelse(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
      #mutate(MainSpecialtyCode = ifelse(MainSpecialtyCode %in% c(300, 326, 430), "medical", "not medical")) %>% 
      #mutate(spec = ifelse(tfc == "medical" | MainSpecialtyCode == "medical", "medical", "not medical")) %>%
      select(nhs_number, arr_id, spec)
    
    dat_ecds_perf <- glue::glue(
      "select
                        NHS_Number,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Departure_Date,
                        Departure_Time,
                        Conclusion_Date,
                        Conclusion_Time,
                        Site_Name,
                        Acuity_Desc,
                        Index_Of_Multiple_Deprivation,
                        Stated_Gender,
                        HES_Age_At_Arrival,
                        Organisation_Code_Commissioner,
                        Arrival_Mode_Desc,
                        Arrival_Category_Desc,
                        Attendance_Source_Desc,
                        destination_desc,
                        Chief_Complaint_Desc,
                        Acuity_Desc,
                        Diagnoses_Code_1_Desc,
                        Investigation_Code_1_Desc,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{ymd(report_start)}' and '{report_end}'
      "
    ) %>%
      sqlQuery(con, .) %>% mutate(NHS_Number = as.character(NHS_Number)) %>%
      mutate(Treatment_Function_Code = coalesce(as.character(Treatment_Function_Code, "Missing"))) %>%
      mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
      mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(conclusion_dt=as.POSIXct(paste0(Conclusion_Date," ",substr(Conclusion_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      select(
        nhs_number = NHS_Number,
        arrival_dt,
        initial_dt,
        treatment_dt,
        dta_dt,
        departure_dt,
        conclusion_dt,
        site = Site_Name
      ) %>%
      rowwise() %>%
      mutate(departure_dt=max(dta_dt,departure_dt, conclusion_dt, na.rm=TRUE)) %>%
      # mutate(departure_dt=max(departure_dt, conclusion_dt, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(dta_time = interval(dta_dt, departure_dt)/dminutes(1)) %>%
      mutate(ed_time = interval(arrival_dt, departure_dt)/dminutes(1)) %>%
      mutate(breach = (interval(arrival_dt, departure_dt)/dminutes(1)) > 4*60) %>%
      mutate(arrival_hour = ceiling_date(arrival_dt, unit = "4 hour")) %>%
      filter(!between(arrival_hour, ymd("2022-04-01"), ymd("2022-07-10"))) %>%
      # Remove rows for weston out of hours
      filter(!(site == "WESTON GENERAL HOSPITAL" & hour(arrival_dt) %in% c(10:12, 1:7))) %>%
      group_by(site, arrival_hour) %>%
      summarise(perf = 1-mean(breach)) %>%
      ungroup()
    
    dat_dtas <- 
      dat_ecds %>%
      select(nhs_number, arrival_dt, dta_dt) %>%
      mutate(
        arrival_day = floor_date(arrival_dt, "days"),
        dta_day = floor_date(dta_dt, "days")
        
      ) %>%
      distinct() %>%
      filter(!is.na(nhs_number)) %>%
      arrange(nhs_number, arrival_day) %>%
      group_by(nhs_number, arrival_day) %>%
      # for any given patient & day of arrival, take the last record (if there is
      # more than one)
      slice_max(arrival_dt, na_rm = TRUE) %>%
      # filter(arrival_dt == max(arrival_dt, na.rm = TRUE)) %>%
      group_by(nhs_number) %>%
      # remove patients that never DTA
      filter(!all(is.na(dta_dt))) %>%
      # earliest arrival with dta
      slice_min(dta_dt) %>%
      group_by(nhs_number, dta_day) %>%
      mutate(id = cur_group_id()) %>%
      ungroup()
    
    dat_ed_attends <- 
      dat_ecds_full %>%
      select(nhs_number, pre_arrival_dt = arrival_dt) %>%
      distinct()
    
    dat_pre_attends <- dat_dtas %>%
      left_join(dat_ed_attends) %>%
      filter(pre_arrival_dt < arrival_dt) %>%
      filter(between(pre_arrival_dt, arrival_dt - ddays(pre_window_days), arrival_dt)) %>%
      group_by(id) %>%
      count(name = "n_ed")
    
    dat_ecds_n_ed <- dat_dtas %>%
      left_join(dat_pre_attends) %>%
      mutate(n_ed = coalesce(n_ed, 0)) %>%
      select(nhs_number, n_ed) %>%
      distinct()
    
    dat <- dat_ecds %>%
      left_join(dat_spec) %>%
      filter(spec == "medical", !is.na(dta_dt)) %>%
      mutate(dta_time = dta_time/60) %>% # convert minutes to hours
      # filter(ed_time < 60*30) %>%
      mutate(age = cut(age, breaks = c(0 , seq(30, 100, 10), Inf), right = FALSE, labels = c(
        "29 years old or under",
        # "20-29 years old" ,
        "30-39 years old" ,
        "40-49 years old" ,
        "50-59 years old" ,
        "60-69 years old" ,
        "70-79 years old" ,
        "80-89 years old" ,
        "90-99 years old" ,
        "100 years old or over" 
      ))
      )  %>%
      filter(sex %in% 1:2) %>%
      mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
      mutate(imd = factor(
        imd,
        levels = 1:10,
        labels = c(
          "1 (least deprived)",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9",
          "10 (most deprived)"
        )
      )) %>%
      mutate(wkend = factor(ifelse(wday(arrival_dt, week_start = 1) < 6, "No", "Yes"))) %>%
      mutate(arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))) %>%
      mutate(arrival_time = forcats::fct_recode(arrival_time,
                                                "00:00-05:59" = "00:00:00",
                                                "06:00-11:59" = "06:00:00",
                                                "12:00-17:59" = "12:00:00",
                                                "18:00-23:59" = "18:00:00")) %>%
      mutate(arrival_month = factor(month.name[month(arrival_dt)])) %>%
      mutate(arrival_month = `levels<-`(arrival_month, month.name)) %>%
      # mutate(arrival_month = fct_expand(arrival_month, month.name)) %>%
      # mutate(arrival_year = factor(year(arrival_dt))) %>% # we can't use this because 2025 hasn't been trained on
      # mutate(arrival_year = factor(2024)) %>%
      mutate(arrival_hour = ceiling_date(arrival_dt, "4 hour")) %>% 
      left_join(dat_ecds_perf) %>%
      left_join(dat_ecds_n_ed) %>%
      select(
        nhs_number,
        wkend,
        arrival_time,
        arrival_month,
        # arrival_year,
        perf,
        n_ed,
        age,
        sex,
        imd,
        dta_time,
        site
        
        # wkend,
        # arrival_time,
        # arrival_month,
        # perf,
        # age,
        # n_ed,
        # sex,
        # imd,
        # dta_time
      )
    
    dat
    
  })
  
  downstream_impacts <- dat_downstream %>%
    nest(.by = site) %>%
    mutate(data = map(data, \(d) bake(vw_impute_rec, new_data = mutate(d, vw_score = NA_real_)))) %>%
    mutate(n_exposure = map_dbl(data, nrow)) %>%
    mutate(pred_mort = map(data, \(d) predict(mort_mod, newdata = d, type = "response"))) %>%
    mutate(pred_mort_cf = map(data, \(d) predict(mort_mod, newdata = mutate(d, dta_time = 1), type = "response"))) %>%
    mutate(pred_readm = map(data, \(d) predict(readm_mod, newdata = d, type = "response"))) %>%
    mutate(pred_readm_cf = map(data, \(d) predict(readm_mod, newdata = mutate(d, dta_time = 1), type = "response"))) %>% 
    mutate(pred_los = map(data, \(d) predict.lm(los_mod, newdata = d))) %>%
    mutate(pred_los_cf = map(data, \(d) predict.lm(los_mod, newdata = mutate(d, dta_time = 1)))) %>%
    mutate(om6_impact = map2_dbl(pred_mort, pred_mort_cf, ~sum(.x-.y, na.rm = TRUE))) %>%
    mutate(om6_mean_or = map2_dbl(pred_mort, pred_mort_cf, ~mean((.x/(1-.x))/(.y/(1-.y))-1, na.rm = TRUE))) %>%
    mutate(om5_impact = map2_dbl(pred_readm, pred_readm_cf, ~sum(.x-.y, na.rm = TRUE))) %>%
    mutate(om5_mean_or = map2_dbl(pred_readm, pred_readm_cf, ~mean((.x/(1-.x))/(.y/(1-.y))-1, na.rm = TRUE))) %>%
    mutate(om4_impact = map2_dbl(pred_los, pred_los_cf, ~sum(.x-.y, na.rm = TRUE))) %>%
    select(grp = site, matches("om"), om4_n_exposure = n_exposure, om5_n_exposure = n_exposure, om6_n_exposure = n_exposure) %>%
    pivot_longer(-grp, names_to = "impact") %>%
    mutate(level = case_when(str_detect(impact, "impact") ~ "aggregate",
                             !str_detect(impact, "impact|n_exposure") ~ "individual",
                             .default = NA_character_), 
           impact = str_remove(impact, "_impact"),
           label = case_when(
             str_detect(impact, "n_exposure") ~ "number impacted",
             impact == "om4" ~ "days more LOS",
             impact == "om4_mean" ~ "average days more LOS",
             impact == "om5" ~ "readmissions",
             impact == "om5_mean_or" ~ "average increase in odds of readmission",
             impact == "om6" ~ "deaths",
             impact == "om6_mean_or" ~ "average increase in odds of mortality"
           ),
           colour = case_when(
             str_detect(impact, "om4") ~ "#006d77",
             str_detect(impact, "om5") ~ "#f07d24",
             str_detect(impact, "om6") ~ "#ef5546"
           )
    )
  
  
  
  
  ## UPSTREAM IMPACTS
  
  handover_arrivals <- local({
    string_actv_swafst <- glue::glue(
      "SELECT DISTINCT
    [ID]
    ,[Derived_Pseudo_NHSNumber]
    ,[Incident_Number]
    ,[Time_Of_Incident]
    ,[CCG]
    ,[Post_Code_Sector]
    ,[Symptom_Group_Chief_Complaint]
    ,[Pathways_AMPDS_Disposition]
    ,[Managed_Incident]
    ,[Incident_Colour]
    ,[Incident_Colour_Split]
    ,[Incident_Colour_Split_Full]
    ,[Response_Target_1_Clock Start]
    ,[Response_Target_2_Clock Start]
    ,[Response_Target_1_Clock Stop]
    ,[Response_Target_2_Clock Stop]
    ,[Outcome]
    ,[Conveyed_To_Ward_Type]
   ,b.[OrganisationName] as 'Site_Name'
    ,[First_Hospital_Attended]
    ,[First_Ward_Attended]
    ,[Time_of_Handover_at_First_Hospital_Attended]
    ,[First_MIU_Attended]
    ,[CCG_of_First_MIU_Attended]
    ,[Number_of_Patients_Carried]
    ,[Contract_Activity]
    ,[Month_of_Call]
    ,[Age_Bracket]
    ,[Residential_or_Nursing_home_Location]
    ,[Residential_or_Nursing_home_Location_Name]
    ,[Authorising_Doctor_Practice]
    ,[Authorising_Hospital]
    ,[Booked_Pickup_Hospital]
    ,[Booked_Pickup_Ward]
    ,[Booked_Destination_Hospital]
    ,[Booked_Destination_Ward]
    ,[Local_Authority]
    ,[Ward]
    ,c.[Patient_Gender]
    ,c.[Handover_Duration__seconds_]
    ,[T0_Time_of_Call]
    ,[T1_Time_Call_answered]
    ,[Time_First_Core_Resource_Allocated]
    ,[Time_Last_Core_Resource_Cleared]
    ,[Mental_Health_Call_Indicator]
    ,[Suicide_Call_Indicator]
    ,[Provisional_Diagnosis]
    ,[High_Intensity_User_Call_Indicator]
    ,[SourceFileID]
    ,c.[Time_Resource_Arrived_at_Receiving_Location]
    ,c.[Time_of_Patient_Handover]
    ,c.[Time_Clear]
FROM [ABI].[dbo].[vw_UrgentCare_Ambulance_SWASFT] a
LEFT JOIN [ABI].[Lard].[vw_Organisation_Lookup] b on a.[First_Hospital_Attended]=b.[OrganisationCode]
LEFT JOIN [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_SWAST_AR119] c on a.Incident_Number=c.Call_Number
where Outcome = 'See & Convey' AND
b.[OrganisationName] in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL') AND
Conveyed_To_Ward_Type = 'A&E' AND
  (CONVERT(DATE, [Time_Of_Incident]) > '{report_start}' OR
  CONVERT(DATE, [T0_Time_of_Call]) > '{report_start}' OR
  CONVERT(DATE, [T1_Time_Call_answered]) > '{report_start}' OR
  CONVERT(DATE, [Time_First_Core_Resource_Allocated]) > '{report_start}' OR
  CONVERT(DATE, [Time_Last_Core_Resource_Cleared]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_1_Clock Start]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_2_Clock Start]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_1_Clock Stop]) > '{report_start}' OR
  CONVERT(DATE, [Response_Target_2_Clock Stop]) > '{report_start}')"
    )
    
    actv_swafst <- RODBC::sqlQuery(con, string_actv_swafst) %>%
      mutate(handover_time = lubridate::parse_date_time(Time_of_Handover_at_First_Hospital_Attended, orders = "ymd HMS")) %>%
      filter(handover_time <= report_end)
    
    handover_df <- actv_swafst %>%
      group_by(site = Site_Name, Outcome) %>%
      nest() %>%
      mutate(handover_time = list(map2(
        snapshot_int,
        data,
        ~ filter(.y, handover_time %within% .x) %>%
          select(nhs_number = Derived_Pseudo_NHSNumber,
                 handover_time = Handover_Duration__seconds_)
      ))) %>%
      mutate(handover_df = map(handover_time, function(handover_time)
        map2_df(handover_time, snapshot_seq, ~ bind_cols(tibble(dt = .y), .x)))) %>%
      select(site, handover_df) %>%
      unnest(handover_df) %>%
      arrange(dt) %>%
      group_by(site, dt) %>%
      count() %>%
      ungroup()
    
  })
  
  dta_dats <- local({
    string_ecds<-glue::glue("select 
                        nhs_number,
                        Attendance_Unique_Identifier,
                        Treatment_Function_Code,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Organisation_Code_Commissioner,
                        Departure_Date,
                        Departure_Time,
                        Site_Name,
                        Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      --and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      --and destination_desc = 'Discharge to ward'
      and coalesce(destination_desc, 'Missing') not in ('Emergency department discharge to emergency department short stay ward',
      'Emergency department discharge to same day emergency care service',
      'Emergency department discharge to ambulatory emergency care service',
      'Emergency department discharge to neonatal intensive care unit',
      'Emergency department discharge to special care baby unit',
      'Admission to the mortuary',
      'Discharge to home',
      'Discharge to hospital at home service',
      'Discharge to nursing home',
      'Discharge to residential home',
      'Discharge to police custody',
      'Patient discharge, to legal custody',
      'Urgent admission to hospice'
      )
      ")
    dat_ecds_raw<-sqlQuery(con,string_ecds)
    
    dat_ecds<-dat_ecds_raw %>%
      mutate(nhs_number = as.character(nhs_number)) %>%
      mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
      mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      select(nhs_number, Attendance_Unique_Identifier,  tfc = Treatment_Function_Code, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
      rowwise() %>%
      mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(dta_int = interval(dta_dt, departure_dt)) %>%
      mutate(dta_time = dta_int/dminutes(1)) %>%
      mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
      mutate(date=floor_date(departure_dt,"day")) %>%
      mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                   TRUE~"major")) %>%
      mutate(arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt)) 
    
    
    string_ecds_full<-glue::glue("select 
                        nhs_number,
                        Attendance_Unique_Identifier,
                        Treatment_Function_Code,
                        Arrival_Date,
                        Arrival_Time,
                        Initial_Assessment_Date,
                        Initial_Assessment_Time,
                        Seen_For_Treatment_Date,
                        Seen_For_Treatment_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Organisation_Code_Commissioner,
                        Departure_Date,
                        Departure_Time,
                        Site_Name,
                        Acuity_Desc
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      --and Treatment_Function_Code in (300, 326, 430)
      and Arrival_Date between '{report_start}' and '{report_end}'
      ")
    
    dat_ecds_full_raw<-sqlQuery(con,string_ecds_full)
    
    dat_ecds_full <- dat_ecds_full_raw %>%
      mutate(nhs_number = as.character(nhs_number)) %>%
      mutate(arrival_dt=as.POSIXct(paste0(Arrival_Date," ",substr(Arrival_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(initial_dt=as.POSIXct(paste0(Initial_Assessment_Date," ",substr(Initial_Assessment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(treatment_dt=as.POSIXct(paste0(Seen_For_Treatment_Date," ",substr(Seen_For_Treatment_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
      mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
      mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
      select(nhs_number, Attendance_Unique_Identifier,  tfc = Treatment_Function_Code, Organisation_Code_Commissioner, arrival_dt,initial_dt,treatment_dt,dta_dt,departure_dt,Site_Name, Acuity_Desc) %>%
      rowwise() %>%
      mutate(departure_dt=max(dta_dt,departure_dt,na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(dta_int = interval(dta_dt, departure_dt)) %>%
      mutate(dta_time = dta_int/dminutes(1)) %>%
      mutate(trt_time = interval(arrival_dt, treatment_dt)/dminutes(1)) %>%
      mutate(date=floor_date(departure_dt,"day")) %>%
      mutate(ed_category=case_when(Acuity_Desc %in% c("Non-urgent level emergency care","Standard level emergency care") ~ "minor",
                                   TRUE~"major")) %>%
      mutate(arr_id = cur_group_id(), .by = c(nhs_number, arrival_dt)) 
    
    
    # arrivals that become dta
    arrivals <- 
      dat_ecds %>%
      filter(!is.na(dta_time),
             arrival_dt >= report_start,
             # nhs_number %in% unique(dat_attr$nhs_number),
             !is.na(nhs_number)) %>%
      group_by(nhs_number, arr_id) %>%
      summarise(
        tfc = tfc[1],
        arrival_dt = arrival_dt, 
        departure_dt = as.Date(departure_dt)[1]
      ) %>%
      ungroup()
    
    # all arrivals
    arrivals_full <- 
      dat_ecds_full %>%
      filter(arrival_dt >= report_start) %>%
      group_by() %>%
      summarise(
        id = Attendance_Unique_Identifier,
        site = Site_Name,
        arrival_dt = arrival_dt, 
      ) %>%
      distinct()
    
    
    dat_spec <- arrivals %>%
      select(nhs_number, arr_id, tfc) %>%
      mutate(spec = ifelse(tfc %in% c(300, 326, 430), "medical", "not medical")) %>%
      #mutate(MainSpecialtyCode = ifelse(MainSpecialtyCode %in% c(300, 326, 430), "medical", "not medical")) %>% 
      #mutate(spec = ifelse(tfc == "medical" | MainSpecialtyCode == "medical", "medical", "not medical")) %>%
      select(nhs_number, arr_id, spec)
    
    
    dat_dtas <- dat_ecds %>%
      left_join(dat_spec) %>%
      filter(spec == "medical") %>%
      group_by(site = Site_Name) %>%
      nest() %>%
      mutate(dtas = list(map2_dbl(snapshot_seq, data, \(snapshot, data) sum(
        snapshot %within% na.omit(data$dta_int)
      ))))  %>%
      mutate(arrivals = list(map2_dbl(snapshot_int,
                                      site,
                                      \(snapshot_int, site_id)
                                      nrow(filter(arrivals_full, arrival_dt %within% snapshot_int, site == site_id))
      )
      )
      ) %>%
      mutate(dta_df = map(dtas, function(n_dta) map2_df(n_dta, snapshot_seq, ~tibble(dt = .y, n_dta = .x))))  %>%
      mutate(arrival_df = map(arrivals, function(n_arrival_all) map_df(n_arrival_all, ~tibble(n_arrival_all = .x)))) %>%
      select(site, dta_df, arrival_df) %>%
      unnest(cols = c(dta_df, arrival_df)) %>%
      left_join(site_resus_maj) %>%
      mutate(n_dta_norm = n_dta/total) %>%
      ungroup()
    
    dat_top_stats <- dat_ecds %>%
      left_join(dat_spec) %>%
      filter(spec == "medical", !is.na(dta_dt)) %>%
      summarise(n_dta = n(), dta_time = sum(dta_time), .by = Site_Name)
    
    list(dta_df = dat_dtas, dta_top_stats = dat_top_stats)
  })
  
  dta_df <- dta_dats$dta_df
  dta_top_stats <- dta_dats$dta_top_stats
  
  resp_df <- local({
    string_actv_swafst <- glue::glue(
      "SELECT DISTINCT
    [ID]
    ,[Derived_Pseudo_NHSNumber]
    ,c.[Patient_Gender]
    ,[Incident_Number]
    ,[Time_Of_Incident]
    ,[CCG]
    ,[Post_Code_Sector]
    ,[Symptom_Group_Chief_Complaint]
    ,[Pathways_AMPDS_Disposition]
    ,[Managed_Incident]
    ,[Incident_Colour]
    ,[Incident_Colour_Split]
    ,[Incident_Colour_Split_Full]
    ,[Response_Target_1_Clock Start]
    ,[Response_Target_2_Clock Start]
    ,[Response_Target_1_Clock Stop]
    ,[Response_Target_2_Clock Stop]
    ,[Outcome]
    ,[Conveyed_To_Ward_Type]
    ,[First_Hospital_Attended]
    ,[First_Ward_Attended]
    ,[Time_of_Handover_at_First_Hospital_Attended]
    ,[First_MIU_Attended]
    ,[CCG_of_First_MIU_Attended]
    ,[Number_of_Patients_Carried]
    ,[Contract_Activity]
    ,[Month_of_Call]
    ,[Age_Bracket]
    ,[Residential_or_Nursing_home_Location]
    ,[Residential_or_Nursing_home_Location_Name]
    ,[Authorising_Doctor_Practice]
    ,[Authorising_Hospital]
    ,[Booked_Pickup_Hospital]
    ,[Booked_Pickup_Ward]
    ,[Booked_Destination_Hospital]
    ,[Booked_Destination_Ward]
    ,[Local_Authority]
    ,[Ward]
    ,[T0_Time_of_Call]
    ,[T1_Time_Call_answered]
    ,[Time_First_Core_Resource_Allocated]
    ,[Time_Last_Core_Resource_Cleared]
    ,[Mental_Health_Call_Indicator]
    ,[Suicide_Call_Indicator]
    ,[Provisional_Diagnosis]
    ,[High_Intensity_User_Call_Indicator]
    ,[SourceFileID]
FROM [ABI].[dbo].[vw_UrgentCare_Ambulance_SWASFT] a
LEFT JOIN [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_SWAST_AR119] c on a.Incident_Number=c.Call_Number
where Outcome in ('See & Convey', 'See & Treat') 
AND CCG = '15C'
AND
  CONVERT(DATE, [T0_Time_of_Call]) >= '{report_start}'"
    )
    
    actv_swafst <- RODBC::sqlQuery(con, string_actv_swafst)
    
    
    dat <- actv_swafst %>%
      mutate(across(matches("Time|Clock"),
                    .f = ~lubridate::parse_date_time(.x, orders = c("ymd_HMS", "dmy_HMS")))) %>%
      filter(Outcome %in% c("See & Treat", "See & Convey"))  %>%
      filter(T0_Time_of_Call >= ymd(report_start),
             T0_Time_of_Call <= ymd(report_end) ) %>%
      mutate(dt = floor_date(T0_Time_of_Call, glue::glue("{n_hour} hours")),
             call_category = Incident_Colour,
             response = (`Response_Target_1_Clock Stop` - `Response_Target_1_Clock Start`)/dminutes(1)) %>%
      filter(call_category %in% str_c("Category ", c(1, 2, 3))) %>%
      mutate(call_category = recode(call_category,
                                    "Category 3"= "cat3",
                                    "Category 2"= "cat2",
                                    "Category 1"= "cat1")) %>%
      mutate(nhs_number = as.character(Derived_Pseudo_NHSNumber)) %>%
      select(nhs_number, dt, age = Age_Bracket, outcome = Outcome, call_category, response) %>%
      group_by(dt = floor_date(dt, "2 hours"), call_category) %>%
      summarise(n = n(), response = sum(response)) %>%
      ungroup() %>%
      na.omit()
    
    dat 
  })
  
  ed_attend_impact <- dta_df %>%
    select(site, n_arrival_all, n_dta_norm) %>%
    mutate(ed_time_impact = (n_dta_norm * ed_time_coef * n_arrival_all)/60) %>%
    select(-n_dta_norm) %>%
    summarise(om3_n_exposure = sum(n_arrival_all),
              ed_time_impact = sum(ed_time_impact),
              .by = site) %>%
    mutate(ed_time_impact_mean = ed_time_impact/om3_n_exposure) %>%
    select(grp = site, ed_time_impact, ed_time_impact_mean, om3_n_exposure)  %>%
    pivot_longer(-grp, names_to = "impact") %>%
    mutate(impact = recode(impact,
                           "ed_time_impact" = "om3",
                           "ed_time_impact_mean" = "om3_mean"
    )) %>%
    mutate(
      level = case_when(
        impact == "om3" ~ "aggregate",
        impact == "om3_mean" ~ "individual"
      ),
      label = case_when(
        str_detect(impact, "n_exposure") ~ "number exposed",
        impact == "om3" ~ "hours longer to be seen for treatment in ED",
        impact == "om3_mean" ~ "average hours waited longer to be seen for treatment in ED"
      ),
      colour = case_when(
        impact == "om3" ~ "#A89532",
        impact == "om3_mean" ~ "#A89532"
      )) 
  
  
  
  handover_impact <- dta_df %>%
    select(site, dt, n_arrival_all, n_dta_norm) %>%
    left_join(handover_arrivals) %>%
    mutate(n_handover = coalesce(n, 0)) %>%
    select(-n, -dt) %>%
    mutate(
      handover_impact = (n_dta_norm * handover_coef * n_handover)/60,
    ) %>%
    select(-n_dta_norm) %>%
    summarise(om2_n_exposure = sum(n_handover),
              handover_impact = sum(handover_impact),
              .by = site) %>%
    mutate(handover_impact_mean = handover_impact/om2_n_exposure) %>%
    select(grp = site, handover_impact, handover_impact_mean, om2_n_exposure)  %>%
    pivot_longer(-grp, names_to = "impact") %>%
    mutate(impact = recode(impact,
                           "handover_impact" = "om2",
                           "handover_impact_mean" = "om2_mean"
    )) %>%
    mutate(
      level = case_when(
        impact == "om2" ~ "aggregate",
        impact == "om2_mean" ~ "individual"
      ),
      label = case_when(
        str_detect(impact, "n_exposure") ~ "number impacted",
        impact == "om2" ~ "hours longer for ambulance handover",
        impact == "om2_mean" ~ "average hours waited longer for ambulance handover"
      ),
      colour = case_when(
        impact == "om2" ~ "#9532a8",
        impact == "om2_mean" ~ "#9532a8"
      )) 
  
  
  resp_impact <- dta_df %>%
    select(site, dt, n_dta_norm) %>%
    summarise(n_dta_norm = mean(n_dta_norm), .by = dt) %>% 
    # expand_grid(call_category = unique(resp_df$call_category)) %>%
    left_join(complete(resp_df, dt = snapshot_seq, call_category, fill = list(n = 0, response = 0))) %>%
    select(-dt) %>%
    left_join(unnest(enframe(resp_coef, name = "call_category", value = "coef"), cols = coef)) %>% 
    mutate(
      resp_impact = (n_dta_norm * coef * n)/60
    ) %>% 
    select(-c(n_dta_norm, coef)) %>%
    summarise(n_exposure = sum(n),
              resp_impact = sum(resp_impact),
              .by = call_category)  %>%
    mutate(resp_impact_mean = resp_impact/n_exposure) %>%
    select(grp = call_category, resp_impact, resp_impact_mean, n_exposure)  %>%
    pivot_longer(-grp, names_to = "impact") %>%
    mutate(impact = case_when(
      grp == "cat1" & str_detect(impact, "resp_impact") ~ str_replace(impact, "resp_impact", "om1a"),
      grp == "cat2" & str_detect(impact, "resp_impact") ~ str_replace(impact, "resp_impact", "om1b"),
      grp == "cat3" & str_detect(impact, "resp_impact") ~ str_replace(impact, "resp_impact", "om1c"),
      grp == "cat1" & str_detect(impact, "n_exposure") ~ str_c("om1a_", impact),
      grp == "cat2" & str_detect(impact, "n_exposure") ~ str_c("om1b_", impact),
      grp == "cat3" & str_detect(impact, "n_exposure") ~ str_c("om1c_", impact),
      .default = impact
    )
    ) %>%
    mutate(
      level = case_when(
        str_detect(impact, "om1[abc]$") ~ "aggregate",
        str_detect(impact, "om1[abc]_mean$") ~ "individual"
      ),
      label = case_when(
        str_detect(impact, "n_exposure") ~ "number impacted",
        str_detect(impact, "om1[abc]$") ~ "hours longer for ambulance response",
        str_detect(impact, "om1[abc]_mean$") ~ "average hours waited longer for ambulance response"
      ),
      colour = case_when(
        str_detect(impact, "om1[abc]$") ~ "#32A895",
        str_detect(impact, "om1[abc]_mean$") ~ "#32A895"
      )) 
  
  
  
  dta_top_stats <- dta_top_stats %>%
    rename(grp = Site_Name) %>%
    pivot_longer(-grp, names_to = "impact") %>%
    mutate(label = case_when(
      impact == "n_dta" ~ "emergency medical admissions",
      impact == "dta_time" ~ "hours waited for admission",
    ),
    colour = NA_character_)
  
  impacts_output <- bind_rows(list(
    dta_top_stats,
    downstream_impacts,
    ed_attend_impact,
    handover_impact,
    resp_impact
  )) %>%
    mutate(report_date = report_end)
  
  impacts_output
}


out <- get_impact_data(report_start = report_start,
                       report_end = report_end)

out_2 <- get_impact_data(report_start = report_start - ddays(7),
                         report_end = report_end - ddays(7))


saveRDS(bind_rows(out, out_2), "data/avoidable_impacts_output.RDS")


# out_ts <- map(seq.Date(ymd("2024-06-01"), ymd("2025-02-01"), by = "weeks"), \(d) {
#   get_impact_data(report_start = d, report_end = d + ddays(7))
#   })
# 
# 
# bind_rows(out_ts) %>%
#   filter(str_detect(impact, "om")) %>%
#   mutate(metric = str_wrap(str_c("Excess ", label, " ", str_to_lower(grp)))) %>%
#   ggplot(aes(x = report_date, y = value, col = str_to_upper(impact))) +
#   geom_path() +
#   facet_wrap(vars(metric), ncol = 3, scales = "free_y", labeller = label_wrap_gen()) +
#   theme_minimal() +
#   labs(col = "") +
#   theme(legend.position = "bottom")
