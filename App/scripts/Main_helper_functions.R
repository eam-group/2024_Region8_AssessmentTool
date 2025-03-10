### Helper functions

# Load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(collapse)
library(dataRetrieval)
library(tigris)
library(EPATADA)

### A helper function to count the data
count_fun <- function(dat, ...){
  dat2 <- dat %>%
    count(across(all_of(...))) %>%
    arrange(desc(n)) %>%
    mutate(`Percent (%)` = n/sum(n))
  
  # Replace NA with ""
  dat2[is.na(dat2)] <- ""
  
  return(dat2)
}

### A helper function to get the parameter names
get_par <- function(dat, par){
  dat2 <- dat %>%
    filter(Standard_Name %in% par)
  variable <- dat2$Parameter
  return(variable)
}

### A function to download data from the Water Quality Portal

# Create a possible version of the readWQPdata
readWQPdata_poss <- possibly(readWQPdata)

# A function to construct the argument list
args_create <- function(statecode = NULL, huc = NULL, 
                        bBox = NULL,
                        project = NULL,
                        startDateLo = NULL, startDateHi = NULL,
                        siteType = NULL,
                        siteid = NULL,
                        characteristicName = NULL,
                        characteristicType = NULL){
  # Construct the arguments for downloads
  args <- list(
    "startDateLo" = startDateLo,
    "startDateHi" = startDateHi,
    "characteristicName" = characteristicName,
    "characteristicType" = characteristicType,
    "statecode" = statecode,
    "siteType" = siteType,
    "huc" = huc,
    "bBox" = bBox,
    "siteid" = siteid,
    "project" = project 
  )
  
  # Remove NULL attribute
  args <- args[map_lgl(args, function(x) !is.null(x))]
  
  return(args)
}

TADA_args_create <- function(
    startDate = "null",
    endDate = "null",
    countrycode = "null",
    countycode = "null",
    huc = "null",
    siteid = "null",
    siteType = "null",
    characteristicName = "null",
    characteristicType = "null",
    sampleMedia = "null",
    statecode = "null",
    organization = "null",
    project = "null",
    providers = "null"
){
  args <- list(
    startDate = startDate,
    endDate = endDate,
    countrycode = countrycode,
    countycode = countycode,
    huc = huc,
    siteid = siteid,
    siteType = siteType,
    characteristicName = characteristicName,
    characteristicType = characteristicType,
    sampleMedia = sampleMedia,
    statecode = statecode,
    organization = organization,
    project = project,
    providers = providers
  )
  return(args)
}

# A function to select the targert columns
TADA_selector <- function(TADA_dat){
  
  TADA_dat2 <- TADA_dat %>%
    # fselect(ActivityTypeCode:TADA.ActivityType.Flag,
    #         TADA.ActivityMediaName:TADA.CharacteristicNameAssumptions,
    #         MethodSpeciationName:ActivityStartDateTime,
    #         ResultMeasureValue:TADA.CensoredMethod,
    #         Depth, 
    #         TADA.ResultDepthHeightMeasure.MeasureValue,
    #         TADA.ActivityDepthHeightMeasure.MeasureValue,
    #         TADA.ActivityTopDepthHeightMeasure.MeasureValue,
    #         TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
    #         StatisticalBaseCode,
    #         TADA.AggregatedContinuousData.Flag,
    #         ResultAnalyticalMethod.MethodName:TADA.MeasureQualifierCode.Def) %>%
    fmutate(ActivityStartDate = ymd(ActivityStartDate)) %>%
    # Add the state name
    left_join(dataRetrieval::stateCd %>% dplyr::select(STATE_NAME, STATE),
              by = c("StateCode" = "STATE")) %>%
    rename(StateName = STATE_NAME) %>%
    relocate(StateName, .before = StateCode)
  
  return(TADA_dat2)
}

# A function to pull site data
site_download <- function(args, ref){
  site <- readWQPdata_poss(args, service = "Station", ignore_attributes = TRUE)
  
  if (is.null(site)|nrow(site) == 0L){
    site <- ref
  }
  
  return(site)
}

# A function to download the result
result_download <- function(args, ref){
  result <- readWQPdata_poss(args, ignore_attributes = TRUE)
  
  if (is.null(result)|nrow(result) == 0L){
    result <- ref
  }
  
  return(result)
}

# A function to download the resultPhysChem
resultPhysChem_download <- function(args, ref){
  resultPhysChem <- readWQPdata_poss(args, dataProfile = "resultPhysChem",
                             ignore_attributes = TRUE)
  
  if (is.null(resultPhysChem)|nrow(resultPhysChem) == 0L){
    resultPhysChem <- ref
  }
  
  return(resultPhysChem)
}

# A function to download the Narrow data
narrow_download <- function(args, ref){
  narrow <- readWQPdata_poss(args, dataProfile = "narrowResult", ignore_attributes = TRUE)
  
  if (is.null(narrow)|nrow(narrow) == 0L){
    narrow <- ref
  }
  
  return(narrow)
}

# A function to download the project data
project_download <- function(args, ref){
  project <- readWQPdata_poss(args, service = "Project", ignore_attributes = TRUE)

  if (is.null(project)){
    project <- ref
  }

  return(project)
}

# A function to download the biological data
biological_download <- function(args, ref){
  biological <- readWQPdata_poss(args, dataProfile = "biological", ignore_attributes = TRUE)
  
  if (is.null(biological)){
    biological <- ref
  }
  
  return(biological)
}

# A function to create a TADA data frame based on site, result, and project
TADA_join3 <- function(site, resultphyschem, projects, ref){
  TADAprofile <- TADA_JoinWQPProfiles(
    FullPhysChem = resultphyschem,
    Sites = site,
    Projects = projects
  )

  if (nrow(TADAprofile) == 0){
    TADAprofile <- ref
  }
  
  TADAprofile2 <- TADAprofile %>%
    fmutate(across(.cols = NULL, .fns = as.character))
  
  return(TADAprofile2)
}

# A function to create a TADA data frame based on site, result, and project
TADA_join <- function(site, resultphyschem, ref){
  TADAprofile <- TADA_JoinWQPProfiles(
    FullPhysChem = resultphyschem,
    Sites = site
  )
 
  if (nrow(TADAprofile) == 0){
    TADAprofile2 <- ref
  } else {
    TADAprofile2 <- TADAprofile %>%
      TADA_ConvertDepthUnits() 
  }
}

# # Create a possible version of the TADA_DataRetrieval
# TADA_DataRetrieval_Fun <- function(huc, startDate, endDate, siteType, 
#                                    applyautoclean = FALSE){
#   TADA_DataRetrieval_poss <- possibly(TADA_DataRetrieval)
#   dat <- TADA_DataRetrieval_poss(huc = HUC_temp,
#                                  startDate = startDate,
#                                  endDate = endDate,
#                                  siteType = siteType ,
#                                  applyautoclean = applyautoclean)
#   return(dat)
# }

# Combine the TADA Depth column
# The order is "ResultDepthHeightMeasure", 
# "ActivityDepthHeightMeasure", "ActivityTopDepthHeightMeasure", "ActivityBottomDepthHeightMeasure"
TADA_depth_combine <- function(TADA_dat){
    TADA_dat2 <- TADA_dat %>%
      ungroup() %>%
      fmutate(Depth = coalesce(
        TADA.ResultDepthHeightMeasure.MeasureValue,
        TADA.ActivityDepthHeightMeasure.MeasureValue,
        TADA.ActivityTopDepthHeightMeasure.MeasureValue,
        TADA.ActivityBottomDepthHeightMeasure.MeasureValue
      )) %>%
      relocate(Depth, .before = "TADA.ResultDepthHeightMeasure.MeasureValue")

  return(TADA_dat2)
}

# Join Tribal Information
TADA_join_tribal <- function(TADA_dat, tribal_sf){
  
  TADA_dat2 <- TADA_dat %>%
    ungroup()
  
  # Create an sf object for all sites
  site <- TADA_dat2 %>%
    distinct(MonitoringLocationIdentifier,
             TADA.LongitudeMeasure,
             TADA.LatitudeMeasure)
  
  site_sf <- site %>%
    st_as_sf(coords = c("TADA.LongitudeMeasure",
                        "TADA.LatitudeMeasure"), crs = 4326)
  
  tribal_sf <- tribal_sf %>%
    st_transform(crs = 4326)

  site2 <- site_sf %>% 

    st_join(tribal_sf) %>%
    st_set_geometry(NULL)
  
  TADA_dat3 <- TADA_dat2 %>%
    left_join(site2, by = "MonitoringLocationIdentifier") %>%
    relocate(MT_Res_Name, .after = StateCode)
  
  return(TADA_dat3)
}

### TADA function transformation to possibly
TADA_AutoClean_poss <- possibly(TADA_AutoClean)
TADA_SimpleCensoredMethods_poss <- possibly(TADA_SimpleCensoredMethods)
TADA_FlagMethod_poss <- possibly(TADA_FlagMethod)
TADA_FlagSpeciation_poss <- possibly(TADA_FlagSpeciation)
TADA_FlagResultUnit_poss <- possibly(TADA_FlagResultUnit)
TADA_FlagFraction_poss <- possibly(TADA_FlagFraction)
TADA_FlagCoordinates_poss <- possibly(TADA_FlagCoordinates)
TADA_FindQCActivities_poss <- possibly(TADA_FindQCActivities)
TADA_FlagMeasureQualifierCode_poss <- possibly(TADA_FlagMeasureQualifierCode)
TADA_FindPotentialDuplicatesSingleOrg_poss <- possibly(TADA_FindPotentialDuplicatesSingleOrg)
TADA_FindPotentialDuplicatesMultipleOrgs_poss <- possibly(TADA_FindPotentialDuplicatesMultipleOrgs)
TADA_FindQAPPApproval_poss <- possibly(TADA_FindQAPPApproval)
TADA_FindQAPPDoc_poss <- possibly(TADA_FindQAPPDoc)
TADA_FlagContinuousData_poss <- possibly(TADA_FlagContinuousData)
TADA_FlagAboveThreshold_poss <- possibly(TADA_FlagAboveThreshold)
TADA_FlagBelowThreshold_poss <- possibly(TADA_FlagBelowThreshold)
TADA_HarmonizeSynonyms_poss <- possibly(TADA_HarmonizeSynonyms)

TADA_depth_combine_poss <- possibly(TADA_depth_combine)

TADA_join_tribal_poss <- possibly(TADA_join_tribal)

TADA_selector_poss <- possibly(TADA_selector)

## Helper functions to convert characteristic names and fractions

# A function to calculate the cumsum grouping
cumsum_group <- function(dat, col, threshold = 25000){
  num <- nrow(dat)
  x <- dat[[col]]
  y <- numeric(num)
  z <- integer(num)
  group_num <- 1L
  current_sum <- 0
  for (i in 1:num){
    if (x[i] >= threshold){
      y[i] <- x[i]
      group_num <- group_num + 1L
      z[i] <- group_num
      current_sum <- 0
      group_num <- group_num + 1L
    } else if (current_sum + x[i] >= threshold){
      current_sum <- x[i]
      group_num <- group_num + 1L
      y[i] <- current_sum
      z[i] <- group_num
    } else {
      current_sum <- current_sum + x[i]
      y[i] <- current_sum
      z[i] <- group_num
    }
  }
  dat2 <- dat %>% fmutate(Cumsum = y, CumGroup = z)
  return(dat2)
}

### Functions to calculate the criteria

dateTime_create <- function(x){
  x2 <- x %>%
    fmutate(DateTime = case_when(
      is.na(ActivityStartTime.Time)  ~
        ymd_hms(paste0(as.character(ActivityStartDate),
                       " ",
                       "00:00:00")),
      TRUE                           ~
        ymd_hms(paste0(as.character(ActivityStartDate),
                                      " ",
                                      as.character(ActivityStartTime.Time)))
      ))
}

column_name_change <- function(x){
  x2 <- x %>%
    rename(Standard_Name = TADA.CharacteristicName, 
           Standard_Fraction = TADA.ResultSampleFractionText,
           Standard_Unit = TADA.ResultMeasure.MeasureUnitCode) %>%
    rename(Original.LongitudeMeasure = LongitudeMeasure, 
           Original.LatitudeMeasure = LatitudeMeasure) %>%
    rename(LongitudeMeasure = TADA.LongitudeMeasure,
           LatitudeMeasure = TADA.LatitudeMeasure)
  return(x2)
}

column_name_change2 <- function(x){
  x2 <- x %>%
    rename(TADA.CharacteristicName = Standard_Name, 
           Updated_Fraction = Standard_Fraction,
           TADA.ResultSampleFractionText = Standard_Fraction_ori,
           TADA.ResultMeasure.MeasureUnitCode = Standard_Unit) %>%
    relocate(TADA.ResultSampleFractionText, .before = Updated_Fraction) %>%
    rename(TADA.LongitudeMeasure = LongitudeMeasure ,
           TADA.LatitudeMeasure = LatitudeMeasure) %>%
    rename(LongitudeMeasure = Original.LongitudeMeasure, 
           LatitudeMeasure = Original.LatitudeMeasure ) #%>%
    # dplyr::select(-StateAbbrev, -Fraction_Sus, -Fraction_Criteria,
    #               -Details_Criteria, -Magnitude_Lower, -Magnitude_Upper,
    #               -Frequency, -Duration, -pH_Notes, -Hardness_Notes,
    #               -Hardness_Threshold, -Temp_Notes, -Notes)
  return(x2)
}

column_name_change3 <- function(x){
  x2 <- x %>%
    rename(Standard_Name = TADA.CharacteristicName, 
           Standard_Fraction = Updated_Fraction,
           Standard_Unit = TADA.ResultMeasure.MeasureUnitCode) %>%
    rename(Original.LongitudeMeasure = LongitudeMeasure, 
           Original.LatitudeMeasure = LatitudeMeasure) %>%
    rename(LongitudeMeasure = TADA.LongitudeMeasure,
           LatitudeMeasure = TADA.LatitudeMeasure)
  return(x2)
}

column_name_change4 <- function(x){
  x2 <- x %>%
    rename(TADA.CharacteristicName = Standard_Name, 
           Updated_Fraction = Standard_Fraction,
           TADA.ResultMeasure.MeasureUnitCode = Standard_Unit) %>%
    rename(TADA.LongitudeMeasure = LongitudeMeasure,
           TADA.LatitudeMeasure = LatitudeMeasure) %>%
    rename(LongitudeMeasure = Original.LongitudeMeasure, 
           LatitudeMeasure = Original.LatitudeMeasure)
  return(x2)
}

# Helper functions to get the pH data
pH_filter <- function(x){
  x2 <- x %>%
    fsubset(Standard_Name %in% "PH") %>%
    fselect(DateTime,
            MonitoringLocationIdentifier, MonitoringLocationTypeName,
            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
            StateName, pH = TADA.ResultMeasureValue) %>%
    # Calculate average if multiple samples exist
    group_by(across(-pH)) %>%
    summarize(pH = mean(pH, na.rm = TRUE)) %>%
    ungroup() %>%
    # Create the upper and lower bound
    mutate(DateTime_upper = DateTime + days(1),
           DateTime_lower = DateTime - days(1))
  
  return(x2)
}

pH_join <- function(x, y){
  
  by <- join_by(MonitoringLocationIdentifier, MonitoringLocationTypeName,
                HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
                StateName, closest(DateTime >= DateTime_lower), 
                closest(DateTime <= DateTime_upper))
  
  x2 <- x %>%
    left_join(y, by = by) %>%
    frename(DateTime = DateTime.x, DateTime_pH = DateTime.y) %>%
    fselect(-DateTime_lower, -DateTime_upper)
    
  return(x2)
}

pH_fun <- function(x){
  pH_dat <- x %>% 
    pH_filter()
  x2 <- x %>%
    pH_join(pH_dat)
  return(x2)
}

# Helper functions to get the temperature data
temp_filter <- function(x){
  x2 <- x %>%
    fsubset(Standard_Name %in% "TEMPERATURE, WATER") %>%
    fselect(DateTime,
            MonitoringLocationIdentifier, MonitoringLocationTypeName,
            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
            StateName, Temperature = TADA.ResultMeasureValue) %>%
    # Calculate average if multiple samples exist
    group_by(across(-Temperature)) %>%
    summarize(Temperature = mean(Temperature, na.rm = TRUE)) %>%
    ungroup() %>%
    # Create the upper and lower bound
    mutate(DateTime_upper = DateTime + days(1),
           DateTime_lower = DateTime - days(1))
  
  return(x2)
}

temp_join <- function(x, y){
  
  by <- join_by(MonitoringLocationIdentifier, MonitoringLocationTypeName,
                HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
                StateName, closest(DateTime >= DateTime_lower), 
                closest(DateTime <= DateTime_upper))
  
  x2 <- x %>%
    left_join(y, by = by) %>%
    frename(DateTime = DateTime.x, DateTime_Temperature = DateTime.y) %>%
    fselect(-DateTime_lower, -DateTime_upper)
    
  return(x2)
}

temp_fun <- function(x){
  temp_dat <- x %>% 
    temp_filter()
  x2 <- x %>%
    temp_join(temp_dat)
  return(x2)
}

# Helper functions to get the hardness data
hardness_filter <- function(x){
  x2 <- x %>%
    fsubset(Standard_Name %in% "HARDNESS, CA, MG") %>%
    fselect(ActivityStartDate, `ActivityStartTime.Time`,
            MonitoringLocationIdentifier, MonitoringLocationTypeName,
            HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure,
            StateName, Hardness = TADA.ResultMeasureValue) %>%
    # Calculate average if multiple samples exist
    group_by(across(-Hardness)) %>%
    summarize(Hardness = mean(Hardness, na.rm = TRUE)) %>%
    ungroup() 
  return(x2)
}

hardness_join <- function(x, y){
  x2 <- x %>%
    left_join(y, by = c(
      "ActivityStartDate", "ActivityStartTime.Time",
      "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
      "HUCEightDigitCode", "LatitudeMeasure", "LongitudeMeasure",
      "StateName"
    ))
  return(x2)
}

hardness_fun <- function(x){
  hard_dat <- x %>% 
    hardness_filter()
  x2 <- x %>%
    hardness_join(hard_dat) %>%
    # Limit the hardness
    fmutate(Hardness = ifelse(Hardness > 400, 400, Hardness))
  return(x2)
}

# A function to calculate the hardness criteria
hardness_criteria_fun <- function(hardness, E_A, E_B, CF_A, CF_B, CF_C){
  if (is.na(CF_A) & is.na(CF_B)){
    CF2 <- CF_C
  } else if (!is.na(CF_A) & !is.na(CF_B)){
    CF <- CF_A - (log(hardness) * CF_B)
    
    CF2 <- CF
    CF2[CF2 > 1] <- 1
  }
  result <- exp(E_A * log(hardness) + E_B) * CF2
  
  return(result)
}

# A function to create flags for pH and temperature join
temperature_pH_flag <- function(x){
  x2 <- x %>%
    fmutate(pH_join_flag = as.integer(DateTime == DateTime_pH)) %>%
    fmutate(Temperature_join_flag = as.integer(DateTime == DateTime_Temperature))
  return(x2)
}

# A function to join the criteria
criteria_join <- function(x, y){
  x2 <- x %>%
    left_join(y, by = c("Standard_Name" = "Constituent",
                        # "Use" = "Use",
                        # "Standard_Fraction" = "Fraction",
                        "Standard_Unit" = "MagUnits",
                        "StateName" = "State"),
              relationship = "many-to-many")
  return(x2)
}

# A function to join the sufficiency
sufficiency_join <- function(x, y){
  
  y2 <- y %>% mutate(Sufficiency_Q = "Yes")
  
  x2 <- x %>%
    left_join(y2, by = c("Standard_Name" = "TADA.Constituent",
                        # "Standard_Fraction" = "Fraction",
                        "Standard_Unit" = "MagUnits",
                        "StateName" = "State"),
              relationship = "many-to-many") %>%
    fmutate(Sufficiency_Q = ifelse(is.na(Sufficiency_Q), "No", Sufficiency_Q))
  return(x2)
}

# A function to calculate the crtieria based on pH and temperature
CMC_criteria_fun <- function(pH, salmonid = TRUE){
  if (salmonid){
    a <- 0.275
    b <- 39
  } else {
    a <- 0.411
    b <- 58.4
  }
  result <- a/(1 + 10^(7.204 - pH)) + b/(1 + 10^(pH - 7.204))
  return(result)
}
  
CCC_criteria_fun <- function(pH, temperature, earlylife = TRUE){
  result_temp <- 0.0577/(1 + 10^(7.688 - pH)) + 2.487/(1 + 10^(pH - 7.688)) 
  
  if (earlylife){
    result <- result_temp * min(c(2.85, 1.45 * 10^(0.028 * (25 - temperature))))
  } else {
    result <- result_temp * 1.45 * 10^(0.028 * (25 - max(c(temperature, 7))))
  }
  
  return(result)
}

# A function to get the summarized year for each group
summarized_year <- function(dat){
  dat2 <- dat %>%
    group_by(HUCEightDigitCode, CumGroup) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(Type = rep(c("Start", "End"), times = n()/2)) %>%
    fselect(HUCEightDigitCode, YearSummarized, CumGroup, Type) %>%
    pivot_wider(names_from = "Type", values_from = "YearSummarized") %>%
    fmutate(Start = ymd(paste0(Start, "-01", "-01")),
            End = ymd(paste0(End, "12", "-31")))
  return(dat2)
}

# A function to return NA if all values are NA, otherwise
# DO sum(x, na.rm = TRUE)
modSum <- function(x){
  if(all(is.na(x))){
    y <- NA
  } else {
    y <- sum(x, na.rm = TRUE)
  }
  return(y)
}

# A function to calculate exceedance
exceedance_fun <- function(x){
  x2 <- x %>%
    mutate(Exceedance = case_when(
      is.na(Criteria_Lower) & !is.na(Criteria_Upper) & 
        TADA.ResultMeasureValue > Criteria_Upper    ~   TRUE,
      !is.na(Criteria_Lower) & is.na(Criteria_Upper) & 
        TADA.ResultMeasureValue < Criteria_Lower    ~   TRUE,
      !is.na(Criteria_Lower) & !is.na(Criteria_Upper) & 
        (TADA.ResultMeasureValue < Criteria_Lower | 
           TADA.ResultMeasureValue > Criteria_Upper)   ~   TRUE,
      TRUE                                             ~  FALSE
    ))
  return(x2)
}

# A function to calculate the exceedance percentage data with criteria
exceedance_cal <- function(x, site_flag){
  
  if(site_flag %in% "Site"){
    x2 <- x %>%
      group_by(MonitoringLocationIdentifier, MonitoringLocationName, 
               LongitudeMeasure, LatitudeMeasure,
               Standard_Name, Standard_Fraction, Standard_Unit, Use, Details, Type,
               Frequency, Duration) %>%
      summarize(Size = n(), 
                Start_Date = min(ActivityStartDate),
                End_Date = max(ActivityStartDate),
                Minimum = min(TADA.ResultMeasureValue),
                Median = median(TADA.ResultMeasureValue),
                Maximum = max(TADA.ResultMeasureValue),
                Num_Years_Exceedance = n_distinct(year(ActivityStartDate[Exceedance])),
                Exceedance_Size = modSum(Exceedance)) %>%
      ungroup() %>%
      fmutate(Percentage = Exceedance_Size/Size)
  } else if (site_flag %in% "AU"){
    x2 <- x %>%
      group_by(AU_ID, AU_NAME, 
               # LongitudeMeasure, LatitudeMeasure,
               Standard_Name, Standard_Fraction, Standard_Unit, Use, Details, Type,
               Frequency, Duration) %>%
      summarize(Size = n(), 
                Start_Date = min(ActivityStartDate),
                End_Date = max(ActivityStartDate),
                Minimum = min(TADA.ResultMeasureValue),
                Median = median(TADA.ResultMeasureValue),
                Maximum = max(TADA.ResultMeasureValue),
                Num_Years_Exceedance = n_distinct(year(ActivityStartDate)[Exceedance]),
                Exceedance_Size = modSum(Exceedance)) %>%
      ungroup() %>%
      fmutate(Percentage = Exceedance_Size/Size)
  }
  
  return(x2)
}

# A function to summarize the criteria
criteria_summary <- function(x, site_flag){
  if (site_flag %in% "Site"){
    x2 <- x %>%
      group_by(Standard_Name, Standard_Fraction, Standard_Unit, Details, Use, Type,
               Frequency, Duration) %>%
      summarize(Site_Num = n_distinct(MonitoringLocationIdentifier),
                Site_Exceedance = sum(Percentage > 0, na.rm = TRUE)) %>%
      ungroup() %>%
      fmutate(Percentage = Site_Exceedance/Site_Num) 
  } else if (site_flag %in% "AU"){
    x2 <- x %>%
      group_by(Standard_Name, Standard_Fraction, Standard_Unit, Details, Use, Type,
               Frequency, Duration) %>%
      summarize(Site_Num = n_distinct(AU_ID),
                Site_Exceedance = sum(Percentage > 0, na.rm = TRUE)) %>%
      ungroup() %>%
      fmutate(Percentage = Site_Exceedance/Site_Num) 
  }
  return(x2)
}

# A function to summarize the data without criteria
criteria_no_summary <- function(x, site_flag){
  if (site_flag %in% "Site"){
    x2 <- x %>%
      group_by(Standard_Name, Standard_Fraction, Standard_Unit, Details, Use, Type,
               Frequency, Duration) %>%
      summarize(Site_Num = n_distinct(MonitoringLocationIdentifier))
  } else if (site_flag %in% "AU"){
    x2 <- x %>%
      group_by(Standard_Name, Standard_Fraction, Standard_Unit, Details, Use, Type,
               Frequency, Duration) %>%
      summarize(Site_Num = n_distinct(AU_ID))
  }
  return(x2)
}

# # Function to prepare the final AN output
# AN_join_final <- function(x, ex){
#   x2 <- x %>%
#     left_join(ex)
#   return(x2)
# }

# Function to prepare the site data for AU join
TADA_site_simplify <- function(x){
  x2 <- x %>%
    distinct(MonitoringLocationIdentifier, StateName,
             MonitoringLocationName, MonitoringLocationTypeName,
             TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>%
    rename(State = StateName, LatitudeMeasure = TADA.LatitudeMeasure,
           LongitudeMeasure = TADA.LongitudeMeasure)
  return(x2)
}

# Function to filter the QA dataset
QA_filter <- function(x){
  x2 <- x %>%
    fsubset(TADA.ActivityType.Flag %in% "Non_QC") %>%
    fsubset(!TADA.MethodSpeciation.Flag %in% c("Rejected")) %>%
    fsubset(!TADA.ResultMeasureValueDataTypes.Flag %in% c("Text", "NA - Not Available")) %>%
    fsubset(!is.na(TADA.ResultMeasureValue)) %>%
    fsubset(!TADA.ResultValueAboveUpperThreshold.Flag %in% "Suspect") %>%
    fsubset(!TADA.ResultValueBelowLowerThreshold.Flag %in% "Suspect") %>%
    fsubset(!TADA.ResultUnit.Flag %in% "Rejected") %>%
    fsubset(!TADA.AnalyticalMethod.Flag %in% "Invalid") %>%
    fsubset(!TADA.MeasureQualifierCode.Flag %in% "Suspect")
  return(x2)
}

### Functions for data sufficiency test

# Group site
su_group <- function(x, ...){
  x2 <- x %>%
    group_by(...) %>%
    summarize(Year_n = n_distinct(Year),
              Sample_n = n()) %>%
    ungroup()
  return(x2)
}

### Assess the overwhelming evidence

over_count <- function(dat, ...){
  
  dat2 <- dat %>%
    mutate(Exceedance = TADA.ResultMeasureValue > 2 * Magnitude_Upper) %>%
    group_by(...) %>%
    summarize(Exceedance_Sum = sum(Exceedance, na.rm = TRUE)) %>%
    ungroup()
  
  return(dat2)
}

### Assess hardness, pH, and  Temperature
su_hardness_pH_temp_count <- function(dat, vars, parname, ...){
  
  newnames <- paste0(parname, c("_Year_n", "_Sample_n"))
  newnames2 <- c("Year_n", "Sample_n")
  names(newnames2) <- newnames
  
  dat2 <- dat %>%
    group_by(...) %>%
    drop_na(any_of(vars)) %>%
    summarize(Year_n = n_distinct(Year),
              Sample_n = n()) %>%
    ungroup() %>%
    frename(newnames2)
    
  return(dat2)
}

### Join SU dataset
Join_su <- function(dat, over, hardness, pH, temp, colname){
  dat2 <- dat %>%
    left_join(hardness,
              by = colname) %>%
    left_join(pH,
              by = colname) %>%
    left_join(temp,
              by = colname) %>%
    left_join(over,
              by = colname)
  return(dat2)
}


### Assess SU Result
Assess_su <- function(dat){
  dat2 <- dat %>%
    fmutate(Year_Flag = case_when(
        is.na(Minimum_Assessment_Period_Years)                                   ~ "Not Required",
        Year_n >= Minimum_Assessment_Period_Years                                ~ "Yes",
        TRUE                                                                     ~ "No"
    )) %>%
    fmutate(Sample_Flag = case_when(
      is.na(Minimum_Data_Points)                                                 ~ "Not Required",
      Sample_n >= Minimum_Data_Points                                            ~ "Yes",
      TRUE                                                                       ~ "No"
    )) %>%
    fmutate(Hardness_Year_Flag = case_when(
      !Hardness_Notes %in% 1                                                     ~ "Not Required",
      Hardness_Notes %in% 1 & Hardness_Year_n >= Minimum_Assessment_Period_Years ~ "Yes",
      Hardness_Notes %in% 1 & Hardness_Year_n < Minimum_Assessment_Period_Years  ~ "No",
      TRUE                                                                       ~ NA_character_
    )) %>%
    fmutate(Hardness_Sample_Flag = case_when(
      !Hardness_Notes %in% 1                                                     ~ "Not Required",
      Hardness_Notes %in% 1 & Hardness_Sample_n >= Minimum_Data_Points           ~ "Yes",
      Hardness_Notes %in% 1 & Hardness_Sample_n < Minimum_Data_Points            ~ "No",
      TRUE                                                                       ~ NA_character_
    )) %>%
    fmutate(pH_Year_Flag = case_when(
      !pH_Notes %in% c(1, 2, 4)                                                  ~ "Not Required",
      pH_Notes %in% c(1, 2, 4) & pH_Year_n >= Minimum_Assessment_Period_Years    ~ "Yes",
      pH_Notes %in% c(1, 2, 4) & pH_Year_n < Minimum_Assessment_Period_Years     ~ "No",
      TRUE                                                                       ~ NA_character_
    )) %>%
    fmutate(pH_Sample_Flag = case_when(
      !pH_Notes %in% c(1, 2, 4)                                                  ~ "Not Required",
      pH_Notes %in% c(1, 2, 4) & pH_Sample_n >= Minimum_Data_Points              ~ "Yes",
      pH_Notes %in% c(1, 2, 4) & pH_Sample_n < Minimum_Data_Points               ~ "No",
      TRUE                                                                       ~ NA_character_
    ))  %>%
    fmutate(Temp_Year_Flag = case_when(
      !Temp_Notes %in% 1                                                         ~ "Not Required",
      Temp_Notes %in% 1 & Temp_Year_n >= Minimum_Assessment_Period_Years         ~ "Yes",
      Temp_Notes %in% 1 & Temp_Year_n < Minimum_Assessment_Period_Years          ~ "No",
      TRUE                                                                       ~ NA_character_
    )) %>%
    fmutate(Temp_Sample_Flag = case_when(
      !Temp_Notes %in% 1                                                         ~ "Not Required",
      Temp_Notes %in% 1 & Temp_Sample_n >= Minimum_Data_Points                   ~ "Yes",
      Temp_Notes %in% 1 & Temp_Sample_n < Minimum_Data_Points                    ~ "No",
      TRUE                                                                       ~ NA_character_
    )) %>%
    fmutate(Overwhelming_Evidence = ifelse(is.na( Overwhelming_Evidence), 0,  Overwhelming_Evidence)) %>%
    fmutate(Overwhelming_Flag = ifelse(is.na(Overwhelming_Flag), "Not Required",  Overwhelming_Flag)) %>%
    # Final flag
    fmutate(Sufficiency = case_when(
      Sufficiency_Q %in% "No"                                                    ~ "Not Required", 
      Sufficiency_Q %in% "Yes" & Overwhelming_Flag %in% "Yes"                    ~ "Sufficient",
     (Overwhelming_Flag %in% "No" & Sufficiency_Q %in% "Yes") & 
      Year_Flag %in% "No" | Sample_Flag %in% "No" |
        Hardness_Year_Flag %in% "No" | Hardness_Sample_Flag %in% "No" |
        pH_Year_Flag %in% "No" | pH_Sample_Flag %in% "No" |
        Temp_Year_Flag %in% "No" | Temp_Sample_Flag %in% "No"                    ~ "Insufficient",
      TRUE                                                                       ~ "Sufficient"
    ))
  return(dat2)
}

### Function to summarize the SU results
Summarize_su <- function(dat){
  dat2 <- dat %>%
    rename(Details = Details_Criteria) %>%
    group_by(
      StateName, Standard_Name, Standard_Fraction, Standard_Unit,
      Use, Details, Sufficiency_Q
    ) %>%
    summarize(
      Site_no = n(),
      Sufficiency = sum(Sufficiency %in% c("Sufficient", "Not Required"))/n()
    )%>%
    ungroup() %>%
    fmutate(Sufficiency = ifelse(Sufficiency_Q %in% "No", NA, Sufficiency))
  return(dat2)
}

### Function to transform the SU results
Transform_su <- function(dat, selected_cols){
  dat2 <- dat %>%
    rename(Details = Details_Criteria) %>%
    dplyr::select(all_of(selected_cols)) %>%
    fmutate(Col_SU = str_c(Use, Details, sep = "-")) %>%
    fmutate(Col_SU = ifelse(is.na(Col_SU), Sufficiency, str_c(Col_SU, Sufficiency, sep = ": ")))%>%
    dplyr::select(-Use, -Details, -Sufficiency) %>%
    group_by(across(StateName:Standard_Unit)) %>%
    summarize(Sufficiency_Notes = paste0(unique(Col_SU), collapse = "; ")) %>%
    ungroup()
  return(dat2)
}

# A function to validate the data
validate_fun <- function(dat, check_cols){
  if (identical(names(dat), check_cols)){
    if (nrow(dat) > 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

# A function to check if AU inoformation exist
AU_check <- function(dat){
  cols <- names(dat)
  if ("AU_ID" %in% cols & "AU_NAME" %in% cols){
    result <- TRUE
  } else {
    result <- FALSE
  }
  return(result)
}

# Transform_su_back <- function(dat){
#   dat2 <- dat %>%
#     separate_longer_delim(c(Site_Sufficiency_Notes, 
#                             AU_Sufficiency_Notes),
#                           delim = "; ") %>%
#     separate_wider_delim(Site_Sufficiency_Notes,
#                          delim = ": ", 
#                          names = c("Use-Details_Site", "Site_Sufficiency"),
#                          too_few = "align_end",
#                          too_many = "merge") %>%
#     separate_wider_delim(AU_Sufficiency_Notes,
#                          delim = ": ", 
#                          names = c("Use-Details_AU", "AU_Sufficiency"),
#                          too_few = "align_start",
#                          too_many = "merge") %>%
#     dplyr::select(-Use-Details_AU) %>%
#     separate_wider_delim(`Use-Details_Site`,
#                          delim = "-",
#                          names = c("Use", "Details"),
#                          too_few = "align_start",
#                          too_many = "merge")
#   return(dat2)
# }

