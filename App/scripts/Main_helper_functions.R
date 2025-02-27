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

# # A function to construct the argument list
# args_create <- function(statecode = NULL, huc = NULL, 
#                         bBox = NULL,
#                         project = NULL,
#                         startDateLo = NULL, startDateHi = NULL,
#                         siteType = NULL,
#                         siteid = NULL,
#                         characteristicName = NULL,
#                         characteristicType = NULL){
#   # Construct the arguments for downloads
#   args <- list(
#     "startDateLo" = startDateLo,
#     "startDateHi" = startDateHi,
#     "characteristicName" = characteristicName,
#     "characteristicType" = characteristicType,
#     "statecode" = statecode,
#     "siteType" = siteType,
#     "huc" = huc,
#     "bBox" = bBox,
#     "siteid" = siteid,
#     "project" = project 
#   )
#   
#   # Remove NULL attribute
#   args <- args[map_lgl(args, function(x) !is.null(x))]
#   
#   return(args)
# }

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
TADA_FlagContinuousData_poss <- possibly(TADA_FlagContinuousData)
TADA_FindQAPPDoc_poss <- possibly(TADA_FindQAPPDoc)
TADA_FlagAboveThreshold_poss <- possibly(TADA_FlagAboveThreshold)
TADA_FlagBelowThreshold_poss <- possibly(TADA_FlagBelowThreshold)
TADA_HarmonizeSynonyms_poss <- possibly(TADA_HarmonizeSynonyms)
TADA_FlagDepthCategory_poss <- possibly(TADA_FlagDepthCategory)
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