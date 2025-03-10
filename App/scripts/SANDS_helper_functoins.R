### This script summarized the needed function in SANDS format for the app

# Load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(collapse)
library(dataRetrieval)
library(EPATADA)
library(tigris)
library(tidycensus)

# A function to convert the upper case parameter names to the original form
original_name <- function(x, WQX){
  x2 <- x %>%
    left_join(WQX, by = c("TADA.CharacteristicName" = "Name_Upper"))
  return(x2)
}

# A function to convert TADA data frame to sample site data
TADA_site <- function(x, ref_col, AU = FALSE){
  
  if (AU){
    ref_col <- c(ref_col, "MT_Res_Name", "AU_ID", "AU_NAME")
  } else {
    ref_col <- c(ref_col, "MT_Res_Name")
  }
  
  x2 <- x %>%
    fselect(-LatitudeMeasure, -LongitudeMeasure) %>%
    frename(LatitudeMeasure = TADA.LatitudeMeasure,
            LongitudeMeasure = TADA.LongitudeMeasure) %>%
    get_vars(ref_col) %>%
    distinct() %>%
    fmutate(StateCode = str_pad(as.character(StateCode), width = 2, pad = "0"))
  return(x2)
}


# A function to format the site to be consistent with the SANDS format
SANDS_clean_site <- function(x, ref, state_county, AU = FALSE){
  
  if (nrow(x) > 0){
    
    ref_col <- names(ref)
    
    if (AU){
      ref_col <- c(ref_col, "ns2:AU_ID", "ns2:AU_Name")
    } 
    
    x2 <- x %>%
      fselect(-ContributingDrainageAreaMeasure.MeasureValue,
              -ContributingDrainageAreaMeasure.MeasureUnitCode,
              -LocalAqfrName, -ProviderName) %>%
      frename(`DrainageAreaMeasure.MeasureValue` = MeasureValue,
              `HorizontalAccuracyMeasure.MeasureValue` = MeasureValue2,
              `VerticalMeasure.MeasureValue` = MeasureValue4,
              `VerticalAccuracyMeasure.MeasureValue` = MeasureValue6,
              `WellDepthMeasure.MeasureValue` = MeasureValue8,
              `WellHoleDepthMeasure.MeasureValue` = MeasureValue10,
              `DrainageAreaMeasure.MeasureUnitCode` = MeasureUnitCode,
              `HorizontalAccuracyMeasure.MeasureUnitCode` = MeasureUnitCode3,
              `VerticalMeasure.MeasureUnitCode` = MeasureUnitCode5,
              `VerticalAccuracyMeasure.MeasureUnitCode` = MeasureUnitCode7,
              `WellDepthMeasure.MeasureUnitCode` = MeasureUnitCode9,
              `WellHoleDepthMeasure.MeasureUnitCode` = MeasureUnitCode11) %>%
      # Join state and county name
      left_join(state_county, by = c("StateCode" = "state_code",
                                     "CountyCode" = "county_code")) %>%
      fselect(-state) %>%
      frename(state_name = "StateName", county = "CountyName") %>%
      frename(\(x) paste0("ns2:", x)) %>%
      # Reorder the columns
      get_vars(ref_col)
  } else {
    x2 <- ref
  }
  
  return(x2)
} 

### RESULTS

# A function to convert TADA data frame to result data
TADA_result <- function(x, ref_col, update = TRUE){
  
  ref_col <- ref_col[!ref_col %in% c("PrecisionValue", "MethodDescriptionText", "PreparationStartDate")]
  
  if (update){
    x2 <- x %>%
      fselect(-LatitudeMeasure, -LongitudeMeasure,
              -CharacteristicName, -ResultSampleFractionText,
              -ResultMeasureValue, -ResultMeasure.MeasureUnitCode,
              -DetectionQuantitationLimitMeasure.MeasureValue,
              -DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
      frename(LatitudeMeasure = TADA.LatitudeMeasure,
              LongitudeMeasure = TADA.LongitudeMeasure,
              CharacteristicName = Name,
              ResultSampleFractionText = TADA.ResultSampleFractionText,
              ResultMeasureValue = TADA.ResultMeasureValue,
              ResultMeasure.MeasureUnitCode = TADA.ResultMeasure.MeasureUnitCode,
              DetectionQuantitationLimitMeasure.MeasureValue = 
                TADA.DetectionQuantitationLimitMeasure.MeasureValue,
              DetectionQuantitationLimitMeasure.MeasureUnitCode = 
                TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  } 
  x3 <- x2 %>% get_vars(ref_col)
  return(x3)
}

# A function to format the result data to be consistent as RESULTS
SANDS_clean_result <- function(x, ref){
  
  ref_col <- names(ref)
  ref_col <- ref_col[!ref_col %in% c("PrecisionValue", "MethodDescriptionText", "PreparationStartDate")]
  
  if (nrow(x) > 0){
    x2 <- x %>%
      frename(function(x) str_replace(x, fixed("."), fixed("/"))) %>%
      # Reorder the columns
      get_vars(ref_col)
  } else {
    x2 <- ref
  }
  
  return(x2)
}

### RESULTS Phys Chem

# A function to select result phys chem data
TADA_resultPhysChem <- function(x, ref_col, update = TRUE){
  if (update){
    x2 <- x %>%
      frename(ResultMeasureValue_Ori = 
                ResultMeasureValue,
              DetectionQuantitationLimitMeasure.MeasureValue_Ori = 
                DetectionQuantitationLimitMeasure.MeasureValue) %>%
      fselect(-LatitudeMeasure, -LongitudeMeasure,
              -CharacteristicName, -ResultSampleFractionText,
              -ResultMeasure.MeasureUnitCode,
              -DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
      frename(LatitudeMeasure = TADA.LatitudeMeasure,
              LongitudeMeasure = TADA.LongitudeMeasure,
              CharacteristicName = Name,
              ResultSampleFractionText = TADA.ResultSampleFractionText,
              ResultMeasureValue = TADA.ResultMeasureValue,
              ResultMeasure.MeasureUnitCode = TADA.ResultMeasure.MeasureUnitCode,
              DetectionQuantitationLimitMeasure.MeasureValue = 
                TADA.DetectionQuantitationLimitMeasure.MeasureValue,
              DetectionQuantitationLimitMeasure.MeasureUnitCode = 
                TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  } 
  x3 <- x2 %>% get_vars(ref_col)
  return(x3)
}

### SUMMARY

# A function to create the SUMMARY table
SANDS_create_summary_part1 <- function(sites, results, resultphyschem, ref, state_county){
  
  if (nrow(sites) > 0 & nrow(results) > 0 & nrow(resultphyschem) > 0){
    # Select the columns in sites
    sites2 <- sites %>%
      semi_join(results, 
                by = c("OrganizationIdentifier", "MonitoringLocationIdentifier")) %>%
      # Join state and county name
      left_join(state_county, by = c("StateCode" = "state_code",
                                     "CountyCode" = "county_code")) %>%
      fselect(-state) %>%
      frename(state_name = "StateName", county = "CountyName") %>%
      fselect(
        "ORG ID" = "OrganizationIdentifier",
        "SITE ID" = "MonitoringLocationIdentifier",
        "SITE NAME" = "MonitoringLocationName",
        "STATE NAME" = "StateName",
        "COUNTY NAME" = "CountyName",
        "LATITUDE" = "LatitudeMeasure",
        "LONGITUDE" = "LongitudeMeasure",
        "COORDINATE REF" = "HorizontalCoordinateReferenceSystemDatumName",
        "SITE TYPE" = "MonitoringLocationTypeName",
        "MT RES NAME" = "MT_Res_Name"
      )
    # Select the columns in resultphyschem
    resultphyschem2 <- resultphyschem %>%
      # Create a within group ID for merging
      # Some rows differs only in ResultIdentifier and AnalysisStartDateTime
      # But ResultIdentifier and AnalysisStartDateTime is not in result
      ftransform(
        ID = rowid(
          OrganizationIdentifier,
          MonitoringLocationIdentifier,
          ActivityStartDate,
          ActivityIdentifier,
          `ActivityStartTime.Time`,
          CharacteristicName,
          ResultMeasureValue,
          ResultMeasure.MeasureUnitCode,
          ResultStatusIdentifier,
          ResultValueTypeName,
          ResultSampleFractionText,
          ResultCommentText,
          SubjectTaxonomicName,
          AnalysisStartDate,
          ResultDetectionConditionText
        )
      ) %>%
      fselect(
        "ORG ID" = "OrganizationIdentifier",
        "SITE ID" = "MonitoringLocationIdentifier",
        "ACTIVITY DATE" = "ActivityStartDate",
        "ACTIVITY ID" = "ActivityIdentifier",
        "ACTIVITY START TIME" = "ActivityStartTime.Time",
        "LAB ANALYSIS DATE" = "AnalysisStartDate",
        "CHARACTERISTIC NAME" = "CharacteristicName",
        "RESULT VALUE" = "ResultMeasureValue",
        "RESULT UNIT" = "ResultMeasure.MeasureUnitCode",
        "RESULT STATUS" = "ResultStatusIdentifier",
        "RESULT VALUE TYPE" = "ResultValueTypeName",
        "SAMPLE FRACTION" = "ResultSampleFractionText",
        "ResultCommentText",
        "ResultDetectionConditionText",
        "MethodSpeciationName",
        "SubjectTaxonomicName",
        "ID",
        "ResultMeasureValue_Ori",
        "DetectionQuantitationLimitMeasure.MeasureValue_Ori"
      )
    # Select the columns in results
    results2 <- results %>%
      ftransform(
        ID = rowid(
          OrganizationIdentifier,
          MonitoringLocationIdentifier,
          ActivityStartDate,
          ActivityIdentifier,
          `ActivityStartTime.Time`,
          CharacteristicName,
          ResultMeasureValue,
          ResultMeasure.MeasureUnitCode,
          ResultStatusIdentifier,
          ResultValueTypeName,
          ResultSampleFractionText,
          ResultCommentText,
          SubjectTaxonomicName,
          AnalysisStartDate,
          ResultDetectionConditionText
        )
      ) %>%
      fselect(
        "ORG ID" = "OrganizationIdentifier",
        "SITE ID" = "MonitoringLocationIdentifier",
        "ACTIVITY DATE" = "ActivityStartDate",
        "ACTIVITY ID" = "ActivityIdentifier",
        "ACTIVITY TYPE" = "ActivityTypeCode",
        "ACTIVITY MEDIA" = "ActivityMediaName",
        "ACTIVITY SUBMEDIA" = "ActivityMediaSubdivisionName",
        "CHARACTERISTIC NAME" = "CharacteristicName",
        "RESULT VALUE" = "ResultMeasureValue",
        "RESULT UNIT" = "ResultMeasure.MeasureUnitCode",
        "QUALIFIER CODE" = "MeasureQualifierCode",
        "RESULT STATUS" = "ResultStatusIdentifier",
        "RESULT VALUE TYPE" = "ResultValueTypeName",
        "SAMPLE FRACTION" = "ResultSampleFractionText",
        "LAB ANALYSIS DATE" = "AnalysisStartDate",
        "SAMPLE COLLECTION METHOD" = "SampleCollectionMethod.MethodName",
        "DETECTION LIMIT TYPE" = "DetectionQuantitationLimitTypeName",
        "DETECTION LIMIT VALUE" = "DetectionQuantitationLimitMeasure.MeasureValue",
        "DETECTION LIMIT UNIT" = "DetectionQuantitationLimitMeasure.MeasureUnitCode",
        "WQX SPECIATION" = "ResultWeightBasisText",
        "ACTIVITY START TIME" = "ActivityStartTime.Time",
        "SubjectTaxonomicName",
        "ResultAnalyticalMethod.MethodIdentifier",
        "ResultAnalyticalMethod.MethodName",
        "ResultAnalyticalMethod.MethodIdentifierContext",
        "ResultCommentText",
        "ResultDetectionConditionText",
        "ID"
      ) %>%
      # Join the sites2 table
      left_join(sites2, by = c("ORG ID", "SITE ID")) %>%
      # Join the resultphyschem2 table
      left_join(resultphyschem2, by = c(
        "ORG ID","SITE ID", "ACTIVITY DATE", "ACTIVITY ID", "ACTIVITY START TIME",
        "CHARACTERISTIC NAME", "RESULT VALUE", "RESULT UNIT", "RESULT STATUS",
        "RESULT VALUE TYPE", "SAMPLE FRACTION", "LAB ANALYSIS DATE", 
        "ResultCommentText", "SubjectTaxonomicName",
        "ResultDetectionConditionText", "ID"
      )) %>%
      # Prepare the COMMENT column
      fmutate(
        Analyte = 
          pmap_chr(list(`CHARACTERISTIC NAME`, `RESULT VALUE`, `RESULT UNIT`, `SAMPLE FRACTION`, 
                        MethodSpeciationName
          ),
          function(x, y, z, u, w){
            x2 <- str_replace_na(x, "")
            y2 <- str_replace_na(y, "")
            z2 <- str_replace_na(z, "")
            u2 <- str_replace_na(u, "")
            w2 <- str_replace_na(w, "")
            
            out <- str_c("Analyte = ", x2, " ", 
                         w2, 
                         " ", y2, " ", 
                         z2, ", ", u2)
            
            return(out)
          })
      ) %>%
      fmutate(
        COMMENTS =
          pmap_chr(list(Analyte, `ResultAnalyticalMethod.MethodIdentifier`,
                        `ResultAnalyticalMethod.MethodName`, ResultCommentText, 
                        `ResultAnalyticalMethod.MethodIdentifierContext`),
                   function(x, y, z, u, w, v){
                     x2 <- str_replace_na(x, "")
                     y2 <- str_replace_na(y, "")
                     z2 <- str_replace_na(z, "")
                     u2 <- str_replace_na(u, "")
                     w2 <- str_replace_na(w, "")
                     
                     out <- str_c(x2,
                                  "\r\r\nMethod ID = ", y2, " ", w2,
                                  "\r\r\nMethod Name = ", z2,
                                  "\r\r\nResult Comment = ", u2) 
                     
                     # Remove the last white spaces
                     out <- str_trim(out)
                     
                     return(out)
                   }
          )) %>%
      # Reorder the columns
      get_vars(c(names(ref), "ResultCommentText", "ResultMeasureValue_Ori",
                 "DetectionQuantitationLimitMeasure.MeasureValue_Ori")) %>%
      # Arrange the columns
      roworderv(c("SITE ID", "ACTIVITY DATE", "ACTIVITY ID"))
  } else {
    results2 <- ref
  }
  
  return(results2)
}

SANDS_create_summary_part2 <- function(x, ref){
  if (nrow(x) > 0){
    x2 <- x %>% fselect(-ResultCommentText, 
                        -ResultMeasureValue_Ori,
                        -DetectionQuantitationLimitMeasure.MeasureValue_Ori)
  } else {
    x2 <- ref
  }
  return(x2)
}

### nutrients tab

# This function takes the summary tab as the input argument
nutrients_tab <- function(x, address_dat, lookup_main, lookup2_spec, resultphyschem_dat,
                          header, TADA_file){
  par_name <- unique(c(unique(lookup_main$Parameter), 
                       unique(lookup2_spec$Parameter)))
  
  if (nrow(x) > 0){
    
    x2 <- x %>% 
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      # Separate the x2 with lookup_main and lookup2_spec
      x2_main <- x2 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% unique(lookup_main$Parameter)) %>%
        mutate(ID = 1:n())
      
      x2_spec <- x2 %>% 
        fsubset(`CHARACTERISTIC NAME` %in% unique(lookup2_spec$Parameter)) %>%
        mutate(ID = 1:n())
      
      # Join x2_main with lookup_main
      x3_main <- x2_main %>%
        left_join(lookup_main, 
                  by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fsubset(`SAMPLE FRACTION` == Fraction | Fraction %in% "ALL FRACTIONS")
      
      # Join x2_spec with resultphyschem_dat and lookup2_spec
      resultphyschem_dat2 <- resultphyschem_dat %>% 
        fselect("ORG ID" = "OrganizationIdentifier",
                "SITE ID" = "MonitoringLocationIdentifier",
                "ACTIVITY DATE" = "ActivityStartDate",
                "ACTIVITY ID" = "ActivityIdentifier",
                "CHARACTERISTIC NAME" = "CharacteristicName",
                "SAMPLE FRACTION" = "ResultSampleFractionText",
                "RESULT VALUE" = "ResultMeasureValue",
                "RESULT UNIT" = "ResultMeasure.MeasureUnitCode",
                "MethodSpeciationName") %>%
        mutate(MethodSpeciationName = str_to_upper(MethodSpeciationName))
      
      x3_spec <- x2_spec %>%
        left_join(resultphyschem_dat2, by = c(
          "ORG ID", "SITE ID", "ACTIVITY DATE",
          "ACTIVITY ID", "CHARACTERISTIC NAME", "SAMPLE FRACTION",
          "RESULT VALUE", "RESULT UNIT"
        ), multiple = "first") %>%
        # Replace UNKNOWN in MethodSpeciationName with NA
        fmutate(MethodSpeciationName = ifelse(MethodSpeciationName %in% "UNKNOWN",
                                              NA_character_, MethodSpeciationName)) %>%
        # Update the RESULT UNIT and DETECTION LIMIT UNIT column with MethodSpeciationName
        fmutate(`RESULT UNIT` = str_trim(str_c(`RESULT UNIT`, 
                                               str_replace_na(MethodSpeciationName, replacement = ""), 
                                               sep = " "))) %>%
        fmutate(`DETECTION LIMIT UNIT` = str_trim(str_c(`DETECTION LIMIT UNIT`,
                                                        str_replace_na(MethodSpeciationName, replacement = ""),
                                                        sep = " "))) %>%
        left_join(lookup2_spec, 
                  by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fsubset(`SAMPLE FRACTION` == Fraction | Fraction %in% "ALL FRACTIONS") 
      
      # Create ranking table
      x3_main_rank <- x3_main %>%
        count(Address, `CHARACTERISTIC NAME`, `SAMPLE FRACTION`, Units) %>%
        arrange(Address, desc(n)) %>%
        group_by(Address) %>%
        mutate(Rank = 1:n()) %>%
        fselect(-n)
      
      x3_spec_rank <- x3_spec %>%
        count(Address, `CHARACTERISTIC NAME`, `SAMPLE FRACTION`, Units, MethodSpeciationName) %>%
        arrange(Address, desc(n)) %>%
        group_by(Address) %>%
        mutate(Rank = 1:n()) %>%
        fselect(-n)
      
      # Join ranking table to x3
      x4_main <- x3_main %>%
        left_join(x3_main_rank, by = c("Address", "CHARACTERISTIC NAME",
                                       "SAMPLE FRACTION", "Units"))
      
      x4_spec <- x3_spec %>%
        left_join(x3_spec_rank, by = c("Address", "CHARACTERISTIC NAME",
                                       "SAMPLE FRACTION", "Units", "MethodSpeciationName"))
      
      x4 <- bind_rows(x4_main, x4_spec)
      
      # Conduct unit conversion
      x5 <- x4 %>% SANDS_unit()
      
      # Get records with address
      x5_Ad <- x5 %>% fsubset(!is.na(Address))
      
      # Based on the rank of address, determine the records to keep
      x6 <- x5_Ad %>%
        group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                 `ACTIVITY ID`, Address) %>%
        drop_na(Value) %>%
        filter(Rank == min(Rank, na.rm = TRUE)) %>%
        # If multiple values exist, only keep the one that `RESULT UNIT` == %in% Units
        mutate(Rank2 = ifelse(`RESULT UNIT` %in% Units, 1, 2)) %>%
        slice(1) %>%
        ungroup()
      
      # Prepare the main dataset
      x7 <- x6 %>%
        # Select columns for join
        fselect("Org ID" = "ORG ID", 
                "Station (Site) Name" = "SITE NAME",
                "Site ID" = "SITE ID",
                "Activity Date" = "ACTIVITY DATE",
                "Activity ID" = "ACTIVITY ID",
                "Latitude" = "LATITUDE",
                "Longitude" = "LONGITUDE",
                "Waterbody Type" = "SITE TYPE",
                "Medium" = "ACTIVITY MEDIA",
                "Medium Detail" = "ACTIVITY SUBMEDIA",
                "Replica" = "ACTIVITY TYPE",
                "DATUM" = "COORDINATE REF",
                "Sample Procedure" = "SAMPLE COLLECTION METHOD",
                "Activity \r\nTime" = "ACTIVITY START TIME",
                "CHARACTERISTIC NAME",
                "SAMPLE FRACTION",
                "Value",
                "COMMENTS",
                "Address",
                "ResultCommentText") %>%
        # Join with table header names
        left_join(address_dat, by = "Address") 
      
      x8 <- x7 %>%
        drop_na(Name) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, 
                -`SAMPLE FRACTION`, -ResultCommentText) %>%
        pivot_wider(names_from = "Name", values_from = "Value",
                    values_fn = \(x) paste0(x, collapse = " ")) %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
        mutate(across(everything(), .fns = as.character))
      
      x8_comment <- x7 %>%
        drop_na(Name) %>%
        fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
        fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
        fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                           paste0(head_copy, ": ", ResultCommentText),
                                           NA)) %>%
        fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                -Value, -head_copy, -Name) %>%
        group_by(across(-ResultCommentText)) %>%
        summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                    paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
        ungroup() %>%
        fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
        mutate(across(everything(), .fns = as.character)) %>%
        fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                Latitude, Longitude, `Waterbody Type`, Medium,  `Medium Detail`, DATUM,
                `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
      
      x9_final <- x8 %>%
        left_join(x8_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                     "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium Detail", "DATUM",
                                     "Activity \r\nTime"))
      
      # Join the TADA_file
      
      if (nrow(TADA_file) == 0){
        x9_final2 <- x9_final
      } else {
        TADA_file2 <- TADA_file %>%
          fselect("Org ID" = "OrganizationIdentifier",
                  "Station (Site) Name" = "MonitoringLocationName",
                  "Site ID" = "MonitoringLocationIdentifier",
                  "Activity Date" = "ActivityStartDate",
                  "Activity ID" = "ActivityIdentifier",
                  "Activity \r\nTime" = "ActivityStartTime.Time",
                  "Latitude" = "TADA.LatitudeMeasure",
                  "Longitude" = "TADA.LongitudeMeasure",
                  "Depth") %>%
          distinct() %>%
          mutate(across(everything(), .fns = as.character)) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`))
        
        x9_final2 <- x9_final %>%
          left_join(TADA_file2, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity \r\nTime",
                                       "Activity ID", "Latitude", "Longitude"))
      }
      
      # Join to the header
      x10 <- header %>%
        mutate(across(everything(), .fns = as.character)) %>%
        bind_rows(x9_final2) %>%
        arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      
    } else {
      x10 <- header
    }
    
  } else {
    x10 <- header
  }
  return(x10)
}

# A function to prepare the GIS output 
GIS_output_nutrient <- function(x){
  x2 <- x %>% mutate(across(c(`Depth`:`DO\r\n(%)`, 
                              `Filamentous Algae Cover`:`Macrophage Cover`,
                              `TVS\r\n(mg/L)`:`E. coli\r\n(MPN/\r\n100ml)`), 
                            .fns = number_clean))
  return(x2)
}

### H20 tab

# This function takes the summary tab as the input argument
H2O_tab <- function(x, address_dat, lookup, header, TADA_file){
  
  par_name <- unique(lookup$Parameter)
  
  if (nrow(x) > 0){
  
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name) 
    
    if (nrow(x2) > 0){
      # Join x2 with lookup
      x3 <- x2 %>%
        left_join(lookup, 
                  by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fsubset(`SAMPLE FRACTION` == Fraction | Fraction %in% "ALL FRACTIONS") 
      
      if (nrow(x3) > 0){
        # Create ranking table
        x3_rank <- x3 %>%
          count(Address, `CHARACTERISTIC NAME`, `SAMPLE FRACTION`, Units) %>%
          arrange(Address, desc(n)) %>%
          group_by(Address) %>%
          mutate(Rank = 1:n()) %>%
          fselect(-n)
        
        # Join ranking table to x3
        x4 <- x3 %>%
          left_join(x3_rank, by = c("Address", "CHARACTERISTIC NAME",
                                    "SAMPLE FRACTION", "Units"))
        
        # Conduct unit conversion
        x5 <- x4 %>% SANDS_unit()
        
        # Get records with address
        x5_Ad <- x5 %>% fsubset(!is.na(Address))
        
        # Based on the rank of address, determine the records to keep
        x6 <- x5_Ad %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          drop_na(Value) %>%
          filter(Rank == min(Rank, na.rm = TRUE)) %>%
          ungroup() %>%
          # If multiple values exist, only keep the one that `RESULT UNIT` %in% Units
          mutate(Rank2 = ifelse(`RESULT UNIT` == Units, 1, 2)) %>%
          arrange(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                  `ACTIVITY ID`, Address, Rank2) %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          slice(1) %>%
          ungroup()
        
        # Get the hardness dataset
        hardness_dat <- x6 %>% 
          fsubset(`CHARACTERISTIC NAME` %in% 
                    c("Hardness, Ca, Mg")) %>%
          # Add the final notes
          fmutate(Value = case_when(
            is.na(Num_Value)                         ~str_c("<  ", as.character(Num_Det_Value)),
            `RESULT VALUE TYPE` %in% "Calculated"    ~str_c("C ", as.character(Num_Value)),
            TRUE                                     ~as.character(Num_Value)
          )) %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                  "Activity\r\nType" = "ACTIVITY TYPE",
                  "DATUM" = "COORDINATE REF",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address") %>%
          # Join with table header names
          left_join(address_dat, by = "Address")
        
        x7 <- x6 %>%
          fsubset(!`CHARACTERISTIC NAME` %in% 
                    c("Hardness, Ca, Mg")) %>%
          fselect(-Rank2)
        
        # Prepare the main dataset
        x8 <- x7 %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                  "Activity\r\nType" = "ACTIVITY TYPE",
                  "DATUM" = "COORDINATE REF",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address",
                  "ResultCommentText") %>%
          # Join with table header names
          left_join(address_dat, by = "Address")
        
        x9 <- x8 %>%
          bind_rows(hardness_dat) %>%
          drop_na(Name) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
          pivot_wider(names_from = "Name", values_from = "Value",
                      values_fn = \(x) paste0(x, collapse = " ")) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
          mutate(across(everything(), .fns = as.character))
        
        x9_comment <- x8 %>%
          bind_rows(hardness_dat) %>%
          drop_na(Name) %>%
          fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
          fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
          fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                             paste0(head_copy, ": ", ResultCommentText),
                                             NA)) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                  -Value, -head_copy, -Name) %>%
          group_by(across(-ResultCommentText)) %>%
          summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                      paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
          ungroup() %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
          mutate(across(everything(), .fns = as.character)) %>%
          fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                  Latitude, Longitude, `Waterbody Type`, Medium,  `Medium\r\nDetail`, DATUM,
                  `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
        
        x9_final <- x9 %>%
          left_join(x9_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                       "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium\r\nDetail", "DATUM",
                                       "Activity \r\nTime"))
        
        # Join the TADA_file
        
        if (nrow(TADA_file) == 0){
          x9_final2 <- x9_final
        } else {
          TADA_file2 <- TADA_file %>%
            fselect("Org ID" = "OrganizationIdentifier",
                    "Station (Site) Name" = "MonitoringLocationName",
                    "Site ID" = "MonitoringLocationIdentifier",
                    "Activity Date" = "ActivityStartDate",
                    "Activity ID" = "ActivityIdentifier",
                    "Activity \r\nTime" = "ActivityStartTime.Time",
                    "Latitude" = "TADA.LatitudeMeasure",
                    "Longitude" = "TADA.LongitudeMeasure",
                    "Depth") %>%
            distinct() %>%
            mutate(across(everything(), .fns = as.character)) %>%
            fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`))
          
          x9_final2 <- x9_final %>%
            left_join(TADA_file2, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity \r\nTime",
                                         "Activity ID", "Latitude", "Longitude"))
        }
        
        # Join to the header
        x10 <- header %>%
          mutate(across(everything(), .fns = as.character)) %>%
          bind_rows(x9_final2) %>%
          arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      } else {
        x10 <- header
      }
      
    } else {
      x10 <- header
    }
    
  } else {
    x10 <- header
  }

  return(x10)
}

# A function to prepare the GIS output 
GIS_output_metal_h2o <- function(x){
  x2 <- x %>% mutate(across(c(`Depth`:`Tl \r\n(ug/L) \r\nD`,
                              `Hardness, Dissolved (mg/L)`:`Dissolved Organic Carbon \r\n(DOC mg/L)`), 
                            .fns = number_clean))
  return(x2)
}

### Sed tab

# This function takes the summary tab as the input argument
sed_tab <- function(x, address_dat, lookup, header){
  
  par_name <- unique(lookup$Parameter)
  
  if (nrow(x) > 0){
    
    # Filter the "ACTIVITY MEDIA" as "Sediment"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Sediment") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    # # Additional steps to convert UG/L to UG/G
    # if (nrow(x2) > 0){
    #   x2 <- x2 %>%
    #     fmutate(Units = ifelse(Units %in% "UG/L", "UG/G", Units))
    # }
    
    if (nrow(x2) > 0){
      # Join x2 with lookup
      x3 <- x2 %>%
        left_join(lookup, 
                  by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fsubset(`SAMPLE FRACTION` == Fraction | Fraction %in% "ALL FRACTIONS") 
      
      if (nrow(x3) > 0){
        # Create ranking table
        x3_rank <- x3 %>%
          count(Address, `CHARACTERISTIC NAME`, `SAMPLE FRACTION`, Units) %>%
          arrange(Address, desc(n)) %>%
          group_by(Address) %>%
          mutate(Rank = 1:n()) %>%
          fselect(-n)
        
        # Join ranking table to x3
        x4 <- x3 %>%
          left_join(x3_rank, by = c("Address", "CHARACTERISTIC NAME",
                                    "SAMPLE FRACTION", "Units"))
        
        # Conduct unit conversion
        x5 <- x4 %>% SANDS_unit()
        
        # Get records with address
        x5_Ad <- x5 %>% fsubset(!is.na(Address))
        
        # Based on the rank of address, determine the records to keep
        x6 <- x5_Ad %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          drop_na(Value) %>%
          filter(Rank == min(Rank, na.rm = TRUE)) %>%
          ungroup() %>%
          # If multiple values exist, only keep the one that `RESULT UNIT` %in% Units
          mutate(Rank2 = ifelse(`RESULT UNIT` == Units, 1, 2)) %>%
          arrange(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                  `ACTIVITY ID`, Address, Rank2) %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          slice(1) %>%
          ungroup()
        
        # Prepare the main dataset
        x7 <- x6 %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium Detail" = "ACTIVITY SUBMEDIA",
                  "Activity Type" = "ACTIVITY TYPE",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address",
                  "ResultCommentText") %>%
          # Join with table header names
          left_join(address_dat, by = "Address") 
        
        x8 <- x7 %>%
          drop_na(Name) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
          pivot_wider(names_from = "Name", values_from = "Value",
                      values_fn = \(x) paste0(x, collapse = " ")) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
          mutate(across(everything(), .fns = as.character))
        
        x8_comment <- x7 %>%
          drop_na(Name) %>%
          fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
          fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
          fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                             paste0(head_copy, ": ", ResultCommentText),
                                             NA)) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                  -Value, -head_copy, -Name) %>%
          group_by(across(-ResultCommentText)) %>%
          summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                      paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
          ungroup() %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
          mutate(across(everything(), .fns = as.character)) %>%
          fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                  Latitude, Longitude, `Waterbody Type`, Medium,  `Medium Detail`,
                  `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
        
        x8_final <- x8 %>%
          left_join(x8_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                       "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium Detail", 
                                       "Activity \r\nTime"))
        
        # Join to the header
        x9 <- header %>%
          mutate(across(everything(), .fns = as.character)) %>%
          bind_rows(x8_final) %>%
          arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      } else {
        x9 <- header
      }
    
      } else {
      x9 <- header
    }
  } else {
    x9 <- header
  }
  return(x9)
}

# A function to prepare the GIS output 
GIS_output_metal_sed <- function(x){
  x2 <- x %>% mutate(across(`Al \r\n(ug/g) \r\nT`:`Tl  \r\n(ug/g) \r\n<63um`, .fns = number_clean))
  return(x2)
}

### salinity tab

# This function takes the summary tab as the input argument
salinity_tab <- function(x, address_dat, lookup, header, TADA_file){
  
  par_name <- unique(lookup$Parameter)
  
  if (nrow(x) > 0){
    
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name) 
    
    if (nrow(x2) > 0){
      # Join x2 with lookup
      x3 <- x2 %>%
        left_join(lookup, 
                  by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fsubset(`SAMPLE FRACTION` == Fraction | Fraction %in% "ALL FRACTIONS") 
      
      if (nrow(x3) > 0){
        # Create ranking table
        x3_rank <- x3 %>%
          count(Address, `CHARACTERISTIC NAME`, `SAMPLE FRACTION`, Units) %>%
          arrange(Address, desc(n)) %>%
          group_by(Address) %>%
          mutate(Rank = 1:n()) %>%
          fselect(-n)
        
        # Join ranking table to x3
        x4 <- x3 %>%
          left_join(x3_rank, by = c("Address", "CHARACTERISTIC NAME",
                                    "SAMPLE FRACTION", "Units"))
        
        # Conduct unit conversion
        x5 <- x4 %>% SANDS_unit()
        
        # Get records with address
        x5_Ad <- x5 %>% fsubset(!is.na(Address))
        
        # Based on the rank of address, determine the records to keep
        x6 <- x5_Ad %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          drop_na(Value) %>%
          filter(Rank == min(Rank, na.rm = TRUE)) %>%
          ungroup() %>%
          # If multiple values exist, only keep the one that `RESULT UNIT` %in% Units
          mutate(Rank2 = ifelse(`RESULT UNIT` == Units, 1, 2)) %>%
          arrange(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                  `ACTIVITY ID`, Address, Rank2) %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          slice(1) %>%
          ungroup()
        
        # Get the hardness dataset
        hardness_dat <- x6 %>% 
          fsubset(`CHARACTERISTIC NAME` %in% 
                    c("Hardness, Ca, Mg")) %>%
          # Add the final notes
          fmutate(Value = case_when(
            is.na(Num_Value)                         ~str_c("<  ", as.character(Num_Det_Value)),
            `RESULT VALUE TYPE` %in% "Calculated"    ~str_c("C ", as.character(Num_Value)),
            TRUE                                     ~as.character(Num_Value)
          )) %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                  "Activity\r\nType" = "ACTIVITY TYPE",
                  "DATUM" = "COORDINATE REF",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address") %>%
          # Join with table header names
          left_join(address_dat, by = "Address")
        
        x7 <- x6 %>%
          fsubset(!`CHARACTERISTIC NAME` %in% 
                    c("Hardness, Ca, Mg", "Hardness, Ca, Mg as CaCO3")) %>%
          fselect(-Rank2)
        
        # Prepare the main dataset
        x8 <- x7 %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium Detail" = "ACTIVITY SUBMEDIA",
                  "Activity Type" = "ACTIVITY TYPE",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address",
                  "ResultCommentText") %>%
          # Join with table header names
          left_join(address_dat, by = "Address") 
        
        x9 <- x8 %>%
          bind_rows(hardness_dat) %>%
          drop_na(Name) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
          pivot_wider(names_from = "Name", values_from = "Value",
                      values_fn = \(x) paste0(x, collapse = " ")) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
          mutate(across(everything(), .fns = as.character))
        
        x9_comment <- x8 %>%
          bind_rows(hardness_dat) %>%
          drop_na(Name) %>%
          fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
          fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
          fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                             paste0(head_copy, ": ", ResultCommentText),
                                             NA)) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                  -Value, -head_copy, -Name) %>%
          group_by(across(-ResultCommentText)) %>%
          summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                      paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
          ungroup() %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
          mutate(across(everything(), .fns = as.character)) %>%
          fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                  Latitude, Longitude, `Waterbody Type`, Medium,  `Medium Detail`,
                  `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
        
        x9_final <- x9 %>%
          left_join(x9_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                       "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium Detail",
                                       "Activity \r\nTime"))
        
        # Join the TADA_file
        
        if (nrow(TADA_file) == 0){
          x9_final2 <- x9_final
        } else {
          TADA_file2 <- TADA_file %>%
            fselect("Org ID" = "OrganizationIdentifier",
                    "Station (Site) Name" = "MonitoringLocationName",
                    "Site ID" = "MonitoringLocationIdentifier",
                    "Activity Date" = "ActivityStartDate",
                    "Activity ID" = "ActivityIdentifier",
                    "Activity \r\nTime" = "ActivityStartTime.Time",
                    "Latitude" = "TADA.LatitudeMeasure",
                    "Longitude" = "TADA.LongitudeMeasure",
                    "Depth") %>%
            distinct() %>%
            mutate(across(everything(), .fns = as.character)) %>%
            fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`))
          
          x9_final2 <- x9_final %>%
            left_join(TADA_file2, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity \r\nTime",
                                         "Activity ID", "Latitude", "Longitude"))
        }
        
        # Join to the header
        x10 <- header %>%
          mutate(across(everything(), .fns = as.character)) %>%
          bind_rows(x9_final2) %>%
          arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      } else {
      
        x10 <- header
      }
    
    } else {
    
      x10 <- header
    }
    
  } else {
    x10 <- header
  }

  return(x10)
}

# A function to prepare the GIS output 
GIS_output_salinity <- function(x){
  x2 <- x %>% mutate(across(c(Depth:`SO4 \r\n(mg/L) \r\nD`,
                              `Hardness, \r\nCa, Mg \r\nas CaCO3\r\nDiss\r\n(mg/L)`), 
                            .fns = number_clean))
  return(x2)
}

### Oil Gas tab

# This function takes the summary tab as the input argument
oil_tab <- function(x, address_dat, lookup, header){
  
  par_name <- unique(lookup$Parameter)
  
  if (nrow(x) > 0){
    
    # Filter the "ACTIVITY MEDIA" as "Water"
    x2 <- x %>% 
      fsubset(`ACTIVITY MEDIA` %in% "Water") %>%
      # Filter the parameter names
      fsubset(`CHARACTERISTIC NAME` %in% par_name)
    
    if (nrow(x2) > 0){
      
      # Join x2 with lookup
      x3 <- x2 %>%
        left_join(lookup, 
                  by = c("CHARACTERISTIC NAME" = "Parameter")) %>%
        fsubset(`SAMPLE FRACTION` == Fraction | Fraction %in% "ALL FRACTIONS")  
      
      if (nrow(x3) > 0){
        # Create ranking table
        x3_rank <- x3 %>%
          count(Address, `CHARACTERISTIC NAME`, `SAMPLE FRACTION`, Units) %>%
          arrange(Address, desc(n)) %>%
          group_by(Address) %>%
          mutate(Rank = 1:n()) %>%
          fselect(-n)
        
        # Join ranking table to x3
        x4 <- x3 %>%
          left_join(x3_rank, by = c("Address", "CHARACTERISTIC NAME",
                                    "SAMPLE FRACTION", "Units"))
        
        # Conduct unit conversion
        x5 <- x4 %>% SANDS_unit()
        
        # Get records with address
        x5_Ad <- x5 %>% fsubset(!is.na(Address))
        
        # Based on the rank of address, determine the records to keep
        x6 <- x5_Ad %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          drop_na(Value) %>%
          filter(Rank == min(Rank, na.rm = TRUE)) %>%
          ungroup() %>%
          # If multiple values exist, only keep the one that `RESULT UNIT` %in% Units
          mutate(Rank2 = ifelse(`RESULT UNIT` == Units, 1, 2)) %>%
          arrange(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                  `ACTIVITY ID`, Address, Rank2) %>%
          group_by(`ORG ID`, `SITE ID`, `SITE NAME`, `ACTIVITY DATE`, 
                   `ACTIVITY ID`, Address) %>%
          slice(1) %>%
          ungroup()
        
        # Get the hardness dataset
        hardness_dat <- x6 %>% 
          fsubset(`CHARACTERISTIC NAME` %in% 
                    c("Hardness, Ca, Mg")) %>%
          # Add the final notes
          fmutate(Value = case_when(
            is.na(Num_Value)                         ~str_c("<  ", as.character(Num_Det_Value)),
            `RESULT VALUE TYPE` %in% "Calculated"    ~str_c("C ", as.character(Num_Value)),
            TRUE                                     ~as.character(Num_Value)
          )) %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                  "Activity\r\nType" = "ACTIVITY TYPE",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address") %>%
          # Join with table header names
          left_join(address_dat, by = "Address")
        
        x7 <- x6 %>%
          fsubset(!`CHARACTERISTIC NAME` %in% 
                    c("Hardness, Ca, Mg")) %>%
          fselect(-Rank2)
        
        # Prepare the main dataset
        x8 <- x7 %>%
          # Select columns for join
          fselect("Org ID" = "ORG ID", 
                  "Station (Site) Name" = "SITE NAME",
                  "Site ID" = "SITE ID",
                  "Activity Date" = "ACTIVITY DATE",
                  "Activity ID" = "ACTIVITY ID",
                  "Latitude" = "LATITUDE",
                  "Longitude" = "LONGITUDE",
                  "Waterbody Type" = "SITE TYPE",
                  "Medium" = "ACTIVITY MEDIA",
                  "Medium\r\nDetail" = "ACTIVITY SUBMEDIA",
                  "Activity\r\nType" = "ACTIVITY TYPE",
                  "Activity \r\nTime" = "ACTIVITY START TIME",
                  "CHARACTERISTIC NAME",
                  "SAMPLE FRACTION",
                  "Value",
                  "COMMENTS",
                  "Address",
                  "ResultCommentText") %>%
          # Join with table header names
          left_join(address_dat, by = "Address") 
        
        x9 <- x8 %>%
          bind_rows(hardness_dat) %>%
          drop_na(Name) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, -ResultCommentText) %>%
          pivot_wider(names_from = "Name", values_from = "Value",
                      values_fn = \(x) paste0(x, collapse = " ")) %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>%
          mutate(across(everything(), .fns = as.character))
        
        x9_comment <- x8 %>%
          bind_rows(hardness_dat) %>%
          drop_na(Name) %>%
          fmutate(`Activity Date` = as.character(`Activity Date`)) %>%
          fmutate(head_copy = str_remove_all(Name, fixed("\r\n"))) %>%
          fmutate(ResultCommentText = ifelse(!is.na(ResultCommentText),
                                             paste0(head_copy, ": ", ResultCommentText),
                                             NA)) %>%
          fselect(-COMMENTS, -Address, -Col_Num, -`CHARACTERISTIC NAME`, -`SAMPLE FRACTION`, 
                  -Value, -head_copy, -Name) %>%
          group_by(across(-ResultCommentText)) %>%
          summarise(`Other Fraction \r\nConcentration\r\n(U find analyte)` = 
                      paste(ResultCommentText[!is.na(ResultCommentText)], collapse = "; ")) %>%
          ungroup() %>%
          fmutate(`Activity \r\nTime` = convert_time(`Activity \r\nTime`)) %>% 
          mutate(across(everything(), .fns = as.character)) %>%
          fselect(`Org ID`, `Station (Site) Name`, `Site ID`, `Activity Date`, `Activity ID`,
                  Latitude, Longitude, `Waterbody Type`, Medium,  `Medium\r\nDetail`,
                  `Activity \r\nTime`, `Other Fraction \r\nConcentration\r\n(U find analyte)`)
        
        x9_final <- x9 %>%
          left_join(x9_comment, by = c("Org ID", "Station (Site) Name", "Site ID", "Activity Date", "Activity ID",
                                       "Latitude", "Longitude", "Waterbody Type", "Medium",  "Medium\r\nDetail",
                                       "Activity \r\nTime"))
        
        # Join to the header
        x10 <- header %>%
          mutate(across(everything(), .fns = as.character)) %>%
          bind_rows(x9_final) %>%
          arrange(`Site ID`, `Activity Date`, `Activity ID`, `Activity \r\nTime`)
      } else {
        x10 <- header
      }
    } else {
      x10 <- header
    }
  } else {
    x10 <- header
  }
  
  return(x10)
}

# A function to prepare the GIS output 
GIS_output_oil <- function(x){
  x2 <- x %>% mutate(across(`Organic\r\nCarbon\r\n(ug/l total)`:`Carbonate\r\n(mg/L)_O`, 
                            .fns = number_clean))
  return(x2)
}

### Helper functions

# A helper function to replace NA with ""
reNA <- function(x){
  x2 <- str_replace_na(x, "")
  return(x2)
}

# A helper function to convert 24 hours to 12 hours AM PM time
convert_time <- function(x){
  y <- format(strptime(x, format = "%H:%M"), "%I:%M %p")
  return(y)
}

# A helper function for unit conversion
SANDS_unit <- function(x){
  x2 <- x %>%
    # Create a column to documnet the numeric value
    fmutate(Num_Value = as.numeric(ResultMeasureValue_Ori)) %>%
    fmutate(Num_Det_Value = as.numeric(`DetectionQuantitationLimitMeasure.MeasureValue_Ori`)) %>%
    # Unit conversion
    fmutate(Value = case_when(
      # If below detection limit: Num_Value = NA & Num_Det_Value not NA
      # Add < and Num_Det_Value
      is.na(Num_Value) & !is.na(Num_Det_Value)  
      ~str_c("<  ", as.character(Num_Det_Value)),
      # If `RESULT VALUE`is NA, add <
      is.na(`RESULT VALUE`) 
      ~str_c("<  ", reNA(`DETECTION LIMIT VALUE`)),
      # If Num_Value is not NA, add the qualifier code
      !is.na(Num_Value) 
      ~str_c(as.character(Num_Value), " ", reNA(`QUALIFIER CODE`)),
      TRUE             ~NA_character_
    ))
  return(x2)
}

# A function to process the unit and numeric values


# A helper function to remove all symbols and letter except decimal point
# to keep numbers
number_clean <- function(x) {
  result <- str_replace_all(x, "[^0-9. ]", "")
  result <- str_trim(result)
  return(result)
}

# ### Excel format function
# 
# # A function to add data to a blank new sheet
# Excel_paste <- function(wb, sheet_from, sheet_to, dat){
#   wb2 <- wb %>%
#     wb_add_worksheet(sheet = sheet_to) %>%
#     wb_add_data(x = dat, na.strings = NULL) %>%
#     wb_clone_sheet_style(from = sheet_from, to = sheet_to)
# 
#   return(wb2)
# }
# 
# # A function to add data to a new sheet and format as a table
# Excel_table <- function(wb, sheet_from, sheet_to, dat){
#   wb2 <- wb %>%
#     wb_add_worksheet(sheet = sheet_to) %>%
#     wb_add_data_table(x = dat, tableStyle = "TableStyleMedium9", na.strings = NULL) %>%
#     wb_clone_sheet_style(from = sheet_from, to = sheet_to)
# }
# 
# # A function to add data to existing columns
# Excel_paste2 <- function(wb, sheet_from, sheet_to, dat,
#                          startCol, startRow){
#   wb2 <- wb %>%
#     wb_add_data(sheet = sheet_to, startCol = startCol, startRow = startRow,
#                 na.strings = NULL, x = dat, colNames = FALSE) %>%
#     wb_clone_sheet_style(from = sheet_from, to = sheet_to)
#   return(wb2)
# }

# # Function to prepare the site data for AU join
# SANDS_site_simplify <- function(x){
#   x <- x2 %>%
#     distinct(`ns2:MonitoringLocationIdentifier`, `ns2:StateName`,
#              `ns2:MonitoringLocationName`, `ns2:MonitoringLocationTypeName`,
#              `ns2:LatitudeMeasure`, `ns2:TADA.LongitudeMeasure`) %>%
#     rename(MonitoringLocationIdentifier = `ns2:MonitoringLocationIdentifier`,
#            State = `ns2:StateName`, 
#            MonitoringLocationName = `ns2:MonitoringLocationName`,
#            MonitoringLocationTypeName = `ns2:MonitoringLocationTypeName`,
#            LatitudeMeasure = `ns2:LatitudeMeasure`,
#            LongitudeMeasure = `ns2:TADA.LongitudeMeasure`)
#   return(x)
# }

### A function to create the factsheet as a list
### Turn lines 81 to 291 a function
fact_sheet_create <- function(sample_w_er3, list_sites, criteria_table, flow_dates, AU = TRUE,
                              Convert = TRUE){
  ### Fact sheet create function
  result_list <- list()
  
  for(i in list_sites) {
    
    #Pull data for relevant AU and change text case
    if (AU){
      filter_sample_pull <- sample_w_er3 %>%
        filter(AU_ID == i)
    } else {
      filter_sample_pull <- sample_w_er3 %>%
        filter(MonitoringLocationIdentifier == i) 
    }
    
    if (Convert){
      filter_sample_pull <- filter_sample_pull %>%
        #Convert all caps to title case
        mutate(TADA.ResultSampleFractionText = ifelse(TADA.ResultSampleFractionText == 'TOTAL',
                                                      'TOTAL RECOVERABLE',
                                                      TADA.ResultSampleFractionText))
    }
    
    #Identify the constituents and their fractions in this AU
    filter_constituents <- filter_sample_pull %>%
      select(TADA.CharacteristicName, TADA.ResultSampleFractionText) %>%
      unique()
    
    #Pull relevant standards
    filter_standards <- criteria_table %>%
      filter(Constituent %in% filter_constituents$TADA.CharacteristicName) %>%
      filter(State %in% "Montana")
    
    #Combine all relevant samples with their standards for analysis
    combine_samples_stands <- filter_sample_pull %>%
      mutate(doy = lubridate::yday(ActivityStartDate)) %>%
      left_join(flow_dates, by = "US_L3NAME")
    
    if (AU){
      combine_samples_stands <- combine_samples_stands %>%
        filter(!is.na(AU_ID))
    } else {
      combine_samples_stands <- combine_samples_stands %>%
        filter(!is.na(MonitoringLocationIdentifier))
    }
    
    #Identify uses for the AU
    filter_uses <- filter_standards %>%
      select(Use) %>%
      filter(Use != 'Trigger Value') %>%
      unique() %>%
      pull()
    
    #Determine which parameters are missing, create columns for them to 
    #cbind to final table
    parameters <- c('Aluminum','Arsenic', 'Cadmium','Chromium', 'Copper', 'Iron',
                    'Lead','Selenium','Silver', 'Zinc','Mercury') %>%
      str_to_upper()
    
    missing_params <- which(!parameters %in% unique(combine_samples_stands$TADA.CharacteristicName))
    missing_params_names <- parameters[missing_params]
    
    if (AU){
      columns_aq_use <- c(i, unique(filter_sample_pull$AU_NAME), 'Aquatic Life', NA, NA, NA, NA,
                          NA, NA, NA, NA, NA)
      
      columns_hh_use <- c(i, unique(filter_sample_pull$AU_NAME), 'Human Health', NA, NA, NA, NA, NA)
    } else {
      columns_aq_use <- c(i, unique(filter_sample_pull$MonitoringLocationIdentifier), 
                          'Aquatic Life', NA, NA, NA, NA,
                          NA, NA, NA, NA, NA)
      
      columns_hh_use <- c(i, unique(filter_sample_pull$MonitoringLocationIdentifier), 
                          'Human Health', NA, NA, NA, NA, NA)
    }
    
    columns_aq_use_to_add <- columns_aq_use %>%
      as.data.frame() %>%
      rep(each = length(missing_params_names)) %>%
      as.data.frame()
    colnames(columns_aq_use_to_add) <- missing_params_names
    
    
    columns_hh_use_to_add <- columns_hh_use %>%
      as.data.frame() %>%
      rep(each = length(missing_params_names)) %>%
      as.data.frame()
    colnames(columns_hh_use_to_add) <- missing_params_names
    
    #Initialize list for loop
    summary_table_list <- list()
    counter <- 1
    
    #Loop through uses
    for(j in filter_uses){
      
      if(j == 'Aquatic Life') {
        if (AU){
          summary_table_temp <- combine_samples_stands %>%
            filter(Use %in% j) %>%
            semi_join(filter_standards, by = c("TADA.CharacteristicName" = "Constituent",
                                               "TADA.ResultSampleFractionText" = "Fraction")) %>%
            group_by(TADA.CharacteristicName, AU_ID, AU_NAME)
        } else {
          summary_table_temp <- combine_samples_stands %>%
            filter(Use %in% j) %>%
            semi_join(filter_standards, by = c("TADA.CharacteristicName" = "Constituent",
                                               "TADA.ResultSampleFractionText" = "Fraction")) %>%
            group_by(TADA.CharacteristicName, 
                     MonitoringLocationIdentifier, 
                     MonitoringLocationName)
        }
        summary_table <- summary_table_temp %>%
          reframe(Use = Use,
                  TADA.CharacteristicName = TADA.CharacteristicName,
                  Sample_Years = paste0(min(year(ActivityStartDate)),
                                        ' - ',
                                        max(year(ActivityStartDate))),
                  Number_Samples = ifelse(all(c("Acute", "Chronic") %in% unique(Details)),
                                          n()/2,
                                          n()),
                  #If location is Lake -> high flow = NA,
                  #If location is stream -> check dates for high flow
                  high_flow = ifelse(str_detect(MonitoringLocationTypeName, 'Lake') == F,
                                     ifelse(doy >= StartDate & doy < EndDate, 1, 0),
                                     NA),
                  Num_High_Flow = ifelse(all(c("Acute", "Chronic") %in% unique(Details)),
                                         sum(high_flow, na.rm = T)/2, 
                                         sum(high_flow, na.rm = T)),
                  Percent_High_Flow = Num_High_Flow/Number_Samples*100,
                  acute_2x = ifelse(Details == 'Acute', 
                                    ifelse(TADA.ResultMeasureValue >= (2*Criteria_Upper), 1, 0),
                                    NA),
                  Num_Samples_2x_Acute = sum(acute_2x, na.rm = T),
                  acute = ifelse(Details == 'Acute', 
                                 ifelse(TADA.ResultMeasureValue >= Criteria_Upper, 1, 0),
                                 NA),
                  Num_Acute_Exceedances = sum(acute, na.rm = T),
                  chronic = ifelse(Details == 'Chronic', 
                                   ifelse(TADA.ResultMeasureValue >= Criteria_Upper, 1, 0),
                                   NA),
                  Num_Chronic_Exceedances = sum(chronic, na.rm = T),
                  Acute_Exceedance_Rate = Num_Acute_Exceedances/Number_Samples*100,
                  Chronic_Exceedance_Rate = Num_Chronic_Exceedances/Number_Samples*100) %>%
          select(!c(high_flow, acute_2x, acute, chronic)) %>%
          unique()
      } else {
        if (AU){
          summary_table_temp <- combine_samples_stands %>%
            filter(Use %in% j) %>%
            semi_join(filter_standards, by = c("TADA.CharacteristicName" = "Constituent",
                                               "TADA.ResultSampleFractionText" = "Fraction")) %>%
            group_by(TADA.CharacteristicName, AU_ID, AU_NAME)
        } else {
          summary_table_temp <- combine_samples_stands %>%
            filter(Use %in% j) %>%
            semi_join(filter_standards, by = c("TADA.CharacteristicName" = "Constituent",
                                               "TADA.ResultSampleFractionText" = "Fraction")) %>%
            group_by(TADA.CharacteristicName, 
                     MonitoringLocationIdentifier, 
                     MonitoringLocationName)
        }
        summary_table <- summary_table_temp %>%
          filter(Use %in% j) %>%
          reframe(Use = Use,
                  TADA.CharacteristicName = TADA.CharacteristicName,
                  Sample_Years = paste0(min(year(ActivityStartDate)),
                                        ' - ',
                                        max(year(ActivityStartDate))),
                  Number_Samples = n(),
                  #If location is Lake -> high flow = NA,
                  #If location is stream -> check dates for high flow
                  high_flow = ifelse(str_detect(MonitoringLocationTypeName, 'Lake') == F,
                                     ifelse(doy >= StartDate & doy < EndDate, 1, 0),
                                     NA),
                  Num_High_Flow = sum(high_flow, na.rm = T),
                  Percent_High_Flow = Num_High_Flow/Number_Samples*100,
                  exceedances = ifelse(TADA.ResultMeasureValue >= Criteria_Upper, 1, 0),
                  Num_HHS_Exceedances = sum(exceedances, na.rm = T)) %>%
          select(!c(high_flow, exceedances)) %>%
          unique()
      }
      
      #Pull column names
      table_col_names <- colnames(summary_table)
      
      #Pull constituent names
      constituent_names <- summary_table %>%
        select(TADA.CharacteristicName) %>%
        pull()
      
      #Transpose table, rename columns, and remove unnecessary row
      summary_table_wide <- as_tibble(t(summary_table)) 
      colnames(summary_table_wide) <- constituent_names
      summary_table_wide <- summary_table_wide %>%
        cbind(table_col_names) %>%
        relocate(table_col_names) %>%
        rename(Variable = table_col_names) %>%
        slice(-1)
      
      #Adding missing columns if necessary
      if(j == 'Aquatic Life' & nrow(columns_aq_use_to_add) > 0) {
        
        #No Chromium in Aquatic Use tables
        if('CHROMIUM' %in% colnames(columns_aq_use_to_add)) {
          columns_aq_use_to_add <- columns_aq_use_to_add %>%
            select(!CHROMIUM)
        }
        
        summary_table_wide <- summary_table_wide %>%
          cbind(columns_aq_use_to_add) 
        
        vars <- colnames(summary_table_wide)[-1]
        #Make variable columns in alphabetical order
        summary_table_wide <- summary_table_wide %>%
          relocate(all_of(sort(vars)), .after = last_col())
        
      } else if (j == 'Human Health' & nrow(columns_hh_use_to_add) > 0){
        
        #No iron or aluminum in human health tables
        if('IRON' %in% colnames(columns_hh_use_to_add)) {
          columns_hh_use_to_add <- columns_hh_use_to_add %>%
            select(!IRON)
        }
        
        if('ALUMINUM' %in% colnames(columns_hh_use_to_add)) {
          columns_hh_use_to_add <- columns_hh_use_to_add %>%
            select(!ALUMINUM)
        }
        
        
        summary_table_wide <- summary_table_wide %>%
          cbind(columns_hh_use_to_add)
        
        vars <- colnames(summary_table_wide)[-1]
        #Make variable columns in alphabetical order
        summary_table_wide <- summary_table_wide %>%
          relocate(all_of(sort(vars)), .after = last_col())
      }
      
      summary_table_list[[counter]] <- summary_table_wide
      counter <- counter + 1
      result_list[[i]] <- summary_table_list
    }
  }
  
  return(result_list)
}

### Functions to join AU to STATION
SANDS_STATION_AU_Join <- function(dat, AU_table){
  dat2 <- dat %>%
    left_join(AU_table %>% 
                fselect(-LatitudeMeasure, -LongitudeMeasure), 
              by = c("ns2:MonitoringLocationIdentifier" = "MonitoringLocationIdentifier",
                     "ns2:MonitoringLocationName" = "MonitoringLocationName",
                     "ns2:MonitoringLocationTypeName" = "MonitoringLocationTypeName",
                     "ns2:StateName" = "State"))
  return(dat2)
}

### Functions to join AU to SUMMARY
SANDS_SUMMARY_AU_Join <- function(dat, AU_table){
  dat2 <- dat %>%
    left_join(AU_table %>% 
                fselect(-LatitudeMeasure, -LongitudeMeasure), 
              by = c("SITE ID" = "MonitoringLocationIdentifier",
                     "SITE NAME" = "MonitoringLocationName",
                     "SITE TYPE" = "MonitoringLocationTypeName",
                     "STATE NAME" = "State"))
  return(dat2)
}
