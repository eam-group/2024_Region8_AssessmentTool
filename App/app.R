### This is a draft version of a tool to download data from the water quality portal

# Load packages
library(tidyverse)
library(lubridate)
library(collapse)
library(data.table)
library(openxlsx)
library(dataRetrieval)
library(shiny)
library(shinybusy)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(DT)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(zip)
library(EPATADA)
library(tigris)
library(tidycensus)
library(spsComps)
library(scales)

# Load the data
load("DataInput/InputData39.RData")
load("DataInput/SANDS_input24.RData")

# Load the tabs
db_main_body <- source("external/db_main_body.R")$value
db_main_sb <- source("external/db_main_sb.R")$value
# tab_Intro <- source("external/tab_Intro.R")$value
tab_Download <- source("external/tab_Download.R")$value
tab_AU <- source('external/tab_AU.R')$value
tab_Sufficiency <- source("external/tab_Sufficiency.R")$value
tab_Analysis <- source("external/tab_Analysis.R")$value
tab_SANDS <- source("external/tab_SANDS.R")$value

# Load the functions
source("scripts/Main_helper_functions.R")
source("scripts/SANDS_helper_functoins.R")
source("scripts/AU_helper_functions.R")

# Set the upload size to 5MB
mb_limit <- 10000
options(shiny.maxRequestSize = mb_limit * 1024^2)

### Helper function to add USGS topo map

# Stored USGS map element names
grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo", "USGS Shaded Relief", "Hydrography")

att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")

# Get the base map
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

add_USGS_base <- function(x){
  x <- leaflet::addWMSTiles(x, GetURL("USGSTopo"),
                            group = grp[1], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSImageryOnly"),
                            group = grp[2], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSImageryTopo"),
                            group = grp[3], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSShadedReliefOnly"),
                            group = grp[4], attribution = att, layers = "0")
  
  # Add the tiled overlay for the National Hydrography Dataset to the map widget:
  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
  x <- leaflet::addWMSTiles(x, GetURL("USGSHydroCached"),
                            group = grp[5], options = opt, layers = "0")
  x <- leaflet::hideGroup(x, grp[5])
  
  # Add layer control
  # Add layer controls
  opt2 <- leaflet::layersControlOptions(collapsed = FALSE)
  x <- leaflet::addLayersControl(x, baseGroups = grp[1:4],
                                 overlayGroups = grp[5], options = opt2)
  
  return(x)
}

## tabs
db_main_sb                     <- source("external/db_main_sb.R"
                                         , local = TRUE)$value
db_main_body                   <- source("external/db_main_body.R"
                                         , local = TRUE)$value
# tab_Intro                      <- source("external/tab_Intro.R"
#                                          , local = TRUE)$value
tab_Download                   <- source("external/tab_Download.R"
                                         , local = TRUE)$value
tab_Analysis                   <- source("external/tab_Analysis.R"
                                         , local = TRUE)$value

## Data and files for AU

path_results <- "Results"

url_au_table <- "https://github.com/eam-group/2024_Region8_AssessmentTool/raw/main/App/data"
url_au_table2 <- file.path(url_au_table, "MonLoc_to_AU_Crosswalk_20240415.xlsx")
temp_au_table <- tempfile(fileext = ".xlsx")
httr::GET(url_au_table2, httr::write_disk(temp_au_table))

df_ML2AU_orig <- as.data.frame(readxl::read_excel(temp_au_table))

df_ML2AU <- df_ML2AU_orig %>%
  select(MonitoringLocationIdentifier, AU_ID, AU_NAME, DrinkingWater_Use
         , Ecological_Use, FishConsumption_Use, Recreational_Use, Other_Use)

## AU Shapefiles ####
load(file = "data/GISlayer_streams.rda")
GISlayer_streams_transformed <- streams_shp %>% 
  sf::st_transform(2818) %>%
  select(AU_ID, AU_NAME, drinkingwa, ecological, fishconsum, recreation
         , other_use) %>%  # trim unneccessary columns
  rename(DrinkingWater_Use = drinkingwa
         , Ecological_Use = ecological
         , FishConsumption_Use = fishconsum
         , Recreational_Use = recreation
         , Other_Use = other_use)

GISlayer_streams_transformed_cen <- GISlayer_streams_transformed %>%
  st_centroid()

load(file = "data/GISlayer_streams_simp.rda")

load(file = "data/GISlayer_lakes.rda")
GISlayer_lakes_transformed <- lakes_shp %>% 
  sf::st_transform(2818) %>%
  select(AU_ID, AU_NAME, drinkingwa, ecological, fishconsum, recreation
         , other_use) %>%  # trim unneccessary columns
  rename(DrinkingWater_Use = drinkingwa
         , Ecological_Use = ecological
         , FishConsumption_Use = fishconsum
         , Recreational_Use = recreation
         , Other_Use = other_use)

GISlayer_lakes_transformed_cen <- GISlayer_lakes_transformed %>%
  st_centroid()


AU_centroids <- bind_rows(GISlayer_streams_transformed_cen, GISlayer_lakes_transformed_cen) %>%
  st_transform(crs = 4326) %>%
  mutate(AU_x = st_coordinates(.)[, 1],
         AU_y = st_coordinates(.)[, 2]) %>%
  st_set_geometry(NULL) %>%
  distinct(AU_ID, AU_NAME, AU_x, AU_y)

## Waterbody types ####
Lake_types <- c("Lake, Reservoir, Impoundment", "Reservoir", "Lake")

Stream_types <- c("Stream", "Stream: Canal", "River/Stream", "Channelized Stream"
                  , "River/Stream Perennial", "River/Stream Intermittent"
                  , "Canal Irrigation", "Canal Drainage")

### UI
ui <- function(request){
  dashboardPage(
    # The header panel
    header = dashboardHeader(title = "WQP Data Tool"),
    # The sidebar panel
    sidebar = dashboardSidebar(db_main_sb("leftsidebarmenu")
    ),
    body = dashboardBody(
      # Call shinyCatch
      spsDepend("shinyCatch"),
      # A call to the use shinyJS package
      useShinyjs(),
      # Set up the contents in the main panel as tabs
      db_main_body("dbBody")
    )
  )
}

### Server
server <- function(input, output, session){
  
  ### The Download tab
  
  # Show or hide the SANDS filter group button in the Data Download tab based on SANDS_check
  observe({
    if(input$SANDS_check){
      shinyjs::show(id = "MT_par_group")
    } else {
      shinyjs::hide(id = "MT_par_group")
    }
  })
  
  observe({
    if(input$SANDS_check2){
      shinyjs::show(id = "MT_par_group2")
    } else {
      shinyjs::hide(id = "MT_par_group2")
    }
  })
  
  # Show or hide the data download button in the Data Download tab for the WQP data
  observe({
    if(!is.null(reVal$WQP_dat2)){
      shinyjs::show(id = "data_download")
    } else {
      shinyjs::hide(id = "data_download")
    }
  })
  
  # Show or hide the data download button in the Data Download tab for the WQP data
  observe({
    if(!is.null(reVal$WQP_dat2)){
      shinyjs::show(id = "data_download")
    } else {
      shinyjs::hide(id = "data_download")
    }
  })
  
  # Show or hide the data download button in the Data Download tab for the WQP_QC data
  observe({
    if(!is.null(reVal$WQP_QC)){
      shinyjs::show(id = "data_download_QC")
    } else {
      shinyjs::hide(id = "data_download_QC")
    }
  })
  
  # Show or hide the data download button in the Data Download tab for the Macroinvertebrate data
  observe({
    if(input$biological_check &
       !is.null(reVal$bio_dat)){
      shinyjs::show(id = "data_download_bio")
    } else {
      shinyjs::hide(id = "data_download_bio")
    }
  })
  
  # Show or hide the data uploader in the Join AU tab for the fact sheet
  observe({
    if(input$crosswalk_upload %in% "Upload a new crosswalk table"){
      shinyjs::show(id = "file_crosswalk")
    } else {
      shinyjs::hide(id = "file_crosswalk")
    }
  })
  
  # Show or hide the data download button in the Join AU tab for the Zip file
  observe({
    if(!is.null(AU_Val$WQP_dat2)){
      shinyjs::show(id = "b_download")
    } else {
      shinyjs::hide(id = "b_download")
    }
  })
  
  # Show or hide the data download button in the Join AU tab for the WQP data
  observe({
    if(!is.null(AU_Val$WQP_dat2)){
      shinyjs::show(id = "WQP_AU_Download")
    } else {
      shinyjs::hide(id = "WQP_AU_Download")
    }
  })
  
  # Show or hide the data download button in the Data Analysis tab for selected data
  observe({
    if(!is.null(input$Par_Select_rows_selected)){
      shinyjs::show(id = "data_download_selected_AN")
    } else {
      shinyjs::hide(id = "data_download_selected_AN")
    }
  })
  
  # Show or hide the data download button in the Data Analysis tab
  observe({
    if(!is.null(AN_Val$ex)){
      shinyjs::show(id = "data_download_AN")
    } else {
      shinyjs::hide(id = "data_download_AN")
    }
  })
  
  # Show or hide the data download button in the Data Sufficiency tab
  observe({
    if(!is.null(SU_Val$WQP_dat3)){
      shinyjs::show(id = "data_download_SU")
    } else {
      shinyjs::hide(id = "data_download_SU")
    }
  })
  
  # Show or hide the site_ID_se dropdown menu
  observe({
    if(!input$all_sites_AU){
      shinyjs::show(id = "site_ID_se")
    } else {
      shinyjs::hide(id = "site_ID_se")
    }
  })
  
  # Show or hide the SANDS_data_download button
  observe({
    if(reVal$SANDS_download_ready & length(input$MT_par_group) > 0){
      shinyjs::show(id = "SANDS_data_download")
    } else {
      shinyjs::hide(id = "SANDS_data_download")
    }
  })
  
  # Show or hide the SANDS_data_download button
  observe({
    if(AU_Val$SANDS_download_ready  & length(input$MT_par_group2) > 0){
      shinyjs::show(id = "SANDS_data_download2")
    } else {
      shinyjs::hide(id = "SANDS_data_download2")
    }
  })
  
  # Show or hide the fact_sheet_data_download button
  observe({
    if(SANDS_Val$fact_sheet_save){
      shinyjs::show(id = "fact_sheet_data_download")
    } else {
      shinyjs::hide(id = "fact_sheet_data_download")
    }
  })
  
  # Control the method to select area
  observeEvent(input$area_se, {
    toggleState(id = "State_se", condition = input$area_se == "State")
    toggleState(id = "HUC_se", condition = input$area_se == "HUC8")
    toggleState(id = "bb_N", condition = input$area_se == "BBox")
    toggleState(id = "bb_S", condition = input$area_se == "BBox")
    toggleState(id = "bb_W", condition = input$area_se == "BBox")
    toggleState(id = "bb_E", condition = input$area_se == "BBox")
  })
  
  ### HUC8 map
  
  # Update input$state_out_bound if "input.area_se == 'State'"
  observe({
    req(input$area_se)
    if (input$area_se == "State"){
      updateCheckboxInput(inputId = "state_out_bound", value = FALSE)
    }
  })
  
  # A reactive object to select HUC8 map
  HUC8_dat <- reactive({
    
    if (!input$state_out_bound){
      return(Region8_simple)
    } else if (input$state_out_bound){
      return(Region8_out_simple)
    }
  })
  
  # Update the HUC8 list
  observe({
    if (!input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_label)
    } else if (input$state_out_bound){
      updateSelectizeInput(session = session, inputId = "HUC_se", choices = HUC8_out_label)
    } 
  })
  
  
  # The map object
  output$HUC8map <- renderLeaflet({
    
    map <- leaflet() %>%
      addPolygons(data = HUC8_dat(), 
                  fill = TRUE,
                  fillColor = "blue", 
                  color = "blue",
                  label = ~huc8,
                  weight = 1,
                  layerId = ~huc8,
                  group = "base_map") %>%
      add_USGS_base()
    
    return(map)
  })
  
  # Update the map selection
  
  HUC_list <- reactiveValues(Selected = character(0))
  
  observeEvent(input$HUC8map_shape_click, {
    
    click <- input$HUC8map_shape_click
    click_id <- click$id
    
    if (isTruthy(!click_id %in% HUC_list$Selected)){
      HUC_list$Selected[[click_id]] <- click_id
    } else {
      HUC_list$Selected <- HUC_list$Selected[!HUC_list$Selected %in% click_id]
    }
    
    # Update the HUC_se
    updateSelectizeInput(session = session, inputId = "HUC_se",
                         selected = HUC_list$Selected)
    
  })
  
  # Update the map with selected HUC8
  HUC8map_proxy <- leafletProxy("HUC8map")
  
  HUC8_temp <- reactive({
    HUC8_temp <- HUC8_dat() %>%
      filter(huc8 %in% input$HUC_se)
    return(HUC8_temp)
  })
  
  observe({
    
    if (nrow(HUC8_temp()) > 0){
      
      HUC8map_proxy %>% clearGroup(group = "highlighted_polygon")
      HUC8map_proxy %>%
        addPolylines(data = HUC8_temp(),
                     stroke = TRUE,
                     color = "red",
                     weight = 2,
                     group = "highlighted_polygon") 
      
    } else {
      HUC8map_proxy %>% clearGroup(group = "highlighted_polygon")
    }
    
  })
  
  # Create the map for BBox
  output$BBox_map <- renderLeaflet({
    map <- leaflet() %>%
      fitBounds(lng1 = -102.11928, lat1 = 50.58677, 
                lng2 = -117.65217, lat2 = 40.37306) %>%
      addDrawToolbar(polylineOptions = FALSE, 
                     circleOptions = FALSE, 
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE, 
                     polygonOptions = FALSE,
                     singleFeature = TRUE) %>%
      add_USGS_base()
    
    return(map)
  })
  
  bbox_reVal <- reactiveValues()
  
  observeEvent(input$BBox_map_draw_new_feature, {
    feat <- input$BBox_map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords_m <- matrix(coords, ncol = 2, byrow = TRUE)
    poly <- st_sf(st_sfc(st_polygon(list(coords_m))), crs = st_crs(27700))
    bbox_temp <- st_bbox(poly)
    
    # Update the bounding box
    bbox_reVal$w_num <- as.numeric(bbox_temp[1])
    bbox_reVal$s_num <- as.numeric(bbox_temp[2])
    bbox_reVal$e_num <- as.numeric(bbox_temp[3])
    bbox_reVal$n_num <- as.numeric(bbox_temp[4])
    
  })
  
  observe({
    updateNumericInput(session = session, inputId = "bb_W", value = bbox_reVal$w_num)
    updateNumericInput(session = session, inputId = "bb_S", value = bbox_reVal$s_num)
    updateNumericInput(session = session, inputId = "bb_E", value = bbox_reVal$e_num)
    updateNumericInput(session = session, inputId = "bb_N", value = bbox_reVal$n_num)
  })
  
  bb_W_debounce <- reactive(input$bb_W) %>% debounce(2000)
  bb_S_debounce <- reactive(input$bb_S) %>% debounce(2000)
  bb_E_debounce <- reactive(input$bb_E) %>% debounce(2000)
  bb_N_debounce <- reactive(input$bb_N) %>% debounce(2000)
  
  # Check if the numbers are within the range
  observe({
    req(input$bb_N, input$bb_S, input$bb_W, input$bb_E)
    if (input$bb_W < -117.65217 | input$bb_W > -102.11928){
      shinyalert(text = "The west bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    if (input$bb_E < -117.65217 | input$bb_E > -102.11928){
      shinyalert(text = "The east bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    if (input$bb_N < 40.37306 | input$bb_N > 50.58677){
      shinyalert(text = "The north bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    if (input$bb_S < 40.37306 | input$bb_S > 50.58677){
      shinyalert(text = "The south bound is out of range. Please adjust the coordinates.", 
                 type = "error")
    }
    
    if (input$bb_W >= input$bb_E){
      shinyalert(text = "The longitude of west bound should not be larger or equal to the east bound. 
                 Please provide a valid longitude entry.", 
                 type = "error")
    }
    
    if (input$bb_S >= input$bb_N){
      shinyalert(text = "The latitude of south bound should not be larger or equal to the north bound.
                 Please provide a valid latitude entry.", 
                 type = "error")
    }
    
  }) %>%
    bindEvent(bb_W_debounce(), bb_S_debounce(), bb_E_debounce(), bb_N_debounce())
  
  
  # Update par_se based on par_group_se
  par_value <- reactiveValues()
  
  observe({
    if ("All" %in% input$par_group_se){
      variable <- parameter_names$Standard_Name
    } else {
      group_par <- parameter_names %>%
        filter(Group %in% input$par_group_se)
      variable <- group_par$Standard_Name
    }
    par_value$variable <- variable
  }) 
  
  observe({
    if (length(input$par_group_se) > 0){
      updateMultiInput(session = session, inputId = "par_se",
                       selected = par_value$variable) 
    } else if (length(input$par_group_se) == 0){
      updateMultiInput(session = session, inputId = "par_se",
                       selected = character(0))
    }
  })
  
  # Reactive objects
  reVal <- reactiveValues(
    download = FALSE, 
    SANDS_download_ready = FALSE,
    WQP_time = Sys.time())
  
  data_storage <- reactiveValues()
  
  ### SADNS data download
  location_ind <- reactive({
    if (!is.null(input$area_se)){
        if (input$area_se == "State"){
          if (!is.null(input$State_se)){
            return(TRUE)
          } else {
            return(FALSE)
          } 
        } else if (input$area_se == "HUC8"){
          if (!is.null(input$HUC_se)){
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else if (input$area_se == "BBox"){
          if (!is.null(input$bb_W) & 
              !is.null(input$bb_N) & 
              !is.null(input$bb_S) & 
              !is.null(input$bb_E)){
            return(TRUE)
          } else {
            FALSE
          }
        } else {
          return(FALSE)
        } 
    } else {
      return(FALSE) 
    }
  })
  
  # Download the data for individual parameters
  observeEvent(input$par_download, {
    req(input$par_date, location_ind())
    
    shinyCatch({
      if (input$download_se %in% c("Characteristic names", "Organic characteristic group",
                                   "All data")){
        
        if (input$download_se %in% c("Characteristic names")){
          req(input$par_se)
          # Get the parameter
          char_par <- get_par(dat = parameter_reference_table, par = input$par_se)
        } else {
          char_par <- NULL
        }
        
        if (input$download_se %in% c("Organic characteristic group")){
          req(input$CharGroup_se)
          char_group <- input$CharGroup_se
        } else {
          char_group <- NULL
        }
        
        showModal(modalDialog(title = "Estimating the sample size...", footer = NULL))
        
        ### Use readWQPsummary and a for loop to download the parameters and report progress
        
        # Calculate the year summary number
        YearSum <- year(Sys.Date()) - year(ymd(input$par_date[1])) + 1
        
        if (YearSum <= 1){
          YearSum2 <- 1
        } else if (YearSum > 1 & YearSum <= 5){
          YearSum2 <- 5
        } else {
          YearSum2 <- "all"
        }
        
        if (input$area_se == "State"){
          req(input$State_se)
          
          if (input$download_se %in% c("Characteristic names", 
                                       "Organic characteristic group")){
            dat_summary_list <- list(readWQPsummary(statecode = input$State_se,
                                                    summaryYears = YearSum2,
                                                    siteType = input$sitetype_se))
          } else if (input$download_se %in% "All data"){
            dat_summary_list <- list(readWQPsummary(statecode = input$State_se,
                                                    summaryYears = YearSum2))
          }
          
        } else if (input$area_se == "HUC8"){
          req(input$HUC_se)
          
          if (input$download_se %in% c("Characteristic names", 
                                       "Organic characteristic group")){
            dat_summary_list <- map(input$HUC_se,
                                    ~readWQPsummary(huc = .x, summaryYears = YearSum2,
                                                    siteType = input$sitetype_se)) 
          } else if (input$download_se %in% "All data"){
            dat_summary_list <- map(input$HUC_se,
                                    ~readWQPsummary(huc = .x, summaryYears = YearSum2)) 
          }
          
        } else if (input$area_se == "BBox"){
          req(input$bb_W >= -117.65217 & input$bb_W <= -102.11928,
              input$bb_E >= -117.65217 & input$bb_E <= -102.11928,
              input$bb_N >= 40.37306 & input$bb_N <= 50.58677,
              input$bb_S >= 40.37306 & input$bb_S <= 50.58677,
              input$bb_N > input$bb_S,
              input$bb_E > input$bb_W)
          bBox <- c(input$bb_W, input$bb_S, input$bb_E, input$bb_N)
          
          if (input$download_se %in% c("Characteristic names", 
                                       "Organic characteristic group")){
            dat_summary_list <- list(readWQPsummary(bBox = bBox, 
                                                    summaryYears = YearSum2,
                                                    siteType = input$sitetype_se)) 
          } else if (input$download_se %in% "All data"){
            dat_summary_list <- list(readWQPsummary(bBox = bBox, 
                                                    summaryYears = YearSum2))  
          }
          
        }
        
        dat_summary_list <- keep(dat_summary_list, .p = function(x) nrow(x) > 0)
        
        dat_summary <- rbindlist(dat_summary_list, fill = TRUE)
        
        if (nrow(dat_summary) == 0 & ncol(dat_summary) == 0){
          dat_summary <- WQP_summary_template
        }
        
        # Filter by char_par or CharacteristicType
        if (input$download_se %in% "Characteristic names"){
          dat_summary_filter <- dat_summary %>%
            fsubset(CharacteristicName %in% char_par)
        } else if (input$download_se %in% "Organic characteristic group"){
          dat_summary_filter <- dat_summary %>%
            fsubset(CharacteristicType %in% char_group)
        } else if (input$download_se %in% "All data"){
          dat_summary_filter <- dat_summary
        }
        
        # Use dat_summary_filter to identify HUC8 that needed to be downloaded
        dat_summary2 <- dat_summary %>%
          fsubset(HUCEightDigitCode %in% unique(dat_summary_filter$HUCEightDigitCode))
        
        # Download the biological profile if biological_check is TRUE
        if (input$biological_check){
          dat_summary_filter_b <- dat_summary %>% fsubset(CharacteristicName %in% "Count")
          
          dat_summary2_b <- dat_summary %>%
            fsubset(HUCEightDigitCode 
                    %in% unique(dat_summary_filter_b$HUCEightDigitCode))
          
        } else {
          dat_summary2_b <- dat_summary %>% slice(0)
        }
        
        # nrow(dat_summary2) > 0, proceed
        if (nrow(dat_summary2) > 0){
          dat_summary3 <- dat_summary2 %>%
            fsubset(YearSummarized >= year(ymd(input$par_date[1])) & 
                      YearSummarized <= year(ymd(input$par_date[2])))
          
          sample_size <- nrow(dat_summary3)
          
          if (sample_size > 100000){
            shinyalert(
              title = "Process Started",
              text = paste0("The estimated sample size is ", 
                            comma(sample_size), 
                            ", which is above 100,000 and could be above the memory limit. Please select a smaller geographic area or a shorter period with multiple downloads."),
              type = "warning"
            )
            removeModal()
            return()
          } 
          
          if (input$biological_check & nrow(dat_summary2_b) > 0){
            dat_summary3_b <- dat_summary2_b %>%
              fsubset(YearSummarized >= year(ymd(input$par_date[1])) & 
                        YearSummarized <= year(ymd(input$par_date[2])))
            
            sample_size_b <- nrow(dat_summary2_b)
            
            if (sample_size_b > 100000){
              shinyalert(
                title = "Process Started",
                text = paste0("The estimated sample size of the macroinvertebrate data is ", 
                              comma(sample_size), 
                              ", which is above 300,000 and could be above the memory limit. Please select a smaller geographic area or a shorter period with multiple downloads."),
                type = "warning"
              )
              removeModal()
              return()
            } 
            
            
          } else {
            dat_summary3_b <- dat_summary2_b %>% slice(0)
          }
          
          # Filter the dat_summary3 by HUC8_dat()
          dat_summary3_sf <- dat_summary3 %>% 
            distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                     MonitoringLocationLongitude, HUCEightDigitCode) %>%
            drop_na(MonitoringLocationLongitude) %>%
            drop_na(MonitoringLocationLatitude) %>%
            st_as_sf(coords = c("MonitoringLocationLongitude",
                                "MonitoringLocationLatitude"),
                     crs = 4326) %>%
            st_filter(HUC8_dat())
          
          if (input$biological_check & nrow(dat_summary3_b) > 0){
            dat_summary3_b_sf <- dat_summary3_b %>% 
              distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                       MonitoringLocationLongitude, HUCEightDigitCode) %>%
              drop_na(MonitoringLocationLongitude) %>%
              drop_na(MonitoringLocationLatitude) %>%
              st_as_sf(coords = c("MonitoringLocationLongitude",
                                  "MonitoringLocationLatitude"),
                       crs = 4326) %>%
              st_filter(HUC8_dat())
          } else {
            dat_summary3_b_sf <- dat_summary3_b %>% slice(0)
          }
          
          removeModal()
          
          # Filter by HUCEightDigitCode
          site_all <- dat_summary3 %>% 
            fsubset(HUCEightDigitCode 
                    %in% unique(dat_summary3_sf$HUCEightDigitCode)) %>%
            group_by(HUCEightDigitCode, YearSummarized) %>%
            fsummarize(ResultCount = fsum(ResultCount)) %>%
            fungroup() %>%
            arrange(HUCEightDigitCode, YearSummarized) 
          
          if (input$biological_check & 
              nrow(dat_summary3_b) > 0 & 
              nrow(dat_summary3_b_sf) > 0){
            site_all_b <- dat_summary3_b %>% 
              fsubset(HUCEightDigitCode 
                      %in% unique(dat_summary3_b_sf$HUCEightDigitCode)) %>%
              group_by(HUCEightDigitCode, YearSummarized) %>%
              fsummarize(ResultCount = fsum(ResultCount)) %>%
              fungroup() %>%
              arrange(HUCEightDigitCode, YearSummarized) 
          } else{
            site_all_b <- dat_summary3_b %>% slice(0)
          }
          
          # If nrow(site_all) > 0, download the data
          if (nrow(site_all) > 0){
            
            site_all_list <- site_all %>% split(.$HUCEightDigitCode)
            site_all2 <- map_dfr(site_all_list, cumsum_group, col = "ResultCount", threshold = 25000)
            site_all3 <- site_all2 %>% summarized_year()
            
            progressSweetAlert(
              session = session, id = "myprogress",
              title = "Download Progress",
              display_pct = TRUE, value = 0
            )
            
            dat_temp_all_list <- list()
            
            # Download resultPhysChem, project, and site profile
            
            for (i in 1:nrow(site_all3)){
              
              updateProgressBar(
                session = session,
                id = "myprogress",
                value = round(i/nrow(site_all3) * 100, 2)
              )
              
              HUC_temp <- site_all3$HUCEightDigitCode[i]
              Start_temp <- site_all3$Start[i]
              End_temp <- site_all3$End[i]
              
              if (input$download_se %in% "Characteristic names"){
                if (length(char_par) <= 5){
                  args_temp <- TADA_args_create(
                    huc = HUC_temp,
                    characteristicName = char_par,
                    startDate = Start_temp,
                    endDate = End_temp,
                    siteType = input$sitetype_se
                  )
                } else {
                  args_temp <- TADA_args_create(
                    huc = HUC_temp,
                    startDate = Start_temp,
                    endDate = End_temp,
                    siteType = input$sitetype_se
                  )
                }
              } else if (input$download_se %in% "Organic characteristic group"){
                args_temp <- TADA_args_create(
                  huc = HUC_temp,
                  characteristicType = char_group,
                  startDate = Start_temp,
                  endDate = End_temp,
                  siteType = input$sitetype_se
                )
              } else if (input$download_se %in% "All data"){
                args_temp <- TADA_args_create(
                  huc = HUC_temp,
                  startDate = Start_temp,
                  endDate = End_temp
                )
              }
              
              output_temp <- TADA_DataRetrieval(
                startDate = as.character(args_temp[["startDate"]]),
                endDate =  as.character(args_temp[["endDate"]]),
                countrycode = args_temp[["countrycode"]],
                countycode = args_temp[["countycode"]],
                huc = args_temp[["huc"]],
                siteid = args_temp[["siteid"]],
                siteType = args_temp[["siteType"]],
                characteristicName = args_temp[["characteristicName"]],
                characteristicType = args_temp[["characteristicType"]],
                sampleMedia = args_temp[["sampleMedia"]],
                statecode = args_temp[["statecode"]],
                organization = args_temp[["organization"]],
                project = args_temp[["project"]],
                providers = args_temp[["providers"]],
                applyautoclean = FALSE
              )
              
              if (is.null(output_temp)){
                output_temp <- TADA_download_temp
              }
              
              dat_temp_all_list[[i]] <- output_temp
            }
            
            closeSweetAlert(session = session)
            sendSweetAlert(
              session = session,
              title = "Download completed !",
              type = "success"
            )
            
            # Subset the site data by the bBox
            if (input$area_se == "BBox"){
              bBox_sf <- st_bbox(
                c(
                  xmin = input$bb_W,
                  xmax = input$bb_E,
                  ymax = input$bb_N,
                  ymin = input$bb_S
                ),
                crs = st_crs(4326)
              ) %>%
                st_as_sfc()
              
              dat_summary3_sf <- dat_summary3_sf %>%
                st_filter(bBox_sf)
            }
            
            dat_temp_com <- rbindlist(dat_temp_all_list, fill = TRUE)
            
            # Filter the data with char_par 
            if (input$download_se %in% "Characteristic names"){
              dat_temp_com1 <- dat_temp_com %>%
                fsubset(CharacteristicName %in% char_par)
            } else if (input$download_se %in% c("Organic characteristic group", "All data")){
              dat_temp_com1 <- dat_temp_com 
            }
            
            dat_temp_all <- dat_temp_com1 %>%
              # Filter with the site ID in the state and dates
              fsubset(MonitoringLocationIdentifier %in% dat_summary3_sf$MonitoringLocationIdentifier) %>%
              fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
            
            # If nrow(site_all) == 0, return an empty data frame with the header as dat_temp_all
          } else {
            
            dat_temp_all <- TADA_download_temp
            
          }
          
          # If nrow(site_all_b) > 0, download the data
          if (nrow(site_all_b) > 0){
            
            site_all_b_list <- site_all_b %>% split(.$HUCEightDigitCode)
            site_all_b2 <- map_dfr(site_all_b_list, cumsum_group, col = "ResultCount", threshold = 25000)
            site_all_b3 <- site_all_b2 %>% summarized_year()
            
            
            progressSweetAlert(
              session = session, id = "myprogress_bio",
              title = "Download Progress (macroinvertbrate count data)",
              display_pct = TRUE, value = 0
            )
            
            biological_temp_all_list <- list()
            
            # Download biological_temp profile
            
            for (j in 1:nrow(site_all_b3)){
              
              updateProgressBar(
                session = session,
                id = "myprogress_bio",
                value = round(j/nrow(site_all_b3) * 100, 2)
              )
              
              HUC_temp <- site_all_b3$HUCEightDigitCode[j]
              Start_temp <- site_all_b3$Start[j]
              End_temp <- site_all_b3$End[j]
              
              if (input$download_se %in% c("Characteristic names", 
                                           "Organic characteristic group")){
                args_temp <- args_create(
                  huc = HUC_temp,
                  characteristicName = "Count",
                  startDateLo = Start_temp,
                  startDateHi = End_temp,
                  siteType = input$sitetype_se
                ) 
              } else if (input$download_se %in% "All data"){
                args_temp <- args_create(
                  huc = HUC_temp,
                  characteristicName = "Count",
                  startDateLo = Start_temp,
                  startDateHi = End_temp
                )
              }
              
              biological_temp <- biological_download(args_temp, ref = biological_template)
              
              if (is.null(biological_temp)){
                biological_temp <- biological_template
              }
              
              biological_temp_all_list[[j]] <- biological_temp
            }
            
            closeSweetAlert(session = session)
            sendSweetAlert(
              session = session,
              title = "Download completed !",
              type = "success"
            )
            
            # Subset the site data by the bBox
            if (input$area_se == "BBox"){
              bBox_sf <- st_bbox(
                c(
                  xmin = input$bb_W,
                  xmax = input$bb_E,
                  ymax = input$bb_N,
                  ymin = input$bb_S
                ),
                crs = st_crs(4326)
              ) %>%
                st_as_sfc()
              
              dat_summary3_b_sf <- dat_summary3_b_sf %>%
                st_filter(bBox_sf)
            }
            
            biological_temp_com <- rbindlist(biological_temp_all_list, fill = TRUE)
            
            biological_temp_all <- biological_temp_com %>%
              # Filter with the site ID in the state and dates
              fsubset(MonitoringLocationIdentifier %in% dat_summary3_sf$MonitoringLocationIdentifier) %>%
              fsubset(ActivityStartDate >= input$par_date[1] & ActivityStartDate <= input$par_date[2])
            
            # If nrow(site_all) == 0, return an empty data frame with the header as dat_temp_all
          } else {
            
            biological_temp_all <- biological_template
            
          }
          
          # If nrow(dat_summary) == 0, return an empty data frame with the header as dat_temp_all
        } else {
          
          dat_temp_all <- TADA_download_temp
          biological_temp_all <- biological_template
        }
        
        reVal$WQP_dat <- dat_temp_all
        reVal$bio_dat <- biological_temp_all
      }
    },
    blocking_level = "error")
  })
  
  ### Clean up the bio_dat
  observe({
    req(reVal$bio_dat, nrow(reVal$bio_dat) > 0)
    
    shinyCatch({
      bio_temp <- reVal$bio_dat %>%
        # Remove duplicates
        distinct() %>%
        # Remove records without scientific names
        fsubset(!is.na(SubjectTaxonomicName)) %>%
        # Filter the data with macroinvertebrate
        fsubset(AssemblageSampledName %in% "Benthic Macroinvertebrates") %>%
        # Filter for ActivityMediaName as Biological
        fsubset(ActivityMediaName %in% "Biological") %>%
        # Filter the BiologicalIntentName that is not "Tissue"
        fsubset(!BiologicalIntentName %in% "Tissue")
      
      reVal$bio_dat2 <- bio_temp
    }, 
    blocking_level = "error")
    
  })
  
  ### Update the UI for data summary
  output$summary_box <- renderUI({
      tagList(
        fluidRow(
          column(
            width = 12,
            box(
              title = "Data Summary",
              status = "warning", solidHeader = TRUE,
              width = 12, collapsible = TRUE,
              h2("Raw Data Summary"),
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(outputId = "Date_Info", placeholder = TRUE)
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  selectInput(inputId = "raw_summary", label = "Data attribute to summarize",
                              choices = c("Site Type" = "MonitoringLocationTypeName",
                                          "Sample Status" = "ResultStatusIdentifier",
                                          "Original Parameter Names" = "CharacteristicName",
                                          "Sample Fraction" = "ResultSampleFractionText"),
                              multiple = TRUE)
                ),
                column(
                  width = 9,
                  DTOutput(outputId = "raw_summary_table")
                )
              ),
              h2("Clean Data Summary"),
              br(),
              fluidRow(
                column(
                  width = 3,
                  selectInput(inputId = "clean_summary", label = "Data attribute to summarize",
                              choices = c("Standard Name" = "TADA.CharacteristicName",
                                          "Standard Fraction" = "TADA.ResultSampleFractionText",
                                          "Censored Data" = "TADA.CensoredData.Flag"),
                              multiple = TRUE)
                ),
                column(
                  width = 9,
                  DTOutput(outputId = "clean_summary_table")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(outputId = "Date_Info2", placeholder = TRUE)
                )
              ),
              if (input$biological_check){
                fluidRow(
                  column(
                    width = 12,
                    h2("Macroinvertebrate Count Data Summary"),
                    fluidRow(
                      column(
                        width = 6,
                        verbatimTextOutput(outputId = "Date_Info_bio", placeholder = TRUE)
                      )
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        selectInput(inputId = "bio_summary", label = "Data attribute to summarize",
                                    choices = c("Taxon" = "SubjectTaxonomicName",
                                                "Biological Intent Name" = "BiologicalIntentName",
                                                "Sample Status" = "ResultStatusIdentifier"),
                                    multiple = TRUE)
                      ),
                      column(
                        width = 9,
                        DTOutput(outputId = "raw_summary_bio_table")
                      )
                    )
                  )
                )
              }
            )
          )
        )
      )
  })
  
  ### Get raw data summary
  observe({
    req(reVal$WQP_dat2)
    
    # Store the download date
    download_date <- format(Sys.time())
    
    # Site number and sample size
    site_number <- length(unique(reVal$WQP_dat2$MonitoringLocationIdentifier))
    sample_size <- nrow(reVal$WQP_dat2)
    
    # Min and Max Date
    if (sample_size == 0){
      min_date <- ""
      max_date <- ""
    } else {
      min_date <- min(reVal$WQP_dat2$ActivityStartDate, na.rm = TRUE)
      max_date <- max(reVal$WQP_dat2$ActivityStartDate, na.rm = TRUE) 
    }
    
    # Save the value
    reVal$download_date <- download_date
    reVal$min_date <- min_date
    reVal$max_date <- max_date
    reVal$site_number <- site_number
    reVal$sample_size <- sample_size
  })
  
  observe({
    req(reVal$WQP_dat2, reVal$sample_size)
    output$Date_Info <- renderText({
      req(reVal$WQP_dat2, reVal$sample_size)
      if (reVal$sample_size == 0){
        paste0("Data are downloaded at ", reVal$download_date, ".\n",
               "There are ", reVal$sample_size, " records from ", reVal$site_number, " sites.\n") 
      } else {
        paste0("Data are downloaded at ", reVal$download_date, ".\n",
               "The date range is from ", reVal$min_date, " to ", reVal$max_date, ".\n",
               "There are ", reVal$sample_size, " records from ", reVal$site_number, " sites.\n") 
      }
    }) 
  })
  
  ### Summarize the raw data property
  observe({
    req(reVal$WQP_dat2, input$raw_summary)
    output$raw_summary_table <- renderDT({
      tt <- count_fun(reVal$WQP_dat2, input$raw_summary)
      datatable(tt, options = list(scrollX = TRUE)) %>%
        formatPercentage(columns = "Percent (%)", digits = 2)
    })
  })
  
  ### Get the macroinvertebrate count data summary
  observe({
    req(reVal$bio_dat2)
    
    # Store the download date
    download_date <- format(Sys.time())
    
    # Site number and sample size
    site_number <- length(unique(reVal$bio_dat2$MonitoringLocationIdentifier))
    sample_size <- nrow(reVal$bio_dat2)
    
    # Min and Max Date
    if (sample_size == 0){
      min_date <- ""
      max_date <- ""
    } else {
      min_date <- min(reVal$bio_dat2$ActivityStartDate, na.rm = TRUE)
      max_date <- max(reVal$bio_dat2$ActivityStartDate, na.rm = TRUE) 
    }
    
    # Save the value
    reVal$bio_download_date <- download_date
    reVal$bio_min_date <- min_date
    reVal$bio_max_date <- max_date
    reVal$bio_site_number <- site_number
    reVal$bio_sample_size <- sample_size
  })
  
  observe({
    req(reVal$bio_dat2, reVal$bio_sample_size)
    output$Date_Info_bio <- renderText({
      req(reVal$bio_dat2, reVal$bio_sample_size)
      if (reVal$bio_sample_size == 0){
        paste0("Data are downloaded at ", reVal$bio_download_date, ".\n",
               "There are ", reVal$bio_sample_size, " records from ", reVal$bio_site_number, " sites.\n") 
      } else {
        paste0("Data are downloaded at ", reVal$bio_download_date, ".\n",
               "The date range is from ", reVal$bio_min_date, " to ", reVal$bio_max_date, ".\n",
               "There are ", reVal$bio_sample_size, " records from ", reVal$bio_site_number, " sites.\n") 
      }
    }) 
  })
  
  ### Summarize the macroinvertebrate count data
  observe({
    req(reVal$bio_dat2, input$bio_summary)
    output$raw_summary_bio_table <- renderDT({
      tt <- count_fun(reVal$bio_dat2, input$bio_summary)
      datatable(tt, options = list(scrollX = TRUE)) %>%
        formatPercentage(columns = "Percent (%)", digits = 2)
    })
  })
  
  ### Clean up the data with the TADA package
  observe({
   
    shinyCatch({
      # TADA_AutoClean
      observe({
        req(reVal$WQP_dat)
        
        # # Log
        # file_sink <- file(file.path(".", "Log", "results_Log.txt"), open = "wt")
        # 
        # # sink output
        # sink(file_sink, type = "output", append = TRUE)
        # sink(file_sink, type = "message", append = TRUE)
        # 
        # message(paste0("Cleaning date and time: ", round(Sys.time())))
        # message("Apply the functions from the TADA package to clean the data")
        # message("Apply the TADA package to clean the data")
        # message(Sys.time())
        # message("Function: TADA_AutoClean")
        
        showModal(modalDialog(title = "Run TADA_AutoClean", 
                              footer = NULL))
        
        temp_dat <- TADA_AutoClean_poss(
          .data = reVal$WQP_dat
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_AutoClean_temp
        }
        
        reVal$WQP_AutoClean <- temp_dat
        reVal$WQP_dat <- NULL
        
        removeModal()
        
      })
      
      # TADA_SimpleCensoredMethods
      observe({
        req(reVal$WQP_AutoClean)
        
        # message("Function: TADA_SimpleCensoredMethods")
        
        showModal(modalDialog(title = "Run TADA_SimpleCensoredMethods", 
                              footer = NULL))
        
        temp_dat <- TADA_SimpleCensoredMethods_poss(
          .data = reVal$WQP_AutoClean,
          nd_multiplier = 1
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_SimpleCensoredMethods_temp
        }
        
        reVal$WQP_SimpleCensoredMethods <- temp_dat
        
        reVal$WQP_AutoClean <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagMethod
      observe({
        req(reVal$WQP_SimpleCensoredMethods)
        
        # message("Function: TADA_FlagMethod")
        
        showModal(modalDialog(title = "Run TADA_FlagMethod", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagMethod_poss(
          .data = reVal$WQP_SimpleCensoredMethods,
          clean = FALSE
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagMethod_temp
        }
        
        reVal$WQP_FlagMethod <- temp_dat
        
        reVal$WQP_SimpleCensoredMethods <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagSpeciation
      observe({
        req(reVal$WQP_FlagMethod)
        
        # message("Function: TADA_FlagSpeciation")
        
        showModal(modalDialog(title = "Run TADA_FlagSpeciation", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagSpeciation_poss(
          .data = reVal$WQP_FlagMethod,
          clean = "none"
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagSpeciation_temp
        }
        
        reVal$WQP_FlagSpeciation <- temp_dat
        
        reVal$WQP_FlagMethod <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagResultUnit
      observe({
        req(reVal$WQP_FlagSpeciation)
        
        # message("Function: TADA_FlagResultUnit")
        
        showModal(modalDialog(title = "Run TADA_FlagResultUnit", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagResultUnit_poss(
          .data = reVal$WQP_FlagSpeciation,
          clean = "none"
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagResultUnit_temp
        }
        
        reVal$WQP_FlagResultUnit <- temp_dat
        
        reVal$WQP_FlagSpeciation <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagFraction
      observe({
        req(reVal$WQP_FlagResultUnit)
        
        # message("Function: TADA_FlagFraction")
        
        showModal(modalDialog(title = "Run TADA_FlagFraction", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagFraction_poss(
          .data = reVal$WQP_FlagResultUnit,
          clean = FALSE
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagFraction_temp
        }
        
        reVal$WQP_FlagFraction <- temp_dat
        
        reVal$WQP_FlagResultUnit <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagCoordinates
      observe({
        req(reVal$WQP_FlagFraction)
        
        # message("Function: TADA_FlagCoordinates")
        
        showModal(modalDialog(title = "Run TADA_FlagCoordinates", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagCoordinates_poss(
          .data = reVal$WQP_FlagFraction
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagCoordinates_temp
        }
        
        reVal$WQP_FlagCoordinates <- temp_dat
        
        reVal$WQP_FlagFraction <- NULL
        
        removeModal()
        
      })
      
      # TADA_FindQCActivities
      observe({
        req(reVal$WQP_FlagCoordinates)
        
        # message("Function: TADA_FindQCActivities")
        
        showModal(modalDialog(title = "Run TADA_FindQCActivities", 
                              footer = NULL))
        
        temp_dat <- TADA_FindQCActivities_poss(
          .data = reVal$WQP_FlagCoordinates
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FindQCActivities_temp
        }
        
        reVal$WQP_FindQCActivities <- temp_dat
        
        reVal$WQP_FlagCoordinates <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagMeasureQualifierCode
      observe({
        req(reVal$WQP_FindQCActivities)
        
        # message("Function: TADA_FlagMeasureQualifierCode")
        
        showModal(modalDialog(title = "Run TADA_FlagMeasureQualifierCode", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagMeasureQualifierCode_poss(
          .data = reVal$WQP_FindQCActivities
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagMeasureQualifierCode_temp
        }
        
        reVal$WQP_FlagMeasureQualifierCode <- temp_dat
        
        reVal$WQP_FindQCActivities <- NULL
        
        removeModal()
        
      })
      
      # TADA_FindPotentialDuplicatesSingleOrg
      observe({
        req(reVal$WQP_FlagMeasureQualifierCode)
        
        # message("Function: TADA_FindPotentialDuplicatesSingleOrg")
        
        showModal(modalDialog(title = "Run TADA_FindPotentialDuplicatesSingleOrg", 
                              footer = NULL))
        
        temp_dat <- TADA_FindPotentialDuplicatesSingleOrg_poss(
          .data = reVal$WQP_FlagMeasureQualifierCode
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FindPotentialDuplicatesSingleOrg_temp
        }
        
        reVal$WQP_FindPotentialDuplicatesSingleOrg <- temp_dat
        
        reVal$WQP_FlagMeasureQualifierCode <- NULL
        
        removeModal()
        
      })
      
      # TADA_FindPotentialDuplicatesMultipleOrgs
      observe({
        req(reVal$WQP_FindPotentialDuplicatesSingleOrg)
        
        # message("Function: TADA_FindPotentialDuplicatesMultipleOrgs")
        
        showModal(modalDialog(title = "Run TADA_FindPotentialDuplicatesMultipleOrgs", 
                              footer = NULL))
        
        temp_dat <- TADA_FindPotentialDuplicatesMultipleOrgs_poss(
          .data = reVal$WQP_FindPotentialDuplicatesSingleOrg
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FindPotentialDuplicatesMultipleOrgs_temp
        }
        
        reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- temp_dat
        
        reVal$WQP_FindPotentialDuplicatesSingleOrg <- NULL
        
        removeModal()
        
      })
      
      # TADA_FindQAPPApproval
      observe({
        req(reVal$WQP_FindPotentialDuplicatesMultipleOrgs)
        
        # message("Function: TADA_FindQAPPApproval")
        
        showModal(modalDialog(title = "Run TADA_FindQAPPApproval", 
                              footer = NULL))
        
        temp_dat <- TADA_FindQAPPApproval_poss(
          .data = reVal$WQP_FindPotentialDuplicatesMultipleOrgs
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FindQAPPApproval_temp
        }
        
        reVal$WQP_FindQAPPApproval <- temp_dat
        
        reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- NULL
        
        removeModal()
        
      })
      
      # TADA_FindQAPPDoc
      observe({
        req(reVal$WQP_FindQAPPApproval)
        
        # message("Function: TADA_FindQAPPDoc")
        
        showModal(modalDialog(title = "Run TADA_FindQAPPDoc", 
                              footer = NULL))
        
        temp_dat <- TADA_FindQAPPDoc_poss(
          .data = reVal$WQP_FindQAPPApproval
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FindQAPPDoc_temp
        }
        
        reVal$WQP_FindQAPPDoc <- temp_dat
        
        reVal$WQP_FindQAPPApproval <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagContinuousData
      observe({
        req(reVal$WQP_FindQAPPDoc)
        
        # message("Function: TADA_FlagContinuousData")
        
        showModal(modalDialog(title = "Run TADA_FlagContinuousData", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagContinuousData_poss(
          .data = reVal$WQP_FindQAPPDoc
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagContinuousData_temp
        }
        
        reVal$WQP_FlagContinuousData <- temp_dat
        
        reVal$WQP_FindQAPPDoc <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagAboveThreshold
      observe({
        req(reVal$WQP_FlagContinuousData)
        
        # message("Function: TADA_FlagAboveThreshold")
        
        showModal(modalDialog(title = "Run TADA_FlagAboveThreshold", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagAboveThreshold_poss(
          .data = reVal$WQP_FlagContinuousData
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagAboveThreshold_temp
        }
        
        reVal$WQP_FlagAboveThreshold <- temp_dat
        
        reVal$WQP_FlagContinuousData <- NULL
        
        removeModal()
        
      })
      
      # TADA_FlagBelowThreshold
      observe({
        req(reVal$WQP_FlagAboveThreshold)
        
        # message("Function: TADA_FlagBelowThreshold")
        
        showModal(modalDialog(title = "Run TADA_FlagBelowThreshold", 
                              footer = NULL))
        
        temp_dat <- TADA_FlagBelowThreshold_poss(
          .data = reVal$WQP_FlagAboveThreshold
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_FlagBelowThreshold_temp
        }
        
        reVal$WQP_FlagBelowThreshold <- temp_dat
        
        reVal$WQP_FlagAboveThreshold <- NULL
        
        removeModal()
        
      })
      
      # TADA_HarmonizeSynonyms
      observe({
        req(reVal$WQP_FlagBelowThreshold)
        
        # message("Function: TADA_HarmonizeSynonyms")
        
        showModal(modalDialog(title = "Run TADA_HarmonizeSynonyms", 
                              footer = NULL))
        
        temp_dat <- TADA_HarmonizeSynonyms_poss(
          .data = reVal$WQP_FlagBelowThreshold, ref = ref_harm
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_HarmonizeSynonyms_temp
        }
        
        reVal$WQP_HarmonizeSynonyms <- temp_dat
        
        reVal$WQP_FlagBelowThreshold <- NULL
        
        removeModal()
        
      })
      
      # TADA_depth_combine
      observe({
        req(reVal$WQP_HarmonizeSynonyms)
        
        showModal(modalDialog(title = "Combine depth columns.", 
                              footer = NULL))
        
        temp_dat <- TADA_depth_combine_poss(
          TADA_dat = reVal$WQP_HarmonizeSynonyms
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_depth_combine_temp
        }
        
        reVal$WQP_depth_combine <- temp_dat
        
        reVal$WQP_HarmonizeSynonyms <- NULL
        
        removeModal()
        
      })
      
      # TADA_join_tribal
      observe({
        req(reVal$WQP_depth_combine)
        
        showModal(modalDialog(title = "Join Tribal Information.", 
                              footer = NULL))
        
        temp_dat <- TADA_join_tribal_poss(
          TADA_dat = reVal$WQP_depth_combine,
          tribal_sf = MT_tribal
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_join_tribal_temp
        }
        
        reVal$WQP_join_tribal <- temp_dat
        
        reVal$WQP_depth_combine <- NULL
        
        removeModal()
        
      })
      
      # Select the correct columns to create WQP_dat2
      observe({
        req(reVal$WQP_join_tribal)
        
        temp_dat <- reVal$WQP_join_tribal %>%
          TADA_selector_poss()
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_selector_temp
        }
        
        if (!validate_fun(temp_dat, WQP_cols)){
          temp_dat <- bind_rows(TADA_selector_temp, temp_dat) %>%
            get_vars(vars = WQP_cols)
        }
        
        reVal$WQP_join_tribal <- NULL
        
        reVal$WQP_dat2 <- temp_dat
        
      })
      
      # Filter the data for the following criteria
      # TADA.ActivityType.Flag: Non_QC
      # TADA.MethodSpeciation.Flag: Remove "Rejected"
      # TADA.ResultMeasureValueDataTypes.Flag: Remove "Text" and "NA - Not Available"
      # TADA.ResultMeasureValue: Remove NA
      # TADA.ResultValueAboveUpperThreshold.Flag: Remove "Suspect"
      # TADA.ResultValueBelowLowerThreshold.Flag: Remove "Suspect"
      # TADA.ResultUnit.Flag: Remove "Rejected"
      # TADA.AnalyticalMethod.Flag: Remove "Invalid"
      # TADA.MeasureQualifierCode.Flag: Remove "Suspect"
      
      observe({
        req(reVal$WQP_dat2)
        
        # message("Filter the data for the following criteria:")
        # message("TADA.ActivityType.Flag: Non_QC")
        # message('TADA.MethodSpeciation.Flag: Remove "Rejected"')
        # message('TADA.ResultMeasureValueDataTypes.Flag: Remove "Text" and "NA - Not Available"')
        # message('TADA.ResultMeasureValue: Remove NA')
        # message('TADA.ResultValueAboveUpperThreshold.Flag: Remove "Suspect"')
        # message('TADA.ResultValueBelowLowerThreshold.Flag: Remove "Suspect"')
        # message('TADA.ResultUnit.Flag: Remove "Rejected"')
        # message('TADA.AnalyticalMethod.Flag: Remove "Invalid"')
        # message('TADA.MeasureQualifierCode.Flag: Remove "Suspect"')
        # 
        # sink(NULL)
        
        reVal$WQP_dat3 <- reVal$WQP_dat2 %>% QA_filter()
        reVal$WQP_time <- Sys.time()
        
        # Prepare a data frame for the Join AU tab
        observe({
          req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
          
          if (validate_fun(reVal$WQP_dat3 , WQP_cols)){
            data_storage$AU_latest_upload <- "Data downloaded from WQP"
            data_storage$AU_time <- reVal$WQP_time
            data_storage$AU_latest_data <- reVal$WQP_dat3
          }
          
          reVal$min_date2 <- min(reVal$WQP_dat3$ActivityStartDate)
          reVal$max_date2 <- max(reVal$WQP_dat3$ActivityStartDate)
          reVal$site_number2 <- length(unique(reVal$WQP_dat3$MonitoringLocationIdentifier))
          reVal$sample_size2 <- nrow(reVal$WQP_dat3)
          
        })
      })
      
      # Filter the data for the QC data
      observe({
        req(reVal$WQP_dat3)
        
        reVal$WQP_QC <- reVal$WQP_dat3
        
      })
      
      # Display data information after data cleaning
      observe({
        req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
        output$Date_Info2 <- renderText({
          req(reVal$WQP_dat3, nrow(reVal$WQP_dat3) > 0)
          paste0("There are ", reVal$sample_size2, " records from ", 
                 reVal$site_number2, " sites after data cleaning.")
        })
      })
      
      observe({
        req(reVal$WQP_dat2, input$clean_summary)
        output$clean_summary_table <- renderDT({
          tt <- count_fun(reVal$WQP_dat2, input$clean_summary)
          datatable(tt, options = list(scrollX = TRUE)) %>%
            formatPercentage(columns = "Percent (%)", digits = 2)
        })
      })
      
    }, 
    blocking_level = "error")
    
  })
  
  SANDS_check <- reactive({
    if (input$SANDS_check | input$SANDS_check2){
      return(TRUE)
    }
  })
  
  ### Create SANDS format
  SANDS_temp <- reactive({
    req(#input$SANDS_check, length(input$MT_par_group) > 0,
        SANDS_check(),
        data_storage$AU_latest_data,
        nrow(data_storage$AU_latest_data) > 0)
    temp_dat <- data_storage$AU_latest_data %>% original_name(WQX = WQX)
    
    return(temp_dat)
  })
  
  # Clean up the SANDS data
  observe({
    req(#input$SANDS_check, length(input$MT_par_group) > 0,
        SANDS_check(),
        data_storage$AU_latest_data, 
        nrow(data_storage$AU_latest_data) > 0,
        SANDS_temp())
    shinyCatch({
      # Create Site, Result, and ResultPhysChem data
      observe({
        # Create Site data
        reVal$site_temp <- SANDS_temp() %>% TADA_site(ref_col = names(site_template))
        
        # Create Result data
        reVal$result_temp <- SANDS_temp() %>% TADA_result(ref_col = names(result_template))
        
        resultPhysChem_col <- c(names(resultPhysChem_template),
                                "ResultMeasureValue_Ori",
                                "DetectionQuantitationLimitMeasure.MeasureValue_Ori")
        
        # Create ResultPhysChem data
        reVal$resultphyschem_temp <- SANDS_temp() %>% 
          TADA_resultPhysChem(ref_col = resultPhysChem_col)
      })
      
      observe({
        req(reVal$site_temp, nrow(reVal$site_temp) > 0,
            reVal$result_temp, nrow(reVal$result_temp) > 0)
        reVal$site_temp2 <- reVal$site_temp %>% 
          SANDS_clean_site(ref = STATIONS, state_county = state_county)
        reVal$result_temp2 <- reVal$result_temp %>% 
          SANDS_clean_result(ref = RESULTS)
      })
    },
    blocking_level = "error")
  })
  
  # Create summary and each tab
  observe({
    req(reVal$site_temp, nrow(reVal$site_temp) > 0,
        reVal$result_temp, nrow(reVal$result_temp) > 0,
        reVal$resultphyschem_temp, nrow(reVal$resultphyschem_temp) > 0,
        SANDS_temp())
    shinyCatch({
      # Prepare Summary dataset
      showModal(modalDialog(title = "Summarizing the data...", footer = NULL))
      
      reVal$summary_temp_part1 <- SANDS_create_summary_part1(
        sites = reVal$site_temp,
        results = reVal$result_temp,
        resultphyschem = reVal$resultphyschem_temp,
        ref = SUMMARY,
        state_county = state_county
      )
      
      reVal$summary_temp <- SANDS_create_summary_part2(
        reVal$summary_temp_part1,
        ref = SUMMARY
      )
      
      reVal$h20_tab <- H2O_tab(
        x = reVal$summary_temp_part1,
        address_dat = metals_h2o_head_long,
        lookup = metals_h2o_lookup,
        header = metals_h2o_head,
        TADA_file = SANDS_temp()
      )
      reVal$h20_GIS_tab <- GIS_output_metal_h2o(reVal$h20_tab)
      
      reVal$sed_tab <- sed_tab(
        x = reVal$summary_temp_part1,
        address_dat = metals_sed_head_long,
        lookup = metals_sed_lookup,
        header = metals_sed_head
      )
      reVal$sed_GIS_tab <- GIS_output_metal_sed(reVal$sed_tab)
      
      reVal$salinity_tab <- salinity_tab(
        x = reVal$summary_temp_part1,
        address_dat = salinity_head_long,
        lookup = salinity_lookup,
        header = salinity_head,
        TADA_file = SANDS_temp()
      )
      reVal$salinity_GIS_tab <- GIS_output_salinity(reVal$salinity_tab)
      
      reVal$oil_tab <- oil_tab(
        x = reVal$summary_temp_part1,
        address_dat = oil_head_long,
        lookup = Oil_lookup,
        header = oil_head
      )
      reVal$oil_GIS_tab <- GIS_output_oil(reVal$oil_tab)
      
      reVal$nutrients_tab <- nutrients_tab(
        x = reVal$summary_temp_part1,
        address_dat = nutrients_head_long,
        lookup_main = nutrients_lookup,
        lookup2_spec = nutrients_special_spec,
        resultphyschem_dat = reVal$resultphyschem_temp,
        header = nutrients_head,
        TADA_file = SANDS_temp()
      )
      reVal$nutrients_GIS_tab <- GIS_output_nutrient(reVal$nutrients_tab)
      
      # A note to show data are ready to download
      reVal$SANDS_download_ready <- TRUE

      reVal$sand_create_time <- Sys.time()
      
      removeModal()
    },
    blocking_level = "error")
  })
  
  ### Save the data
  output$data_download <- downloadHandler(
    filename = function() {
      paste0("WQP_data_", Sys.Date(), '.csv')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(reVal$WQP_dat2, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  output$data_download_QC <- downloadHandler(
    filename = function() {
      paste0("WQP_data_", Sys.Date(), '_QC.csv')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(reVal$WQP_QC, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  # Download the data
  output$SANDS_data_download <- downloadHandler(
    filename = function() {
      req(reVal$site_temp2, reVal$result_temp2, reVal$resultphyschem_temp,
          reVal$summary_temp, input$MT_par_group)
      paste0("WQP_data_SANDS_", Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      req(reVal$site_temp2, reVal$result_temp2, reVal$resultphyschem_temp,
          reVal$summary_temp)
      
      showModal(modalDialog(title = "Saving the ZIP...", footer = NULL))
      
      file_names <- character()
      
      if (nrow(reVal$site_temp2) > 0){
        
        write_csv(reVal$site_temp2, file.path(tempdir(), "STATIONS.csv"), na = "")
        
        file_names["STATIONS"] <- "STATIONS.csv"
      }
      
      if (nrow(reVal$result_temp2) > 0){
        
        write_csv(reVal$result_temp2, file.path(tempdir(), "RESULTS.csv"), na = "")
        
        file_names["RESULTS"] <- "RESULTS.csv"
        
      }
      
      if (nrow(reVal$summary_temp) > 0){
        
        write_csv(reVal$summary_temp, file.path(tempdir(), "SUMMARY.csv"), na = "")
        
        file_names["SUMMARY"] <- "SUMMARY.csv"
        
      }
      
      if (nrow(reVal$resultphyschem_temp) > 0){
        
        write_csv(reVal$resultphyschem_temp, file.path(tempdir(), "ResultPhysChem.csv"), na = "")
        
        file_names["ResultPhysChem"] <- "ResultPhysChem.csv"
        
      }
      
      if ("nutrients" %in% input$MT_par_group & is.data.frame(reVal$nutrients_tab)){
        if (nrow(reVal$nutrients_tab) > 0){
          
          write_csv(reVal$nutrients_tab, file.path(tempdir(), "Nutrients.csv"), na = "")
          
          file_names["Nutrients"] <- "Nutrients.csv"
          
          write_csv(reVal$nutrients_GIS_tab, file.path(tempdir(), "Nutrients_GIS.csv"), na = "")
          
          file_names["Nutrients_GIS"] <- "Nutrients_GIS.csv"
          
        }
      }
      
      if ("metal_w" %in% input$MT_par_group & is.data.frame(reVal$h20_tab)){
        if (nrow(reVal$h20_tab) > 0){
          
          write_csv(reVal$h20_tab, file.path(tempdir(), "Metals_H2O.csv"), na = "")
          
          file_names["Metals(H2O)"] <- "Metals_H2O.csv"
          
          write_csv(reVal$h20_GIS_tab, file.path(tempdir(), "Metals_H2O_GIS.csv"), na = "")
          
          file_names["Metals(H2O)_GIS"] <- "Metals_H2O_GIS.csv"
          
        }
      }
      
      if ("metal_s" %in% input$MT_par_group & is.data.frame(reVal$sed_tab)){
        if (nrow(reVal$sed_tab) > 0){
          
          write_csv(reVal$sed_tab, file.path(tempdir(), "Metals_Sediment.csv"), na = "")
          
          file_names["Metals(Sediment)"] <- "Metals_Sediment.csv"
          
          write_csv(reVal$sed_GIS_tab, file.path(tempdir(), "Metals_Sediment_GIS.csv"), na = "")
          
          file_names["Metals(Sediment)_GIS"] <- "Metals_Sediment_GIS.csv"
          
        }
      }
      
      if ("salinity" %in% input$MT_par_group & is.data.frame(reVal$salinity_tab)){
        if (nrow(reVal$salinity_tab) > 0){
          
          write_csv(reVal$salinity_tab, file.path(tempdir(), "Salinity.csv"), na = "")
          
          file_names["Salinity"] <- "Salinity.csv"
          
          write_csv(reVal$salinity_GIS_tab, file.path(tempdir(), "Salinity_GIS.csv"), na = "")
          
          file_names["Salinity_GIS"] <- "Salinity_GIS.csv"
          
        }
      }
      
      if ("oil_gas" %in% input$MT_par_group & is.data.frame(reVal$oil_tab)){
        if (nrow(reVal$oil_tab) > 0){
          
          write_csv(reVal$oil_tab, file.path(tempdir(), "Oil_Gas.csv"), na = "")
          
          file_names["Oil_Gas"] <- "Oil_Gas.csv"
          
          write_csv(reVal$oil_GIS_tab, file.path(tempdir(), "Oil_Gas_GIS.csv"), na = "")
          
          file_names["Oil_Gas_GIS"] <- "Oil_Gas_GIS.csv"
          
        }
      }
      
      zip::zip(con, files = map_chr(file_names, function(x) file.path(tempdir(), x)),
               mode = "cherry-pick")
      
      removeModal()
    }
  )
  
  output$data_download_bio <- downloadHandler(
    filename = function() {
      paste0("WQP_data_", Sys.Date(), '_macro_count.csv')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(reVal$bio_dat2, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  output$data_download_selected_AN <- downloadHandler(
    filename = function() {
      paste0("WQP_data_selected_AN_", Sys.Date(), '.csv')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(plot_dat(), file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  output$data_download_AN <- downloadHandler(
    filename = function() {
      paste0("WQP_data_AN_", Sys.Date(), '.csv')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(AN_final_dat(), file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  output$data_download_SU <- downloadHandler(
    filename = function() {
      paste0("WQP_data_SU_", Sys.Date(), '.csv')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(SU_final_dat(), file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  observe({
    reVal$WQP_dat <- NULL
    reVal$bio_dat <- NULL
    reVal$bio_dat2 <- NULL
    reVal$download_date <- NULL
    reVal$min_date <- NULL
    reVal$max_date <- NULL
    reVal$site_number <- NULL
    reVal$sample_size <- NULL
    reVal$WQP_AutoClean <- NULL
    reVal$WQP_SimpleCensoredMethods <- NULL
    reVal$WQP_FlagMethod <- NULL
    reVal$WQP_FlagSpeciation <- NULL
    reVal$WQP_FlagResultUnit <- NULL
    reVal$WQP_FlagFraction <- NULL
    reVal$WQP_FlagCoordinates <- NULL
    reVal$WQP_FindQCActivities <- NULL
    reVal$WQP_FlagMeasureQualifierCode <- NULL
    reVal$WQP_FindPotentialDuplicatesSingleOrg <- NULL
    reVal$WQP_FindPotentialDuplicatesMultipleOrgs <- NULL
    reVal$WQP_FindQAPPApproval <- NULL
    reVal$WQP_FindQAPPDoc <- NULL
    reVal$WQP_FindContinuousData <- NULL
    reVal$WQP_FlagAboveThreshold <- NULL
    reVal$WQP_FlagBelowThreshold <- NULL
    reVal$WQP_HarmonizeSynonyms <- NULL
    reVal$WQP_depth_combine <- NULL
    reVal$WQP_join_tribal <- NULL
    reVal$WQP_dat2 <- NULL
    reVal$WQP_time <- NULL
    reVal$WQP_dat3 <- NULL
    reVal$WQP_QC <- NULL
    reVal$site_temp <- NULL
    reVal$result_temp <- NULL
    reVal$resultphyschem_temp <- NULL
    reVal$site_temp2 <- NULL
    reVal$result_temp2 <- NULL
    reVal$summary_temp_part1 <- NULL
    reVal$summary_temp <- NULL
    reVal$h20_tab <- NULL
    reVal$h20_GIS_tab <- NULL
    reVal$sed_tab <- NULL
    reVal$sed_GIS_tab <- NULL
    reVal$salinity_tab <- NULL
    reVal$salinity_GIS_tab <- NULL
    reVal$oil_tab <- NULL
    reVal$oil_GIS_tab <- NULL
    reVal$nutrients_tab <- NULL
    reVal$nutrients_GIS_tab <- NULL
    reVal$SANDS_download_ready <- FALSE

    bbox_reVal$w_num <- NULL
    bbox_reVal$s_num <- NULL
    bbox_reVal$e_num <- NULL
    bbox_reVal$n_num <- NULL

    par_value$variable <- NULL

    HUC_list$Selected <- character(0)
  }) %>%
    bindEvent(input$Reset)

  observeEvent(input$Reset,{
    reset("download_se")
    reset("par_group_se")
    reset("sitetype_se")
    reset("CharGroup_se")
    reset("SANDS_check")
    reset("MT_par_group")
    reset("biological_check")
    reset("par_se")
    reset("par_date")
    # reset("area_se")
    reset("State_se")
    reset("HUC_se")
    reset("bb_W")
    reset("bb_N")
    reset("bb_S")
    reset("bb_E")
    updateRadioGroupButtons(session = session,
                            inputId = "area_se", selected = "State")
  })

  observe({
    AU_Val$upload_time <- NULL
    AU_Val$WQP_AU <- NULL
    AU_Val$site_number <- NULL
    AU_Val$crosswalk_time <- NULL
    AU_Val$WQP_dat2 <- NULL
    AU_Val$WQP_time <- NULL
    AU_Val$df_MonLocAU_4Map <- NULL
    AU_Val$SANDS_download_ready <- FALSE
    AU_Val$site_temp <- NULL
    AU_Val$summary_temp <- NULL
    
    data_storage$AU_latest_upload <- NULL
    data_storage$AU_time <- NULL
    data_storage$AU_latest_data <- NULL
    
  }) %>%
    bindEvent(input$Reset2)

  observeEvent(input$Reset2,{
    reset("file_AU")
    reset("file_crosswalk")
    reset("crosswalk_upload")
    reset("input_site_choice")
    reset("SANDS_check2")
    reset("MT_par_group2")
  })

  observe({
    SU_Val$WQP_dat <- NULL
    SU_Val$min_date2 <- NULL
    SU_Val$max_date2 <- NULL
    SU_Val$site_number2 <- NULL
    SU_Val$sample_size2 <- NULL
    SU_Val$WQP_dat2 <- NULL
    SU_Val$WQP_dat3 <- NULL
    SU_Val$site_select <- NULL
    SU_Val$WQP_time <- NULL
    SU_Val$upload_time <- NULL
    
    SANDS_Val$fact_sheet_create <- FALSE
    SANDS_Val$WQP_dat_fact <- NULL
    SANDS_Val$fact_sheet <- NULL
    SANDS_Val$fact_sheet_save <- FALSE
    
    data_storage$SU_latest_upload <- NULL
    data_storage$SU_time <- NULL
    data_storage$SU_latest_data <- NULL
    
  }) %>%
    bindEvent(input$Reset3)

  observeEvent(input$Reset3,{
    reset("file_SU")
    reset("sites_or_AU_SU")
    reset("all_fractions_SU")
    reset("fraction_combined_SU")
    reset("Site_AU_fact_SANDS")
  })
  
  observe({

    AN_Val$min_date2 <- NULL
    AN_Val$max_date2 <- NULL
    AN_Val$site_number2 <- NULL
    AN_Val$sample_size2 <- NULL
    AN_Val$WQP_dat2 <- NULL
    AN_Val$WQP_dat3 <- NULL
    AN_Val$ex <- NULL
    AN_Val$ex_sum_site <- NULL
    AN_Val$ex_sum_AU <- NULL
    AN_Val$site_summary <- NULL
    AN_Val$AU_summary <- NULL
    AN_Val$WQP_site <- NULL
    AN_Val$site_select <- NULL
    AN_Val$upload_time <- NULL
    
    data_storage$AN_latest_upload <- NULL
    data_storage$AN_time <- NULL
    data_storage$AN_latest_data <- NULL
    
  }) %>%
    bindEvent(input$Reset4)
  
  observeEvent(input$Reset4,{
    reset("file")
    reset("SU_filter")
    reset("sites_or_AU_AN")
    reset("all_sites_AU")
    reset("site_ID_se")
    reset("slider_ex")
    reset("time_log_trans")
    reset("time_group")
    reset("box_log_trans")
    reset("box_group")
  })
  
  observeEvent(input$Reset5,{
    reset("Site_AU_fact_SANDS")
    reset("total_convert")
  })
  
  ### The Analysis tab
  
  AN_Val <- reactiveValues()
  
  # File upload indicator
  upload_dat_AN <- reactive({
    req(input$file_AN)
    
    if (str_detect(input$file_AN$name, "\\.csv$")){
      temp <- read_csv(input$file_AN$datapath)
    } else {
      temp <- NULL
    }
    
    AN_Val$upload_time <- Sys.time()
    
    return(temp)
  })
  
  # Replace reVal$WQP_dat2 with upload_dat() if upload_dat() meets the requirements
  observe({
    req(upload_dat_AN())
    
    if (validate_fun(upload_dat_AN() , AN_cols)){
      data_storage$AN_latest_upload <- "Upload from the Data Analysis tab"
      data_storage$AN_time <- AN_Val$upload_time
      data_storage$AN_latest_data <- upload_dat_AN()
    }
  })
  
  output$file_indicator_AN <- renderText({
    if (is.null(data_storage$AN_latest_upload)){
      temp_text <- "Require downloading data or uploading a CSV file with the data."
    } else {
      temp_text <- paste0(data_storage$AN_latest_upload, " at ", 
                          round(data_storage$AN_time))
    }
    return(temp_text)
  })
  
  # Display data information after data cleaning
  observe({
    req(data_storage$AN_latest_data, nrow(data_storage$AN_latest_data) > 0)
    
    AN_Val$min_date2 <- min(data_storage$AN_latest_data$ActivityStartDate)
    AN_Val$max_date2 <- max(data_storage$AN_latest_data$ActivityStartDate)
    AN_Val$site_number2 <- length(unique(data_storage$AN_latest_data$MonitoringLocationIdentifier))
    AN_Val$sample_size2 <- nrow(data_storage$AN_latest_data)
    
    output$Date_Info_AN <- renderText({
      paste0("There are ", AN_Val$sample_size2, " records from ", 
             AN_Val$site_number2, " sites.")
    })
  })
  
  # Update site ID
  site_ID_list <- reactive({
    req(data_storage$AN_latest_data, nrow(data_storage$AN_latest_data) > 0)
    site_ID <- sort(unique(data_storage$AN_latest_data$MonitoringLocationIdentifier))
    return(site_ID)
  })
  
  AU_ID_list <- reactive({
    req(data_storage$AN_latest_data, nrow(data_storage$AN_latest_data) > 0)
    AU_ID <- sort(unique(data_storage$AN_latest_data$AU_ID))
    return(AU_ID)
  })
  
  
  observeEvent(input$all_sites_AU, {
    req(data_storage$AN_latest_data, nrow(data_storage$AN_latest_data) > 0,
        site_ID_list(), AU_ID_list())

    if (input$sites_or_AU_AN %in% "Site"){
      updateSelectizeInput(session = session, inputId = "site_ID_se",
                           choices = site_ID_list(), selected = character(0))
    } else if (input$sites_or_AU_AN %in% "AU"){
      updateSelectizeInput(session = session, inputId = "site_ID_se",
                           choices = AU_ID_list(), selected = character(0))
    }
  })
  
  observeEvent(input$sites_or_AU_AN, {
    req(data_storage$AN_latest_data, nrow(data_storage$AN_latest_data) > 0,
        site_ID_list(), AU_ID_list())
    
    if (input$sites_or_AU_AN %in% "Site"){
      updateSelectizeInput(session = session, inputId = "site_ID_se",
                           choices = site_ID_list(), selected = character(0))
    } else if (input$sites_or_AU_AN %in% "AU"){
      updateSelectizeInput(session = session, inputId = "site_ID_se",
                           choices = AU_ID_list(), selected = character(0))
    }
  })
  
  # Show type definition
  observeEvent(input$type_ex, {
    showModal(modalDialog(
      strong("Type 1:"),
      p("The criteria is only evaluated when pH is between 6.5 to 9.0."),
      strong("Type 2:"),
      p("The criteria applies except where the receiving water after mixing has a pH >= 7.0 and a CaCO3 hardness >= 50 mg/L, where the 750 µg/L acute criterion will apply."),
      strong("Type 3:"),
      p("The criteria is dynamically based on the hardness."),
      strong("Type 4:"),
      p("The criteria is a fixed value."),
      strong("Type 5:"),
      p("The criteria is dynamically based on pH."),
      strong("Type 6:"),
      p("The criteria is dynamically based on pH and temperature."),
      strong("Type 7:"),
      p("No criteria information."),
      title = "The Criteria Type Definition ", footer = NULL, easyClose = TRUE))
  })
  
  ### Calculate the criteria
  
  observe({
    req(data_storage$AN_latest_data, nrow(data_storage$AN_latest_data) > 0)
    
    dat <- data_storage$AN_latest_data
    
    # Add pH, temperature, and Hardness data
    dat <- dat %>%
      column_name_change3()
      
    # Filter the data based on SU_filter
    if (input$SU_filter %in% "Report site with any sufficient data"){
      dat <- dat %>%
        fsubset(str_detect(Site_Sufficiency_Notes, fixed("Sufficient")) | 
                  Site_Sufficiency_Notes %in% "Not Required")
    } else if (input$SU_filter %in% "Report AU with any sufficient data"){
      dat <- dat %>%
        fsubset(str_detect(AU_Sufficiency_Notes, fixed("Sufficient")) |
                  AU_Sufficiency_Notes %in% "Not Required")
    } else {
      dat <- dat
    }
      
    # Filter the data based on the sufficiency test result
    AN_Val$WQP_dat2 <- dat
    
  })
  
  observe({
    req(AN_Val$WQP_dat2)
    # Filter the site ID
    if (!input$all_sites_AU){
      if (input$sites_or_AU_AN %in% "Site"){
        dat <- AN_Val$WQP_dat2 %>%
          fsubset(MonitoringLocationIdentifier %in% input$site_ID_se)

      } else if (input$sites_or_AU_AN %in% "AU"){
        dat <- AN_Val$WQP_dat2 %>%
          fsubset(AU_ID %in% input$site_ID_se)
      }
    } else {
    dat <- AN_Val$WQP_dat2
    }
    AN_Val$WQP_dat3 <- dat
  })
  
  observe({
    # req(AN_Val$WQP_dat2, nrow(AN_Val$WQP_dat2) > 0)
    req(AN_Val$WQP_dat2, nrow(AN_Val$WQP_dat2) > 0)
    
    dat <- AN_Val$WQP_dat3
    
    # Join criteria table
    dat <- dat %>%
      criteria_join(criteria_table)

    # Separate the data with and without criteria
    dat_criteria <- dat %>% 
      fsubset(!is.na(Use) & !is.na(Details)) 
    
    dat_no_criteria <- dat %>% 
      fsubset(is.na(Use) & is.na(Details)) %>%
      fmutate(Criteria_Upper = NA, 
              Criteria_Lower = NA,
              Type = 7)
    
    # Separate the data based on pH and Hardness_Notes
    group132 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 1 & Hardness_Notes %in% 3 & Temp_Notes %in% 2)
    group222 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 2 & Hardness_Notes %in% 2 & Temp_Notes %in% 2)
    group312 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 3 & Hardness_Notes %in% 1 & Temp_Notes %in% 2)
    group332 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 3 & Hardness_Notes %in% 3 & Temp_Notes %in% 2)
    group431 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 4 & Hardness_Notes %in% 3 & Temp_Notes %in% 1)
    group432 <- dat_criteria %>% 
      fsubset(pH_Notes %in% 4 & Hardness_Notes %in% 3 & Temp_Notes %in% 2)
    
    # Group 132: pH 6.5-9.0 only
    
    # If pH < 6.5 or pH > 9, do not calculate the criteria
    group132_2 <- group132 %>%
      fmutate(Criteria_Upper = ifelse(pH >= 6.5 & pH <= 9, Magnitude_Upper, NA_real_)) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 1)
    
    # Group 222: "Applies except where the receiving water after mixing has 
    # a pH >= 7.0 and a CaCO3 hardness >= 50 mg/L, where the 750 µg/L acute criterion will apply."
    
    # If pH < 7 and hardness >= 50, use 750 ug/L as the criteria
    group222_2 <- group222 %>%
      fmutate(Criteria_Upper = case_when(
        pH >= 7 & Hardness >= 50                  ~750,
        is.na(pH) | is.na(Hardness)               ~NA_real_,
        TRUE                                      ~Magnitude_Upper
      )) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 2)
    
    group312_2 <- group312 %>%
      left_join(Equation_table %>% fselect(-Fraction), 
                by = c("StateName" = "State", 
                       "StateAbbrev",
                       "Standard_Name" = "Constituent",
                       # "Standard_Fraction" = "Fraction",
                       "Use", "Details")) %>%
      fmutate(Criteria_Upper = pmap_dbl(list(hardness = Hardness, 
                                             CF_A = CF_A, 
                                             CF_B = CF_B, 
                                             CF_C = CF_C, 
                                             E_A = E_A, 
                                             E_B = E_B), .f = hardness_criteria_fun)) %>%
      select(-(CF_A:E_B)) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>% 
      fmutate(Type = 3)
    
    # group332: The threshold is in the Magnitude
    group332_2 <- group332 %>% 
      fmutate(Criteria_Upper = Magnitude_Upper) %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 4)
    
    # group431: The threshold calculation needs pH and temperature
    group431_2 <- group431 %>%
      fmutate(Criteria_Upper = case_when(
        # Details %in% "Acute, salmonids present"              ~CMC_criteria_fun(pH, salmonid = TRUE),
        # Details %in% "Acute, salmonids absent"               ~CMC_criteria_fun(pH, salmonid = FALSE),
        Details %in% "Chronic, early life stages present"    ~CCC_criteria_fun(pH, Temperature, earlylife = TRUE),
        Details %in% "Chronic, early life stages absent"     ~CCC_criteria_fun(pH, Temperature, earlylife = FALSE),
        TRUE                                                 ~NA_real_
      ))  %>%
      fmutate(Criteria_Lower = Magnitude_Lower) %>%
      fmutate(Type = 5)
    
    # group432: The threshold calculation needs pH
    group432_2 <- group432 %>%
      fmutate(Criteria_Upper = case_when(
        Details %in% "Acute, salmonids present"              ~CMC_criteria_fun(pH, salmonid = TRUE),
        Details %in% "Acute, salmonids absent"               ~CMC_criteria_fun(pH, salmonid = FALSE),
        # Details %in% "Chronic, early life stages present"    ~CCC_criteria_fun(pH, Temperature, earlylife = TRUE),
        # Details %in% "Chronic, early life stages absent"     ~CCC_criteria_fun(pH, Temperature, earlylife = FALSE),
        TRUE                                                 ~NA_real_
      ))  %>%
      fmutate(Type = 6)
    
    # Combine group13_2, group22_2, group31_2, and group33_2
    group_criteria <- rbindlist(list(group132_2, group222_2, group312_2, group332_2, group431_2, group432_2),
                                fill = TRUE)
    
    dat_ex <- group_criteria %>% exceedance_fun()
    
    dat_ex_sum_site <- dat_ex %>% exceedance_cal(site_flag = "Site")
    dat_ex_sum_AU <- dat_ex %>% 
      dplyr::select(-LongitudeMeasure, -LatitudeMeasure) %>%
      exceedance_cal(site_flag = "AU")
    
    # A function to prepare the site table for data without criteria
    dat_no_ex <- dat_no_criteria %>% exceedance_fun()
    dat_no_ex_sum_site <- dat_no_ex %>% exceedance_cal(site_flag = "Site")
    dat_no_ex_sum_AU <- dat_no_ex %>% exceedance_cal(site_flag = "AU")
    
    ex <- rbindlist(list(dat_ex, dat_no_ex))
    ex_sum_site <- rbindlist(list(dat_ex_sum_site, dat_no_ex_sum_site))
    ex_sum_AU <- rbindlist(list(dat_ex_sum_AU, dat_no_ex_sum_AU)) %>% 
      left_join(AU_centroids, by = c("AU_ID", "AU_NAME"))
    
    # Save the data
    AN_Val$ex <- ex
    AN_Val$ex_sum_site <- ex_sum_site
    AN_Val$ex_sum_AU <- ex_sum_AU
    
  })
  
  # # Create a reactive object for ex_sum
  # ex_sum_re <- reactive({
  #   req(AN_Val$ex_sum_site, AN_Val$ex_sum_AU)
  #   if (input$sites_or_AU_AN %in% "Site"){
  #     temp_dat <- AN_Val$ex_sum_site
  #   } else if (input$sites_or_AU_AN %in% "AU"){
  #     temp_dat <- AN_Val$ex_sum_AU
  #   }
  #   return(temp_dat)
  # })
  
  # Calculate the site summary
  observe({
    req(AN_Val$ex_sum_site)
    dat_criteria <- AN_Val$ex_sum_site %>% fsubset(!Type %in% 7)
    dat_no_criteria <- AN_Val$ex_sum_site %>% fsubset(Type %in% 7)
    
    site_criteria_summary <- dat_criteria %>% 
      criteria_summary(site_flag = "Site")
    site_no_criteria_summary <- dat_no_criteria %>%
      criteria_no_summary(site_flag = "Site")
    
    site_summary <- rbindlist(list(site_criteria_summary, 
                                   site_no_criteria_summary), fill = TRUE)
    
    AN_Val$site_summary <- site_summary
    
  })
  
  # Calculate the AU summary
  observe({
    req(AN_Val$ex_sum_AU)
    dat_criteria <- AN_Val$ex_sum_AU %>% fsubset(!Type %in% 7)
    dat_no_criteria <- AN_Val$ex_sum_AU %>% fsubset(Type %in% 7)
    
    AU_criteria_summary <- dat_criteria %>% 
      criteria_summary(site_flag = "AU")
    AU_no_criteria_summary <- dat_no_criteria %>%
      criteria_no_summary(site_flag = "AU")
    
    AU_summary <- rbindlist(list(AU_criteria_summary, 
                                 AU_no_criteria_summary), fill = TRUE)
    
    AN_Val$AU_summary <- AU_summary
    
  })
  
  # Create a reactive object for site_ or AU_summary
  loc_summary <- reactive({
    req(AN_Val$site_summary, AN_Val$AU_summary)
    if (input$sites_or_AU_AN %in% "Site"){
      temp_dat <- AN_Val$site_summary
    } else if (input$sites_or_AU_AN %in% "AU"){
      temp_dat <- AN_Val$AU_summary
    }
    return(temp_dat)
  })
  
  # Present the table as a DT
  output$Par_Select <- renderDT({
    req(loc_summary())
    dat <- loc_summary() %>%
      fselect(-Standard_Unit) %>%
      fmutate(Standard_Name = as.factor(Standard_Name),
              Standard_Fraction = as.factor(Standard_Fraction),
              Use = factor(Use, levels = c("Aquatic Life", "Human Health", "Trigger Value", "Recretion", "All")), 
              Details = factor(Details, levels = sort(unique(criteria_table$Details))),
              Type = factor(Type, levels = 1:7),
              Frequency = as.factor(Frequency),
              Duration = as.factor(Duration)) %>%
      relocate(Use, .before = Details)
    
    if (input$sites_or_AU_AN %in% "Site"){
      col_name <- c("Parameter", "Fraction", 
                    "Use", "Details", "Type", 
                    "Frequency", "Duration",
                    "No. Site", "No. Site Exceedance", 
                    "Exceedance Percentage")
    } else if (input$sites_or_AU_AN %in% "AU"){
      col_name <- c("Parameter", "Fraction", 
                    "Use", "Details", "Type", 
                    "Frequency", "Duration",
                    "No. AU", "No. AU Exceedance", 
                    "Exceedance Percentage")
    }
    
    datatable(dat,
              filter = "top", extensions = "Buttons",
              colnames = col_name,
              selection = "single",
              options = list(scrollX = TRUE,
                             dom = "Blfrtip",
                             pageLength = 20,
                             lengthMenu = c(5, 10, 20, 50, 100),
                             buttons = list(
                               list(
                                 extend = "csv",
                                 filename = "Par_Summary_Table_AN",
                                 exportOptions = list(
                                   modifier = list(
                                     page = "all"
                                   )
                                 )
                               )
                             ))) %>%
      formatPercentage(columns = "Percentage", digits = 2)
  }, server = FALSE)
  
  Par_Select_table_proxy <- dataTableProxy("Par_Select")
  
  ### Monitoring Site Selection Table
  
  # Update the table by the parameter criteria table
  
  selected_par <- reactive({
    req(loc_summary())
    
    if (!is.null(input$Par_Select_rows_selected)){
      sel_dat <- loc_summary() %>%
        slice(input$Par_Select_rows_selected)
    } else {
      sel_dat <- loc_summary() %>% slice(0)
    }
    return(sel_dat)
  })
  
  # Get the selected parameter
  selected_par_name <- reactive({
    req(loc_summary(), input$Par_Select_rows_selected)
    
    par_title <- paste0(selected_par()$Standard_Name[1], ", ", 
                        selected_par()$Standard_Fraction[1])
    
    return(par_title)
    
  })
  
  # Update the parameter names
  output$parameter_name1 <- renderText({
    req(loc_summary(), input$Par_Select_rows_selected, selected_par_name())
    return(selected_par_name())
  })
  
  # Update the parameter names
  output$parameter_name2 <- renderText({
    req(loc_summary(), input$Par_Select_rows_selected, selected_par_name())
    return(selected_par_name())
  })
  
  # Update the parameter names
  output$parameter_name3 <- renderText({
    req(loc_summary(), input$Par_Select_rows_selected, selected_par_name())
    return(selected_par_name())
  })
  
  # Update the parameter names
  output$parameter_name4 <- renderText({
    req(loc_summary(), input$Par_Select_rows_selected, selected_par_name())
    return(selected_par_name())
  })
  
  # Update the criteria information
  output$criteria_info_AN <- renderText({
    req(loc_summary(), AN_Val$ex, input$Par_Select_rows_selected,
        selected_par(), selected_par_name())
    
    dat <- AN_Val$ex %>%
      semi_join(selected_par(), by = c("Standard_Name", "Standard_Fraction",
                                       "Standard_Unit", "Details", "Use", "Type", 
                                       "Frequency", "Duration"))
    
    if (!all(unique(dat$Standard_Name)[1] %in% c("pH", "Dissolved oxygen (DO)"))){
      if (length(unique(dat$Criteria_Upper)) == 1L){
        criteria <- paste0(round(unique(dat$Criteria_Upper)[1], 2))
      } else {
        criteria_mean <- round(mean(dat$Criteria_Upper, na.rm = TRUE), 2)
        criteria_max <- round(max(dat$Criteria_Upper, na.rm = TRUE), 2)
        criteria_min <- round(min(dat$Criteria_Upper, na.rm = TRUE), 2)
        
        criteria <- paste0("Average ", criteria_mean, ", with a range from ", criteria_min, " to ", criteria_max)
        
      }
    } else if (unique(dat$Standard_Name)[1] %in% "Dissolved oxygen (DO)"){
      if (length(unique(dat$Criteria_Lower)) == 1L){
        criteria <- paste0(round(unique(dat$Criteria_Lower)[1], 2))
      } else {
        criteria <- NA_character_
      }
    } else if (unique(dat$Standard_Name)[1] %in% "pH"){
      if (length(unique(dat$Criteria_Upper)) == 1L & length(unique(dat$Criteria_Lower)) == 1L){
        criteria <- paste0(round(unique(dat$Criteria_Lower)[1], 2), " - ",
                           round(unique(dat$Criteria_Upper)[1], 2))
      } else {
        criteria <- NA_character_
      }
    } else {
      criteria <- NA_character_
    }
    
    temp_text <- paste0("Parameter: ", selected_par()$Standard_Name[1], "\n",
                        "Fraction: ", selected_par()$Standard_Fraction[1], "\n",
                        "Unit: ", selected_par()$Standard_Unit[1], "\n",
                        "Details: ", selected_par()$Details[1], "\n",
                        "Use: ", selected_par()$Use[1], "\n",
                        "Criteria Type: ", selected_par()$Type[1], "\n",
                        "Frequency: ", selected_par()$Frequency[1], "\n",
                        "Duration: ", selected_par()$Duration[1], "\n",
                        "Criteria: ", criteria)
    return(temp_text)
  })
  
  # Prepare the WQP_site table
  observe({
    req(selected_par(), AN_Val$ex_sum_site, AN_Val$ex_sum_AU)
    
    if (input$sites_or_AU_AN %in% "Site"){
      dat <- AN_Val$ex_sum_site
    } else if (input$sites_or_AU_AN %in% "AU"){
      dat <- AN_Val$ex_sum_AU
    }
    
    WQP_site <- dat %>%
      semi_join(selected_par(), by = c(
        "Standard_Name", "Standard_Fraction", "Standard_Unit",
        "Details", "Use", "Duration"
      )) 
    
    AN_Val$WQP_site <- WQP_site 
    
  })
  
  # Update WQP_site by slider_ex
  AN_WQP_site <- reactive({
    req(AN_Val$WQP_site, input$slider_ex, input$Par_Select_rows_selected)
    
    if (input$sites_or_AU_AN %in% "Site"){
      WQP_site2 <- AN_Val$WQP_site %>% 
        # Select columns
        fselect(MonitoringLocationIdentifier, MonitoringLocationName, LongitudeMeasure, 
                LatitudeMeasure, 
                Size, Start_Date, End_Date, Minimum, Median, Maximum, Num_Years_Exceedance,
                Exceedance_Size, Percentage) %>%
        filter(Percentage >= input$slider_ex/100 | is.na(Percentage))  
    } else if (input$sites_or_AU_AN %in% "AU"){
      WQP_site2 <- AN_Val$WQP_site %>% 
        # Select columns
        fselect(AU_ID, AU_NAME, AU_x, 
                AU_y, Size, Start_Date, End_Date, Minimum, Median, Maximum, Num_Years_Exceedance,
                Exceedance_Size, Percentage) %>%
        filter(Percentage >= input$slider_ex/100 | is.na(Percentage)) 
    }
    return(WQP_site2)
  })
  
  output$site_table <- renderDT({
    req(AN_WQP_site(), input$slider_ex, input$Par_Select_rows_selected)
    
    if (input$sites_or_AU_AN %in% "Site"){
      datatable(AN_WQP_site(), filter = "none", rownames = FALSE,
                extensions = "Buttons",
                colnames = c("Station No." = "MonitoringLocationIdentifier",
                             "Station Name" = "MonitoringLocationName",
                             "Longitude" = "LongitudeMeasure",                             
                             "Latitude" = "LatitudeMeasure",
                             "N" = "Size",
                             "Start Date" = "Start_Date",
                             "End Date" = "End_Date",
                             "Minimum" = "Minimum",
                             "Median" = "Median",
                             "Maximum" = "Maximum",
                             "No Years Exceedance" = "Num_Years_Exceedance",
                             "No. of Exceedance" = "Exceedance_Size",
                             "Exceedance Percentage" = "Percentage"),
                selection = list(mode = "multiple"),
                options = list(pageLength = 20,
                               lengthMenu = c(5, 10, 20, 50, 100),
                               dom = "Blfrtip",
                               scrollX = TRUE,
                               buttons = list(
                                 list(
                                   extend = "csv",
                                   filename = "Site_Table_AN",
                                   exportOptions = list(
                                     modifier = list(
                                       page = "all"
                                     )
                                   )
                                 )
                               ))) %>%
        formatPercentage(columns = "Exceedance Percentage", digits = 2) %>%
        formatRound(columns = c("Longitude", "Latitude"), digits = 3)
      
    } else if (input$sites_or_AU_AN %in% "AU"){
      datatable(AN_WQP_site(), filter = "none", rownames = FALSE,
                extensions = "Buttons",
                colnames = c("AU ID" = "AU_ID",
                             "AU Name" = "AU_NAME",
                             "Longitude" = "AU_x",                             
                             "Latitude" = "AU_y",
                             "N" = "Size",
                             "Start Date" = "Start_Date",
                             "End Date" = "End_Date",
                             "Minimum" = "Minimum",
                             "Median" = "Median",
                             "Maximum" = "Maximum",
                             "No Years Exceedance" = "Num_Years_Exceedance",
                             "No. of Exceedance" = "Exceedance_Size",
                             "Exceedance Percentage" = "Percentage"),
                selection = list(mode = "multiple"),
                options = list(pageLength = 20,
                               lengthMenu = c(5, 10, 20, 50, 100),
                               dom = "Blfrtip",
                               scrollX = TRUE,
                               buttons = list(
                                 list(
                                   extend = "csv",
                                   filename = "Site_Table",
                                   exportOptions = list(
                                     modifier = list(
                                       page = "all"
                                     )
                                   )
                                 )
                               ))) %>%
        formatPercentage(columns = "Exceedance Percentage", digits = 2) %>%
        formatRound(columns = c("Longitude", "Latitude"), digits = 3)
    }
    
  }, server = FALSE)
  
  site_table_proxy <- dataTableProxy("site_table")
  
  ### Map

  # Create a subset of AN_Val$WQP_site2 based on input$site_table_rows_selected
  selected_sites <- reactive({
    req(AN_WQP_site(), input$Par_Select_rows_selected)

    if (!is.null(input$site_table_rows_selected)){
      sel_dat <- AN_WQP_site() %>%
        slice(input$site_table_rows_selected)
    } else {
      sel_dat <- AN_WQP_site() %>% slice(0)
    }
    return(sel_dat)
  })

  site_proxy <- leafletProxy("site_map")

  output$site_map <- renderLeaflet({
    req(nrow(AN_WQP_site()) > 0, input$Par_Select_rows_selected)

    if (input$sites_or_AU_AN %in% "Site"){
      # Create an indicator column to show if any data are above limit
      temp_dat <- AN_WQP_site() %>%
        mutate(Indicator = ifelse(Exceedance_Size > 0, "Yes", "No")) %>%
        mutate(Indicator = factor(Indicator, levels = c("Yes", "No"))) %>%
        # Create a label column
        mutate(label = paste0("Site No: ", "<strong>", MonitoringLocationIdentifier, "</strong>", "<br/>",
                              "Site Name:", "<strong>", MonitoringLocationName, "</strong>", "<br/>",
                              "N: ", "<strong>", Size, "</strong>", "<br/>",
                              "Start Date: ", "<strong>", Start_Date, "</strong>", "<br/>",
                              "End Date: ", "<strong>", End_Date, "</strong>", "<br/>",
                              "Minimum: ", "<strong>", Minimum, "</strong>", "<br/>",
                              "Median: ", "<strong>", Median, "</strong>", "<br/>",
                              "Maximum: ", "<strong>", Maximum, "</strong>", "<br/>",
                              "No Years Exceedance: ", "<strong>", Num_Years_Exceedance, "</strong>", "<br/>",
                              "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                              "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%"))


      labs <- as.list(temp_dat$label)

      pal_sc <- colorFactor(c("#FF7F00", "#1F78B4"), temp_dat$Indicator)

      leaflet(temp_dat) %>%
        add_USGS_base() %>%
        addCircleMarkers(lng = ~LongitudeMeasure,
                         lat = ~LatitudeMeasure,
                         layerId = ~MonitoringLocationIdentifier,
                         radius = 8, stroke = TRUE, weight = 1,
                         color = "black",
                         fillColor = ~pal_sc(Indicator),
                         fillOpacity = 1,
                         opacity = 0.5,
                         label = map(labs, HTML),
                         group = "base_map") %>%
        addLegend("topright",
                  pal = pal_sc,
                  values = temp_dat$Indicator,
                  title = "Any Data Above Limit",
                  opacity = 1) %>%
        setView(lng = -107.3045253, lat = 44.8494123, zoom = 5)
    } else if (input$sites_or_AU_AN %in% "AU"){
      # Create an indicator column to show if any data are above limit
      WQP_AN_stream <- streams_simp_shp %>%
        semi_join(AN_WQP_site(), by = c("AU_ID", "AU_NAME")) %>%
        left_join(AN_WQP_site(), by = c("AU_ID", "AU_NAME"))
      
      WQP_AN_lakes <- lakes_shp %>%
        semi_join(AN_WQP_site(), by = c("AU_ID", "AU_NAME")) %>%
        left_join(AN_WQP_site(), by = c("AU_ID", "AU_NAME"))
      
      temp_dat_stream <- WQP_AN_stream  %>%
        mutate(Indicator = ifelse(Exceedance_Size > 0, "Yes", "No")) %>%
        mutate(Indicator = factor(Indicator, levels = c("Yes", "No"))) %>%
        # Create a label column
        mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                              "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                              "N: ", "<strong>", Size, "</strong>", "<br/>",
                              "Start Date: ", "<strong>", Start_Date, "</strong>", "<br/>",
                              "End Date: ", "<strong>", End_Date, "</strong>", "<br/>",
                              "Minimum: ", "<strong>", Minimum, "</strong>", "<br/>",
                              "Median: ", "<strong>", Median, "</strong>", "<br/>",
                              "Maximum: ", "<strong>", Maximum, "</strong>", "<br/>",
                              "No Years Exceedance: ", "<strong>", Num_Years_Exceedance, "</strong>", "<br/>",
                              "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                              "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%"))
      
      temp_dat_lakes <- WQP_AN_lakes  %>%
        mutate(Indicator = ifelse(Exceedance_Size > 0, "Yes", "No")) %>%
        mutate(Indicator = factor(Indicator, levels = c("Yes", "No"))) %>%
        # Create a label column
        mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                              "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                              "N: ", "<strong>", Size, "</strong>", "<br/>",
                              "Start Date: ", "<strong>", Start_Date, "</strong>", "<br/>",
                              "End Date: ", "<strong>", End_Date, "</strong>", "<br/>",
                              "Minimum: ", "<strong>", Minimum, "</strong>", "<br/>",
                              "Median: ", "<strong>", Median, "</strong>", "<br/>",
                              "Maximum: ", "<strong>", Maximum, "</strong>", "<br/>",
                              "No Years Exceedance: ", "<strong>", Num_Years_Exceedance, "</strong>", "<br/>",
                              "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                              "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%"))

      temp_dat <- bind_rows(
        temp_dat_stream %>% st_set_geometry(NULL),
        temp_dat_lakes %>% st_set_geometry(NULL)
      )
      
      labs <- as.list(temp_dat$label)

      pal_sc <- colorFactor(c("#FF7F00", "#1F78B4"), temp_dat$Indicator)

      leaflet(temp_dat) %>%
        add_USGS_base() %>%
        addPolylines(data = temp_dat_stream, 
                     color = ~pal_sc(Indicator), 
                     opacity = 1,
                     weight = 10,
                     group = "base_map_river",
                     label = map(labs, HTML)) %>%
        addPolygons(data = temp_dat_lakes, 
                    color = "#03F", 
                    weight = 10, 
                    opacity = 1,
                    fillColor = ~pal_sc(Indicator),
                    fillOpacity = 1,
                    group = "base_map_lake",
                    label = map(labs, HTML)) %>%
        addLegend("topright",
                  pal = pal_sc,
                  values = temp_dat$Indicator,
                  title = "Any Data Above Limit",
                  opacity = 1) %>%
        setView(lng = -107.3045253, lat = 44.8494123, zoom = 5)
    }

  })

  observe({
    req(AN_WQP_site(), selected_sites())
    if (nrow(selected_sites()) > 0){

      if (input$sites_or_AU_AN %in% "Site"){
        temp_dat <- selected_sites() %>%
          mutate(new_ID = paste0(MonitoringLocationIdentifier, "_new")) %>%
          # Create a label column
          mutate(label = paste0("Site No: ", "<strong>", MonitoringLocationIdentifier, "</strong>", "<br/>",
                                "Site Name:", "<strong>", MonitoringLocationName, "</strong>", "<br/>",
                                "N: ", "<strong>", Size, "</strong>", "<br/>",
                                "Start Date: ", "<strong>", Start_Date, "</strong>", "<br/>",
                                "End Date: ", "<strong>", End_Date, "</strong>", "<br/>",
                                "Minimum: ", "<strong>", Minimum, "</strong>", "<br/>",
                                "Median: ", "<strong>", Median, "</strong>", "<br/>",
                                "Maximum: ", "<strong>", Maximum, "</strong>", "<br/>",
                                "No Years Exceedance: ", "<strong>", Num_Years_Exceedance, "</strong>", "<br/>",
                                "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                                "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%"))
        labs <- as.list(temp_dat$label)

        site_proxy %>% clearGroup(group = "highlighted_point")
        site_proxy %>%
          addCircleMarkers(lng = temp_dat$LongitudeMeasure,
                           lat = temp_dat$LatitudeMeasure,
                           layerId = temp_dat$new_ID,
                           radius = 8, stroke = TRUE, weight = 1,
                           color = "black", fillColor = "red",
                           fillOpacity = 1,
                           opacity = 0.5,
                           label = map(labs, HTML),
                           group = "highlighted_point")
      } else if (input$sites_or_AU_AN %in% "AU"){
        
        WQP_AN_stream <- streams_simp_shp %>%
          semi_join(selected_sites(), by = c("AU_ID", "AU_NAME")) %>%
          left_join(selected_sites(), by = c("AU_ID", "AU_NAME"))
        
        WQP_AN_lakes <- lakes_shp %>%
          semi_join(selected_sites(), by = c("AU_ID", "AU_NAME")) %>%
          left_join(selected_sites(), by = c("AU_ID", "AU_NAME"))
        
        temp_dat_stream <- WQP_AN_stream %>%
          mutate(new_ID = paste0(AU_ID, "_new")) %>%
          # Create a label column
          mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                                "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                                "N: ", "<strong>", Size, "</strong>", "<br/>",
                                "Start Date: ", "<strong>", Start_Date, "</strong>", "<br/>",
                                "End Date: ", "<strong>", End_Date, "</strong>", "<br/>",
                                "Minimum: ", "<strong>", Minimum, "</strong>", "<br/>",
                                "Median: ", "<strong>", Median, "</strong>", "<br/>",
                                "Maximum: ", "<strong>", Maximum, "</strong>", "<br/>",
                                "No Years Exceedance: ", "<strong>", Num_Years_Exceedance, "</strong>", "<br/>",
                                "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                                "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%"))
        
        temp_dat_lakes <- WQP_AN_lakes %>%
          mutate(new_ID = paste0(AU_ID, "_new")) %>%
          # Create a label column
          mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                                "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                                "N: ", "<strong>", Size, "</strong>", "<br/>",
                                "Start Date: ", "<strong>", Start_Date, "</strong>", "<br/>",
                                "End Date: ", "<strong>", End_Date, "</strong>", "<br/>",
                                "Minimum: ", "<strong>", Minimum, "</strong>", "<br/>",
                                "Median: ", "<strong>", Median, "</strong>", "<br/>",
                                "Maximum: ", "<strong>", Maximum, "</strong>", "<br/>",
                                "No Years Exceedance: ", "<strong>", Num_Years_Exceedance, "</strong>", "<br/>",
                                "No. of Exceedances: ", "<strong>", Exceedance_Size, "</strong>", "<br/>",
                                "Exceedance %: ", "<strong>", round(Percentage * 100, digits = 2), "</strong>", "%"))
        
        temp_dat <- bind_rows(
          temp_dat_stream %>% st_set_geometry(NULL),
          temp_dat_lakes %>% st_set_geometry(NULL)
        )
        
        labs <- as.list(temp_dat$label)

        site_proxy %>% clearGroup(group = "highlighted_point")
        site_proxy %>%
          addPolylines(data = temp_dat_stream, 
                       layerId = temp_dat$new_ID,
                       weight = 10,
                       color = "red",
                       opacity = 1,
                       group = "highlighted_point",
                       label = map(labs, HTML)) %>%
          addPolygons(data = temp_dat_lakes, 
                      layerId = temp_dat$new_ID,
                      weight = 10, 
                      fillColor = "red",
                      fillOpacity = 1,
                      opacity = 0.5,
                      group = "highlighted_point",
                      label = map(labs, HTML))
      }

    } else {
      site_proxy %>% clearGroup(group = "highlighted_point")
    }
  })

  # See the map click event
  observeEvent(input$site_map_marker_click, {
    req(AN_WQP_site())

    temp_map_click_sites <- input$site_map_marker_click$id

    if (input$sites_or_AU_AN %in% "Site"){
      # if the temp_map_click_sites ends with "_new", need to remove "_new
      if (str_detect(temp_map_click_sites, regex("_new$"))){

        temp_map_remove_sites <- str_remove(temp_map_click_sites, regex("_new$"))
        temp_site_remove_row <- which(AN_WQP_site()$MonitoringLocationIdentifier %in%
                                        temp_map_remove_sites)

        AN_Val$site_select <- AN_Val$site_select[!AN_Val$site_select %in% temp_site_remove_row]

      } else {

        # Get the row ID based on MonitoringLocationIdentifier
        temp_site_row <- which(AN_WQP_site()$MonitoringLocationIdentifier %in%
                                 temp_map_click_sites)

        AN_Val$site_select <- unique(c(AN_Val$site_select,
                                       temp_site_row,
                                       input$site_table_rows_selected))
      }
    } else if (input$sites_or_AU_AN %in% "AU"){
      # if the temp_map_click_sites ends with "_new", need to remove "_new
      if (str_detect(temp_map_click_sites, regex("_new$"))){

        temp_map_remove_sites <- str_remove(temp_map_click_sites, regex("_new$"))
        temp_site_remove_row <- which(AN_WQP_site()$AU_ID %in%
                                        temp_map_remove_sites)

        AN_Val$site_select <- AN_Val$site_select[!AN_Val$site_select %in% temp_site_remove_row]

      } else {

        # Get the row ID based on MonitoringLocationIdentifier
        temp_site_row <- which(AN_WQP_site()$AU_ID %in%
                                 temp_map_click_sites)

        AN_Val$site_select <- unique(c(AN_Val$site_select,
                                       temp_site_row,
                                       input$site_table_rows_selected))
      }
    }

    # Update the site_table
    site_table_proxy %>%
      selectRows(selected = AN_Val$site_select)
  })
  
  ### Time series plot
  
  # Subset AN_Val$WQP_dat2 based on selected_sites
  selected_dat <- reactive({
    req(AN_WQP_site(), 
        nrow(selected_sites()) > 0, nrow(selected_par()) > 0,
        AN_Val$ex)
    
    if (input$sites_or_AU_AN %in% "Site"){
      temp_dat <- AN_Val$ex %>%
        semi_join(selected_par(), by = c(
          "Standard_Name", "Standard_Fraction", "Standard_Unit",
          "Details", "Use", "Frequency", "Duration"
        )) %>%
        fsubset(MonitoringLocationIdentifier %in% selected_sites()$MonitoringLocationIdentifier)
    } else if (input$sites_or_AU_AN %in% "AU"){
      temp_dat <- AN_Val$ex %>%
        semi_join(selected_par(), by = c(
          "Standard_Name", "Standard_Fraction", "Standard_Unit",
          "Details", "Use", "Frequency", "Duration"
        )) %>%
        fsubset(AU_ID %in% selected_sites()$AU_ID)
    }
    
    
    
    return(temp_dat)
  })
  
  plot_dat <- reactive({
    req(AN_WQP_site(), nrow(selected_sites()) > 0, selected_dat())
    
    if (input$sites_or_AU_AN %in% "Site"){
      temp_dat <- selected_dat() %>%
        rename(Site = MonitoringLocationIdentifier,
               Value = TADA.ResultMeasureValue)
    } else if (input$sites_or_AU_AN %in% "AU"){
      temp_dat <- selected_dat() %>%
        rename(Site = AU_ID,
               Value = TADA.ResultMeasureValue)
    }
    
    temp_dat2 <- temp_dat %>%
      fmutate(`ActivityStartTime.Time` = as.character(`ActivityStartTime.Time`)) %>%
      fmutate(`ActivityStartTime.Time` = ifelse(is.na(`ActivityStartTime.Time`),
                                                "00:00:00",
                                                `ActivityStartTime.Time`)) %>%
      fmutate(DateTime = ymd_hms(paste0(ActivityStartDate, " ", `ActivityStartTime.Time`))) %>%
      fmutate(Condition = case_when(
        TADA.CensoredData.Flag %in% "Uncensored" ~ "Uncensored",
        TADA.CensoredData.Flag %in% "Non-Detect" ~ "Non-Detect",
        TADA.CensoredData.Flag %in% "Over-Detect" ~ "Over-Detect",
        TRUE                                      ~"Other"
      )) %>%
      fmutate(Condition = factor(Condition,
                                 levels = c("Uncensored", "Non-Detect", "Over-Detect", "Other"))) %>%
      fmutate(Site = factor(Site)) %>%
      fmutate(Exceedance = ifelse(Exceedance, "Yes", "No")) %>%
      # Round the criteria
      fmutate(Criteria_Upper = round(Criteria_Upper, digits = 2),
              Criteria_Lower = round(Criteria_Lower, digits = 2))
    
    return(temp_dat2)
  })
  
  plot_unit <- reactive({
    req(AN_WQP_site(), nrow(selected_sites()) > 0, selected_dat(), plot_dat())
    
    temp_unit <- unique(plot_dat()$Standard_Unit)[1]
    
    return(temp_unit)
  })
  
  output$time_plot <- renderPlotly({
    req(AN_WQP_site(), nrow(selected_sites()) > 0, selected_dat(), 
        plot_dat(), plot_unit())
    
    temp_dat <- plot_dat() %>%
      mutate(Site_Text = Site)
    
    # Group sites
    if (input$time_group){
      temp_dat <- temp_dat %>%
        mutate(Site = "Combined")
    } else {
      temp_dat <- temp_dat
    }
    
    site_title <- unique(temp_dat$Site)
    
    p <- ggplot(temp_dat, aes(x = DateTime, y = Value, 
                              text = paste("Site:", Site_Text))) +
      geom_point(aes(shape = Condition), color = "blue") +
      scale_x_datetime(name = "") +
      scale_shape_manual(values = c("Uncensored" = 16,
                                    "Non-Detect" = 2,
                                    "Over-Detect" = 5,
                                    "Other" = 7),
                         limits = levels(temp_dat$Condition)) 
    
    if (length(unique(temp_dat$Site)) > 1){
      p <- p +
        facet_grid(Site ~ ., switch = "y")
    } else {
      p <- p +
        ggtitle(site_title)
    }
    
    p <- p +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    # If the Type 3, 5, and 6, plot the criteria data as points
    # Otherwise, plot the criteria as line
    
    if (unique(selected_dat()$Type)[1] %in% c(3, 5, 6)){
      p <- p +
        geom_point(aes(x = DateTime, y = Criteria_Upper), color = "orange", shape = 16) 
    } else if (unique(selected_dat()$Type)[1] %in% c(7)){
      p <- p
    } else if (unique(selected_dat()$Standard_Name)[1] %in% "Dissolved oxygen (DO)"){
      
      Criteria_Lower <- unique(selected_dat()$Criteria_Lower)[1]
      
      p <- p +
        geom_hline(yintercept = Criteria_Lower, color = "orange", linewidth = 1)
    } else if (unique(selected_dat()$Standard_Name)[1] %in% "pH"){ 
      
      Criteria_Lower <- unique(selected_dat()$Criteria_Lower)[1]
      Criteria_Upper <- unique(selected_dat()$Criteria_Upper)[1]
      
      p <- p +
        geom_hline(yintercept = Criteria_Upper, color = "orange", linewidth = 1) +
        geom_hline(yintercept = Criteria_Lower, color = "orange", linewidth = 1)
    } else {
      
      Criteria_Upper <- unique(selected_dat()$Criteria_Upper)[1]
      
      p <- p +
        geom_hline(yintercept = Criteria_Upper, color = "orange", linewidth = 1)
    }
    
    if (input$time_log_trans){
      p <- p +
        scale_y_log10(name = plot_unit())
    } else {
      p <- p +
        scale_y_continuous(name = plot_unit())
    }
    
    # shiny::validate(
    #   need(nrow(selected_dat()) > 1, "Plot not shown as there is only one data point.")
    # )
    
    gp <- p %>%
      ggplotly() %>%
      layout(legend = list(orientation = 'h'))
    
    return(gp)
  })
  
  
  ###  Box plot plot
  
  output$box_plot <- renderPlotly({
    req(AN_WQP_site(), nrow(selected_sites()) > 0, selected_dat(), 
        plot_dat(), plot_unit())
    
    temp_dat <- plot_dat() %>% mutate(Site_Text = Site)
    
    # Group sites
    if (input$box_group){
      temp_dat <- temp_dat %>%
        mutate(Site = "Combined")
    } else {
      temp_dat <- temp_dat
    }
    
    # Select sites with sample size more than 3
    temp_dat1 <- temp_dat  %>%
      group_by(Site) %>%
      filter(n() >= 3) %>%
      ungroup() %>%
      as.data.table()
    
    if (nrow(temp_dat1) == 0){
      p <- ggplot(temp_dat, aes(x = Site, y = Value)) +
        geom_jitter(aes(shape = Condition,
                        height = 0,
                        text = paste("Site:", Site_Text, "\n",
                                     "DateTime:", DateTime, "\n",
                                     "Value:", Value, "\n",
                                     "Condition:", Condition))) +
        scale_shape_manual(values = c("Uncensored" = 16,
                                      "Non-Detect" = 2,
                                      "Over-Detect" = 5,
                                      "Other" = 7),
                           limits = levels(plot_dat()$Condition))
    } else {
      p <- ggplot(temp_dat1, aes(x = Site, y = Value)) +
        geom_boxplot(outlier.shape = NA,
                     outlier.color = NA,
                     outlier.size = 0,
                     width = 0.2) +
        geom_jitter(dat = temp_dat1, 
                    aes(shape = Condition,
                        height = 0,
                        text = paste("Site:", Site_Text, "\n",
                                     "DateTime:", DateTime, "\n",
                                     "Value:", Value, "\n",
                                     "Condition:", Condition)),
                    width = 0.2) +
        scale_shape_manual(values = c("Uncensored" = 16,
                                      "Non-Detect" = 2,
                                      "Over-Detect" = 5,
                                      "Other" = 7),
                           limits = levels(plot_dat()$Condition))
    }
    
    p <- p +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    if (input$box_log_trans){
      p <- p +
        scale_y_log10(name = plot_unit())
    } else {
      p <- p +
        scale_y_continuous(name = plot_unit())
    }
    
    gp <- p %>%
      ggplotly(tooltip = "text") %>%
      layout(legend = list(orientation = 'h'))
    
    # shiny::validate(
    #  need(nrow(temp_dat1) > 1, "Plot not shown as there is only one data point.")
    # )
    
    if (nrow(temp_dat1) > 1){
      gp <- plotly_build(gp)
      
      for(i in 1:length(gp$x$data)) {
        gp$x$data[1] <- lapply(gp$x$data[1], FUN = function(x){
          x$marker = list(opacity = 0)
          return(x)
        })
      }
    } 
    
    return(gp)
    
  })
  
  # Prepare the final output for the AN tab
  AN_final_dat <- reactive({
    req(AN_Val$ex)
    temp_dat <- AN_Val$ex %>%
      column_name_change4()
    return(temp_dat)
  })
  
  ### Server functions for the AU Join tab
  
  AU_Val <- reactiveValues(
    SANDS_download_ready = FALSE
  )

  # File upload indicator
  upload_dat_AU <- reactive({
    req(input$file_AU)
    
    if (str_detect(input$file_AU$name, "\\.csv$")){
      temp <- read_csv(input$file_AU$datapath)
    } else {
      temp <- NULL
    }
    
    AU_Val$upload_time <- Sys.time()
    
    return(temp)
  })
  
  # Replace reVal$WQP_dat2 with upload_dat() if upload_dat() meets the requirements
  observe({
    req(upload_dat_AU())
    
    if (validate_fun(upload_dat_AU() , WQP_cols)){
      data_storage$AU_latest_upload <- "Upload from the Join AU tab"
      data_storage$AU_time <- AU_Val$upload_time
      data_storage$AU_latest_data <- upload_dat_AU()
    }
  })
  
  output$file_indicator_AU <- renderText({
    if (is.null(data_storage$AU_latest_upload)){
      temp_text <- "Require downloading data or uploading a CSV file with the data."
    } else {
      temp_text <- paste0(data_storage$AU_latest_upload, " at ", 
                          round(data_storage$AU_time))
    }
    return(temp_text)
  })
  
  observe({
    req(!is.null(data_storage$AU_latest_data))
    AU_Val$WQP_AU <- data_storage$AU_latest_data %>% TADA_site_simplify()
  })
  
  observe({
    req(AU_Val$WQP_AU, nrow(AU_Val$WQP_AU) > 0)
    AU_Val$site_number <- nrow(AU_Val$WQP_AU)
    
    output$Date_Info_AU <- renderText({
      paste0("There are ", AU_Val$site_number, " sites.")
    })
  })
  
  # Upload the crosswalk table
  
  upload_dat_crosswalk <- reactive({
    req(input$file_crosswalk)
    
    if (str_detect(input$file_crosswalk$name, "\\.csv$")){
      temp <- read_csv(input$file_crosswalk$datapath)
    } else {
      temp <- NULL
    }
    
    AU_Val$crosswalk_time <- Sys.time()
    
    return(temp)
  })
  
  observe({
    req(upload_dat_crosswalk())
    
    if (validate_fun(upload_dat_crosswalk(), AU_crosswalk_cols)){
      data_storage$AU_crosswalk_label <- "Upload from the users"
      data_storage$AU_crosswalk_table <- upload_dat_crosswalk()
      data_storage$AU_crosswalk_time <- AU_Val$crosswalk_time
    }
    
  })
  
  observe({
    req(input$crosswalk_upload %in% "Use the default crosswalk table")
    data_storage$AU_crosswalk_table <- df_ML2AU
    data_storage$AU_crosswalk_label <- "Default crosswalk table"
  })
  
  output$file_indicator_crosswalk <- renderText({
    if (input$crosswalk_upload %in% "Use the default crosswalk table"){
      temp_text <- "Use the default crosswalk table."
    } else if (input$crosswalk_upload %in% "Upload a new crosswalk table"){
      if (data_storage$AU_crosswalk_label %in% "Upload from the users"){
        temp_text <- paste0("Crosswalk table uploaded at ", 
                            round(data_storage$AU_crosswalk_time))
      } else {
        temp_text <- "Use the default crosswalk table."
      }
    }
    return(temp_text)
  })
  
  # b_Calc, AUjoin ####
  shiny::observeEvent(input$b_Calc, {
    req(AU_Val$WQP_AU)
    
    shinyCatch({
      shiny::withProgress({
        
        # Number of increments
        n_inc <- 10
        
        # # sink output
        # file_sink <- file(file.path(".", "Results", "results_Log.txt")
        #                   , open = "wt")
        # sink(file_sink, type = "output", append = TRUE)
        # sink(file_sink, type = "message", append = TRUE)
        
        # # Log
        # message("Results Log from AU Spatial Join App")
        # message(Sys.time())
        
        # if (data_storage$AU_latest_upload %in% "Upload from the Join AU tab"){
        #   inFile <- input$file_AU
        #   AU_filename <- inFile$name
        # } else if (data_storage$AU_latest_upload %in% "Data downloaded from WQP"){
        #   AU_filename <- "WQP"
        # }
        
        # message(paste0("Imported file name: ", AU_filename))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Data, Initialize")
        Sys.sleep(0.25)
        
        # Import data
        # data
        df_MonLocData <- AU_Val$WQP_AU
        
        # QC, FAIL if TRUE
        if (is.null(df_MonLocData)) {
          return(NULL)
        }
        
        ## 1. Join crosswalk ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Join, crosswalk")
        Sys.sleep(0.25)
        
        df_joinedMonLocAU <- dplyr::left_join(df_MonLocData, df_ML2AU
                                              , by = "MonitoringLocationIdentifier")
        
        # 2. Filter matched MonLoc ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Data, Filter")
        Sys.sleep(0.25)
        
        df_MonLocMatched <- df_joinedMonLocAU %>%
          filter(!is.na(AU_ID))
        
        # 3. Unmatched MonLoc by Type ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Data, Prepare Unmatched")
        Sys.sleep(0.25)
        
        df_Lake_Unmatched <- df_joinedMonLocAU %>%
          filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Lake_types)) %>% 
          select(-c(AU_ID, AU_NAME, DrinkingWater_Use, Ecological_Use
                    , FishConsumption_Use, Recreational_Use, Other_Use))
        
        df_Stream_Unmatched <- df_joinedMonLocAU %>%
          filter(is.na(AU_ID) & (MonitoringLocationTypeName %in% Stream_types)) %>% 
          select(-c(AU_ID, AU_NAME, DrinkingWater_Use, Ecological_Use
                    , FishConsumption_Use, Recreational_Use, Other_Use))
        
        # 4. Spatial Joins ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Spatial Join, Lakes")
        Sys.sleep(0.25)
        
        ### 4a. Lake Spatial Join ####
        
        # QC check
        num_sites <- nrow(df_Lake_Unmatched)
        
        # only run spatial join if there are sites
        if(num_sites == 0){
          message(paste("There are NO lake monitoring locations missing AU data."))
          
          df_Lake_SpatJoin_Final <- NULL
          
        } else {
          message(paste("There are", num_sites, "lake monitoring locations missing AU data."))
          
          ### convert to geospatial layer (sf object)
          lakes_pts <- sf::st_as_sf(x = df_Lake_Unmatched, coords = c("LongitudeMeasure"
                                                                      ,"LatitudeMeasure")
                                    , crs = "+proj=longlat +datum=WGS84")%>%
            sf::st_transform(st_crs(GISlayer_lakes_transformed))
          
          ### spatial join
          lake_SpatJoin <- sf::st_join(lakes_pts, GISlayer_lakes_transformed
                                       , join = st_nearest_feature) # join points and AUs
          
          ### determine distance (m) between points and nearest feature
          near_feat <- sf::st_nearest_feature(lakes_pts, GISlayer_lakes_transformed)
          Dist_to_AU_m <- sf::st_distance(lakes_pts, GISlayer_lakes_transformed[near_feat,]
                                          , by_element = TRUE)
          
          ### join distance measurements to join results
          lake_SpatJoin2 <- cbind(lake_SpatJoin, Dist_to_AU_m)
          
          ### results and export data
          df_Lake_SpatJoin_Final <- lake_SpatJoin2 %>%
            sf::st_transform(4326) %>%
            mutate(LongitudeMeasure = unlist(map(geometry,1)),
                   LatitudeMeasure = unlist(map(geometry,2))) %>%
            sf::st_drop_geometry()
          
        }# end if/else statement
        
        ### 4b. Stream Spatial Join ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Spatial Join, Streams")
        Sys.sleep(0.25)
        
        # QC check
        num_sites <- nrow(df_Stream_Unmatched)
        
        # only run spatial join if there are sites
        if(num_sites == 0){
          message(paste("There are NO stream monitoring locations missing AU data."))
          
          df_Stream_SpatJoin_Final <- NULL
          
        } else {
          message(paste("There are", num_sites, "stream monitoring locations missing AU data."))
          
          ### convert to geospatial layer (sf object)
          streams_pts <- sf::st_as_sf(x = df_Stream_Unmatched, coords = c("LongitudeMeasure"
                                                                          ,"LatitudeMeasure")
                                      , crs = "+proj=longlat +datum=WGS84")%>%
            sf::st_transform(st_crs(GISlayer_streams_transformed))
          
          ### spatial join
          stream_SpatJoin <- sf::st_join(streams_pts, GISlayer_streams_transformed
                                         , join = st_nearest_feature) # join points and AUs
          # select(MonitoringLocationIdentifier, MonitoringLocationName
          #        , MonitoringLocationTypeName, State, AU_ID, AU_NAME) # trim unneccessary columns
          
          ### determine distance (m) between points and nearest feature
          near_feat <- sf::st_nearest_feature(streams_pts, GISlayer_streams_transformed)
          Dist_to_AU_m <- sf::st_distance(streams_pts, GISlayer_streams_transformed[near_feat,]
                                          , by_element = TRUE)
          
          ### join distance measurements to join results
          stream_SpatJoin2 <- cbind(stream_SpatJoin, Dist_to_AU_m)
          
          ### results and export data
          df_Stream_SpatJoin_Final <- stream_SpatJoin2 %>%
            sf::st_transform(4326) %>%
            mutate(LongitudeMeasure = unlist(map(geometry,1)),
                   LatitudeMeasure = unlist(map(geometry,2))) %>%
            sf::st_drop_geometry()
          
        }# end if/else statement
        
        ### 4c. Join data ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Spatial Join, Combine")
        Sys.sleep(0.25)
        
        if (is.null(df_Lake_SpatJoin_Final) & is.null(df_Stream_SpatJoin_Final)){
          df_SpatJoin_Final <- NULL # both datasets are NULL
        } else if (!is.null(df_Lake_SpatJoin_Final) 
                   & is.null(df_Stream_SpatJoin_Final)) {
          df_SpatJoin_Final <- df_Lake_SpatJoin_Final # only streams dataset is NULL
        } else if (is.null(df_Lake_SpatJoin_Final) 
                   & !is.null(df_Stream_SpatJoin_Final)) {
          df_SpatJoin_Final <- df_Stream_SpatJoin_Final # only lakes dataset is NULL
        } else {
          df_SpatJoin_Final <- rbind(df_Lake_SpatJoin_Final, df_Stream_SpatJoin_Final)
          # both datasets have data
        }# END ~ IF/ELSE
        
        ### 4d. Join to matched data ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Data, Combine all")
        Sys.sleep(0.25)
        
        df_MonLocMatched$Dist_to_AU_m <- 0
        
        if (is.null(df_SpatJoin_Final)){
          AU_Val$df_MonLocAU_4Map <- df_MonLocMatched %>% 
            mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                          , TRUE ~ "Matched")) %>%
            select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
                      , Recreational_Use, Other_Use))
        } else {
          AU_Val$df_MonLocAU_4Map <- rbind(df_MonLocMatched, df_SpatJoin_Final)%>% 
            mutate(MatchGroup = case_when((Dist_to_AU_m > 0) ~ "UnMatched"
                                          , TRUE ~ "Matched")) %>%
            select(-c(DrinkingWater_Use, Ecological_Use, FishConsumption_Use
                      , Recreational_Use, Other_Use))
        } # END ~ if/else
        
        ## 5. Display data ####
        ### 5a. Table ####
        output$df_results_DT <- DT::renderDT({
          
          if (is.null(AU_Val$df_MonLocAU_4Map)) {
            return(NULL)
          }##IF~is.null~END
          
          # Filter df_MonLocAU_4Map based on input_site_choice
          filtered_df <- AU_Val$df_MonLocAU_4Map
          
          if (input$input_site_choice != "") {
            mySite <- input$input_site_choice
            
            filtered_df <- AU_Val$df_MonLocAU_4Map %>% 
              filter(MonitoringLocationIdentifier == mySite)
          }##IF~END
          
          # Return the filtered dataframe
          return(filtered_df)
          
          # # datatable(df_MonLocAU_4Map, selection = "single")
          # return(df_MonLocAU_4Map)
          
        }##expression~END
        , selection = "single"
        , filter = "top"
        , options = list(scrollX = TRUE
                         , pageLength = 5
                         , lengthMenu = c(5, 10, 25, 50, 100)
                         , autoWidth = TRUE) 
        
        )##output$df_import_DT~END
        
        # browser()
        
        ### 5b. Counts ####
        # Define a reactive value for storing the row count
        row_count_matched <- reactiveVal(0)
        row_count_unmatched_G50 <- reactiveVal(0)
        row_count_unmatched_L50 <- reactiveVal(0)
        
        # Observe the changes in df_MonLocAU_4Map and update the row count
        observe({
          req(AU_Val$df_MonLocAU_4Map)
          nMatched <- sum(AU_Val$df_MonLocAU_4Map$MatchGroup == "Matched")
          nUnmatched_G50 <- sum(AU_Val$df_MonLocAU_4Map$MatchGroup == "UnMatched"
                                & AU_Val$df_MonLocAU_4Map$Dist_to_AU_m >= 50)
          nUnmatched_L50 <- sum(AU_Val$df_MonLocAU_4Map$MatchGroup == "UnMatched"
                                & AU_Val$df_MonLocAU_4Map$Dist_to_AU_m < 50)
          row_count_matched(nMatched)
          row_count_unmatched_G50(nUnmatched_G50)
          row_count_unmatched_L50(nUnmatched_L50)
        })# END ~ observe
        
        # Render the valueBox based on the row count
        output$MatchCount <- renderValueBox({
          valueBox(value = row_count_matched()
                   , subtitle = "Number of Monitoring Locations matched to AU crosswalk table"
                   , icon = icon("hashtag")
                   , color = "aqua"
          )
        })# END ~ renderValueBox
        
        output$UnMatchCount_G50 <- renderValueBox({
          valueBox(value = row_count_unmatched_G50()
                   , subtitle = "Number of Monitoring Locations with AU spatial join >= 50m"
                   , icon = icon("hashtag")
                   , color = "red"
          )
        })# END ~ renderValueBox
        
        output$UnMatchCount_L50 <- renderValueBox({
          valueBox(value = row_count_unmatched_L50()
                   , subtitle = "Number of Monitoring Locations with AU spatial join < 50m"
                   , icon = icon("hashtag")
                   , color = "yellow"
          )
        })# END ~ renderValueBox
        
        ## 6. Map ####
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Display results, ~15 sec")
        Sys.sleep(0.25)
        
        output$mymap <- renderLeaflet({
          req(AU_Val$df_MonLocAU_4Map)
          
          # Subset data by Match group
          data_match <- AU_Val$df_MonLocAU_4Map %>% 
            filter(MatchGroup == "Matched")
          
          data_unmatch <- AU_Val$df_MonLocAU_4Map %>% 
            filter(MatchGroup == "UnMatched")
          
          leaflet("mymap") %>%
            addTiles() %>%
            addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "Esri Ortho") %>% 
            addCircleMarkers(data = data_match, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                             , group = "Matched Sites"
                             , popup = paste("SiteID:", data_match$MonitoringLocationIdentifier, "<br>"
                                             ,"Site Name:", data_match$MonitoringLocationName, "<br>"
                                             ,"Site Type:", data_match$MonitoringLocationTypeName, "<br>"
                                             ,"State:", data_match$State, "<br>"
                                             ,"AU_ID:", data_match$AU_ID, "<br>"
                                             ,"AU NAME:", data_match$AU_NAME)
                             , color = "black", fillColor = "blue", fillOpacity = 1, stroke = TRUE
                             # , clusterOptions = markerClusterOptions()
            ) %>% 
            addCircleMarkers(data = data_unmatch, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure
                             , group = "Unmatched Sites"
                             , popup = paste("SiteID:", data_unmatch$MonitoringLocationIdentifier, "<br>"
                                             ,"Site Name:", data_unmatch$MonitoringLocationName, "<br>"
                                             ,"Site Type:", data_unmatch$MonitoringLocationTypeName, "<br>"
                                             ,"State:", data_unmatch$State, "<br>"
                                             ,"AU_ID:", data_unmatch$AU_ID, "<br>"
                                             ,"AU NAME:", data_unmatch$AU_NAME)
                             , color = "black", fillColor = "red", fillOpacity = 1, stroke = TRUE
                             # , clusterOptions = markerClusterOptions()
            ) %>%
            addPolylines(data = streams_simp_shp, color = "blue", weight = 3
                         , label = streams_simp_shp$AU_ID, group = "River AUs"
                         , popup = paste("<b> AU_ID:</b>", streams_simp_shp$AU_ID, "<br>"
                                         ,"<b> AU_Name:</b>", streams_simp_shp$AU_NAME)) %>%
            addPolygons(data = lakes_shp, color = "black", weight = 2, opacity = 1
                        , fillColor = "#df65b0", fillOpacity = 0.25, group = "Lake AUs"
                        , popup = paste("<b> AU_ID:</b>", lakes_shp$AU_ID, "<br>"
                                        ,"<b> AU_Name:</b>", lakes_shp$AU_NAME)) %>%
            addLayersControl(overlayGroups = c("Matched Sites", "Unmatched Sites"
                                               , "River AUs", "Lake AUs")
                             ,baseGroups = c("Esri WSM", "Esri Ortho")
                             ,options = layersControlOptions(collapsed = TRUE)) %>%
            addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap
                       , position = "bottomright")
        }) # END ~ renderLeaflet
        
        ## Map Zoom ####
        # Map that filters output data to a single location
        observeEvent(input$input_site_choice, {
          req(input$input_site_choice != "")
          
          mySite <- input$input_site_choice
          
          df_MonLocAU_select <- AU_Val$df_MonLocAU_4Map %>% 
            filter(MonitoringLocationIdentifier == mySite)
          
          site_long <- df_MonLocAU_select$LongitudeMeasure # longitude
          site_lat <- df_MonLocAU_select$LatitudeMeasure # latitude
          
          # modfiy map
          leafletProxy("mymap") %>%
            # setView(lng = site_long, lat = site_lat, zoom = 12)
            flyTo(lng = site_long, lat = site_lat, zoom = 16)
        })#observeEvent ~ END
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n_inc, detail = "Process completed")
        Sys.sleep(0.25)
        
        # 7. Save results ####
        # Save, Matched
        fn_results <- "MonitoringLocations_AU_Matched.csv"
        dn_results <- path_results
        pn_results <- file.path(dn_results, fn_results)
        write.csv(df_MonLocMatched, pn_results, row.names = FALSE)
        
        # Save, SpatJoin
        fn_results <- "MonitoringLocations_AU_SpatialJoin.csv"
        dn_results <- path_results
        pn_results <- file.path(dn_results, fn_results)
        write.csv(df_SpatJoin_Final, pn_results, row.names = FALSE)
        
        # Save, Import Data
        # if (data_storage$AU_latest_upload %in% "Upload from the Join AU tab"){
        #   inFile <- input$file_AU
        #   AU_filename <- inFile$name
        # } else if (data_storage$AU_latest_upload %in% "Data downloaded from WQP"){
        #   AU_filename <- paste0("WQP_", Sys.Date(), "_Import.csv")
        # }
        
        fn_results <- "Import.csv"
        dn_results <- path_results
        pn_results <- file.path(dn_results, fn_results)
        write.csv(AU_Val$WQP_AU, pn_results, row.names = FALSE)
        
        # Create zip file of results
        fn_4zip <- list.files(path = path_results
                              , full.names = TRUE)
        zip::zip(file.path(path_results, "results.zip"), fn_4zip)
        
        # button, enable, download
        # shinyjs::enable("b_download")
        
        # Pop up ####
        n_match <- sum(AU_Val$df_MonLocAU_4Map$MatchGroup == "Matched")
        n_joinG50 <- sum(AU_Val$df_MonLocAU_4Map$MatchGroup == "UnMatched"
                         & AU_Val$df_MonLocAU_4Map$Dist_to_AU_m >= 50)
        n_joinL50 <- sum(AU_Val$df_MonLocAU_4Map$MatchGroup == "UnMatched"
                         & AU_Val$df_MonLocAU_4Map$Dist_to_AU_m < 50)
        msg <- paste0("The AU join is complete, but results take ~30 seconds to render in the app!"
                      , " Please be patient and wait for the map to render to use in QC review of spatial joins.", "\n\n"
                      , "Number of sites matched to AUs = ", n_match, "\n\n"
                      , "Number of sites spatially joined to AUs (<50m distance) = ", n_joinL50, "\n\n"
                      , "Number of sites spatially joined to AUs (>=50m distance) = ", n_joinG50, "\n\n")
        shinyalert::shinyalert(title = "Task Complete"
                               , text = msg
                               , type = "success"
                               , closeOnEsc = TRUE
                               , closeOnClickOutside = TRUE
                               , size = "m")
        # validate(msg)
        
        ### Join AU information to SANDS' SUMMARY and STATION sheet
        observe({
          req(#reVal$SANDS_download_ready,
              reVal$site_temp2, nrow(reVal$site_temp2) > 0,
              reVal$summary_temp, nrow(reVal$summary_temp) > 0,
              AU_Val$df_MonLocAU_4Map)
          
          AU_Val$site_temp <- SANDS_STATION_AU_Join(
            dat = reVal$site_temp2,
            AU_table = AU_Val$df_MonLocAU_4Map)
          
          AU_Val$summary_temp <- SANDS_SUMMARY_AU_Join(
            dat = reVal$summary_temp,
            AU_table = AU_Val$df_MonLocAU_4Map)
          
          AU_Val$SANDS_download_ready <- TRUE
        })
        
        # Join the AU assessment data to the reVal$WQP_dat3
        observe({
          req(data_storage$AU_latest_data)
          
          # df_AU_join <- bind_rows(df_MonLocMatched, df_SpatJoin_Final)
          
          AU_Val$WQP_dat2 <- data_storage$AU_latest_data %>%
            left_join(AU_Val$df_MonLocAU_4Map %>%
                        fselect(-LatitudeMeasure, -LongitudeMeasure), 
                      by = c("MonitoringLocationIdentifier",
                             "StateName" = "State",
                             "MonitoringLocationName",
                             "MonitoringLocationTypeName"))
          
          AU_Val$WQP_time <- Sys.time()
          # Prepare a data frame for the Join AU tab
          observe({
            req(AU_Val$WQP_dat2, nrow(AU_Val$WQP_dat2) > 0)
            
            if (validate_fun(AU_Val$WQP_dat2 , SU_cols)){
              data_storage$SU_latest_upload <- "Data processed from the Join AU tab"
              data_storage$SU_time <- reVal$WQP_time
              data_storage$SU_latest_data <- AU_Val$WQP_dat2
            }
          })
          
        })
        
      }) #END ~ withProgress
    },
    blocking_level = "error")
    
  }) # END ~ observeEvent
  
  ## Download data ####
  output$b_download <- downloadHandler(
    
    filename = function() {
      
      # if (data_storage$AU_latest_upload %in% "Upload from the Join AU tab"){
      #   inFile <- input$file_AU
      #   AU_filename <- inFile$name
      # } else if (data_storage$AU_latest_upload %in% "Data downloaded from WQP"){
      #   AU_filename <- "WQP"
      # }
      
      AU_filename <- "WQP"
      
      file_AU_base <- tools::file_path_sans_ext(AU_filename)
      paste0(file_AU_base
             , "_results_"
             , format(Sys.Date(), "%Y%m%d")
             , ".zip")
    } ,
    content = function(fname) {##content~START
      
      file.copy(file.path(path_results, "results.zip"), fname)
      
    }##content~END
    #, contentType = "application/zip"
  )##download ~ TaxaTrans
  
  output$WQP_AU_Download <- downloadHandler(
    filename = function() {
      paste0("WQP_data_AU_", Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      showModal(modalDialog(title = "Saving the CSV spreadsheet...", footer = NULL))
      
      fwrite(AU_Val$WQP_dat2, file = con, na = "", bom = TRUE)
      
      removeModal()
    }
  )
  
  ### Data Sufficiency
  
  SU_Val <- reactiveValues()
  
  # File upload indicator
  upload_dat_SU <- reactive({
    req(input$file_SU)
    
    if (str_detect(input$file_SU$name, "\\.csv$")){
      temp <- read_csv(input$file_SU$datapath)
    } else {
      temp <- NULL
    }
    
    SU_Val$upload_time <- Sys.time()
    
    return(temp)
  })
  
  # Replace reVal$WQP_dat2 with upload_dat() if upload_dat() meets the requirements
  observe({
    req(upload_dat_SU())
    
    if (validate_fun(upload_dat_SU() , SU_cols)){
      data_storage$SU_latest_upload <- "Upload from the Data Sufficiency tab"
      data_storage$SU_time <- SU_Val$upload_time
      data_storage$SU_latest_data <- upload_dat_SU()
    }
  })
  
  output$file_indicator_SU <- renderText({
    if (is.null(data_storage$SU_latest_upload)){
      temp_text <- "Require downloading data or uploading a CSV file with the data."
    } else {
      temp_text <- paste0(data_storage$SU_latest_upload, " at ", 
                          round(data_storage$SU_time))
    }
    return(temp_text)
  })
  
  # Display data information after data cleaning
  observe({
    req(data_storage$SU_latest_data, nrow(data_storage$SU_latest_data) > 0)
    
    SU_Val$min_date2 <- min(data_storage$SU_latest_data$ActivityStartDate)
    SU_Val$max_date2 <- max(data_storage$SU_latest_data$ActivityStartDate)
    SU_Val$site_number2 <- length(unique(data_storage$SU_latest_data$MonitoringLocationIdentifier))
    SU_Val$sample_size2 <- nrow(data_storage$SU_latest_data)
    
    output$Date_Info_SU <- renderText({
      paste0("There are ", SU_Val$sample_size2, " records from ", 
             SU_Val$site_number2, " sites.")
    })
  })
  
  # Update fraction
  observeEvent(input$all_fractions_SU, {
    req(data_storage$SU_latest_data, nrow(data_storage$SU_latest_data) > 0)
    
    WQP_dat3 <- data_storage$SU_latest_data
    
    fraction_list <- sort(unique(WQP_dat3$TADA.ResultSampleFractionText))
    
    updateSelectizeInput(session = session, inputId = "fraction_combined_SU",
                         choices = fraction_list)
    
  })
  
  ### Run the data sufficiency test
  observe({
    req(data_storage$SU_latest_data, nrow(data_storage$SU_latest_data) > 0)
    
    dat <- data_storage$SU_latest_data
    
    # Add pH, temperature, and Hardness data
    dat <- dat %>% 
      column_name_change() %>%
      dateTime_create() %>%
      pH_fun() %>% 
      temp_fun() %>%
      hardness_fun() %>%
      temperature_pH_flag()
    
    # Join data_sus2 to dat
    dat2 <- dat %>% 
      sufficiency_join(data_sus2)
    
    # Remove dat to save memory
    rm(dat)
    
    dat3 <- dat2 %>%
      fmutate(Year = year(DateTime), Month = month(DateTime)) %>%
      fmutate(RYear = ifelse(Month >= 4, Year + 1, Year))
    
    # Remove dat2 to save memory
    rm(dat2)
    
    # Filter the fraction
    if (input$all_fractions_SU){
      dat3 <- dat3 %>%
        # fsubset(Standard_Fraction %in% input$fraction_combined_SU) %>%
        fmutate(Standard_Fraction_ori = Standard_Fraction) %>%
        fmutate(Standard_Fraction = case_when(
          Standard_Fraction %in% input$fraction_combined_SU           ~"Combined",
          TRUE                                                        ~Standard_Fraction
        ))
    } else {
      dat3 <- dat3 %>% 
        fmutate(Standard_Fraction_ori = Standard_Fraction) 
    }
    
    SU_Val$WQP_dat2 <- dat3
  })
  
  ### Sufficiency test on the site level
  Site_dat_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    
    dat <- SU_Val$WQP_dat2 %>% 
      su_group(StateName, Standard_Name, AU_ID, AU_NAME, 
               MonitoringLocationIdentifier, MonitoringLocationName,
               LatitudeMeasure, LongitudeMeasure,
               Standard_Fraction, # This is the place to combined standard
               Standard_Unit,
               Use, Details_Criteria,
               Minimum_Assessment_Period_Years, 
               Minimum_Data_Points,
               Other_Requirements,
               Hardness_Notes,
               Temp_Notes,
               pH_Notes,
               Sufficiency_Q)
    
    return(dat)
  })
  
  ### Sufficiency test on the AU level
  AU_dat_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    
    dat <- SU_Val$WQP_dat2 %>% 
      su_group(StateName, Standard_Name, AU_ID, AU_NAME, 
               Standard_Fraction, # This is the place to combined standard
               Standard_Unit,
               Use, Details_Criteria,
               Minimum_Assessment_Period_Years, 
               Minimum_Data_Points,
               Other_Requirements,
               Hardness_Notes,
               Temp_Notes,
               pH_Notes, Sufficiency_Q)
    
    return(dat)
    
  })
  
  ### Overwhelming evidence on the site level
  Site_over_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    
    ### Overwhelming evidence 
    
    # Metals 
    dat3_over_metals <- SU_Val$WQP_dat2 %>%
      filter(Standard_Name %in% over_acute_metals) %>%
      filter(Details_Criteria %in% "Acute", 
             StateName %in% "Montana",
             Standard_Unit %in% "UG/L") %>%
      mutate(ID = 1:n()) %>%
      mutate(Overwhelming_Evidence = 1)
    
    group312 <- dat3_over_metals %>% 
      fsubset(pH_Notes %in% 3 & Hardness_Notes %in% 1 & Temp_Notes %in% 2) 
    
    group312_2 <- group312 %>%
      left_join(Equation_table %>% fselect(-Fraction), 
                by = c("StateName" = "State", 
                       "StateAbbrev",
                       "Standard_Name" = "Constituent",
                       # "Standard_Fraction" = "Fraction",
                       "Use", "Details_Criteria" = "Details")) %>%
      fmutate(Criteria_Upper = pmap_dbl(list(hardness = Hardness, 
                                             CF_A = CF_A, 
                                             CF_B = CF_B, 
                                             CF_C = CF_C, 
                                             E_A = E_A, 
                                             E_B = E_B), .f = hardness_criteria_fun)) %>%
      select(-(CF_A:E_B)) %>%
      fmutate(Magnitude_Upper = Criteria_Upper) %>%
      fselect(-Criteria_Upper)
    
    group132 <- dat3_over_metals %>% 
      fsubset(pH_Notes %in% 1 & Hardness_Notes %in% 3 & Temp_Notes %in% 2) 
    
    group132_2 <- group132 %>%
      fmutate(Magnitude_Upper = ifelse(pH >= 6.5 & pH <= 9, Magnitude_Upper, NA_real_)) 
    
    dat3_over_metals_c <- dat3_over_metals %>%
      anti_join(group132, by = "ID") %>%
      anti_join(group312, by = "ID")
    
    dat3_over_metals2 <- bind_rows(dat3_over_metals_c, group312_2, group132_2)
    
    dat3_over_metals3 <- dat3_over_metals2 %>%
      mutate(Exceedance = TADA.ResultMeasureValue > 2 * Magnitude_Upper) %>%
      group_by(StateName, Standard_Name, AU_ID, AU_NAME, 
               MonitoringLocationIdentifier,
               LatitudeMeasure, LongitudeMeasure,
               Standard_Fraction, # This is the place to combined standard
               Standard_Unit,
               Use, Details_Criteria,
               Minimum_Assessment_Period_Years, 
               Minimum_Data_Points,
               Other_Requirements,
               Sufficiency_Q,
               Overwhelming_Evidence) %>%
      summarize(Exceedance_Sum = sum(Exceedance, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Overwhelming_Flag = ifelse(Exceedance_Sum > 0, "Yes", "No")) %>%
      dplyr::select(-Exceedance_Sum)
    
    # E. coli and Ammonia
    dat3_over_others <- SU_Val$WQP_dat2 %>%
      filter(Standard_Name %in% c("ESCHERICHIA COLI", "AMMONIA")) %>%
      # Change the criteria
      left_join(data_over2 %>% 
                  dplyr::select(-StateAbbrev, -Details) %>%
                  rename(Minimum_Assessment_Period_Years_Over = Minimum_Assessment_Period_Years,
                         Minimum_Data_Points_Over = Minimum_Data_Points,
                         Other_Requirements_Over = Other_Requirements), 
                by = c("StateName" = "State",
                       "Waterbody_Type", "Constituent_Group", "Constituent",
                       "Standard_Name" = "TADA.Constituent",
                       "Standard_Unit" = "MagUnits",
                       "Fraction_Criteria" = "Fraction",
                       "Use",
                       # "Details_Criteria" = "Details",
                       "Listing_methodology")) %>%
      dplyr::select(-Minimum_Assessment_Period_Years, -Minimum_Data_Points, 
                    -Other_Requirements) %>%
      rename(Minimum_Assessment_Period_Years = Minimum_Assessment_Period_Years_Over,
             Minimum_Data_Points = Minimum_Data_Points_Over,
             Other_Requirements = Other_Requirements_Over)
    
    ### Count sample size and years
    
    dat3_over_others2 <- dat3_over_others %>%
      su_group(StateName, Standard_Name, AU_ID, AU_NAME, 
               MonitoringLocationIdentifier,
               LatitudeMeasure, LongitudeMeasure,
               Standard_Fraction, # This is the place to combined standard
               Standard_Unit,
               Use, Details_Criteria,
               Minimum_Assessment_Period_Years, 
               Minimum_Data_Points,
               Other_Requirements,
               Sufficiency_Q,
               Overwhelming_Evidence) %>%
      fmutate(Year_Over_Flag = case_when(
        is.na(Minimum_Assessment_Period_Years)                                   ~ "Not Required",
        Year_n >= Minimum_Assessment_Period_Years                                ~ "Yes",
        TRUE                                                                     ~ "No"
      )) %>%
      fmutate(Sample_Over_Flag = case_when(
        is.na(Minimum_Data_Points)                                                 ~ "Not Required",
        Sample_n >= Minimum_Data_Points                                            ~ "Yes",
        TRUE                                                                       ~ "No"
      )) %>%
      mutate(Overwhelming_Flag = case_when(
        Year_Over_Flag %in% "Yes" & Sample_Over_Flag %in% "Yes"                    ~ "Yes",
        Year_Over_Flag %in% "No" & Sample_Over_Flag %in% "Yes"                     ~ "No",
        Year_Over_Flag %in% "Yes" & Sample_Over_Flag %in% "No"                     ~ "No",
        Year_Over_Flag %in% "No" & Sample_Over_Flag %in% "No"                      ~ "No",
        TRUE                                                                       ~ "Not Required"
      )) %>%
      dplyr::select(-Year_Over_Flag, -Sample_Over_Flag, -Year_n, -Sample_n)
    
    over_dat_summary <- bind_rows(dat3_over_metals3, dat3_over_others2) %>%
      dplyr::select(-Minimum_Assessment_Period_Years, -Minimum_Data_Points, 
                    -Other_Requirements, -Sufficiency_Q)
    
    return(over_dat_summary)
  })

  ### Overwhelming evidence on the AU level
  AU_over_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    
    ### Overwhelming evidence 
    
    # Metals 
    dat3_over_metals <- SU_Val$WQP_dat2 %>%
      filter(Standard_Name %in% over_acute_metals) %>%
      filter(Details_Criteria %in% "Acute", 
             StateName %in% "Montana",
             Standard_Unit %in% "UG/L") %>%
      mutate(ID = 1:n()) %>%
      mutate(Overwhelming_Evidence = 1)
    
    group312 <- dat3_over_metals %>% 
      fsubset(pH_Notes %in% 3 & Hardness_Notes %in% 1 & Temp_Notes %in% 2) 
    
    group312_2 <- group312 %>%
      left_join(Equation_table %>% fselect(-Fraction), 
                by = c("StateName" = "State", 
                       "StateAbbrev",
                       "Standard_Name" = "Constituent",
                       # "Standard_Fraction" = "Fraction",
                       "Use", "Details_Criteria" = "Details")) %>%
      fmutate(Criteria_Upper = pmap_dbl(list(hardness = Hardness, 
                                             CF_A = CF_A, 
                                             CF_B = CF_B, 
                                             CF_C = CF_C, 
                                             E_A = E_A, 
                                             E_B = E_B), .f = hardness_criteria_fun)) %>%
      select(-(CF_A:E_B)) %>%
      fmutate(Magnitude_Upper = Criteria_Upper) %>%
      fselect(-Criteria_Upper)
    
    group132 <- dat3_over_metals %>% 
      fsubset(pH_Notes %in% 1 & Hardness_Notes %in% 3 & Temp_Notes %in% 2) 
    
    group132_2 <- group132 %>%
      fmutate(Magnitude_Upper = ifelse(pH >= 6.5 & pH <= 9, Magnitude_Upper, NA_real_)) 
    
    dat3_over_metals_c <- dat3_over_metals %>%
      anti_join(group132, by = "ID") %>%
      anti_join(group312, by = "ID")
    
    dat3_over_metals2 <- bind_rows(dat3_over_metals_c, group312_2, group132_2)
    
    dat3_over_metals3 <- dat3_over_metals2 %>%
      mutate(Exceedance = TADA.ResultMeasureValue > 2 * Magnitude_Upper) %>%
      group_by(StateName, Standard_Name, AU_ID, AU_NAME, 
               Standard_Fraction, # This is the place to combined standard
               Standard_Unit,
               Use, Details_Criteria,
               Minimum_Assessment_Period_Years, 
               Minimum_Data_Points,
               Other_Requirements,
               Sufficiency_Q,
               Overwhelming_Evidence) %>%
      summarize(Exceedance_Sum = sum(Exceedance, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Overwhelming_Flag = ifelse(Exceedance_Sum > 0, "Yes", "No")) %>%
      dplyr::select(-Exceedance_Sum)
    
    # E. coli and Ammonia
    dat3_over_others <- SU_Val$WQP_dat2 %>%
      filter(Standard_Name %in% c("ESCHERICHIA COLI", "AMMONIA")) %>%
      # Change the criteria
      left_join(data_over2 %>% 
                  dplyr::select(-StateAbbrev, -Details) %>%
                  rename(Minimum_Assessment_Period_Years_Over = Minimum_Assessment_Period_Years,
                         Minimum_Data_Points_Over = Minimum_Data_Points,
                         Other_Requirements_Over = Other_Requirements), 
                by = c("StateName" = "State",
                       "Waterbody_Type", "Constituent_Group", "Constituent",
                       "Standard_Name" = "TADA.Constituent",
                       "Standard_Unit" = "MagUnits",
                       "Fraction_Criteria" = "Fraction",
                       "Use",
                       # "Details_Criteria" = "Details",
                       "Listing_methodology")) %>%
      dplyr::select(-Minimum_Assessment_Period_Years, -Minimum_Data_Points, 
                    -Other_Requirements) %>%
      rename(Minimum_Assessment_Period_Years = Minimum_Assessment_Period_Years_Over,
             Minimum_Data_Points = Minimum_Data_Points_Over,
             Other_Requirements = Other_Requirements_Over)
    
    ### Count sample size and years
    
    dat3_over_others2 <- dat3_over_others %>%
      su_group(StateName, Standard_Name, AU_ID, AU_NAME, 
               Standard_Fraction, # This is the place to combined standard
               Standard_Unit,
               Use, Details_Criteria,
               Minimum_Assessment_Period_Years, 
               Minimum_Data_Points,
               Other_Requirements,
               Sufficiency_Q,
               Overwhelming_Evidence) %>%
      fmutate(Year_Over_Flag = case_when(
        is.na(Minimum_Assessment_Period_Years)                                   ~ "Not Required",
        Year_n >= Minimum_Assessment_Period_Years                                ~ "Yes",
        TRUE                                                                     ~ "No"
      )) %>%
      fmutate(Sample_Over_Flag = case_when(
        is.na(Minimum_Data_Points)                                                 ~ "Not Required",
        Sample_n >= Minimum_Data_Points                                            ~ "Yes",
        TRUE                                                                       ~ "No"
      )) %>%
      mutate(Overwhelming_Flag = case_when(
        Year_Over_Flag %in% "Yes" & Sample_Over_Flag %in% "Yes"                    ~ "Yes",
        Year_Over_Flag %in% "No" & Sample_Over_Flag %in% "Yes"                     ~ "No",
        Year_Over_Flag %in% "Yes" & Sample_Over_Flag %in% "No"                     ~ "No",
        Year_Over_Flag %in% "No" & Sample_Over_Flag %in% "No"                      ~ "No",
        TRUE                                                                       ~ "Not Required"
      )) %>%
      dplyr::select(-Year_Over_Flag, -Sample_Over_Flag, -Year_n, -Sample_n)
    
    over_dat_summary <- bind_rows(dat3_over_metals3, dat3_over_others2) %>%
      dplyr::select(-Minimum_Assessment_Period_Years, -Minimum_Data_Points, 
                    -Other_Requirements, -Sufficiency_Q)
    
    return(over_dat_summary)
    
  })
  
  ### Evaluatate hardness, pH, and temperature on the site level
  Site_hardness_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    dat <- SU_Val$WQP_dat2 %>%
      su_hardness_pH_temp_count(vars = "Hardness",
                                parname = "Hardness",
                                StateName, Standard_Name, AU_ID, AU_NAME, MonitoringLocationIdentifier,
                                LatitudeMeasure, LongitudeMeasure,
                                Standard_Fraction, # This is the place to combined standard
                                Standard_Unit,
                                Use, Details_Criteria)
    return(dat)
  })
  
  Site_pH_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    dat <- SU_Val$WQP_dat2 %>%
      su_hardness_pH_temp_count(vars = "pH",
                                parname = "pH",
                                StateName, Standard_Name, AU_ID, AU_NAME, MonitoringLocationIdentifier,
                                LatitudeMeasure, LongitudeMeasure,
                                Standard_Fraction, # This is the place to combined standard
                                Standard_Unit,
                                Use, Details_Criteria)
    return(dat)
  })
  
  Site_Temp_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    dat <- SU_Val$WQP_dat2 %>%
      su_hardness_pH_temp_count(vars = "Temperature",
                                parname = "Temp",
                                StateName, Standard_Name, AU_ID, AU_NAME, MonitoringLocationIdentifier,
                                LatitudeMeasure, LongitudeMeasure,
                                Standard_Fraction, # This is the place to combined standard
                                Standard_Unit,
                                Use, Details_Criteria)
    return(dat)
  })
  
  ### # Evalutate hardness, pH, and temperature on the AU level
  AU_hardness_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    dat <- SU_Val$WQP_dat2 %>%
      su_hardness_pH_temp_count(vars = "Hardness",
                                parname = "Hardness",
                                StateName, Standard_Name, AU_ID, AU_NAME, 
                                Standard_Fraction, # This is the place to combined standard
                                Standard_Unit,
                                Use, Details_Criteria) 
    return(dat)
  })
  
  AU_pH_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    dat <- SU_Val$WQP_dat2 %>%
      su_hardness_pH_temp_count(vars = "pH",
                                parname = "pH",
                                StateName, Standard_Name, AU_ID, AU_NAME, 
                                Standard_Fraction, # This is the place to combined standard
                                Standard_Unit,
                                Use, Details_Criteria)
    return(dat)
  })
  
  AU_Temp_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0)
    dat <- SU_Val$WQP_dat2 %>%
      su_hardness_pH_temp_count(vars = "Temperature",
                                parname = "Temp",
                                StateName, Standard_Name, AU_ID, AU_NAME, 
                                Standard_Fraction, # This is the place to combined standard
                                Standard_Unit,
                                Use, Details_Criteria)
    return(dat)
  })
  
  ### Join the results
  Site_join_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, Site_dat_su(),
        Site_over_su(), Site_hardness_su(), Site_pH_su(), Site_Temp_su())
    
    colname <- c("StateName", "Standard_Name", "AU_ID", "AU_NAME", 
                 "MonitoringLocationIdentifier",
                 "LatitudeMeasure", "LongitudeMeasure",
                 "Standard_Fraction",
                 "Standard_Unit",
                 "Use", "Details_Criteria")
    
    dat <- Site_dat_su() %>%
      Join_su(
        over = Site_over_su(), hardness = Site_hardness_su(),
        pH = Site_pH_su(), temp = Site_Temp_su(),
        colname = colname
      )
    
    return(dat)
  })
  
  AU_join_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, AU_dat_su(),
        AU_over_su(), AU_hardness_su(), c, AU_Temp_su())
    
    colname <- c("StateName", "Standard_Name", "AU_ID", "AU_NAME", 
                 "Standard_Fraction",
                 "Standard_Unit",
                 "Use", "Details_Criteria")
    
    dat <- AU_dat_su() %>%
      Join_su(
        over = AU_over_su(), hardness = AU_hardness_su(),
        pH = AU_pH_su(), temp = AU_Temp_su(),
        colname = colname
      )
    return(dat)
  })
  
  ### Assess the sufficiency results
  Site_assess_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, Site_join_su())
    dat <- Site_join_su() %>% Assess_su()
    return(dat)
  })

  AU_assess_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, AU_join_su())
    dat <- AU_join_su() %>% Assess_su()
    return(dat)
  })
  
  ### Summarize the results
  Site_summary_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, Site_assess_su())
    dat <- Site_assess_su() %>% Summarize_su()
    return(dat)
  })
  
  AU_summary_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, AU_assess_su())
    dat <- AU_assess_su() %>% Summarize_su()
    return(dat)
  })
  
  ### Transform the results
  Site_transform_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, Site_assess_su())
    dat <- Site_assess_su() %>% 
      Transform_su(selected_cols = c(
        "StateName", "Standard_Name", "AU_ID", "AU_NAME", 
        "MonitoringLocationIdentifier", "Standard_Fraction", "Standard_Unit",
        "Use", "Details", "Sufficiency"
      )) %>%
      rename(Site_Sufficiency_Notes = Sufficiency_Notes)
    return(dat)
  })
  
  AU_transform_su <- reactive({
    req(SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, AU_assess_su())
    dat <- AU_assess_su() %>% 
      Transform_su(selected_cols = c(
        "StateName", "Standard_Name", "AU_ID", "AU_NAME", 
        "Standard_Fraction", "Standard_Unit",
        "Use", "Details", "Sufficiency"
      )) %>%
      rename(AU_Sufficiency_Notes = Sufficiency_Notes)
    return(dat)
  })
  
  
  ### Join Site_transform_su and AU_transform_su to the data
  observe({
    req(data_storage$SU_latest_data, nrow(data_storage$SU_latest_data) > 0,
        SU_Val$WQP_dat2, nrow(SU_Val$WQP_dat2) > 0, 
        Site_transform_su(), AU_transform_su())
    
    dat <- data_storage$SU_latest_data %>%
      column_name_change() %>%
      dateTime_create() %>%
      pH_fun() %>% 
      temp_fun() %>%
      hardness_fun() %>%
      temperature_pH_flag() 
    
    # Filter the fraction
    if (!input$all_fractions_SU){
      dat <- dat %>%
        # fsubset(Standard_Fraction %in% input$fraction_combined_SU) %>%
        fmutate(Standard_Fraction_ori = Standard_Fraction) %>%
        fmutate(Standard_Fraction = case_when(
          Standard_Fraction %in% input$fraction_combined_SU           ~"Combined",
          TRUE                                                        ~Standard_Fraction
        ))
    } else {
      dat <- dat %>% 
        fmutate(Standard_Fraction_ori = Standard_Fraction) 
    }
    
    dat2 <- dat %>%
      left_join(Site_transform_su(),
                by = c("StateName", "Standard_Name", "AU_ID", "AU_NAME",
                       "MonitoringLocationIdentifier",
                       "Standard_Fraction", "Standard_Unit")) %>%
      left_join(AU_transform_su(),
                by = c("StateName", "Standard_Name", "AU_ID", "AU_NAME",
                       "Standard_Fraction", "Standard_Unit")) %>%
      column_name_change2()
    
    SU_Val$WQP_dat3 <- dat2
    
    SU_Val$WQP_time <- Sys.time()
    
    # Prepare a data frame for the Join AU tab
    observe({
      req(SU_Val$WQP_dat3, nrow(SU_Val$WQP_dat3) > 0)
      
      if (validate_fun(SU_Val$WQP_dat3 , AN_cols)){
        data_storage$AN_latest_upload <- "Data processed from Data Sufficiency tab"
        data_storage$AN_time <- SU_Val$WQP_time
        data_storage$AN_latest_data <- SU_Val$WQP_dat3
      }
    })
    
  })
  
  SU_final_dat <- reactive({
    req(SU_Val$WQP_dat3, nrow(SU_Val$WQP_dat3) > 0)
    temp_dat <- SU_Val$WQP_dat3
    return(temp_dat)
  })
  
  
  ### Select the results
  SU_result <- reactive({
    req(Site_summary_su(), AU_summary_su())
    if (input$sites_or_AU_SU %in% "Site"){
      dat <- Site_summary_su()
    }  else if (input$sites_or_AU_SU %in% "AU") {
      dat <- AU_summary_su()
    }
    return(dat)
  })
  
  ### Display the results
  output$Par_Select_SU <- renderDT({
    req(SU_result())
    
    if (input$sites_or_AU_SU %in% "Site"){
      col_site_temp <- "No. Site"
    } else if (input$sites_or_AU_SU %in% "AU"){
      col_site_temp <- "No. AU"
    }
    
    col_name <- c("State", "Parameter", "Fraction", "Unit",
                  "Use", "Details", "Sufficiency Analysis",
                  col_site_temp, "Sufficiency (%)")
    
    datatable(SU_result(),
              filter = "top",
              extensions = "Buttons",
              colnames = col_name,
              selection = "single",
              options = list(scrollX = TRUE,
                             autoWidth = TRUE,
                             dom = "Blfrtip",
                             pageLength = 20,
                             lengthMenu = c(5, 10, 20, 50, 100),
                             buttons = list(
                               list(
                                 extend = "csv",
                                 filename = "Par_Summary_SU_Table",
                                 exportOptions = list(
                                   modifier = list(
                                     page = "all"
                                   )
                                 )
                               )
                             ))) %>%
      formatPercentage(columns = c("Sufficiency"), 
                       digits = 2)
  }, server = FALSE)
  
  Par_Select_SU_table_proxy <- dataTableProxy("Par_Select_SU")
  
  ### Monitoring Site Selection Table
  
  # Update the table by the parameter criteria table
  
  selected_par_SU <- reactive({
    req(SU_result())
    
    if (!is.null(input$Par_Select_SU_rows_selected)){
      sel_dat <- SU_result() %>%
        slice(input$Par_Select_SU_rows_selected)
    } else {
      sel_dat <- SU_result() %>% slice(0)
    }
    return(sel_dat)
  })
  
  # Get the selected parameter
  selected_par_name_SU <- reactive({
    req(selected_par_SU(), input$Par_Select_SU_rows_selected)
    
    par_title <- paste0(selected_par_SU()$Standard_Name[1], ", ", 
                        selected_par_SU()$Standard_Fraction[1])
    
    return(par_title)
    
  })
  
  # Update the parameter names
  output$parameter_name1_SU <- renderText({
    req(selected_par_SU(), nrow(selected_par_SU()) > 0,
        input$Par_Select_SU_rows_selected, selected_par_name_SU())
    return(selected_par_name_SU())
  })
  
  # Update the parameter names
  output$parameter_name2_SU <- renderText({
    req(selected_par_SU(), nrow(selected_par_SU()) > 0,
        input$Par_Select_SU_rows_selected, selected_par_name_SU())
    return(selected_par_name_SU())
  })
  
  # Prepare the WQP_site table
  SU_WQP_site <- reactive({
    req(selected_par_SU(), nrow(selected_par_SU()) > 0,
        Site_assess_su(), AU_assess_su())
    
    if (input$sites_or_AU_SU %in% "Site"){
      dat <- Site_assess_su()
    } else if (input$sites_or_AU_SU %in% "AU"){
      dat <- AU_assess_su()
    }
    
    WQP_site <- dat %>%
      rename(Details = Details_Criteria) %>%
      fsubset(
        StateName %in% unique(selected_par_SU()$StateName) &
        Standard_Name %in% unique(selected_par_SU()$Standard_Name) &
        Standard_Fraction %in% unique(selected_par_SU()$Standard_Fraction) &
        Standard_Unit %in% unique(selected_par_SU()$Standard_Unit) &
        Details %in% unique(selected_par_SU()$Details) &
        Use %in% unique(selected_par_SU()$Use)
      ) %>%
      relocate(Overwhelming_Flag, .before = "Hardness_Notes") %>%
      dplyr::select(-Hardness_Notes:-pH_Notes, -Sufficiency_Q:-Temp_Sample_Flag)
    
    return(WQP_site)
  })
  
  output$site_table_SU <- renderDT({
    req(SU_WQP_site(), input$Par_Select_SU_rows_selected)

    # if (input$sites_or_AU_SU %in% "Site"){
    #   col_name_temp1 <-
    #   col_name_temp2 <-
    # } else if (input$sites_or_AU_SU %in% "AU"){
    #   col_name_temp1 <-
    #   col_name_temp2 <-
    # }
    #
    # col_name <- c("AU ID" = "AU_ID",
    #   "AU Name" = "AU_NAME",
    #   "Longitude" = "LongitudeMeasure",
    #   "Latitude" = "LatitudeMeasure",
    #   "N" = "Size",
    #   "No. of Exceedance" = "Exceedance_Size",
    #   "Exceedance Percentage" = "Percentage")

    datatable(SU_WQP_site(), filter = "none", rownames = FALSE,
              extensions = "Buttons",
              # colnames = col_name,
              selection = list(mode = "multiple"),
              options = list(pageLength = 20,
                             lengthMenu = c(5, 10, 20, 50, 100),
                             dom = "Blfrtip",
                             scrollX = TRUE,
                             buttons = list(
                               list(
                                 extend = "csv",
                                 filename = "Site_Table_SU",
                                 exportOptions = list(
                                   modifier = list(
                                     page = "all"
                                   )
                                 )
                               )
                             )))

  }, server = FALSE)

  site_table_SU_proxy <- dataTableProxy("site_table_SU")
  
  ### Map

  output$site_map_SU <- renderLeaflet({
    req(nrow(SU_WQP_site()) > 0, input$Par_Select_SU_rows_selected)
    
    if (input$sites_or_AU_SU %in% "Site"){
      # Create an indicator column to show if any data are above limit
      temp_dat <- SU_WQP_site() %>%
        mutate(Indicator = factor(Sufficiency, levels = c("Sufficient", "Insufficient", "Not Required"))) %>%
        # Create a label column
        mutate(label = paste0("Site No: ", "<strong>", MonitoringLocationIdentifier, "</strong>", "<br/>",
                              "Site Name:", "<strong>", MonitoringLocationName, "</strong>", "<br/>",
                              # "Year_Flag: ", "<strong>", Year_Flag, "</strong>", "<br/>",
                              # "Sample_Flag: ", "<strong>", Sample_Flag, "</strong>", "<br/>",
                              # "Hardness_Year_Flag: ", "<strong>", Hardness_Year_Flag, "</strong>", "<br/>",
                              # "Hardness_Sample_Flag: ", "<strong>", Hardness_Sample_Flag, "</strong>", "<br/>",
                              # "pH_Year_Flag: ", "<strong>", pH_Year_Flag, "</strong>", "<br/>",
                              # "pH_Sample_Flag: ", "<strong>", pH_Sample_Flag, "</strong>", "<br/>",
                              # "Temp_Year_Flag: ", "<strong>", Temp_Year_Flag, "</strong>", "<br/>",
                              # "Temp_Sample_Flag: ", "<strong>", Temp_Sample_Flag, "</strong>", "<br/>",
                              "Overwhelming_Flag: ", "<strong>", Overwhelming_Flag, "</strong>", "<br/>",
                              "Sufficiency: ", "<strong>", Sufficiency, "</strong>", "<br/>")) 
      
      labs <- as.list(temp_dat$label)
      
      pal_sc <- colorFactor(c("#FF7F00", "#1F78B4", "#666666"), temp_dat$Indicator)
      
      leaflet(temp_dat) %>%
        add_USGS_base() %>%
        addCircleMarkers(lng = ~LongitudeMeasure,
                         lat = ~LatitudeMeasure,
                         layerId = ~MonitoringLocationIdentifier,
                         radius = 8, stroke = TRUE, weight = 1,
                         color = "black",
                         fillColor = ~pal_sc(Indicator),
                         fillOpacity = 1,
                         opacity = 0.5,
                         label = map(labs, HTML),
                         group = "base_map") %>%
        addLegend("topright",
                  pal = pal_sc,
                  values = temp_dat$Indicator,
                  title = "Data Sufficiency",
                  opacity = 1) %>%
        setView(lng = -107.3045253, lat = 44.8494123, zoom = 5)
    } else if (input$sites_or_AU_SU %in% "AU"){
      
      WQP_SU_stream <- streams_simp_shp %>%
        semi_join(SU_WQP_site(), by = c("AU_ID", "AU_NAME")) %>%
        left_join(SU_WQP_site(), by = c("AU_ID", "AU_NAME"))
        
      WQP_SU_lakes <- lakes_shp %>%
        semi_join(SU_WQP_site(), by = c("AU_ID", "AU_NAME")) %>%
        left_join(SU_WQP_site(), by = c("AU_ID", "AU_NAME"))
      
      # Create an indicator column to show if any data are above limit
      temp_dat_stream <- WQP_SU_stream %>%
        mutate(Indicator = factor(Sufficiency, levels = c("Sufficient", "Insufficient", "Not Required"))) %>%
        # Create a label column
        mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                              "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                              # "Year_Flag: ", "<strong>", Year_Flag, "</strong>", "<br/>",
                              # "Sample_Flag: ", "<strong>", Sample_Flag, "</strong>", "<br/>",
                              # "Hardness_Year_Flag: ", "<strong>", Hardness_Year_Flag, "</strong>", "<br/>",
                              # "Hardness_Sample_Flag: ", "<strong>", Hardness_Sample_Flag, "</strong>", "<br/>",
                              # "pH_Year_Flag: ", "<strong>", pH_Year_Flag, "</strong>", "<br/>",
                              # "pH_Sample_Flag: ", "<strong>", pH_Sample_Flag, "</strong>", "<br/>",
                              # "Temp_Year_Flag: ", "<strong>", Temp_Year_Flag, "</strong>", "<br/>",
                              # "Temp_Sample_Flag: ", "<strong>", Temp_Sample_Flag, "</strong>", "<br/>",
                              "Overwhelming_Flag: ", "<strong>", Overwhelming_Flag, "</strong>", "<br/>",
                              "Sufficiency: ", "<strong>", Sufficiency, "</strong>", "<br/>")) 
      
      # Create an indicator column to show if any data are above limit
      temp_dat_lakes <- WQP_SU_lakes %>%
        mutate(Indicator = factor(Sufficiency, levels = c("Sufficient", "Insufficient", "Not Required"))) %>%
        # Create a label column
        mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                              "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                              # "Year_Flag: ", "<strong>", Year_Flag, "</strong>", "<br/>",
                              # "Sample_Flag: ", "<strong>", Sample_Flag, "</strong>", "<br/>",
                              # "Hardness_Year_Flag: ", "<strong>", Hardness_Year_Flag, "</strong>", "<br/>",
                              # "Hardness_Sample_Flag: ", "<strong>", Hardness_Sample_Flag, "</strong>", "<br/>",
                              # "pH_Year_Flag: ", "<strong>", pH_Year_Flag, "</strong>", "<br/>",
                              # "pH_Sample_Flag: ", "<strong>", pH_Sample_Flag, "</strong>", "<br/>",
                              # "Temp_Year_Flag: ", "<strong>", Temp_Year_Flag, "</strong>", "<br/>",
                              # "Temp_Sample_Flag: ", "<strong>", Temp_Sample_Flag, "</strong>", "<br/>",
                              "Overwhelming_Flag: ", "<strong>", Overwhelming_Flag, "</strong>", "<br/>",
                              "Sufficiency: ", "<strong>", Sufficiency, "</strong>", "<br/>")) 
      
      temp_dat <- bind_rows(
        temp_dat_stream %>% st_set_geometry(NULL),
        temp_dat_lakes %>% st_set_geometry(NULL)
      )
      
      labs <- as.list(temp_dat$label)
      
      pal_sc <- colorFactor(c("#FF7F00", "#1F78B4", "#666666"), temp_dat$Indicator)
      
      leaflet(temp_dat) %>%
        add_USGS_base() %>%
        addPolylines(data = temp_dat_stream, 
                     color = ~pal_sc(Indicator), 
                     opacity = 1,
                     weight = 10,
                     group = "base_map_river",
                     label = map(labs, HTML)) %>%
        addPolygons(data = temp_dat_lakes, 
                    color = "#03F", 
                    weight = 10, 
                    opacity = 1,
                    fillColor = ~pal_sc(Indicator),
                    fillOpacity = 1,
                    group = "base_map_lake",
                    label = map(labs, HTML)) %>%
        addLegend("topright",
                  pal = pal_sc,
                  values = temp_dat$Indicator,
                  title = "Data Sufficiency",
                  opacity = 1) %>%
        setView(lng = -107.3045253, lat = 44.8494123, zoom = 5)
    }
    
  })
  
  observe({
    req(SU_WQP_site(), selected_sites_SU())
    if (nrow(selected_sites_SU()) > 0){
      
      if (input$sites_or_AU_SU %in% "Site"){
        temp_dat <- selected_sites_SU() %>%
          mutate(new_ID = paste0(MonitoringLocationIdentifier, "_new")) %>%
          # Create a label column
          mutate(label = paste0("Site No: ", "<strong>", MonitoringLocationIdentifier, "</strong>", "<br/>",
                                "Site Name:", "<strong>", MonitoringLocationName, "</strong>", "<br/>",
                                # "Year_Flag: ", "<strong>", Year_Flag, "</strong>", "<br/>",
                                # "Sample_Flag: ", "<strong>", Sample_Flag, "</strong>", "<br/>",
                                # "Hardness_Year_Flag: ", "<strong>", Hardness_Year_Flag, "</strong>", "<br/>",
                                # "Hardness_Sample_Flag: ", "<strong>", Hardness_Sample_Flag, "</strong>", "<br/>",
                                # "pH_Year_Flag: ", "<strong>", pH_Year_Flag, "</strong>", "<br/>",
                                # "pH_Sample_Flag: ", "<strong>", pH_Sample_Flag, "</strong>", "<br/>",
                                # "Temp_Year_Flag: ", "<strong>", Temp_Year_Flag, "</strong>", "<br/>",
                                # "Temp_Sample_Flag: ", "<strong>", Temp_Sample_Flag, "</strong>", "<br/>",
                                "Overwhelming_Flag: ", "<strong>", Overwhelming_Flag, "</strong>", "<br/>",
                                "Sufficiency: ", "<strong>", Sufficiency, "</strong>", "<br/>")) 
        labs <- as.list(temp_dat$label)
        
        site_SU_proxy %>% clearGroup(group = "highlighted_point")
        site_SU_proxy %>%
          addCircleMarkers(lng = temp_dat$LongitudeMeasure,
                           lat = temp_dat$LatitudeMeasure,
                           layerId = temp_dat$new_ID,
                           radius = 8, stroke = TRUE, weight = 1,
                           color = "black", fillColor = "red",
                           fillOpacity = 1,
                           opacity = 0.5,
                           label = map(labs, HTML),
                           group = "highlighted_point")
      } else if (input$sites_or_AU_SU %in% "AU"){
        
        WQP_SU_stream <- streams_simp_shp %>%
          semi_join(selected_sites_SU(), by = c("AU_ID", "AU_NAME")) %>%
          left_join(selected_sites_SU(), by = c("AU_ID", "AU_NAME"))
        
        WQP_SU_lakes <- lakes_shp %>%
          semi_join(selected_sites_SU(), by = c("AU_ID", "AU_NAME")) %>%
          left_join(selected_sites_SU(), by = c("AU_ID", "AU_NAME"))
        
        temp_dat_stream <- WQP_SU_stream %>%
          mutate(new_ID = paste0(AU_ID, "_new")) %>%
          # Create a label column
          mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                                "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                                # "Year_Flag: ", "<strong>", Year_Flag, "</strong>", "<br/>",
                                # "Sample_Flag: ", "<strong>", Sample_Flag, "</strong>", "<br/>",
                                # "Hardness_Year_Flag: ", "<strong>", Hardness_Year_Flag, "</strong>", "<br/>",
                                # "Hardness_Sample_Flag: ", "<strong>", Hardness_Sample_Flag, "</strong>", "<br/>",
                                # "pH_Year_Flag: ", "<strong>", pH_Year_Flag, "</strong>", "<br/>",
                                # "pH_Sample_Flag: ", "<strong>", pH_Sample_Flag, "</strong>", "<br/>",
                                # "Temp_Year_Flag: ", "<strong>", Temp_Year_Flag, "</strong>", "<br/>",
                                # "Temp_Sample_Flag: ", "<strong>", Temp_Sample_Flag, "</strong>", "<br/>",
                                "Overwhelming_Flag: ", "<strong>", Overwhelming_Flag, "</strong>", "<br/>",
                                "Sufficiency: ", "<strong>", Sufficiency, "</strong>", "<br/>")) 
        
        temp_dat_lakes <- WQP_SU_lakes %>%
          mutate(new_ID = paste0(AU_ID, "_new")) %>%
          # Create a label column
          mutate(label = paste0("AU No: ", "<strong>", AU_ID, "</strong>", "<br/>",
                                "AU Name:", "<strong>", AU_NAME, "</strong>", "<br/>",
                                # "Year_Flag: ", "<strong>", Year_Flag, "</strong>", "<br/>",
                                # "Sample_Flag: ", "<strong>", Sample_Flag, "</strong>", "<br/>",
                                # "Hardness_Year_Flag: ", "<strong>", Hardness_Year_Flag, "</strong>", "<br/>",
                                # "Hardness_Sample_Flag: ", "<strong>", Hardness_Sample_Flag, "</strong>", "<br/>",
                                # "pH_Year_Flag: ", "<strong>", pH_Year_Flag, "</strong>", "<br/>",
                                # "pH_Sample_Flag: ", "<strong>", pH_Sample_Flag, "</strong>", "<br/>",
                                # "Temp_Year_Flag: ", "<strong>", Temp_Year_Flag, "</strong>", "<br/>",
                                # "Temp_Sample_Flag: ", "<strong>", Temp_Sample_Flag, "</strong>", "<br/>",
                                "Overwhelming_Flag: ", "<strong>", Overwhelming_Flag, "</strong>", "<br/>",
                                "Sufficiency: ", "<strong>", Sufficiency, "</strong>", "<br/>")) 
        
        temp_dat <- bind_rows(
          temp_dat_stream %>% st_set_geometry(NULL),
          temp_dat_lakes %>% st_set_geometry(NULL)
        )
        
        labs <- as.list(temp_dat$label)
        
        site_SU_proxy %>% clearGroup(group = "highlighted_point")
        site_SU_proxy %>%
          addPolylines(data = temp_dat_stream, 
                       layerId = temp_dat$new_ID,
                       weight = 10,
                       color = "red",
                       opacity = 1,
                       group = "highlighted_point",
                       label = map(labs, HTML)) %>%
          addPolygons(data = temp_dat_lakes, 
                      layerId = temp_dat$new_ID,
                      weight = 10, 
                      fillColor = "red",
                      fillOpacity = 1,
                      opacity = 0.5,
                      group = "highlighted_point",
                      label = map(labs, HTML))
      }
      
    } else {
      site_SU_proxy %>% clearGroup(group = "highlighted_point")
    }
  })
  
  # See the map click event
  observeEvent(input$site_map_SU_marker_click, {
    req(SU_WQP_site())
    
    temp_map_SU_click_sites <- input$site_map_SU_marker_click$id
    
    if (input$sites_or_AU_SU %in% "Site"){
      # if the temp_map_click_sites ends with "_new", need to remove "_new
      if (str_detect(temp_map_SU_click_sites, regex("_new$"))){
        
        temp_map_SU_remove_sites <- str_remove(temp_map_SU_click_sites, regex("_new$"))
        temp_site_remove_row <- which(SU_WQP_site()$MonitoringLocationIdentifier %in%
                                        temp_map_SU_remove_sites)
        
        SU_Val$site_select <- SU_Val$site_select[!SU_Val$site_select %in% temp_site_remove_row]
        
      } else {
        
        # Get the row ID based on MonitoringLocationIdentifier
        temp_site_SU_row <- which(SU_WQP_site()$MonitoringLocationIdentifier %in%
                                    temp_map_SU_click_sites)
        
        SU_Val$site_select <- unique(c(SU_Val$site_select, 
                                       temp_site_SU_row, 
                                       input$site_table_SU_rows_selected))
      }
    } else if (input$sites_or_AU_SU %in% "AU"){
      # if the temp_map_click_sites ends with "_new", need to remove "_new
      if (str_detect(temp_map_click_sites, regex("_new$"))){
        
        temp_map_SU_remove_sites <- str_remove(temp_map_SU_click_sites, regex("_new$"))
        temp_site_SU_remove_row <- which(SU_WQP_site()$AU_ID %in%
                                           temp_map_SU_remove_sites)
        
        SU_Val$site_select <- SU_Val$site_select[!SU_Val$site_select %in% temp_site_SU_remove_row]
        
      } else {
        
        # Get the row ID based on MonitoringLocationIdentifier
        temp_siteSU_row <- which(SU_WQP_site()$AU_ID %in%
                                   temp_map_SU_click_sites)
        
        SU_Val$site_select <- unique(c(SU_Val$site_select, 
                                       temp_site_SU_row, 
                                       input$site_table_SU_rows_selected))
      }
    }
    
    # Update the site_table
    site_table_SU_proxy %>%
      selectRows(selected = SU_Val$site_select)
  })
  
  
  # Create a subset of AN_Val$WQP_site2 based on input$site_table_rows_selected
  selected_sites_SU <- reactive({
    req(SU_WQP_site(), input$Par_Select_SU_rows_selected)

    if (!is.null(input$site_table_SU_rows_selected)){
      sel_dat <- SU_WQP_site() %>%
        slice(input$site_table_SU_rows_selected)
    } else {
      sel_dat <- SU_WQP_site() %>% slice(0)
    }
    return(sel_dat)
  })

  site_SU_proxy <- leafletProxy("site_map_SU")
  
  # ### Sync the two controls for SANDS in both the Download and AU tab
  # observe({
  #   updateCheckboxInput(session = session, inputId = "SANDS_check2", value = input$SANDS_check)
  # })
  # 
  # observe({
  #   updateCheckboxInput(session = session, inputId = "SANDS_check", value = input$SANDS_check2)
  # })
  # 
  # observe({
  #   updateCheckboxGroupButtons(session = session, inputId = "MT_par_group2", 
  #                              selected = input$MT_par_group)
  # })
  # 
  # observe({
  #   updateCheckboxGroupButtons(session = session, inputId = "MT_par_group", 
  #                              selected = input$MT_par_group2)
  # })
  
  # Download the SANDS data
  output$SANDS_data_download2 <- downloadHandler(
    filename = function() {
      req(AU_Val$site_temp, reVal$result_temp2, reVal$resultphyschem_temp,
          AU_Val$summary_temp, input$MT_par_group2)
      paste0("WQP_data_SANDS_", Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      req(AU_Val$site_temp, reVal$result_temp2, reVal$resultphyschem_temp,
          AU_Val$summary_temp)
      
      showModal(modalDialog(title = "Saving the ZIP...", footer = NULL))
      
      file_names <- character()
      
      if (nrow(AU_Val$site_temp) > 0){
        
        write_csv(AU_Val$site_temp, file.path(tempdir(), "STATIONS.csv"), na = "")
        
        file_names["STATIONS"] <- "STATIONS.csv"
      }
      
      if (nrow(reVal$result_temp2) > 0){
        
        write_csv(reVal$result_temp2, file.path(tempdir(), "RESULTS.csv"), na = "")
        
        file_names["RESULTS"] <- "RESULTS.csv"
        
      }
      
      if (nrow(AU_Val$summary_temp) > 0){
        
        write_csv(AU_Val$summary_temp, file.path(tempdir(), "SUMMARY.csv"), na = "")
        
        file_names["SUMMARY"] <- "SUMMARY.csv"
        
      }
      
      if (nrow(reVal$resultphyschem_temp) > 0){
        
        write_csv(reVal$resultphyschem_temp, file.path(tempdir(), "ResultPhysChem.csv"), na = "")
        
        file_names["ResultPhysChem"] <- "ResultPhysChem.csv"
        
      }
      
      if ("nutrients" %in% input$MT_par_group2 & is.data.frame(reVal$nutrients_tab)){
        if (nrow(reVal$nutrients_tab) > 0){
          
          write_csv(reVal$nutrients_tab, file.path(tempdir(), "Nutrients.csv"), na = "")
          
          file_names["Nutrients"] <- "Nutrients.csv"
          
          write_csv(reVal$nutrients_GIS_tab, file.path(tempdir(), "Nutrients_GIS.csv"), na = "")
          
          file_names["Nutrients_GIS"] <- "Nutrients_GIS.csv"
          
        }
      }
      
      if ("metal_w" %in% input$MT_par_group2 & is.data.frame(reVal$h20_tab)){
        if (nrow(reVal$h20_tab) > 0){
          
          write_csv(reVal$h20_tab, file.path(tempdir(), "Metals_H2O.csv"), na = "")
          
          file_names["Metals(H2O)"] <- "Metals_H2O.csv"
          
          write_csv(reVal$h20_GIS_tab, file.path(tempdir(), "Metals_H2O_GIS.csv"), na = "")
          
          file_names["Metals(H2O)_GIS"] <- "Metals_H2O_GIS.csv"
          
        }
      }
      
      if ("metal_s" %in% input$MT_par_group2 & is.data.frame(reVal$sed_tab)){
        if (nrow(reVal$sed_tab) > 0){
          
          write_csv(reVal$sed_tab, file.path(tempdir(), "Metals_Sediment.csv"), na = "")
          
          file_names["Metals(Sediment)"] <- "Metals_Sediment.csv"
          
          write_csv(reVal$sed_GIS_tab, file.path(tempdir(), "Metals_Sediment_GIS.csv"), na = "")
          
          file_names["Metals(Sediment)_GIS"] <- "Metals_Sediment_GIS.csv"
          
        }
      }
      
      if ("salinity" %in% input$MT_par_group2 & is.data.frame(reVal$salinity_tab)){
        if (nrow(reVal$salinity_tab) > 0){
          
          write_csv(reVal$salinity_tab, file.path(tempdir(), "Salinity.csv"), na = "")
          
          file_names["Salinity"] <- "Salinity.csv"
          
          write_csv(reVal$salinity_GIS_tab, file.path(tempdir(), "Salinity_GIS.csv"), na = "")
          
          file_names["Salinity_GIS"] <- "Salinity_GIS.csv"
          
        }
      }
      
      if ("oil_gas" %in% input$MT_par_group2 & is.data.frame(reVal$oil_tab)){
        if (nrow(reVal$oil_tab) > 0){
          
          write_csv(reVal$oil_tab, file.path(tempdir(), "Oil_Gas.csv"), na = "")
          
          file_names["Oil_Gas"] <- "Oil_Gas.csv"
          
          write_csv(reVal$oil_GIS_tab, file.path(tempdir(), "Oil_Gas_GIS.csv"), na = "")
          
          file_names["Oil_Gas_GIS"] <- "Oil_Gas_GIS.csv"
          
        }
      }
      
      zip::zip(con, files = map_chr(file_names, function(x) file.path(tempdir(), x)),
               mode = "cherry-pick")
      
      removeModal()
    }
  )
  
  # If download is ready, prepare a text to show in the tool
  observe({
      output$SANDS_info <- renderText({
        if(AU_Val$SANDS_download_ready){
          "SANDS format files with updated AU information are ready to save."
        } else if (!AU_Val$SANDS_download_ready & !is.null(AU_Val$WQP_AU)){
          "Data downloaded from the Water Quality Portal are available, but the tool needs to join the sites to the AU information. Please follow the instruction in the 'App Control' section to join sites to the AU. After joining the AU, please click the check box 'Create the SANDS output as CSV files in a ZIP folder' and select the filter group to create the SANDS format."
        } else if (!AU_Val$SANDS_download_ready & is.null(AU_Val$WQP_AU)){
          "SANDS format files with updated AU information are unavailable. Please download the data from the 'Data Download' tab or upload the data using the 'WQP Data Upload' section, and follow the instruction in the 'App Control' section to join sites to the AU. After joining the AU, please click the check box 'Create the SANDS output as CSV files in a ZIP folder' and select the filter group to create the SANDS format."
        }
      })
  })

  ### Create the fact sheet
  SANDS_Val <- reactiveValues()
  SANDS_Val$fact_sheet_create <- FALSE
  SANDS_Val$fact_sheet_save <- FALSE
  
  # Check if there are any needed data for factsheet creation
  observe({
    req(AN_final_dat(), nrow(AN_final_dat()) > 0)
    SANDS_Val$WQP_dat_fact <- AN_final_dat() %>%
      fsubset(TADA.CharacteristicName %in% factsheet_par &
                TADA.ResultMeasure.MeasureUnitCode %in% "UG/L")
    if (nrow(SANDS_Val$WQP_dat_fact) > 0){
      SANDS_Val$fact_sheet_create <- TRUE
    } 
  })
  
  output$factsheet_info <- renderText({
    req(AN_final_dat(), nrow(AN_final_dat()) > 0)
    if (SANDS_Val$fact_sheet_create){
      "Data are available to summarize and generate the fact sheets"
    } else {
      "No data to summarize and generate the fact sheets"
    }
  })
  
  observeEvent(input$create_factsheets, {
    req(SANDS_Val$WQP_dat_fact, nrow(SANDS_Val$WQP_dat_fact) > 0)
    
    shinyCatch({
      showModal(modalDialog(title = "Summarizing the data...", footer = NULL))
      
      ### Data preparation
      
      ####Spatial Join MLs to ER3####
      mls_filtered_spatial <- SANDS_Val$WQP_dat_fact %>%
        select(MonitoringLocationIdentifier, TADA.LongitudeMeasure, TADA.LatitudeMeasure) %>%
        unique() %>%
        st_as_sf(coords = c('TADA.LongitudeMeasure','TADA.LatitudeMeasure'), remove = F) %>%
        st_set_crs(4617) %>% #NAD83 EPSG
        st_transform(st_crs(er3))
      
      #Identify which ER3 the ML is in
      mls_w_er3 <- mls_filtered_spatial %>%
        st_intersection(er3)
      
      #Attach ER3 to sample data
      sample_w_er3 <- SANDS_Val$WQP_dat_fact %>%
        right_join(mls_w_er3, by = c('MonitoringLocationIdentifier'))
      
      
      ####List Sites####
      if (input$Site_AU_fact_SANDS %in% "Site"){
        list_sites <- SANDS_Val$WQP_dat_fact %>%
          distinct(MonitoringLocationIdentifier) %>%
          drop_na() %>%
          pull()
      } else if (input$Site_AU_fact_SANDS %in% "AU"){
        list_sites <- SANDS_Val$WQP_dat_fact %>%
          distinct(AU_ID) %>%
          drop_na() %>%
          filter(!str_detect(AU_ID, 'WY')) %>% #Remove one WY site
          pull()
      }
      
      # Update the criteria_table 
      criteria_table2 <- criteria_table %>%
        filter(State == 'Montana') %>%
        mutate(Fraction = str_to_upper(Fraction),
               Constituent = str_to_upper(Constituent))
      
      if (input$Site_AU_fact_SANDS %in% "Site"){
        temp_result <- fact_sheet_create(
          sample_w_er3 = sample_w_er3,
          list_sites = list_sites,
          criteria_table = criteria_table2,
          flow_dates = flow_dates,
          AU = FALSE,
          Convert = input$total_convert
        )
      } else if (input$Site_AU_fact_SANDS %in% "AU"){
        temp_result <- fact_sheet_create(
          sample_w_er3 = sample_w_er3,
          list_sites = list_sites,
          criteria_table = criteria_table2,
          flow_dates = flow_dates,
          AU = TRUE,
          Convert = input$total_convert
        )
      }
    
      SANDS_Val$fact_sheet <- temp_result
      
      if (!is.null(SANDS_Val$fact_sheet)){
        SANDS_Val$fact_sheet_save <- TRUE
      }
      
      removeModal()
      
    },
    blocking_level = "error")
  })
  
  output$factsheet_info2 <- renderText({
    req(AN_final_dat(), nrow(AN_final_dat()) > 0)
    if (SANDS_Val$fact_sheet_save){
      "Fact sheets are ready to save."
    } else {
      "Click 'Create' to create the fact sheets."
    }
  })
  
  # Save the fact sheet
  output$fact_sheet_data_download <- downloadHandler(
    filename = function() {
      paste0("WQP_data_fact_sheet_", Sys.Date(), '.xlsx')
    },
    content = function(file) {
      showModal(modalDialog(title = "Saving the Excel file...", footer = NULL))

      AU_names <- names(SANDS_Val$fact_sheet)
      
      wb <- createWorkbook()

      for (i in seq_along(SANDS_Val$fact_sheet)){

        #Write to same tab in an excel workbook
        addWorksheet(wb, AU_names[[i]])

        summary_table_list <- SANDS_Val$fact_sheet[[i]]

        curr_row <- 1
        for(k in seq_along(summary_table_list)) {
          writeData(wb, AU_names[[i]], names(summary_table_list)[k], startCol = 1,
                    startRow = curr_row)
          writeData(wb, AU_names[[i]], summary_table_list[[k]], startCol = 1,
                    startRow = curr_row+1)
          curr_row <- curr_row + nrow(summary_table_list[[k]]) + 5 #Add spacing between tables
        }
      }
      
      saveWorkbook(wb, file = file, overwrite = TRUE)
      
      removeModal()

    }
  )
  
  ### Ensure that sink is reset when the app stops
  session$onSessionEnded(function() {
    while (sink.number() > 0) {
      sink(NULL)
    }
  })
  
}

# Run the app
shinyApp(ui, server)