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
library(scales)

# Load the data
load("DataInput/InputData38.RData")

# Load the tabs
db_main_body <- source("external/db_main_body.R")$value
db_main_sb <- source("external/db_main_sb.R")$value
# tab_Intro <- source("external/tab_Intro.R")$value
tab_Download <- source("external/tab_Download.R")$value

# Load the functions
source("scripts/Main_helper_functions.R")

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
          
          # Filter the dat_summary3 by HUC8_dat()
          dat_summary3_sf <- dat_summary3 %>%
            distinct(MonitoringLocationIdentifier, MonitoringLocationLatitude,
                     MonitoringLocationLongitude, HUCEightDigitCode) %>%
            drop_na(MonitoringLocationLongitude) %>%
            drop_na(MonitoringLocationLatitude) %>%
            st_as_sf(coords = c("MonitoringLocationLongitude",
                                "MonitoringLocationLatitude"),
                     crs = 4326) %>%
            st_transform(crs = st_crs(HUC8_dat())) %>%
            st_filter(HUC8_dat())
          
          glimpse(dat_summary3)
          
          removeModal()
          
          # Filter by HUCEightDigitCode
          site_all <- dat_summary3 %>% 
            fsubset(HUCEightDigitCode 
                    %in% unique(dat_summary3_sf$HUCEightDigitCode)) %>%
            group_by(HUCEightDigitCode, YearSummarized) %>%
            fsummarize(ResultCount = fsum(ResultCount)) %>%
            fungroup() %>%
            arrange(HUCEightDigitCode, YearSummarized) 
          
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
            
            # Download the data as TADA data frame
            
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
          
          # If nrow(dat_summary) == 0, return an empty data frame with the header as dat_temp_all
        } else {
          dat_temp_all <- TADA_download_temp
        }
        
        reVal$WQP_dat <- dat_temp_all
      }
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
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(outputId = "Date_Info", placeholder = TRUE)
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
              )
            )
          )
        )
      )
  })
  
  ### Clean up the data with the EPATADA package
  observe({
   
    shinyCatch({
      # TADA_AutoClean
      observe({
        req(reVal$WQP_dat)
        
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
          .data = reVal$WQP_FlagBelowThreshold
        )
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_HarmonizeSynonyms_temp
        }
        
        reVal$WQP_HarmonizeSynonyms <- temp_dat
        
        reVal$WQP_FlagBelowThreshold <- NULL
        
        removeModal()
        
      })
      
      # # TADA_FlagDepthCategory
      # observe({
      #   req(reVal$WQP_HarmonizeSynonyms)
      #   
      #   showModal(modalDialog(title = "Run TADA_FlagDepthCategory", 
      #                         footer = NULL))
      #   
      #   temp_dat <- TADA_FlagDepthCategory_poss(
      #     .data = reVal$WQP_HarmonizeSynonyms
      #   )
      #   
      #   if (is.null(temp_dat)){
      #     temp_dat <- TADA_FlagDepthCategory_temp
      #   }
      #   
      #   reVal$WQP_FlagDepthCategory <- temp_dat
      #   
      #   reVal$WQP_HarmonizeSynonyms <- NULL
      #   
      #   removeModal()
      #   
      # })
      
      # Select the correct columns to create WQP_dat2
      observe({
        req(reVal$WQP_HarmonizeSynonyms)
        
        temp_dat <- reVal$WQP_HarmonizeSynonyms %>%
          TADA_selector_poss()
        
        if (is.null(temp_dat)){
          temp_dat <- TADA_Final_temp
        }
        
        reVal$WQP_HarmonizeSynonyms <- NULL
        
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
        
        reVal$min_date2 <- min(reVal$WQP_dat3$ActivityStartDate)
        reVal$max_date2 <- max(reVal$WQP_dat3$ActivityStartDate)
        reVal$site_number2 <- length(unique(reVal$WQP_dat3$MonitoringLocationIdentifier))
        reVal$sample_size2 <- nrow(reVal$WQP_dat3)
      })
      
      # Filter the data for the QC data
      observe({
        req(reVal$WQP_dat3)
        
        reVal$WQP_QC <- reVal$WQP_dat3
        
      })
      
      # Display data information
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
          req(reVal$WQP_dat2, input$clean_summary)
          tt <- count_fun(reVal$WQP_dat2, input$clean_summary)
          datatable(tt, options = list(scrollX = TRUE)) %>%
            formatPercentage(columns = "Percent (%)", digits = 2)
        })
      })
      
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
  
  ### Reset
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
    reVal$WQP_FlagContinuousData <- NULL
    reVal$WQP_FlagAboveThreshold <- NULL
    reVal$WQP_FlagBelowThreshold <- NULL
    reVal$WQP_HarmonizeSynonyms <- NULL
    reVal$WQP_dat2 <- NULL
    reVal$WQP_time <- NULL
    reVal$WQP_dat3 <- NULL
    reVal$WQP_QC <- NULL
    
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
    reset("par_se")
    reset("par_date")
    reset("State_se")
    reset("HUC_se")
    reset("bb_W")
    reset("bb_N")
    reset("bb_S")
    reset("bb_E")
    reset("clean_summary")
    updateRadioGroupButtons(session = session,
                            inputId = "area_se", selected = "State")
  })
  
  ### Ensure that sink is reset when the app stops
  session$onSessionEnded(function() {
    while (sink.number() > 0) {
      sink(NULL)
    }
  })
  
}

# Run the app
shinyApp(ui, server)