# Download page
function(){
  tabItem(
    # The tab name
    tabName = "Download",
    h2("Data Download"),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Parameter Input", 
          status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          column(
            width = 12,
              column(
                width = 8,
                selectInput(inputId = "download_se", 
                            label = "Choose the data type to download",
                            choices = c("Characteristic names", 
                                        "Organic characteristic group",
                                        "All data"),
                            selected = "Characteristic names"),
                conditionalPanel(condition = "input.download_se == 'Characteristic names'",
                                 awesomeCheckboxGroup(inputId = "par_group_se", label = "Choose the parameter group",
                                                      choices = c("Metal", "Nutrient", "Pestcide", "Other", 
                                                                  "Oil & Gas", "All"),
                                                      inline = TRUE,
                                                      status = "primary"),
                                 multiInput(inputId = "par_se", label = "Select the parameter to download",
                                            choiceNames = parameter_names$Group_Name,
                                            choiceValues = parameter_names$Standard_Name)
                ),
                conditionalPanel(condition = "input.download_se == 'Organic characteristic group'",
                                 selectizeInput(inputId = "CharGroup_se", label = "Select the organic group",
                                                choices = c("Organics, BDEs", "Organics, Other",
                                                            "Organics, PCBs", "Organics, Pesticide",
                                                            "PFAS,Perfluorinated Alkyl Substance",
                                                            "PFOA, Perfluorooctanoic Acid"),
                                                multiple = TRUE)
                ),
                conditionalPanel(condition = "input.download_se != 'All data'",
                                 selectizeInput(inputId = "sitetype_se", label = "Select site type",
                                                choices = c("Lake, Reservoir, Impoundment", "Stream"), 
                                                multiple = TRUE,
                                                selected = "Stream")
                                 
                )
              )
          )
        ),
        box(
          title = "Geographic and Date Input", 
          status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          dateRangeInput(inputId = "par_date", label = "Select the date range",
                         start = "2000-01-01",
                         min = "1900-01-01",
                         end = Sys.Date(),
                         max = Sys.Date(),
                         width = "40%"),
          radioGroupButtons(inputId = "area_se", label = "Choose the method to select geographic area",
                            choices = c("State", "HUC8", "BBox"),
                            selected = "State"),
          conditionalPanel(condition = "input.area_se == 'HUC8' || input.area_se == 'BBox'",
                           fluidRow(
                             column(
                               width = 6,
                               checkboxInput(inputId = "state_out_bound", 
                                             label = "Including sites outside the state boundary but within HUC8 intersecting with the state boundary")
                             )
                           )
          ),
          conditionalPanel(condition = "input.area_se == 'State'",
                           fluidRow(
                             column(
                               width = 4,
                               selectizeInput(inputId = "State_se", label = "Select state",
                                              choices = c("Colorado", "Montana", 
                                                          "North Dakota", "South Dakota",
                                                          "Utah", "Wyoming"), multiple = FALSE)
                             ),
                             column(
                               width = 8
                             )
                           )
          ),
          conditionalPanel(condition = "input.area_se == 'HUC8'",
                           fluidRow(
                             column(
                               width = 6,
                               strong("Select the HUC8 based on the map or the dropdown menu"),
                               leafletOutput("HUC8map")
                             ),
                             column(
                               width = 6,
                               selectizeInput(inputId = "HUC_se", label = "HUC8 dropdown menu",
                                              choices = HUC8_label, multiple = TRUE,
                                              width = "60%")
                             )
                           )
          ),
          conditionalPanel(condition = "input.area_se == 'BBox'",
                           fluidRow(
                             column(
                               width = 6,
                               strong("Provide the coordinates (latitude and longitude) by drawing a rectangle on 
                                           the map or type in the values"),
                               leafletOutput("BBox_map")
                             ),
                             column(
                               width = 6,
                               fluidRow(
                                 id = "BBox_Panel",
                                 column(
                                   width = 3,
                                   br(),
                                   br(),
                                   numericInput(inputId = "bb_W", value = -117.65217, label = "West bound",
                                                min = -117.65217, max = -102.11928)
                                 ),
                                 column(
                                   width = 3,
                                   numericInput(inputId = "bb_N", value = 50.58677, label = "North bound",
                                                min = 40.37306, max = 50.58677),
                                   numericInput(inputId = "bb_S", value = 40.37306, label = "Sound bound",
                                                min = 40.37306, max = 50.58677)
                                 ),
                                 column(
                                   width = 3,
                                   br(),
                                   br(),
                                   numericInput(inputId = "bb_E", value = -102.11928, label = "East bound",
                                                min = -117.65217, max = -102.11928)
                                 )
                               )
                             )
                           )
                           
          )
        )
      ),
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Download Data from WQP", 
          status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          actionButton(inputId = "par_download", "Download the data")
        ) 
      )
    ),
    uiOutput("summary_box"),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Save Data", 
          status = "warning", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          downloadButton(outputId = "data_download", label = "Save data"),
          downloadButton(outputId = "data_download_QC", label = "Save QC data"),
          actionButton(inputId = "Reset", "Reset")
        )
      )
    )
  )
}