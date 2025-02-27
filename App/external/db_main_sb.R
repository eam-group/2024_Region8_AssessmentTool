# Sidebar

library(shiny)
library(shinybusy)
library(shinyWidgets)

# Sidbar function
function(id) {
  dashboardSidebar(
    
    # Use Shiny Busy
    add_busy_spinner(spin = "circle",
                     position = "top-right"
    ),
    useSweetAlert(),
    
    # The sidebar manual
    sidebarMenu(
      id = "tabs",
      # # Tab 1: Introduction Page
      # menuItem(
      #   text = "Introduction Page",
      #   tabName = "Intro",
      #   icon = icon("map")
      # ),
      # Tab 2: Download Page
      menuItem(
        text = "Data Download",
        tabName = "Download",
        icon = icon("pen"))
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END