# Main body

library(shiny)
library(shinyjs)

# Main body function
function(id){
  tabItems(
    # tabItem(tabName = "Intro",
    #         tab_Intro()),
    tabItem(tabName = "Download",
            tab_Download())
  )## tabItems
} ## Mian body function ends