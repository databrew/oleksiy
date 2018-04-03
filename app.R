# Oleksiy Anokhin
# March 7, 2018

# Visualizing DC crimes in 2017 through Shiny app

#------------
# Global Code
#------------

# Install packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinythemes)

# Set working directory
setwd("C:/Users/wb516241/IFC/!My Shiny apps/App DC Crimes 2017")

# Read the initial file
incidents <- read.csv("Crime_Incidents_in_2017.csv", header = TRUE, stringsAsFactors = FALSE)


# User interface
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Crimes in Washington, DC (2017)"), 
                sidebarLayout(
                  sidebarPanel(
                    selectInput("offenceInput", "Type of Offence",
                                choices = c("Choose type", 
                                            "Arson",
                                            "Assault w/danderous weapon",
                                            "Burglary",
                                            "Homicide",
                                            "Motor vehicle theft",
                                            "Robbery", 
                                            "Sex abuse",
                                            "Theft f/auto",
                                            "Theft/Other"),
                                selected = "Choose type"),
                    selectInput("methodInput", "Method of Offence",
                                choices = c("Choose method", 
                                            "Gun",
                                            "Knife"),
                                selected = "Choose method"),
                    selectInput("shiftInput", "Police Shift",
                                choices = c("Choose shift", 
                                            "Day", 
                                            "Evening", 
                                            "Midnight"),
                                selected = "Choose shift")
                  ),
                  
                  mainPanel(leafletOutput(outputId = 'map', height = 800) 
                  )
                )
)


# Server 

server <- shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet(incidents) %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      setView(-77.0369, 38.9072, zoom = 12) #%>%
    
  })
  # observers
  
  # selected offence
  selectedOffense <- reactive({ # SO THIS LINE IS A PROBLEM
    incidents[incidents$OFFENSE == input$offenceInput, ] 
    # tmp <- incidents[!is.na(incidents$OFFENCE),]
    #tmp[tmp$OFFENCE == input$offenceInput, ] 
  })
  # AND SURPRISINGLY POP-UP 
  # state_popup1 <- paste0("<strong>Type of offence: </strong>", 
  #                       selectedOffence()$OFFENCE, 
  #                       "<br><strong> Method of Offence: </strong>", 
  #                       selectedOffence()$METHOD,
  #                       "<br><strong> Police shift: </strong>", 
  #                       selectedOffence()$SHIFT)
  observe({
    
    leafletProxy("map", data =  selectedOffense()) %>%
      clearShapes() %>%
      addCircles(lng = incidents$Lon, lat = incidents$Lat, weight = 1,
               color = "red", radius = 10, opacity = 0.9#, popup = ~state_popup1
               ) 
  })
  
  selectedMethod <- reactive({ # SO THIS LINE IS A PROBLEM
    incidents[incidents$METHOD == input$methodInput, ] 
    # tmp <- incidents[!is.na(incidents$OFFENCE),]
    #tmp[tmp$OFFENCE == input$offenceInput, ] 
  })
 
  observe({
    
    leafletProxy("map", data =  selectedMethod()) %>%
      clearShapes() %>%
      addCircles(lng = incidents$Lon, lat = incidents$Lat, weight = 1,
                 color = "red", radius = 10, opacity = 0.9#, popup = ~state_popup1
      ) 
  })
  
  selectedShift <- reactive({ # SO THIS LINE IS A PROBLEM
    incidents[incidents$SHIFT == input$shiftInput, ] 
    # tmp <- incidents[!is.na(incidents$OFFENCE),]
    #tmp[tmp$OFFENCE == input$offenceInput, ] 
  })
  
  observe({
    
    leafletProxy("map", data =  selectedShift()) %>%
      clearShapes() %>%
      addCircles(lng = incidents$Lon, lat = incidents$Lat, weight = 1,
                 color = "red", radius = 10, opacity = 0.9#, popup = ~state_popup1
      ) 
  })
})

shinyApp(ui = ui, server = server)