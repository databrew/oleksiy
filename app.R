#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(knitr)
library(kableExtra)
library(RColorBrewer)

# Read the initial file
incidents <- read.csv("Crime_Incidents_in_2017.csv", header = TRUE, stringsAsFactors = FALSE)

# Define function for legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, ...){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, ...))
}

# User interface
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Crimes in Washington, DC (2017)"), 
                fluidRow(column(4,
                                selectInput("offenceInput", "Type of Offence",
                                            choices = sort(unique(incidents$OFFENSE)),
                                            selected = sort(unique(incidents$OFFENSE)),
                                            multiple = TRUE),
                                selectInput("methodInput", "Method of Offence",
                                            choices = sort(unique(incidents$METHOD)),
                                            selected = sort(unique(incidents$METHOD)),
                                            multiple = TRUE),
                                selectInput("shiftInput", "Police Shift",
                                            choices = sort(unique(incidents$SHIFT)),
                                            selected = sort(unique(incidents$SHIFT)),
                                            multiple = TRUE)
                ),
                
                column(8,
                       leafletOutput(outputId = 'map', height = 600)
                )
                )
                
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {  
  
  # Filter the data based on inputs
  filtered_data <- reactive({
    selected_offence <- input$offenceInput
    selected_method <- input$methodInput
    selected_shift <- input$shiftInput
    
    out <- incidents
    
    # Offense filtering
    
    if(!is.null(selected_offence)){
      if(!all(selected_offence == '')){
        message('Keeping the following offences:')
        message(paste0('---', selected_offence, '\n', collapse = ''))
        out <- out %>%
          filter(OFFENSE %in% selected_offence)
      }
      
    }
    
    # Method filtering filtering
    if(!is.null(selected_method)){
      if(!all(selected_method == '')){
        message('Keeping the following methods:')
        message(paste0('---', selected_method, '\n', collapse = ''))
        out <- out %>%
          filter(METHOD %in% selected_method)
      }
    }
    
    # Shift filtering 
    if(!is.null(selected_shift)){
      if(!all(selected_shift == '')){
        message('Keeping the following shifts:')
        message(paste0('---', selected_shift, '\n', collapse = ''))
        out <- out %>%
          filter(SHIFT %in% selected_shift)
      }
    }
    
    message('Filtered data has ', nrow(out), ' rows.')
    return(out)
  })
  
  output$map <- renderLeaflet({
    
    
    # Get the filtered data first
    df <- filtered_data()
    
    # If there is any data, carry on
    if(nrow(df) > 0){
      
      l <- 
        leaflet(data = df) %>% 
        addProviderTiles(providers$Stamen.Toner) %>% 
        setView(-77.0369, 38.9072, zoom = 12)
      
      message(nrow(df), ' crimes filtered.')
      
      # Define a color vector
      color_vector <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(length(unique(df$OFFENSE)))
      color_labels <- sort(unique(df$OFFENSE))
      pal <- colorFactor(
        color_vector,
        domain = color_labels)
      
      l <- l %>%
        addCircles(lng = df$Lon, lat = df$Lat, weight = 1,
                   popup = paste0(df$OFFENSE, ' at ', df$BLOCK),
                   color = ~pal(df$OFFENSE),
                   radius = 20, opacity = 0.9) %>%
        addLegendCustom(colors = color_vector, 
                        labels = color_labels, sizes = rep(20, length(color_vector)),
                        position = 'bottomright')
      
    } else {
      message('No crimes with current filter settings.')
      l <- l <- 
        leaflet() %>% 
        addProviderTiles(providers$Stamen.Toner) %>% 
        setView(-77.0369, 38.9072, zoom = 12)
    }
    return(l)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

