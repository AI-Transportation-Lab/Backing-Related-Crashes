library(shiny)
library(leaflet)
library(data.table)
library(shinyjs)
library(dplyr)

# Access token for Mapbox, replace with your token.
mapbox_token <- 'pk.eyJ1IjoiY3Jld2NoaWVmODUiLCJhIjoiY2p2em1wb3RuMDF5czRhbzFmN3YxNnBlYiJ9.dVekUDeGvplq4jlB_wQo2Q'

# Load the new data correctly.
data <- fread("0_Rohit_data/0_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Ensure Latitude and Longitude are numeric.
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)

# Remove rows with NA in latitude or longitude
data <- data %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Calculate the center point of the data
center_lat <- mean(data$latitude, na.rm = TRUE)
center_lng <- mean(data$longitude, na.rm = TRUE)

# Define a color palette based on the unique values in the 'CrashSeverity' column.
highway_pal <- colorFactor(topo.colors(length(unique(data$`Driver injury`))), domain = data$`Driver injury`)

# Function to generate HTML table content with user-friendly variable names
generate_details_html <- function(data_row, name_mapping) {
  paste0(
    '<table class="details-table">',
    paste0(
      '<tr><th>',
      sapply(names(data_row), function(var) {
        if (var %in% names(name_mapping)) {
          name_mapping[[var]]
        } else {
          var # Use the original name if no mapping exists
        }
      }),
      '</th><td>',
      as.character(data_row),
      '</td></tr>',
      collapse = ''
    ),
    '</table>'
  )
}

# Define UI.
ui <- fluidPage(
  useShinyjs(), # Include shinyjs
  tags$head(
    # Include jQuery UI CSS
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    # Include jQuery UI JS
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    # Include Mapbox GL JS
    tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.6.1/mapbox-gl.js"),
    tags$link(rel = "stylesheet", href = "https://api.mapbox.com/mapbox-gl-js/v2.6.1/mapbox-gl.css"),
    # Include Google Maps JavaScript API
    tags$script(src = "https://maps.googleapis.com/maps/api/js?key=AIzaSyCX32_Bs_Yl6uhd-oAuaehQRW__Hn3UFes")
  ),
  tags$style(type = "text/css", 
             "html, body {width:100%;height:100%; overflow: hidden;} 
            #mainMap {z-index: 0;}  /* Set z-index of main map */
            #insetMapContainer, #iframeContainer, #detailsPanel {display: none;}
            .close-btn {position: absolute; top: -10px; right: -10px; cursor: pointer; font-size: 18px; font-weight: bold; background: white; border-radius: 50%; padding: 2px 6px; box-shadow: 0 0 5px rgba(0,0,0,0.5); z-index: 9999;}
            .details-panel {position: relative; padding-top: 10px; padding-right: 10px; overflow: visible;}
            .scrollable-content {max-height: calc(100% - 40px); overflow-y: auto; padding-right: 20px;}
            .details-table {font-family: Arial, sans-serif; border-collapse: collapse; width: 100%;}
            .details-table th, .details-table td {border: 1px solid #dddddd; text-align: left; padding: 8px; width: 50%;}
            .details-table th {background-color: #f2f2f2; font-weight: bold;}
            .details-table tr:nth-child(even) {background-color: #f9f9f9;}"),
  leafletOutput("mainMap", width = "100%", height = "100vh"),
  absolutePanel(id = "insetMapContainer", top = 10, right = 10, width = "25%", height = "40%",
                style = "z-index: 1; opacity: 0.9; border-radius: 8px; background-color: #ffffff; box-shadow: 0 0 5px rgba(0,0,0,0.5); padding: 10px;",
                tags$div(class = "close-btn", "X", onclick = "$('#insetMapContainer').css('display', 'none');"),
                tags$div(id = "mapbox", style = "width:100%; height:100%;")),
  absolutePanel(id = "iframeContainer", bottom = 10, right = 10, width = "25%", height = "40%",
                style = "z-index: 1; opacity: 0.9; border-radius: 8px; background-color: #ffffff; box-shadow: 0 0 5px rgba(0,0,0,0.5); padding: 10px;",
                tags$div(class = "close-btn", "X", onclick = "$('#iframeContainer').css('display', 'none');"),
                tags$div(id = "streetView", style = "width:100%; height:100%;")),
  absolutePanel(id = "detailsPanel", top = 60, left = 10, width = "25%", height = "40%",
                class = "details-panel",
                style = "z-index: 1; opacity: 0.9; border-radius: 8px; background-color: #ffffff; box-shadow: 0 0 5px rgba(0,0,0,0.5); padding: 10px; overflow: visible;",
                tags$div(class = "close-btn", "X", onclick = "$('#detailsPanel').css('display', 'none');"),
                tags$div(class = "scrollable-content", tags$div(id = "details", style = "width:100%; height:100%;")),
                tags$script(HTML("
                $(function() {
                  $('#detailsPanel').draggable().resizable();
                });
              "))
  ),
  absolutePanel(id = "notePanel", top = 10, left = 10, width = "25%", height = "auto",
                style = "z-index: 1; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 8px; box-shadow: 0 0 5px rgba(0,0,0,0.5);",
                tags$div(class = "close-btn", "X", onclick = "$('#notePanel').css('display', 'none');"),
                tags$div("Click on a circle marker to see the StreetView and Mapbox details.")
  ),
  tags$script(HTML("
    $(function() {
      $('#insetMapContainer').draggable().resizable();
      $('#insetMapContainer').on('resize', function() {
        if (window.map) {
          window.map.resize();
        }
      });
      $('#iframeContainer');

      // Add click event listener to the map
      $('#mainMap').on('click', function() {
        $('#insetMapContainer').css('display', 'block');
        $('#iframeContainer').css('display', 'block');
        $('#detailsPanel').css('display', 'block');
      });
    });
  "))
)

server <- function(input, output, session) {
  
  # Ensure CrashSeverity is a factor with the desired levels order
  data$CrashSeverity <- factor(data$`Driver injury`, levels = c("Injury", "No Injury"))
  
  # Define the color palette with the ordered levels and brighter colors
  severity_colors <- c("Injury" = "red", 
                       "No Injury" = "orange")
  
  highway_pal <- colorFactor(severity_colors, levels = names(severity_colors), domain = data$`Driver injury`)
  
  # Mapping of original variable names to user-friendly names
  variable_name_mapping <- c(
    "CRASH_NUM" = "Crash Number",
    "Year" = "Year of Crash",
    "latitude" = "Latitude",
    "longitude" = "Longitude",
    "adt" = "Average Daily Traffic",
    "Alignment" = "Road Alignment",
    "Location type" = "Location Type",
    "Occupant" = "Number of Occupants",
    "Vehicle type" = "Vehicle Type",
    "Driver injury" = "Driver Injury Status",
    "CrashSeverity" = "Crash Severity"
  )
  
  output$mainMap <- renderLeaflet({
    leaflet(data = data, options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
      setView(lng = center_lng, lat = center_lat, zoom = 6) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       color = ~highway_pal(CrashSeverity), 
                       radius = 5,
                       stroke = FALSE, fillOpacity = 0.8,
                       popup = ~paste("Crash Severity:", CrashSeverity),
                       layerId = ~paste0("marker-", seq_len(nrow(data)))) %>%
      addLegend(
        position = "bottomleft",
        pal = highway_pal,
        values = ~CrashSeverity,
        title = "Collision Type"
      )
  })
  
  observeEvent(input$mainMap_marker_click, {
    marker <- input$mainMap_marker_click
    lng <- marker$lng
    lat <- marker$lat
    
    # Debugging line to check if click event is captured
    print(paste("Marker clicked at:", lng, lat)) 
    
    # Find the corresponding row in the data
    clicked_point <- data %>% filter(longitude == lng & latitude == lat)
    
    # Filter out columns with NA values
    filtered_point <- clicked_point %>%
      select_if(~ !all(is.na(.)))
    
    # Generate the HTML content using the function
    details_html <- generate_details_html(filtered_point[1, ], variable_name_mapping)
    
    # Update the details div
    shinyjs::html("details", details_html)
    
    js_code <- sprintf("
      mapboxgl.accessToken = '%s';
      window.map = new mapboxgl.Map({
        container: 'mapbox',
        style: 'mapbox://styles/mapbox/streets-v11',
        center: [%f, %f],
        zoom: 16.77,
        pitch: 75,
        bearing: 95.2,
        interactive: false // Make the map non-draggable
      });
      
      var panorama = new google.maps.StreetViewPanorama(
        document.getElementById('streetView'), {
          position: {lat: %f, lng: %f},
          pov: {heading: 165, pitch: 0},
          zoom: 1
        });
    ", mapbox_token, lng, lat, lat, lng)
    
    shinyjs::runjs(js_code)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
