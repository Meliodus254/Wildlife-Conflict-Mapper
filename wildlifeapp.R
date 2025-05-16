library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(geosphere)
library(leaflet.extras)
library(DT)
library(shinyWidgets)
library(digest)

# Helper function
get_icon_url <- function(species) {
  case_when(
    grepl("Crocodile|Alligator", species, ignore.case = TRUE) ~ "crocodile.png",
    grepl("Elephant", species, ignore.case = TRUE) ~ "elephant.png",
    grepl("Lion", species, ignore.case = TRUE) ~ "lion.png",
    grepl("Hippo", species, ignore.case = TRUE) ~ "hippo.png",
    grepl("Hyena", species, ignore.case = TRUE) ~ "hyena.png",
    grepl("Buffalo", species, ignore.case = TRUE) ~ "buffalo.png",
    grepl("Python|Boa", species, ignore.case = TRUE) ~ "python.png",
    grepl("Cobra", species, ignore.case = TRUE) ~ "cobra.png",
    grepl("Viper|Adder", species, ignore.case = TRUE) ~ "viper.png",
    grepl("Mamba", species, ignore.case = TRUE) ~ "mamba.png",
    grepl("Snake", species, ignore.case = TRUE) ~ "snake.png",
    TRUE ~ "default.png"
  )
}

# Load & save data
load_data <- function() {
  files <- list.files(pattern = "observations-\\d+\\.csv$", full.names = TRUE)
  data <- lapply(files, function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      select(any_of(c("id", "observed_on", "common_name", "latitude", "longitude",
                      "place_guess", "description", "positional_accuracy"))) %>%
      mutate(observed_on = as.Date(observed_on))
  }) %>% bind_rows()
  
  data %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(observed_on)) %>%
    mutate(
      common_name = factor(common_name),
      description = ifelse(is.na(description), "No description", description),
      icon_url = paste0("www/", get_icon_url(common_name))
    )
}

save_new_data <- function(new_df) {
  existing_data <- load_data()
  new_df$hash <- apply(new_df, 1, digest)
  existing_hashes <- apply(existing_data, 1, digest)
  unique_new <- new_df[!new_df$hash %in% existing_hashes, ]
  unique_new$hash <- NULL
  
  if (nrow(unique_new) > 0) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    write_csv(unique_new, paste0("observations-", timestamp, ".csv"))
  }
}
##UI
ui <- fluidPage(
  tags$head(
    # Custom Dark Mode CSS
    tags$style(HTML("
      body.dark-mode {
        background-color: #2c3e50;
        color: #ecf0f1;
      }

      .dark-mode .well,
      .dark-mode .panel,
      .dark-mode .form-control,
      .dark-mode .leaflet-container,
      .dark-mode .shiny-input-container,
      .dark-mode .selectize-input,
      .dark-mode .selectize-dropdown,
      .dark-mode .picker,
      .dark-mode .dropdown-menu,
      .dark-mode .irs-bar,
      .dark-mode .irs-line,
      .dark-mode .irs-grid,
      .dark-mode .irs-single,
      .dark-mode .irs-from,
      .dark-mode .irs-to,
      .dark-mode .selectize-control.single .selectize-input,
      .dark-mode .selectize-control.multi .selectize-input {
        background-color: #34495e !important;
        color: #ecf0f1 !important;
        border-color: #7f8c8d !important;
      }

      .dark-mode .selectize-dropdown-content .option {
        background-color: #34495e;
        color: #ecf0f1;
      }

      .dark-mode .selectize-dropdown-content .option.active {
        background-color: #2c3e50;
      }

      .dark-mode .btn {
        background-color: #3498db !important;
        border-color: #2980b9 !important;
        color: #ecf0f1 !important;
      }

      .dark-mode .btn-default {
        background-color: #2c3e50 !important;
        color: #ecf0f1 !important;
        border: 1px solid #7f8c8d;
      }

      .dark-mode .form-group label,
      .dark-mode .control-label,
      .dark-mode .checkbox label {
        color: #ecf0f1 !important;
      }

      .dark-mode .checkbox input[type='checkbox'] {
        accent-color: #3498db;
      }

      .dark-mode input[type='file'] {
        background-color: #34495e;
        color: #ecf0f1;
      }

      /* DT tables */
      .dark-mode table.dataTable {
        background-color: #2c3e50;
        color: #ecf0f1;
      }

      .dark-mode .dataTables_wrapper .dataTables_filter input,
      .dark-mode .dataTables_wrapper .dataTables_length select {
        background-color: #34495e;
        color: #ecf0f1;
        border: 1px solid #7f8c8d;
      }

      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #ecf0f1 !important;
        background-color: #34495e !important;
      }

      .dark-mode pre {
        background-color: #34495e;
        color: #ecf0f1;
        border: 1px solid #7f8c8d;
        padding: 10px;
        border-radius: 5px;
      }
    ")),
    
    # JS for toggling dark mode
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleDark', function(isDark) {
        if (isDark) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
    "))
  ),
  
  titlePanel(
    div(style = "text-align:center; font-family: 'Segoe UI', sans-serif;",
        h1("🦁 Dangerous Wildlife Observations in Kenya"),
        p(style = "font-size: 16px; color: #7f8c8d;", "Stay informed. Stay safe.")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "border-radius: 10px; padding: 20px;",
      
      switchInput("dark_mode", "🌙 Dark Mode", onLabel = "ON", offLabel = "OFF", size = "mini"),
      actionButton("locate", "📍 Find My Location", icon = icon("location-dot"), class = "btn btn-info"),
      
      tags$hr(),
      fileInput("upload", "📁 Upload Observations CSV", accept = ".csv"),
      
      sliderInput("radius", "📏 Search Radius (km):", min = 1, max = 100, value = 10, step = 1),
      
      pickerInput("species", "🦓 Select Species:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `none-selected-text` = "Search or select species..."
                  )
      ),
      
      uiOutput("date_selector"),
      
      sliderInput("accuracy", "🎯 Positional Accuracy (meters):", 0, 1000, c(0, 1000)),
      
      uiOutput("timeline_slider"),
      
      checkboxInput("heatmap", "🔥 Show Heatmap"),
      
      downloadButton("downloadData", "⬇️ Download Filtered Data"),
      
      tags$hr(),
      tags$p(strong("⚠️ Disclaimer:"), "All observations shown are based on data collected up to 15/05/2025."),
      tags$p("Data is sourced from ",
             tags$a(href = "https://www.inaturalist.org", "iNaturalist.org", target = "_blank"),
             " and might not represent all wildlife observations in the area.")
    ),
    
    mainPanel(
      style = "padding: 10px; border-radius: 10px;",
      
      leafletOutput("map", height = "600px"),
      
      tags$br(),
      h4("📍 Nearby Sightings"),
      DTOutput("nearbyTable"),
      
      tags$br(),
      h4("📊 Summary Statistics"),
      verbatimTextOutput("stats")
    )
  )
)




# Server
server <- function(input, output, session) {
  user_location <- reactiveVal()
  filtered_data <- reactiveVal(data.frame())
  selected_row_coords <- reactiveVal(NULL)
  data_storage <- reactiveVal(load_data())
  
  # Handle file upload
  observeEvent(input$upload, {
    req(input$upload)
    new_data <- read_csv(input$upload$datapath, show_col_types = FALSE) %>%
      select(any_of(c("id", "observed_on", "common_name", "latitude", "longitude",
                      "place_guess", "description", "positional_accuracy"))) %>%
      mutate(observed_on = as.Date(observed_on),
             description = ifelse(is.na(description), "No description", description))
    save_new_data(new_data)
    data_storage(load_data())
    updatePickerInput(session, "species", choices = levels(data_storage()$common_name))
  })
  
  output$date_selector <- renderUI({
    req(data_storage())
    dateRangeInput("dates", "Observation Dates:",
                   start = min(data_storage()$observed_on),
                   end = max(data_storage()$observed_on))
  })
  
  output$timeline_slider <- renderUI({
    req(data_storage())
    sliderInput("time", "Observation Timeline:",
                min = min(data_storage()$observed_on),
                max = max(data_storage()$observed_on),
                value = max(data_storage()$observed_on),
                animate = animationOptions(interval = 1000))
  })
  
  observe({
    req(data_storage())
    updatePickerInput(session, "species", choices = levels(data_storage()$common_name))
  })
  
  pal <- reactive({
    colorFactor("Set1", domain = data_storage()$common_name)
  })
  
  # Initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Streets") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
      addTiles(group = "Light") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      setView(lng = 37.9062, lat = 0.0236, zoom = 6) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Streets", "Topographic", "Light", "Dark"),
        overlayGroups = c("user"), # optional: include overlay groups if needed
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  # Switch tile layer on dark mode toggle
  observeEvent(input$dark_mode, {
    leafletProxy("map") %>%
      showGroup(ifelse(input$dark_mode, "Dark", "Light")) %>%
      hideGroup(ifelse(input$dark_mode, "Light", "Dark"))
    
    session$sendCustomMessage("toggleDark", input$dark_mode)
  })
  
  # Update map markers
  observe({
    req(data_storage(), input$dates, input$species, pal())
    
    data <- data_storage() %>%
      filter(
        common_name %in% input$species,
        observed_on >= input$dates[1],
        observed_on <= input$dates[2],
        positional_accuracy >= input$accuracy[1],
        positional_accuracy <= input$accuracy[2],
        observed_on <= input$time
      )
    
    filtered_data(data)
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearHeatmap() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = ~makeIcon(iconUrl = icon_url, iconWidth = 32, iconHeight = 32),
        clusterOptions = markerClusterOptions(),
        popup = ~paste(
          "<b>Species:</b>", common_name, "<br>",
          "<b>Date:</b>", observed_on, "<br>",
          "<b>Location:</b>", place_guess, "<br>",
          "<b>Accuracy:</b>", positional_accuracy, "m"
        )
      )
    
    if (input$heatmap) {
      leafletProxy("map", data = data) %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~positional_accuracy,
          blur = 20,
          max = 0.05,
          radius = 15
        )
    }
  })
  
  # Geolocation
  observeEvent(input$locate, {
    session$sendCustomMessage(type = "geolocate", message = list())
  })
  
  observeEvent(input$geo, {
    req(input$geo)
    loc <- input$geo
    user_location(c(loc$longitude, loc$latitude))
    
    leafletProxy("map") %>%
      clearGroup("user") %>%
      addMarkers(
        lng = loc$longitude,
        lat = loc$latitude,
        icon = makeIcon(
          iconUrl = "https://cdn-icons-png.flaticon.com/512/684/684908.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        popup = "You are here",
        group = "user"
      ) %>%
      addCircles(
        lng = loc$longitude,
        lat = loc$latitude,
        radius = input$radius * 1000,
        color = "#0073e6",
        fillColor = "#0073e6",
        opacity = 0.5,
        fillOpacity = 0.2,
        group = "user"
      ) %>%
      setView(lng = loc$longitude, lat = loc$latitude, zoom = 12)
  })
  
  # Table and interactions
  output$nearbyTable <- renderDT({
    req(user_location(), filtered_data())
    
    result <- filtered_data() %>%
      mutate(
        distance = round(distm(cbind(longitude, latitude), matrix(user_location(), nrow = 1)) / 1000, 2)
      ) %>%
      filter(distance <= input$radius) %>%
      select(common_name, observed_on, distance, place_guess, latitude, longitude) %>%
      arrange(distance)
    
    datatable(result, selection = "single", options = list(dom = 'tp'))
  })
  
  observeEvent(input$nearbyTable_rows_selected, {
    row <- input$nearbyTable_rows_selected
    if (!is.null(row)) {
      coords <- filtered_data() %>%
        mutate(
          distance = round(distm(cbind(longitude, latitude), matrix(user_location(), nrow = 1)) / 1000, 2)
        ) %>%
        filter(distance <= input$radius) %>%
        arrange(distance) %>%
        slice(row) %>%
        select(longitude, latitude)
      
      leafletProxy("map") %>%
        setView(lng = coords$longitude, lat = coords$latitude, zoom = 14)
    }
  })
  
  output$stats <- renderPrint({
    req(filtered_data())
    data <- filtered_data()
    
    if (nrow(data) == 0) return("⚠️ No data available for the selected filters.")
    
    cat("📊🔍 Wildlife Observation Summary\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("🧮 Total Observations       : ", nrow(data), "\n")
    cat("🦁 Unique Species Observed  : ", length(unique(data$common_name)), "\n")
    cat("📅 Date Range               : ", format(min(data$observed_on)), " ➡ ", format(max(data$observed_on)), "\n\n")
    
    cat("🏆 Top 3 Most Observed Species:\n")
    most_common_species <- data %>%
      count(common_name, sort = TRUE) %>%
      slice_head(n = 3)
    print(most_common_species)
    
    cat("\n📍 Top 3 Observation Locations:\n")
    top_places <- data %>%
      filter(!is.na(place_guess)) %>%
      count(place_guess, sort = TRUE) %>%
      slice_head(n = 3)
    print(top_places)
    
    cat("\n🌍 Species with Widest Geographic Spread (in km):\n")
    spread_species <- data %>%
      group_by(common_name) %>%
      summarise(
        spread_km = distHaversine(
          cbind(min(longitude, na.rm = TRUE), min(latitude, na.rm = TRUE)),
          cbind(max(longitude, na.rm = TRUE), max(latitude, na.rm = TRUE))
        ) / 1000
      ) %>%
      arrange(desc(spread_km)) %>%
      slice_head(n = 3)
    print(spread_species)
    
    cat("\n📏 Positional Accuracy:\n")
    cat("   ⏺️ Average (m): ", round(mean(data$positional_accuracy, na.rm = TRUE), 2), "\n")
    cat("   ⏹️ Maximum (m): ", max(data$positional_accuracy, na.rm = TRUE), "\n")
    
    recent_obs <- sum(data$observed_on >= (Sys.Date() - 7))
    cat("\n🕒 Observations in the Last 7 Days: ", recent_obs, "\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("wildlife-observations-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# JS
js <- "
Shiny.addCustomMessageHandler('toggleDark', function(enabled) {
  document.body.classList.toggle('dark-mode', enabled);
});
$(document).ready(function() {
  $('#locate').on('click', function() {
    navigator.geolocation.getCurrentPosition(function(position) {
      Shiny.setInputValue('geo', {
        latitude: position.coords.latitude,
        longitude: position.coords.longitude
      });
    });
  });
});
"

# Launch App
shinyApp(
  ui = tagList(tags$head(tags$script(HTML(js))), ui),
  server = server
)
