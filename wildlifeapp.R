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
library(shinyjs)
options(shiny.maxRequestSize = 100 * 1024^2)  # 50 MB limit

# Helper function
get_icon_url <- function(species) {
  case_when(
    # Crocodiles
    grepl("Crocodile|Nile Crocodile|Alligator", species, ignore.case = TRUE) ~ "crocodile.png",
    
    # Elephants
    grepl("Elephant|African Elephant|Savannah Elephant|Forest Elephant", species, ignore.case = TRUE) ~ "elephant.png",
    
    # Big cats
    grepl("Lion|Southern Lion", species, ignore.case = TRUE) ~ "lion.png",
    grepl("Leopard|Panther", species, ignore.case = TRUE) ~ "leopard.png",
    grepl("Cheetah", species, ignore.case = TRUE) ~ "cheetah.png",
    
    # Hippos
    grepl("Hippo|Hippopotamus|Common Hippopotamus|Pygmy Hippopotamus", species, ignore.case = TRUE) ~ "hippo.png",
    
    # Hyenas & relatives
    grepl("Hyena|Spotted Hyena|Striped Hyena|Brown Hyena|Hyaena|Aardwolf", species, ignore.case = TRUE) ~ "hyena.png",
    
    # Buffalo
    grepl("Buffalo|Cape Buffalo|African Buffalo", species, ignore.case = TRUE) ~ "buffalo.png",
    
    # Rhinos
    grepl("Rhino|Rhinoceros|Black Rhino|White Rhino", species, ignore.case = TRUE) ~ "rhino.png",
    
    # Giraffes
    grepl("Giraffe|Masai Giraffe|Reticulated Giraffe", species, ignore.case = TRUE) ~ "giraffe.png",
    
    # Zebras
    grepl("Zebra|Plains Zebra|Grévy's Zebra|Mountain Zebra", species, ignore.case = TRUE) ~ "zebra.png",
    
    # Antelopes & Gazelles
    grepl("Impala|Gazelle|Thomson's Gazelle|Grant's Gazelle|Eland|Kudu|Waterbuck|Sable Antelope|Hartebeest|Topi|Oribi|Bushbuck", species, ignore.case = TRUE) ~ "antelope.png",
    
    # Primates
    grepl("Baboon|Vervet Monkey|Colobus Monkey|Sykes Monkey|Tantalus Monkey|Chimpanzee|Monkey", species, ignore.case = TRUE) ~ "monkey.png",
    
    grepl("Wild Boar|Bush Pig|Giant Forest Hog", species, ignore.case = TRUE) ~ "wild_boar.png",
    
    grepl("Wild Dog|African Wild Dog|Painted Dog", species, ignore.case = TRUE) ~ "wild_dog.png",
    
    grepl("Tortoise", species, ignore.case = TRUE) ~ "tortoise.png",
    
    grepl("Turtle", species, ignore.case = TRUE) ~ " turtle.png",
    
    grepl("Crab", species, ignore.case = TRUE) ~ "crab.png",
    
    grepl("Jellyfish", species, ignore.case = TRUE) ~ "jellyfish.png",
    
    grepl("Octopus|Squid", species, ignore.case = TRUE) ~ "octopus.png",

    grepl("Seahorse", species, ignore.case = TRUE) ~ "seahorse.png",
  
    grepl("Stingray|Ray", species, ignore.case = TRUE) ~ "stingray.png",

    grepl("Walrus", species, ignore.case = TRUE) ~ "walrus.png",

    grepl("Shark", species, ignore.case = TRUE) ~ "shark.png",
    
    grepl("Lobster", species, ignore.case = TRUE) ~ "lobster.png",
    
    grepl("Prawn", species, ignore.case = TRUE) ~ "prawn.png",
    
    grepl("Starfish", species, ignore.case = TRUE) ~ "starfish.png",
    
    grepl("Whale", species, ignore.case = TRUE) ~ "whale.png",
    
    grepl("Bear", species, ignore.case = TRUE) ~ "bear.png",
    
    grepl("Bearded dragon", species, ignore.case = TRUE) ~ "bearded_dragon.png",
    
    grepl("Beaver", species, ignore.case = TRUE) ~ "beaver.png",
    
    grepl("Jaguar", species, ignore.case = TRUE) ~ "jaguar.png",
    
    grepl("Deer", species, ignore.case = TRUE) ~ "deer.png",
    
    grepl("Fox", species, ignore.case = TRUE) ~ "fox.png",
    
    grepl("Kangaroo", species, ignore.case = TRUE) ~ "kangaroo.png",
    
    grepl("Panda", species, ignore.case = TRUE) ~ "panda.png",
    
    grepl("Lemur", species, ignore.case = TRUE) ~ "lemur.png",
    
    grepl("Llama", species, ignore.case = TRUE) ~ "llama.png",
    
    grepl("Puma", species, ignore.case = TRUE) ~ "puma.png",
    
    grepl("Sloth", species, ignore.case = TRUE) ~ "sloth.png",
    
    grepl("Slug", species, ignore.case = TRUE) ~ "slug.png",
    
    grepl("Tapir", species, ignore.case = TRUE) ~ "tapir.png",
    
    grepl("Wildbeest", species, ignore.case = TRUE) ~ "wildbeest.png",
    
    grepl("Wolf", species, ignore.case = TRUE) ~ "wolf.png",
    
    grepl("Racoon", species, ignore.case = TRUE) ~ "racoon.png",

    grepl("Millipede|Centipede|Myriapod", species, ignore.case = TRUE) ~ "millipede_centipede.png",
    
    grepl("Dove", species, ignore.case = TRUE) ~ "dove.png",
    
    # Snakes
    grepl("Python|Boa|African Rock Python|Burmese Python", species, ignore.case = TRUE) ~ "python.png",
    grepl("Cobra|Black Mamba|Green Mamba|Spitting Cobra|Egyptian Cobra", species, ignore.case = TRUE) ~ "cobra.png",
    grepl("Viper|Puff Adder|Gaboon Viper|Rhinoceros Viper|Horned Viper|Saw-scaled Viper", species, ignore.case = TRUE) ~ "viper.png",
    grepl("Snake|Adder|Sand Snake|Whip Snake|Night Adder", species, ignore.case = TRUE) ~ "snake.png",
    
    # Insects
    grepl("Mosquito|Anopheles|Aedes|Culex", species, ignore.case = TRUE) ~ "mosquito.png",
    grepl("Fly|Mayfly|Crane|Firefly|Gallinipper|Tsetse Fly", species, ignore.case = TRUE) ~ "tsetse_fly.png",
    grepl("Termite", species, ignore.case = TRUE) ~ "termite.png",
    grepl("Spider|Orbweaver|Meshweaver|Sheet-weaver|Micropanther", species, ignore.case = TRUE) ~ "spider.png",
    grepl("Butterfly|Monarch|Viceroy|Swallowtail|Morpho|Fritillary|Checkerspot|Skipper", species, ignore.case = TRUE) ~ "butterfly.png",
    grepl("Beetle|Scarab|Cadelle|Ladybug|Weevil|Discoderus", species, ignore.case = TRUE) ~ "beetle.png",
    grepl("Ant", species, ignore.case = TRUE) ~ "ant.png",
    grepl("Grasshopper|Katydid|Cricket|Locust", species, ignore.case = TRUE) ~ "grasshopper.png",
    grepl("Bug|Aphid|Leafhopper|Planthopper|Phylloxera|Mealybug", species, ignore.case = TRUE) ~ "bug.png",
    grepl("Bee|Beewolf|Bee-killer", species, ignore.case = TRUE) ~ "bee.png",
    grepl("Wasp|Mantid|Mantis", species, ignore.case = TRUE) ~ "wasp.png",
    grepl("Moth|Sheepmoth|Cloudywing|Duskywing", species, ignore.case = TRUE) ~ "moth.png",
    grepl("Damselfly|Dancer", species, ignore.case = TRUE) ~ "damselfly.png",
    grepl("Springtail|Bristletail|Prostig|Orthoclad|Crustacean|Isopod|Ostracod|Copepod|Amphipod|Anomopod|Arachnid|Harvestman|Scorpion|Solifuge", species, ignore.case = TRUE) ~ "arachnid.png",
    grepl("Hanging-thieves|Crescents|Cicadas|Commas|Grass-Veneers|Pillbugs|Caddisflies|Plumetops|Queen|Coneheads|Microcaddisflies|Marauders|Harvester|Midgets|Microleafhoppers|Triops|Meadowhawks|Termites|Casebearers|Roadside-Skippers|Streaktails|Baskettails|Prostigs|Bladetails|Mesostigs|Aphideaters|Sedgesitters|Hayworms|Globetails|Amberwings|Psylloids|Pleurolomas|Springflies|Saw-wing|Boopies|Stoneflies|Scallopwings|Sanddragons|Pitheads|Malachite|Cruisers|Mygalomorphs|Wood-Nymphs|Spongillaflies|Firebrat|Endothenias|Pufftails|Phylloxerans|curved-ribbon|Earwigs|Horntails|Clipper|Owlflies|Ringtails|Sharpshooters|Donacia|Dogfaces|Kite-Swallowtails|Shadowdragons|Lovebug|Fleas|Mochas", species, ignore.case = TRUE) ~ "insect.png",
    grepl("Dragonfly", species, ignore.case = TRUE) ~ "dragonfly.png",
    
     # Birds (some common large species)
    grepl("Ostrich", species, ignore.case = TRUE) ~ "ostrich.png",
    grepl("Eagle|Vulture|Kite|Harrier", species, ignore.case = TRUE) ~ "bird_of_prey.png",
    grepl("Hornbill", species, ignore.case = TRUE) ~ "hornbill.png",
    grepl("Secretary Bird", species, ignore.case = TRUE) ~ "secretary_bird.png",
    grepl("Flamingo", species, ignore.case = TRUE) ~ "flamingo.png",
    grepl("Marabou Stork", species, ignore.case = TRUE) ~ "marabou_stork.png",
    grepl("Pelican", species, ignore.case = TRUE) ~ "pelican.png",
    grepl("Kingfisher", species, ignore.case = TRUE) ~ "kingfisher.png",
    grepl("Weaver|Sunbird|Drongo", species, ignore.case = TRUE) ~ "small_bird.png",
    grepl("duck", species, ignore.case = TRUE) ~ "duck.png",
    grepl("owl", species, ignore.case = TRUE) ~ "owl.png",
    grepl("parrot", species, ignore.case = TRUE) ~ "parrot.png",
    grepl("Crane|Crowned Crane", species, ignore.case = TRUE) ~ "crane.png",
    
    # Smaller carnivores
    grepl("Caracal", species, ignore.case = TRUE) ~ "caracal.png",
    grepl("Serval", species, ignore.case = TRUE) ~ "serval.png",
    grepl("Mongoose", species, ignore.case = TRUE) ~ "mongoose.png",
    grepl("Honey Badger|Ratel", species, ignore.case = TRUE) ~ "honey_badger.png",
    
    #🐊 Reptiles
    grepl("Monitor Lizard|Lizard|Agama", species, ignore.case = TRUE) ~ "lizard.png",
    grepl("Gecko", species, ignore.case = TRUE) ~ "gecko.png",
    grepl("Newt|Salamander", species, ignore.case = TRUE) ~ "salamander.png",
    grepl("chameleon", species, ignore.case = TRUE) ~ "chameleon.png",
    
    # Rodents & small mammals
    grepl("Porcupine", species, ignore.case = TRUE) ~ "porcupine.png",
    grepl("Rat|Mouse|Gerbil|Dormouse", species, ignore.case = TRUE) ~ "rodent.png",
    grepl("Squirrel", species, ignore.case = TRUE) ~ "squirrel.png",
    grepl("Mole|Shrew", species, ignore.case = TRUE) ~ "mole.png",
    grepl("Hedgehog", species, ignore.case = TRUE) ~ "hedgehog.png",
    
    # Amphibians
    grepl("Frog|Toad", species, ignore.case = TRUE) ~ "frog.png",
    
    # Aquatic & semi-aquatic
    grepl("Otter", species, ignore.case = TRUE) ~ "otter.png",
    grepl("Seal|Sea Lion", species, ignore.case = TRUE) ~ "seal.png",
    grepl("Dolphin|Whale", species, ignore.case = TRUE) ~ "dolphin.png",
    
    grepl("Tilapia|Catfish|Nile Perch|fish", species, ignore.case = TRUE) ~ "fish.png",
    
    
    # Others
    grepl("Warthog", species, ignore.case = TRUE) ~ "warthog.png",
    grepl("Jackal|Fox", species, ignore.case = TRUE) ~ "jackal.png",
    grepl("Flying Fox|Bat|Fruit Bat", species, ignore.case = TRUE) ~ "bat.png",
    grepl("Hare|Rabbit|Scrub Hare", species, ignore.case = TRUE) ~ "hare.png",
    
    # Default icon for unknown species
    TRUE ~ "default.png"
  )
}




# Ensure the data folder exists
ensure_data_dir <- function() {
  dir_path <- "data"
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  return(dir_path)
}

# Load data from the persistent folder
load_data <- function() {
  data_dir <- ensure_data_dir()
  master_file <- file.path(data_dir, "observations_master.csv")
  
  # Files to include in addition to master
  specific_files <- c(
    "observations-574907.csv", "observations-574910.csv", "observations-574923.csv",
    "observations-574925.csv", "observations-574927.csv", "observations-574987.csv",
    "observations-574989.csv"
  )
  
  files_to_load <- c(master_file, file.path(data_dir, specific_files))
  files_to_load <- files_to_load[file.exists(files_to_load)]  # Only existing files
  
  if (length(files_to_load) == 0) {
    return(data.frame())  # Nothing to load
  }
  
  data <- lapply(files_to_load, function(f) {
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




# Save new unique data into the persistent folder
save_new_data <- function(new_df) {
  data_dir <- ensure_data_dir()
  master_file <- file.path(data_dir, "observations_master.csv")
  
  required_cols <- c("id", "observed_on", "common_name", "latitude", "longitude")
  if (!all(required_cols %in% colnames(new_df))) {
    stop("Uploaded data is missing one or more required columns: ", 
         paste(setdiff(required_cols, colnames(new_df)), collapse = ", "))
  }
  
  new_df <- new_df[complete.cases(new_df), ]
  if (nrow(new_df) == 0) {
    stop("All rows contain missing values after filtering. No valid data to save.")
  }
  
  # Load existing master data if it exists, else empty df
  if (file.exists(master_file)) {
    existing_data <- read_csv(master_file, show_col_types = FALSE) %>%
      mutate(observed_on = as.Date(observed_on))
  } else {
    existing_data <- data.frame()
  }
  
  # Ensure columns match (ignore icon_url if present)
  existing_cols <- colnames(existing_data)
  new_cols <- colnames(new_df)
  
  existing_cols_no_icon <- setdiff(existing_cols, "icon_url")
  
  missing_cols <- setdiff(existing_cols_no_icon, new_cols)
  if (length(missing_cols) > 0) {
    stop("New data is missing some columns present in existing data (excluding 'icon_url'): ", 
         paste(missing_cols, collapse = ", "))
  }
  
  if (length(existing_cols_no_icon) > 0) {
    new_df <- new_df[, existing_cols_no_icon, drop = FALSE]
  }
  
  # Combine existing + new
  combined <- bind_rows(existing_data[, existing_cols_no_icon, drop=FALSE], new_df)
  
  # Remove duplicates
  # Use the same hash method
  hash_row <- function(df) {
    apply(df, 1, function(row) digest(paste(as.character(row), collapse = "|")))
  }
  
  combined$hash <- hash_row(combined)
  combined_unique <- combined[!duplicated(combined$hash), ]
  combined_unique$hash <- NULL
  
  # Save combined unique back to master CSV
  write_csv(combined_unique, master_file)
  
  message(paste("Saved", nrow(combined_unique) - nrow(existing_data), "new unique rows."))
}



##UI

ui <- fluidPage(
  useShinyjs(),
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

      /* Fullscreen map styles */
      #map_container.fullscreen {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        z-index: 1050 !important; /* on top of everything */
        border-radius: 0 !important;
        box-shadow: none !important;
      }
      
      /* Hide sidebar and mainPanel elements in fullscreen */
      #sidebarPanel.hidden, 
      #mainPanel.hidden {
        display: none !important;
      }
    ")),
    
    # JS for toggling dark mode and fullscreen map
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleDark', function(isDark) {
        if (isDark) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
      
      Shiny.addCustomMessageHandler('toggleFullscreenMap', function(isFullscreen) {
        var mapContainer = document.getElementById('map_container');
        var sidebar = document.getElementById('sidebarPanel');
        var main = document.getElementById('mainPanel');
        if (isFullscreen) {
          mapContainer.classList.add('fullscreen');
          if(sidebar) sidebar.classList.add('hidden');
          if(main) main.classList.add('hidden');
        } else {
          mapContainer.classList.remove('fullscreen');
          if(sidebar) sidebar.classList.remove('hidden');
          if(main) main.classList.remove('hidden');
        }
      });
    "))
  ),
  
  titlePanel(
    div(style = "text-align:center; font-family: 'Segoe UI', sans-serif;",
        h1("🦁🦓🦒 Wildlife Observations 🐘🐆🌍"),
        p(style = "font-size: 16px; color: #7f8c8d;", "Stay informed. Stay safe.")
    )
  ),
  
  # Wrap sidebarPanel and mainPanel with IDs so we can hide/show
  sidebarLayout(
    sidebarPanel(
      id = "sidebarPanel",
      style = "border-radius: 10px; padding: 20px;",
      
      switchInput("dark_mode", "🌙 Dark Mode", onLabel = "ON", offLabel = "OFF", size = "mini"),
      actionButton("locate", "📍 Find My Location", icon = icon("location-dot"), class = "btn btn-info"),
      
      tags$hr(),
      fileInput("upload", "📁 Upload Observations CSV", accept = ".csv"),
      tags$div(
        style = "font-size: 14px; color: #95a5a6; margin-top: -10px; margin-bottom: 15px;",
        HTML("🔎 You can <b>download wildlife observation data</b> from <a href='https://www.inaturalist.org/observations/' target='_blank'>iNaturalist.org</a>.<br>
        🌍 Upload data from <b>your own country</b> to help expand the observation system.")
      ),
      
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
      id = "mainPanel",
      style = "padding: 10px; border-radius: 10px;",
      
      actionButton("toggle_map", "Expand/Collapse Map", icon = icon("arrows-alt"), class = "btn btn-secondary"),
      
      div(
        id = "map_container",
        style = "height: 600px;",
        leafletOutput("map", width = "100%", height = "100%")
      ),
      
      # Smooth transition style for map container height changes
      tags$style(HTML("
        #map_container {
          transition: all 0.5s ease;
          overflow: hidden;
        }
      ")),
      
      tags$br(),
      h4("📍 Nearby Sightings"),
      DTOutput("nearbyTable"),
      
      tags$br(),
      h4("📊 Summary Statistics"),
      verbatimTextOutput("stats")
    )
  )
)


options(shiny.maxRequestSize = 100 * 1024^2) 

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
  
  
  map_expanded <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_map, {
    if (map_expanded()) {
      runjs("document.getElementById('map_container').style.height = '600px';")
      map_expanded(FALSE)
    } else {
      runjs("document.getElementById('map_container').style.height = '90vh';")
      map_expanded(TRUE)
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
