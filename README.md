🐾 WILDLIFE CONFLICT MAPPER

An interactive R Shiny web application to map and analyze dangerous wildlife sightings in Kenya using publicly available crowd-sourced data. The goal is to inform locals and tourists about recent wildlife conflicts to enhance awareness and safety.

🔗 Live Demo: https://collokim-apps.shinyapps.io/wildlife-conflict-mapper/



🚀 FEATURES

📍 Interactive Map of wildlife sightings in Kenya

🐘 Filter by species, location, and time range

📁 Upload your own CSV data of sightings

🔥 Toggleable Heatmap to highlight conflict zones

🌙 Custom Dark Mode interface

📊 Summary statistics and a table of recent nearby observations

⬇️ Download filtered data for offline analysis

📡 Locate and center map based on your current position



📂 PROJECT STRUCTURE

```
Wildlife-Conflict-Mapper/
│
├── data/                         # Uploaded observation CSVs
├── www/                          # Image icons for different species
│   ├── elephant.png
│   ├── lion.png
│   └── ...
├── cleaned_observations.csv      # Cleaned data sample
├── observations-*.csv            # Uploaded datasets
├── wildlifeapp.R                 # Main Shiny app script
├── LICENSE                       # License file
└── README.md                     # Project readme
```


📊 DATA SOURCES

The application uses wildlife sighting data from:

iNaturalist.org

#Disclaimer: The data shown may not represent all wildlife activity and is limited to what's available on iNaturalist as of 15 May 2025.



📦 R Packages Used

-Shiny – App framework

-Leaflet, leaflet.extras – Interactive maps and heatmaps

-Dplyr, lubridate, readr – Data manipulation

-DT – Interactive data tables

-ShinyWidgets – UI enhancements

-Digest – Data deduplication using hashing



🛠️ How to Run Locally

Clone the repository:

```
git clone https://github.com/Meliodus254/Wildlife-Conflict-Mapper.git
```
```
cd Wildlife-Conflict-Mapper
```
Open wildlifeapp.R in RStudio or run:
```
shiny::runApp("wildlifeapp.R")
```
Upload or explore sample CSV files from the data/ folder.


👨‍💻 Author
Collins Kimani Mumo (@Meliodus254)



📄 License
This project is licensed under the MIT License. See the LICENSE file for more details.

