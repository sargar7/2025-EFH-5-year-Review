###testing .gpkg for EFH map 

##do not want to modify original script that was working

library(sf)
library(tidyr)
library(dplyr)
library(stringr)
library(ggspatial)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(rmapshaper) # simplify shapefiles

setwd("C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat")

# List all habitat type folders
habitattype_folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
print(habitattype_folders)

# Define base output directory for saving all files
output_dir <- "C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/Maps_Output"

# Define directory for shapefiles inside the output directory
shp_dir <- file.path(output_dir, "Shapefiles")

# Create the shapefiles directory if it doesn't exist
dir.create(shp_dir, showWarnings = FALSE)

# Optional: Also create an "HTML_Maps" directory for the map HTML/PNG files
maps_dir <- file.path(output_dir, "HTML_Maps")
dir.create(maps_dir, showWarnings = FALSE)

# Print paths to check if directories are created correctly
print(paste("Shapefiles will be saved to:", shp_dir))
print(paste("Maps (HTML/PNG) will be saved to:", maps_dir))

# Initialize an empty list to store all spatial files
shapefile_list <- list()

# Loop through each habitat type folder
for (folder in habitattype_folders) {
  
  # List all .shp and .gpkg files in the folder (and subfolders)
  shp_files <- list.files(path = folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  gpkg_files <- list.files(path = folder, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)
  
  # Read all shapefiles (.shp)
  for (file in shp_files) {
    shapefile_name <- gsub(".*/(.*)\\.shp", "\\1", file)
    shapefile_list[[shapefile_name]] <- st_read(file, quiet = TRUE)
  }
  
  # Read all geopackages (.gpkg), extracting layer names and reading them
  for (file in gpkg_files) {
    layers <- st_layers(file)$name
    for (layer in layers) {
      layer_name <- paste0(tools::file_path_sans_ext(basename(file)), "_", layer)
      shapefile_list[[layer_name]] <- st_read(file, layer = layer, quiet = TRUE)
    }
  }
}

# Clean non-geometry columns to numeric where possible
clean_shapefiles <- function(shapefile) {
  shapefile[] <- lapply(shapefile, function(col) {
    if (!inherits(col, "sfc")) {
      return(suppressWarnings(as.numeric(col)))  # may coerce characters
    }
    return(col)
  })
  return(shapefile)
}
shapefile_list <- lapply(shapefile_list, clean_shapefiles)

# Reproject all to WGS84 (EPSG:4326)
shapefile_list <- lapply(shapefile_list, function(shp) st_transform(shp, crs = 4326))

# Simplify if needed based on feature count
simplify_threshold <- 5000
get_keep_ratio <- function(n_features) {
  if (n_features > 50000) return(0.005)
  if (n_features > 20000) return(0.01)
  if (n_features > 10000) return(0.02)
  return(0.03)
}
shapefile_list <- lapply(names(shapefile_list), function(name) {
  shp <- shapefile_list[[name]]
  n_feat <- nrow(shp)
  if (n_feat > simplify_threshold) {
    kr <- get_keep_ratio(n_feat)
    message(sprintf("Simplifying %s (%d features) with keep_ratio %.3f", name, n_feat, kr))
    shp <- simplify_shape(shp, keep_ratio = kr)
  }
  return(shp)
})
names(shapefile_list) <- tolower(names(shapefile_list))
# -----------------------------
# Save each processed layer to .gpkg
# -----------------------------

# Create output directory for GPKG files
gpkg_dir <- "C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat/Converted_GPKG"
dir.create(gpkg_dir, showWarnings = FALSE)

# Write each cleaned, reprojected, simplified shapefile as GPKG
for (name in names(shapefile_list)) {
  shp <- shapefile_list[[name]]
  gpkg_path <- file.path(gpkg_dir, paste0(name, ".gpkg"))
  st_write(shp, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
}

View(shapefile_list)
names(shapefile_list) <- tolower(names(shapefile_list))
View(shapefile_list)

####################### Gag dataset #############################
Gag<- read.csv("C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/Gag_EX_data_Rcode.csv")
View(Gag)

##separate values that are followed by commas into separate row
Gag_long <-Gag %>%
  separate_rows(HabitatZone, sep = ",\\s*") %>%
  separate_rows(HabitatType, sep = ",\\s*") %>%
  separate_rows(Ecoregion, sep = ",\\s*")

View(Gag_long)

##rename man to mangrove 

Gag_long$HabitatType[Gag_long$HabitatType == "man"] <- "mangrove"

View(Gag_long)

##overlaying shapefiles based on Gag_long 
##clean up, make all lowercase and remove any additional spaces
Gag_clean <- Gag_long %>%
  mutate(
    HabitatType = str_trim(str_to_lower(HabitatType)), #EM, HB,mangrove, oyster, reef, sand, SAV, shelf, SB, WCA
    HabitatZone = str_trim(str_to_lower(HabitatZone)), #est, near, off
    Species = str_trim(str_to_lower(Species)),
    Lifestage = str_trim(str_to_lower(Lifestage)),
    Ecoregion = paste0 ("er", Ecoregion), # ER1, ER2, ER3, ER4, ER5 
    shapefile_name = paste(HabitatType, HabitatZone, Ecoregion, sep ="_"))
View(Gag_clean)

########################## test EJ out of loop ########################################

# Filter only early juvenile from the cleaned data
early_juv_group <- Gag_clean %>%
  filter(Lifestage == "earlyjuvenile") %>%
  group_by(Species, Lifestage) %>%
  group_split() %>%
  .[[1]]  # Pull the data frame out of the list

# Basic info
species <- unique(early_juv_group$Species)
lifestage <- unique(early_juv_group$Lifestage)

# Titles and filenames
title_text <- paste(str_to_title(species), "-", str_to_title(lifestage), "EFH")
safe_id <- gsub("[^[:alnum:]_]", "_", paste(species, lifestage, sep = "_"))
html_file <- file.path(output_dir, paste0("map_", safe_id, ".html"))
png_file <- file.path(output_dir, paste0("map_", safe_id, ".png"))
shp_file <- file.path(shp_dir, paste0("EFH_", safe_id, ".shp"))

# Grab shapefile names
shapes <- unique(early_juv_group$shapefile_name)
shape_list <- list()

# Start leaflet map
efh_map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = -89, lat = 25, zoom = 5) %>%
  addLabelOnlyMarkers(
    lng = -98, lat = 31,
    label = title_text,
    labelOptions = labelOptions(noHide = TRUE, direction = "center", textsize = "20px", fontWeight = "bold", opacity = 1)
  )


# Loop through shapefiles
for (shape_name in shapes) {
  if (shape_name %in% names(shapefile_list)) {
    shp <- shapefile_list[[shape_name]]
    
    shape_list[[length(shape_list) + 1]] <- shp
    
    efh_map <- efh_map %>%
      addPolygons(data = shp,
                  fillColor = "red",
                  fillOpacity = 0.5,
                  color = "red",
                  weight = 1,
                  label = shape_name)
  } else {
    warning(paste("Missing shapefile:", shape_name))
  }
}

# Show map
print(efh_map)

# Pause to inspect
readline(prompt = "Press [Enter] to finish earlyjuvenile testing...")

###### not producing map value- nomenclature issue when moving to .gpkg value##### 

cat("Example from Gag_clean:\n")
print(unique(Gag_clean$shapefile_name)[1:5])

cat("\nExample names in shapefile_list:\n")
print(names(shapefile_list)[10:15])

