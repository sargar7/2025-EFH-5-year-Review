###testing .gpkg for EFH map 

##do not want to modify original script that was working
install.packages("viridisLite")
library(sf)
library(tidyr)
library(dplyr)
library(stringr)
library(ggspatial)
library(leaflet)
library(htmlwidgets) #save html package 
library(webshot2)
library(rmapshaper) # simplify shapefiles
library(viridisLite)

getwd()

##upload habitat shapefiles by habitat zone and ecoregion 
habitat_dir<-"C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat"

# List all habitat type folders (directories) in the current directory
habitattype_folders <- list.dirs(path = habitat_dir, full.names = TRUE, recursive = FALSE)
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
# Several shapefiles >5000
simplify_threshold <- 5000
get_keep_ratio <- function(n_features) {
  if (n_features > 50000) return(0.005)
  if (n_features > 20000) return(0.01)
  if (n_features > 10000) return(0.02)
  return(0.03)
}

# Apply simplification and preserve names
shapefile_list <- setNames(lapply(names(shapefile_list), function(name) {
  shp <- shapefile_list[[name]]
  if (!is.null(shp)) {
    n_feat <- nrow(shp)
    if (n_feat > simplify_threshold) {
      kr <- get_keep_ratio(n_feat)
      message(sprintf("Simplifying %s (%d features) with keep_ratio %.3f", name, n_feat, kr))
      shp <- rmapshaper::ms_simplify(shp, keep = kr, keep_shapes = TRUE)
    }
  }
  return(shp)
}), tolower(names(shapefile_list)))  # also convert names to lowercase here

# Remove any NULLs to prevent st_write() errors
shapefile_list <- shapefile_list[!sapply(shapefile_list, is.null)]

################### Save each processed layer to .gpkg ########################

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

####### DO NOT NEED TO RUN SHAPEFILES NOW THAT .GPKG FILES ARE CREATED ######
########################START HERE TO HELP MEMORY ###################
gpkg_dir <- "C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat/Converted_GPKG"
dir.create(gpkg_dir, showWarnings = FALSE)

# List all .gpkg files in the directory
gpkg_files <- list.files(gpkg_dir, pattern = "\\.gpkg$", full.names = TRUE)
View(gpkg_files)

# Initialize an empty list to store the shapefiles from the .gpkg files
shapefile_list <- list()

# Loop through the .gpkg files and read the layers
for (file in gpkg_files) {
  # Get the available layers in the .gpkg file
  layers <- st_layers(file)$name
  
  # Loop through each layer and read it into the shapefile list
  for (layer in layers) {
    # Correct the naming convention to match "habitattype_habitatzone_ecoregion"
    # Extract the correct file name (i.e., the first part of the layer name)
    # assuming the layer name is not in the format you expect.
    # For example, layer_name could be "wca_off_er1" or something else.
    
    # Use only the part of the layer name that corresponds to your naming convention
    layer_name_parts <- strsplit(layer, "_")[[1]]
    correct_name <- paste(tolower(layer_name_parts[1]), 
                          tolower(layer_name_parts[2]), 
                          toupper(layer_name_parts[3]), 
                          sep = "_")
    
    # Store the shapefile in the list with the corrected name
    shapefile_list[[correct_name]] <- st_read(file, layer = layer, quiet = TRUE)
  }
}

# Rename shapefile list elements to lowercase
names(shapefile_list) <- tolower(names(shapefile_list))

# View the list of shapefiles with corrected names
View(shapefile_list)

######################### .gpkg summary ####################################

# Create a summary table of key info
shapefile_summary <- lapply(names(shapefile_list), function(name) {
  shp <- shapefile_list[[name]]
  gpkg_path <- file.path(gpkg_dir, paste0(name, ".gpkg"))
  
  list(
    name = name,
    crs = st_crs(shp)$epsg,
    features = nrow(shp),
    file_size_mb = if (file.exists(gpkg_path)) file.info(gpkg_path)$size / 1024^2 else NA,
    geometry_type = unique(st_geometry_type(shp))
  )
}) %>% bind_rows()

# View in Viewer pane
View(shapefile_summary)


####test plot with .gpkg files ###

library(leaflet)

# Extract the layers
wca_layer <- shapefile_list[["wca_off_er1"]]
mangrove_layer <- shapefile_list[["mangrove_est_er2"]]

str(wca_layer)
str(mangrove_layer)

# Basic leaflet map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = wca_layer, 
              fillColor = "red", 
              fillOpacity = 0.5, 
              color = "red", 
              weight = 1,
              group = "WCA OFF ER1",
              label = ~paste("WCA")) %>%
  addPolygons(data = mangrove_layer, 
              fillColor = "green", 
              fillOpacity = 0.5, 
              color = "green", 
              weight = 1,
              group = "Mangrove EST ER2",
              label = ~paste("Mangrove")) %>%
  addLayersControl(
    overlayGroups = c("WCA OFF ER1", "Mangrove EST ER2"),
    options = layersControlOptions(collapsed = FALSE)
  )

 

#add em est er1

em_est_er1 <-shapefile_list[["em_est_er1"]]
str(em_est_er1)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = wca_layer, 
              fillColor = "red", 
              fillOpacity = 0.5, 
              color = "red", 
              weight = 1,
              group = "WCA OFF ER1",
              label = ~paste("WCA")) %>%
  addPolygons(data = mangrove_layer, 
              fillColor = "green", 
              fillOpacity = 0.5, 
              color = "green", 
              weight = 1,
              group = "Mangrove EST ER2",
              label = ~paste("Mangrove")) %>%
  addPolygons(data = em_est_er1, 
              fillColor = "purple", 
              fillOpacity = 0.5, 
              color = "purple", 
              weight = 1,
              group = "EM EST ER1",
              label = ~paste("EM EST ER1")) %>%
  addLayersControl(
    overlayGroups = c("WCA OFF ER1", "Mangrove EST ER2", "EM EST ER1"),
    options = layersControlOptions(collapsed = FALSE)
  )

##################### success now onto GAG map ##################

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

###successful EJ map 

###################### Loop through all species ##################

# Define lifestages and assign colorblind-safe colors
lifestages <- unique(Gag_clean$Lifestage)
stage_colors <- setNames(viridis(length(lifestages)), lifestages)

# Loop through each lifestage
for (stage in lifestages) {
  stage_group <- Gag_clean %>%
    filter(Lifestage == stage) %>%
    group_by(Species, Lifestage) %>%
    group_split() %>%
    .[[1]]
  
  shapes <- unique(stage_group$shapefile_name)
  species <- unique(stage_group$Species)
  
  title_text <- paste(str_to_title(species), "-", str_to_title(stage), "EFH")
  safe_id <- gsub("[^[:alnum:]_]", "_", paste(species, stage, sep = "_"))
  html_file <- file.path(output_dir, paste0("map_", safe_id, ".html"))
  
  # Get color for this lifestage
  color <- stage_colors[[stage]]
  
  # Start map
  efh_map <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lng = -89, lat = 25, zoom = 5) %>%
    addLabelOnlyMarkers(
      lng = -98, lat = 31,
      label = title_text,
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textsize = "20px", fontWeight = "bold", opacity = 1)
    )
  
  # Add each habitat polygon
  for (shape_name in shapes) {
    gpkg_file <- file.path(gpkg_dir, paste0(shape_name, ".gpkg"))
    
    if (file.exists(gpkg_file)) {
      try({
        shp <- st_read(gpkg_file, quiet = TRUE)
        
        efh_map <- efh_map %>%
          addPolygons(data = shp,
                      fillColor = color,
                      fillOpacity = 0.5,
                      color = color,
                      weight = 1,
                      group = shape_name,
                      label = shape_name)
      }, silent = TRUE)
    } else {
      warning(paste("Missing GPKG:", gpkg_file))
    }
  }
  
  # Add layers control
  efh_map <- efh_map %>%
    addLayersControl(
      overlayGroups = shapes,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # 🔄 Show map in Viewer pane
  print(efh_map)
  
  #pause between maps
  readline(prompt = paste("Showing map for", stage, "- Press [Enter] to continue..."))
}
 

 # Save HTML map- commented out for memory issues 
 # saveWidget(efh_map, file = html_file, selfcontained = TRUE)
  #message(paste("Saved:", html_file))


##WCA_off_ER1 and WCA_off_ER2 not showing up for postlarvae
##EJ memory issues- taking a long time to load - can likely remove interactive map


##### 05/05/25 Notes ####
#create a loop for all species
