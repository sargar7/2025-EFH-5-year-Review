###EFH 5 year review 
##Species habitat maps 
##Adding habitat type shapefiles to read by R 
##Creating habitat map for each species by lifestage

##Step one upload habitat type shapefiles into R 

install.packages("sf")
install.packages("raster")
install.packages("ggmap")
install.packages("stringr")
install.packages("tmap")
install.packages("ggspatial")
install.packages("leaflet")
install.packages("webshot2") #saving .png image of maps 
install.packages("htmlwidgets")
install.packages ("rmapshaper") ##simplify shapfiles but maintains spatial data 

library(sf)
library(dplyr)
library(stringr)
library(ggspatial)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(rmapshaper) ##simplify shapefiles to help processing time for maps


setwd("C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat")
##list habitat type folders (subfolders in main directory)

##upload habitat shapefiles by habitat zone and ecoregion 

# List all habitat type folders (directories) in the current directory
habitattype_folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
print(habitattype_folders)

# Initialize an empty list to store shapefiles
shapefile_list <- list()

# Loop through each habitat type folder
for (folder in habitattype_folders) {
  
  # List of shapefiles (.shp) in the current folder (and subfolders)
  shapefiles <- list.files(path = folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  # Check if there are shapefiles in the folder
  if (length(shapefiles) > 0) {
    # Loop through each shapefile and load it
    for (shapefile in shapefiles) {
      # Extract the shapefile name without path and extension
      shapefile_name <- gsub(".*/(.*)\\.shp", "\\1", shapefile)
      
      # Read the shapefile and store it in the list
      shapefile_list[[shapefile_name]] <- st_read(shapefile)
    }
  }
}

# Print the names of the shapefiles that were loaded
print(names(shapefile_list))

##cleanup shapefiles 

# Check column types of each shapefile in the list
lapply(shapefile_list, function(x) sapply(x, class))

#some are numerical and some are characters, some are SFC_POLYGON

##SFC polygons remain the same since those are how the code plots the polygons on the map, converting others 
# Function to clean and ensure consistent column types across all shapefiles
clean_shapefiles <- function(shapefile) {
  # Convert all non-geometry columns to numeric (or character if needed)
  shapefile[] <- lapply(shapefile, function(col) {
    # Check if column is not geometry
    if (!inherits(col, "sfc")) {
      # Convert columns to numeric (or character as necessary)
      return(as.numeric(col))
    }
    return(col)  # Keep geometry columns as they are
  })
  
  return(shapefile)
}

# Apply the cleaning function to all shapefiles in shapefile_list
shapefile_list <- lapply(shapefile_list, clean_shapefiles)
################## REPROJECTING #####################

##shapefiles contain different CRS (coordinate reference system), which can cause alignment issues when being overlayed on a map. 

#reproject shapefiles to WGS84 (ESPG:4326)->> World Geodetic System 1984. It is a global geodetic reference system used for GPS (Global Positioning System) and other navigation and mapping applications. 

shapefile_list_reprojected <- lapply(shapefile_list, function(shp) {st_transform(shp, crs = 4326)})

# Check the CRS of the reprojected shapefiles
st_crs(shapefile_list_reprojected[["mangrove_est_ER1"]])

all(sapply(shapefile_list_reprojected, function(x) st_crs(x)$epsg == 4326)) ##are all shapefiles CRS 

View(shapefile_list_reprojected)

names(shapefile_list_reprojected) <- tolower(names(shapefile_list_reprojected)) ##all lowercase naming 



##check and summarize shapefile contents to see which ones are "heavy" and running into errors in processing. 

shapefile_summary <- lapply(shapefile_list_reprojected, function(shp) {
  list(
    n_features = nrow(shp),
    geometry_type = unique(st_geometry_type(shp)),
    crs = st_crs(shp)$epsg
  )
})

# Convert to a data frame for easy viewing
shapefile_summary_df <- tibble::tibble(
  shapefile_name = names(shapefile_summary),
  n_features = sapply(shapefile_summary, `[[`, "n_features"),
  geometry_type = sapply(shapefile_summary, function(x) paste(x$geometry_type, collapse = ", ")),
  crs = sapply(shapefile_summary, `[[`, "crs")
)

# Sort by number of features (heaviest first)
shapefile_summary_df <- shapefile_summary_df %>%
  arrange(desc(n_features))

# View it
print(shapefile_summary_df)

##need to simplify SAV and other shapefiles- VERY LARGE- looking to simplify those with n_features>5000 as it is slowing down the loop
#using rmapshaper 
#rmapshaper is SUPPOSE to retain the spatial data integrity and suppose to be 'safer' than st_simplify

simplify_threshold <- 5000
# simplified-scaling by shapefile size 

get_keep_ratio <- function(n_features) {
  if (n_features > 50000) return(0.005)
  if (n_features > 20000) return(0.01)
  if (n_features > 10000) return(0.02)
  return(0.03)
}
 #reproject and simplify if necessary
shapefile_list_reprojected <- lapply(names(shapefile_list), function(name) {
  shp <- shapefile_list[[name]]
  shp <- st_transform(shp, crs = 4326)
  n_feat <- nrow(shp)
  
  if (n_feat > simplify_threshold) {
    kr <- get_keep_ratio(n_feat)
    message(sprintf("Simplifying %s (%d features) with keep_ratio %.3f", name, n_feat, kr))
    shp <- simplify_shape(shp, keep_ratio = kr)
  }
  
  return(shp)
})
names(shapefile_list_reprojected) <- names(shapefile_list) ##match names for future looping




###########################################################################################################

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
Gag_long$HabitatHype <- NULL

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


############################# Gag EX Leaflet ################################
################### Full Dataset ######################

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

##simplify shapefiles for data-heavy pulls 
simplify_shape <- function(shape, keep_ratio = 0.05) {
  tryCatch({
    ms_simplify(shape, keep = keep_ratio, keep_shapes = TRUE)
  }, error = function(e) {
    message("Error simplifying shape: ", e$message)
    return(shape)  # fallback to original
  })
}

#define "heavy" shapefile threshold
simplify_threshold <- 10

# Group by Species and Lifestage
gag_grouped <- Gag_clean %>%
  group_by(Species, Lifestage) %>%
  group_split()

# Set bounding box for Gulf of Mexico
bbox_gom <- sf::st_bbox(c(xmin = -98, xmax = -90, ymin = 25, ymax = 31), crs = sf::st_crs(gag_grouped))

# Loop through each species/lifestage group
for (group in gag_grouped) {
  species <- unique(group$Species)
  lifestage <- unique(group$Lifestage)
  
  # Create a title and safe file name
  title_text <- paste(str_to_title(species), "-", str_to_title(lifestage), "EFH")
  safe_id <- gsub("[^[:alnum:]_]", "_", paste(species, lifestage, sep = "_"))
  
  html_file <- file.path(output_dir, paste0("map_", safe_id, ".html"))
  png_file <- file.path(output_dir, paste0("map_", safe_id, ".png"))
  shp_file <- file.path(shp_dir, paste0("EFH_", safe_id, ".shp"))
  
  # Grab shapefile names from Gag_clean
  shapes <- unique(group$shapefile_name)
  shape_list <- list()
  
  # Start leaflet map
  efh_map <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lng = -89, lat = 25, zoom = 5)

    efh_map <- efh_map %>% ##add title on map
    addLabelOnlyMarkers(
      lng = -98, lat = 31, # Coordinates for where you want the label: top left
      label = title_text, 
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textsize = "20px", fontWeight = "bold", opacity = 1)
    )
  ##auto simplify if number of shapefiles exceeds threshole (>10)
    simplify_needed <-length(shapes) > simplify_threshold
    
  # Loop through and add shapefiles
  for (shape_name in shapes) {
    if (shape_name %in% names(shapefile_list_reprojected)) {
      shp <- shapefile_list_reprojected[[shape_name]]
      
      if(simplify_needed) {
        shp<- simplify_shape (shp, keep_ratio=0.02) ##can adjust keep ratio
      }
      
      shape_list[[length(shape_list) + 1]] <- shp
      
      efh_map <- efh_map %>%  ############# eventually add in color for each lifestage
        addPolygons(data = shp,
                    fillColor = "blue",
                    fillOpacity = 0.5,
                    color = "white",
                    weight = 1,
                    label = shape_name)
    } else {
      warning(paste("Missing shapefile:", shape_name))
    }
  }
    
    # Display the map in Viewer pane
    print(efh_map)
    
    # Pause to view it
    readline(prompt = "Press [Enter] to continue to the next map...")
    
    # Skip Save map and shapefile for now
   # saveWidget(efh_map, file = html_file, selfcontained = TRUE)

    print("Moving to the next map...")
    }

  ##removed webshot so the png file isnt slowing down the code
  

####running into processing errors- taking FOREVER!!!
##need to debug ##


##running into error wiht internal(strsplit....) reached elapsed time limit. trying to debug. 


  ###################### running into error combining shapefiles ############################
  
  ## need to inspect col names, align col and combine shapefiles using bind_rows....this may be longer processing time...
  
  # Step 1: Inspect column names of all shapefiles
  print("Checking column names in shape_list:")
  column_names <- lapply(shape_list, names)
  print(column_names)
  
  # Step 2: Identify common columns across all shapefiles
  common_columns <- Reduce(intersect, lapply(shape_list, names))
  print("Common columns across shapefiles:")
  print(common_columns)
  
  # Step 3: Standardize the shapefiles to only include the common columns
  # This will make sure we have the same columns in each shapefile
  shape_list_standardized <- lapply(shape_list, function(shp) {
    shp <- shp[, common_columns, drop = FALSE]  # Keep only common columns
    return(shp)
  })
  
  # Step 4: Now combine the shapefiles
  if (length(shape_list_standardized) > 0) {
    # Combine shapefiles into one sf object
    combined_sf <- do.call(rbind, shape_list_standardized)
    
    # Save combined shapefile
    st_write(combined_sf, shp_file, delete_layer = TRUE, quiet = TRUE)
  } else {
    warning("No shapefiles to combine!")
  }
  
  
  ##### test EJ out of loop ########
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
  
  # Check if simplification is needed
  simplify_needed <- length(shapes) > simplify_threshold
  
  # Loop through shapefiles
  for (shape_name in shapes) {
    if (shape_name %in% names(shapefile_list_reprojected)) {
      shp <- shapefile_list_reprojected[[shape_name]]
      
      if (simplify_needed) {
        message(paste("Simplifying", shape_name, "for", lifestage))
        shp <- simplify_shape(shp, keep_ratio = 0.02)
      }
      
      shape_list[[length(shape_list) + 1]] <- shp
      
      efh_map <- efh_map %>%
        addPolygons(data = shp,
                    fillColor = "blue",
                    fillOpacity = 0.5,
                    color = "white",
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
  
 ############################### Gag EJ EX tmap #################################
 
 ##gag example dataset using sf- rbind and tmap
 ##EJ shapefiles manual pull 
 
 gag_EJ_shapes <- c(
   "mangrove_est_ER1","mangrove_est_ER2","mangrove_est_ER3",
   "mangrove_near_ER1","mangrove_near_ER2","mangrove_near_ER3","SAV_est_ER1","SAV_est_ER2","SAV_est_ER3",
   "SAV_near_ER1","SAV_near_ER2","SAV_near_ER3"
 ) 
 
 #check shapefile list 
 missing_shapes <- setdiff(gag_EJ_shapes, names(shapefile_list_reprojected))
 
 if(length(missing_shapes) > 0) {
   print("Missing shapefiles:")
   print(missing_shapes)
 } else {
   print("All shapefiles found in shapefile_list.")
 }
 
 #combine into one sf object 
 gag_EJ_sf<- lapply(gag_EJ_shapes, function(name) shapefile_list_reprojected[[name]])%>%
   bind_rows(.id = "Source") ##source shows which shapefile each feature came from
 
 #check combined 
 print(gag_EJ_sf)
 
 #time to plot 
 
 # Set bounding box for Gulf of Mexico
 bbox_gom <- sf::st_bbox(c(xmin = -98, xmax = -80, ymin = 18, ymax = 31), crs = sf::st_crs(gag_EJ_sf))
 
 
 tmap_mode("plot")
 gag_EJ<-tm_shape(gag_EJ_sf,bbox= bbox_gom)+
   tm_basemap("Esri.WorldImagery") + 
   tm_fill(fill= "lightblue", col= "lightblue")+
   tm_title ("Gag Early Juvenile EFH")+
   tm_layout(legend.position = c("left", "bottom"), legend.bg.color ="white", frame=FALSE)
 
 ##see map 
 print(gag_EJ)
  