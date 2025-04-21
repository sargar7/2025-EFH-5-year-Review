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
install.packages("maptiles")
install.packages("ggspatial")
install.packages("prettymapr")
install.packages("rosm")


library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(tidyr)
library(dplyr)
library(stringr)
library(tmap)
library(maptiles)
library(ggspatial)
library(rosm)
library(prettymapr)


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

##shapefiles contain different CRS (coordinate reference system), which can cause alignment issues when being overlayed on a map. 

#reproject shapefiles to WGS84 (ESPG:4326)->> World Geodetic System 1984. It is a global geodetic reference system used for GPS (Global Positioning System) and other navigation and mapping applications. 

shapefile_list_reprojected <- lapply(shapefile_list, function(shp) {st_transform(shp, crs = 4326)})

# Check the CRS of the reprojected shapefiles
st_crs(shapefile_list_reprojected[["mangrove_est_ER1"]])

###########################################################################################################

##gag example dataset
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

Gag_clean <- Gag_long %>%
  mutate(
    HabitatType = str_to_title(HabitatType), #EM, HB,mangrove, oyster, reef, sand, SAV, shelf, SB, WCA
    HabitatZone = str_to_lower(HabitatZone), #est, near, off
    Ecoregion = paste0 ("ER", Ecoregion), # ER1, ER2, ER3, ER4, ER5 
    shapefile_name = paste(HabitatType, HabitatZone, Ecoregion, sep ="_"))
View(Gag_clean)

##early juvenile test map 

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
  tm_fill(fill= "lightblue", col= NA, fill_alpha =1)+
  tm_borders(col=NA)+
  tm_title ("Gag Early Juvenile EFH")+
  tm_layout(legend.position = c("left", "bottom"), legend.bg.color ="white", frame=FALSE)

##see map 
print(gag_EJ)

##cool the bbox function worked to create where i want the map cropped, and the esri is the correct base map
##now to sort out the coloring and legend placement. 
##need to figure out how to automate the shapefiles to pull the right shapefile for each species without hand-coding it. 
##maybe ask John? Verena?