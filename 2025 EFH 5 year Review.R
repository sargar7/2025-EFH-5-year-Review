###EFH 5 year review 
##Species habitat maps 
##Adding habitat type shapefiles to read by R 
##Creating habitat map for each species by lifestage

##Step one upload habitat type shapefiles into R 

install.packages("sf")
install.packages("raster")
install.packages("ggmap")

library(sf)
library(raster)
library(ggplot2)
library(ggmap)


#import shapefile 
shapefile_path <-file.choose ()
shapefile <-st_read(shapefile_path)

EM_gulfwide <- st_read("C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat/EM/shapefiles/EM_gulfwide_2025.shp")
summary(EM_gulfwide)

##view geometry type 
st_geometry_type(EM_gulfwide)
st_crs(EM_gulfwide)
st_bbox(EM_gulfwide)

##plot the shapefile 

plot(EM_gulfwide)

ggplot(data= EM_gulfwide) +
  geom_sf(data=EM_gulfwide, size=1.5, color="black", fill="lightblue") +
  ggtitle("EM_gulfwide") + 
  coord_sf()

ggplot(data = EM_gulfwide) +
  geom_sf(size = 1.5, color = "black", fill = "lightblue") +
  ggtitle("EM_gulfwide") + 
  coord_sf()
  



##upload habitat shapefiles by habitat zone and ecoregion 

setwd("C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat")
##list habitat type folders (subfolders in main directory)

habitattype_folders <-list.dirs (path= ".", full.names= TRUE, recursive = FALSE)
print(habitattype_folders)

##initialize empty list to store shapefiles 
shapefile_list <-list()

##create a loop through each habitat type folder

for(habitattype in habitattype_folders) {
  #list of shapefiles (.shp) in the subfolders 
  shapefiles<- list.files(path =habitattype, pattern = "\\.shp$", full.names=TRUE, recursive =TRUE)
}

##are the shapefiles in the folder?

if (length(shapefiles) >0) {
  ##loop through each shp and load it 
  for (shapefile in shapefiles) {
    #load shapefile using sf package 
    shapefile_name <- gsub(".*/(.*)\\.shp", "\\1", shapefile)  # Extract file name without path and extension
    shapefile_list[[shapefile_name]] <- st_read(shapefile)
  }
}

print(names(shapefile_list))

####test from chatgpt
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

##running into issues with WCA files not containing WCA in the name- may need to rename in GIS so that it translates better
##i think i may need to "code" my shapefiles differently/ organize them in a folder by habitat zone then eco region then habitat type? 
##not sure how i would automate this to run the code and pull the correct shapefiles for each species lifestage- currently the code would be just 
#labor intensive as running it by hand in GIS 

##the only pro is that it would be reproduceable once shapefiles were updated? 

##gag example dataset

gag<- read.csv(file.choose())
View(gag)
file_path <- normalizePath("Gag_EX_data_Rcode.csv")
print(file_path)

##convert habitat type, zone and ecoregion to factor type 

gag$Species <- factor(gag$Species)
gag$Lifestage <- factor(gag$Lifestage, levels = c("egg", "larvae", "postlarvae", "earlyjuvenile", "latejuvenile", "adult", "spawning adult"))
gag$HabitatZone <- factor(gag$HabitatZone, levels = c("off", "near", "est"))
gag$HabitatType <- factor(gag$HabitatType, levels = c("EM", "HB", "Man", "oyster", "reef", "sand", "SAV", "shelf", "SB", "WCA"))
gag$Ecoregion <- factor(gag$Ecoregion, levels = 1:5)

