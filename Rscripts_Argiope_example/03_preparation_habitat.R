############################################################
#
#		DATA PREPARATION: HABITAT MAPS
#
# Species:	ARGIOPE bruennichi
#
# Author:	Daniel Romero Mujalli
#		University of Greifswald
# Last
# Update:   11.01.2022
#
# Comments: This script uses the fitted model to perform
#		spatial projection on the modelling region
#		of interest that will be study with AdaptR.
#		
#		Habitat maps for each year are created using
#		the projected map conditional to the target
#		predictor (see Script 02_), for each time
#		step (i.e., year)
#
############################################################
#-----------------------------------------------------------
# STEP 0: PREPARE IN-OUT DIR / FILES TIME STEP AND EXTENT 
#	    OF INTEREST

# species name
myspecies <- "Argiope bruennichi"

# working directory
# set to $/AdaptR/Agiope/Rscripts/

# Output sdm projection will be stored into 
# "../input/species_inputs folder
if(!dir.exists("../input/"))
	dir.create("../input/")

# name of destination folder
# for habitat maps
dest_folder <- "../input/habitat/" 
if(!dir.exists(dest_folder))
	dir.create(dest_folder)

# target data files directory
target_datadir <- "../input/tmin/"
head(list.files(target_datadir))
tail(list.files(target_datadir))

# define time steps
yrs <- seq(from = 1902, to = 1910, by = 1) # max year 2015

# load model object
# directory where models are stored
outdir_models <- paste0("../sdm/models_",myspecies)
# read models from data using readRDS base function 
# to read info on r objects
maxnet_mod <- readRDS(paste0(outdir_models,"/maxnet_mod.rds"))

# read data with predictions
dat <- read.csv(paste0("../sdm/dat+preds_",myspecies,".csv"), header = TRUE)
head(dat)

# extent of interest
my_window <- c(-10,30,35,70)
myextent <- terra::ext(my_window) # Europe

# define a colour palette and colour breaks (intensity), 
# so that all maps of model predictions show a comparable 
# gradient
# HCL stands for Hue Chroma Luminance
mypalette <- hcl.colors( n = 10         # the number of colors
                       , palette = "viridis" # desired palette
                       )
colorbreaks <- seq(from = 0, to = 1, by = 0.1)

# END OF STEP 0 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 1:	CHECK RESPONSE CURVES 

# before projecting the models, it is recommended to check 
# the response curves to make sure that they make ecological 
# sense
# Please check shared folder on modelling frameworks
# (SDMs from start to finish) to learn more on how to do 
# this. 

# END OF STEP 1 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 2: SPATIAL PROJECTION

### FIRST, IMPORT OCCURRENCE RECORDS, PREDICTORS MAPS, AND 
### BACKGROUND COUNTRY BORDERS
# This will help us creating the maps

# import the cleaned occurrence records
# species data files directory
sp_datadir <- "../data/species/"
# filename with species records
sp_filename <- "krehenwinkel_Pre_Post_1960.csv"

df <- read.csv(paste0(sp_datadir,sp_filename))
head(df)

# convert occurrence records dataframe to spatial vector
records <- terra::vect(df, geom = c("GPS.E" # the x
					     ,"GPS.N" # the y
					     )
			    ,crs = "+proj=longlat"
			    )

# create spatvector from existing file of countries political 
# borders
# using the vect function of terra package
countries <- terra::vect("../countries/world_countries.shp")

# crop data to region of interest (EU)

countries <- terra::crop(countries, myextent)
records   <- terra::crop(records,   myextent)

# visual check
terra::plot(countries, col = "grey95"
	     ,xlim = c(my_window[1], my_window[2])
	     ,ylim = c(my_window[3], my_window[4])
	     ,main = paste("Current Distribution", myspecies)
	     )
terra::plot(records, bg = "darkseagreen4", pch = 21, add = TRUE)

# Import the complete (global coverage) predictor / variable layers
# $: indicate that the pattern is at end of the string
# the character "\\" ensures to match extension files ending with
# the pattern ".tif" (in this case), in case unwanted files with
# similar ending exist (e.g., ".atif" <- excluded)
layers <- terra::rast(list.files(path = "../sdm/sdmpredictors/"
                                ,pattern = "\\.tif$" # find pattern
                                ,full.names = TRUE # names with path
                                )
                     )
# visual check
terra::plot(layers[[1:4]]) # plot the first 4 predictor layers

# correct layer names if necessary
# (e.g., remove "_lonlat" part of the name)
names(layers) <- sub(pattern = "_lonlat"
			  ,replacement = ""
			  ,x = names(layers)
			  )

### SECOND, DEFINE THE PROJECTION EXTENT OF THE REGION OF INTEREST

# select the projection extent: this is, the region where to project
# model predictions
# crop layers (x) to the extent of interest given by (y)
layers_proj <- terra::crop(x = layers, y = myextent)
# visual check
terra::plot(x = layers_proj, col = mypalette)

### THIRD, PREDICT WITH THE MODEL THE EXPECTED DISTRIBUTION
### ON THE REGION OF INTEREST
# maxnet
maxnet_proj <- raster::predict(raster::stack(layers_proj)
                              ,maxnet_mod
                              ,type = "cloglog"
                              )
                     
maxnet_proj <- terra::rast(maxnet_proj)
# FOURTH, MAP THE EXTRAPOLATED PROJECTIONS
# One can add country borders (optional)
# maxnet
terra::plot(maxnet_proj
	     ,col = mypalette, breaks = colorbreaks
           ,main = "Spatial projection"
           )

## BINARY PROJECTION
threshold <- 0.5
binary_proj <- maxnet_proj
terra::values(binary_proj) <- ifelse(terra::values(binary_proj) >= threshold
						,yes = 1, no = 0
						)
visual check
terra::plot(binary_proj
	     ,col = c("lightgrey", "darkgreen")
           ,main = "Spatial projection"
           )

# save projection to file
# define the template suitability map
# based on binary projection
sdm <- binary_proj

# raster object (binary predictions) on disk
raster::writeRaster(x = raster::raster(binary_proj)	# raster object
			 ,filename = paste0("../sdm/"
					 ,"argiope_sdm_proj.asc")
			 ,format = "ascii"
			 ,overwrite = TRUE
			 )


# END OF STEP 2 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 3: CREATE HABITAT MAPS
# conditional to the target predictor


sdm <- raster::raster("../sdm/argiope_sdm_proj.asc")

# using custom functions
source("customFunctions.R")

tolerance_limit <- -35 # taken from Barbara

for (yr in yrs)
{
	create_and_write_habitat(yr = yr
			     ,sdm = sdm
		  	     ,limit = tolerance_limit
			     ,tdir = target_datadir
			     ,fdest = dest_folder
			     )
}

# quick check
list.files(dest_folder)
terra::plot(terra::rast(paste0(dest_folder
					,list.files(dest_folder)[1]
					)
			     )
		)

# END OF STEP 3 
#-----------------------------------------------------------


