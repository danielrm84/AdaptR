############################################################
#
#		DATA PREPARATION: INITIAL DISTRIBUTION
#
# Species:	ARGIOPE bruennichi
# Source:   sdm raster objects (projected and model region)
#
# Author:	Daniel Romero Mujalli
#		University of Greifswald
# Last
# Update:   11.01.2022
#
# Comments: This script creates the initial distribution
#		of desired extent (e.g., Europe)
#		by masking the region of interest (e.g., Europe)
#		with the projected habitat suitability for the 
#		region covering the initial distributional range.
#		Here, it is assumed that Argiope expanded its 
#		range starting from southern Europe.
#
############################################################
#-----------------------------------------------------------
# STEP 0: READ DATA

# working directory
# set to $/AdaptR/Agiope/Rscript 

# extent of interest
my_window <- c(-10,30,35,70)
myextent <- terra::ext(my_window) # Europe

# sdm projection
sdm_proj_file <- "../sdm/argiope_sdm_proj.asc"
file.exists(sdm_proj_file)
sdm_proj <- terra::rast(sdm_proj_file)

# species data files directory
sp_datadir <- "../data/species/"
head(list.files(sp_datadir))
tail(list.files(sp_datadir))

# filename with species records
sp_filename <- "krehenwinkel_Pre_Post_1960.csv"

# create destination folder
dest_folder <- "../input/species_inputs"
# make sure that the directory is correct
dir.exists(dest_folder)
	

# END OF STEP 0 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 1:	GET INITIAL RANGE BASED ON SDM SPATIAL PROJECTION

# Here we select a region (southern Europe) where to place 
# the species initial distribution. Basically we crop the
# spatial projection (see Rscript 03) to the countries of
# interest.

# read species records
df <- read.csv(paste0(sp_datadir, sp_filename), header = TRUE)
head(df)
# the columns containing data coordinates are
# GPS.E -> longitude
# GPS.N -> latitude

# get the occurrence records as terra vector
# the data is in geographic longitude/latitude crs 
#(crs: coordinate reference system)
records <- terra::vect(df, geom = c("GPS.E" # the x
					     ,"GPS.N" # the y
					     )
			    ,crs = "+proj=longlat"
			    )

# create spatvector from existing file of countries political 
# borders
# using the vect function of terra package
countries <- terra::vect("../countries/world_countries.shp")

# crop to the extent of interest (Europe)
countries <- terra::crop(countries, myextent)
records   <- terra::crop(records,   myextent)

# limit species distributional range to Southern Europe
# by selecting those countries in SE with species records

# from countries, select those with occurrence records
countries_with_records <- countries[records, ]

# Create a unique polygon identifier for 
# "countries_with_records" and add it as labels to the map, 
# to see which polygon(s) we want to select:
countries_with_records$my_id <- 1:length(countries_with_records)

terra::plot(countries_with_records, border = "tan")
terra::plot(records, col = "darkseagreen4", cex = 0.1, add = TRUE)
terra::text(countries_with_records
	     ,labels = countries_with_records$my_id
	     ,col = "red"
	     ,font = 2
	     ,halo = TRUE
	     )

# select the desired polygons as modelling region
selected_polygons <- c(10,6,3,16,28,8)

# select the region for initial distribution
region <- subset(countries_with_records
		    ,countries_with_records$my_id %in%
		     selected_polygons
		    )

# visual check
terra::plot(region, col = "lightgrey")

# get sharp cut (one would need to find the most
# convenient strategy
region <- terra::crop(countries, terra::ext(region))

# aggregate modelling region into a single polygon:
#region <- terra::aggregate(region)
terra::plot(region, col = "lightgrey")

# crop(trim) sdm projections to region (initial dist),
# then mask the values using the model region as template
ini_dist <- terra::mask(terra::crop(sdm_proj, region)
                         ,mask = region
                         )
# visual check of the initial distribution
terra::plot(ini_dist)

# from here on, we work with raster
ini_dist <- raster::raster(ini_dist)

# END OF STEP 1 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 2:	EXTEND TO THE FULL REGION AND MASK 
#		THEN WRITE INITIAL DIST TO FILE

# Remember that all raster layers that will be used for the
# AdaptR model need to have the same extent and resolution

# set all values from projected region to null
sdm_proj <- raster::raster(sdm_proj)
raster::values(sdm_proj) <- ifelse(raster::values(sdm_proj) == 1
					    ,yes = 0
					    ,no = 0
					    )
# visual check
raster::plot(sdm_proj)

# extend sdm modelled region to match desired extent
dummy <- raster::extend(ini_dist, sdm_proj, value = NA)
dummy
# visual check
# whether the initial distributional range meets our
# expectations
raster::plot(dummy)

par(mfrow=c(1,2))
raster::plot(dummy) # before
# mask sdm_proj with values on dummy
ini_dist <- raster::mask(x = sdm_proj
		     ,mask = dummy
		     ,inverse = FALSE
		     ,maskvalue = 1
		     ,updatevalue = 1
		     ,updateNA = FALSE
		     )
raster::plot(ini_dist) # after

# When done, proceed to store the initial distribution
# in the corresponding folder for later use by the
# AdaptR model

# write initial distribution to file
raster::writeRaster(x = ini_dist
			 ,filename = file.path(dest_folder
			    ,"argiope_initial_distribution.asc")
		       ,format = "ascii"
			 ,overwrite = TRUE
			 )

# END OF STEP 2 
#-----------------------------------------------------------
 