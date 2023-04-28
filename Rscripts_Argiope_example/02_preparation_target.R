############################################################
#
#		DATA PREPARATION: TARGET PREDICTOR
#
# Species:	ARGIOPE bruennichi
# Target:   minimum annual temperature (Tmin)
# Source:   https://envicloud.wsl.ch/#/
#		?prefix=chelsa%2Fchelsa_V1%2Fchelsa_cruts
#
# Author:	Daniel Romero Mujalli
#		University of Greifswald
# Last
# Update:   11.01.2022
#
# Comments: This script prepares the target variable data
#		as required by the AdaptR model.
#		It takes advantage of a function found in
#		customFunctions.R
#
############################################################
#-----------------------------------------------------------
# STEP 0: 
# working directory
# set to $/AdaptR/Agiope/Rscripts/

# Output will be stored into "../input/ folder
if(!dir.exists("../input/"))
	dir.create("../input/")

# tmin files directory
# this contains the minimum temperature data previously
# downloaded from the above website
tmin_filedir <- "../data/tmin/"
head(list.files(tmin_filedir))
tail(list.files(tmin_filedir))

# file names follow the same pattern and differ only in year
filename_start <- "CHELSAcruts_tmin_"
filename_end   <- "_V.1.0.tif"

# name of destination folder
# Prepared data will be stored in the following folder
dest_folder <- "../input/tmin/" 

if(!dir.exists(dest_folder))
	dir.create(dest_folder)

# Only data for December, January and February were
# downloaded. Tmin is expected to occur at those months
# (winter northern hemisphere -> study area)
# Starting at 1902, since December data for 1900 is missing
yrs <- seq(from = 1902, to = 1910, by = 1) # max year 2015

# species name
#myspecies <- "Argiope_bruennichi"

# extent of interest
#myextent <- c(-10,30,35,70) # Europe

# raster template for extent and resolution
template <- raster::raster("../data/template/template.tif")

# END OF STEP 0 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 1:	CREATE AND WRITE RASTER MAPS FOR TARGET 

# we use a custom function to create raster maps and write
# them as *.asc to destination folder, for later use by the 
# AdaptR model
source("customFunctions.R")

for (yr in yrs)
{
	create_and_write_target(yr = yr
			     ,fdir = tmin_filedir
		  	     ,fstart = filename_start
			     ,fend = filename_end
			     ,fdest = dest_folder
			     ,ext = myextent
			     ,ref = template
			     )
}

# quick check
list.files(dest_folder)
terra::plot(terra::rast(paste0(dest_folder
					,list.files(dest_folder)[1]
					)
			     )
		)

# END OF STEP 1 
#-----------------------------------------------------------

