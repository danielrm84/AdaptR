############################################################
#
#		DATA PREPARATION: SDM
#
# Species:	ARGIOPE bruennichi
# Source:   Krehenwinkel et al (2013)
#	     (krehenwinkel_Pre_Post_1960.csv)
#
# Author:	Daniel Romero Mujalli
#		University of Greifswald
# Last
# Update:   07.01.2022
#
# Comments: This script uses the above data to train an
#		SDM model on the region of interest.
#
#		Depending on the assumptions and objectives
#		of the study, the modelling region could be
#		constrained to a subset of the full
#		distributional range of the species (e.g.,
#		to southern Europe)
#
############################################################
#-----------------------------------------------------------
# STEP 0: PREPARE IN-OUT DIR / FILES AND EXTENT OF INTEREST

# species name
myspecies <- "Argiope_bruennichi"

# working directory
# set to $/AdaptR/Agiope/Rscripts/

# destination folder to store sdm
dest_folder <- "../sdm"
if(!dir.exists(dest_folder))
	dir.create(dest_folder)

# species data files directory
sp_datadir <- "../data/species/"
head(list.files(sp_datadir))
tail(list.files(sp_datadir))

# filename with species records
sp_filename <- "krehenwinkel_Pre_Post_1960.csv"

# extent of interest
my_window <- c(-10,30,35,70)
myextent <- terra::ext(my_window) # Europe

# END OF STEP 0 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 1:	GET SPECIES RECORDS FOR THE REGION OF INTEREST 

# read file
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

# Plot the occurrence records
# first, get the countries political borders
# then, crop data to the extent of interest
# finally, plot records on country map:

# create spatvector from existing file of countries political 
# borders
# using the vect function of terra package
countries <- terra::vect("../countries/world_countries.shp")

countries <- terra::crop(countries, myextent)
records   <- terra::crop(records,   myextent)

terra::plot(countries, col = "grey95"
	     ,xlim = c(my_window[1], my_window[2])
	     ,ylim = c(my_window[3], my_window[4])
	     ,main = paste("Current Distribution", myspecies)
	     )
terra::plot(records, bg = "darkseagreen4", pch = 21, add = TRUE)

# END OF STEP 1 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 2:	SELECT MODELLING REGION

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
selected_polygons <- c(6,23,3,16,28)

# select the modelling region
model_region <- subset(countries_with_records
			    ,countries_with_records$my_id %in%
			     selected_polygons
			    )
# select the occurrences / records for the modelling region
records <- records[model_region, ]

# visual check
terra::plot(model_region, col = "lightgrey")
terra::plot(records, cex = 0.1, col = "darkseagreen4", add = TRUE)

# One can further delimit the modelling region using a buffer
# of a given distance (e.g., 1 geographic degree, 100km, mean
# distance, etc). Not yet implemented here!

# aggregate modelling region into a single polygon:
model_region <- terra::aggregate(model_region)
terra::plot(model_region)


# END OF STEP 2 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 3:	GET RELEVANT PREDICTORS FOR REGION OF INTEREST

# Using sdmpredictors package to access different online
# datasets

pred_datasets <- sdmpredictors::list_datasets(terrestrial = TRUE
                                             ,marine      = FALSE
                                             )
# inspect the data available for download
pred_datasets
names(pred_datasets)
# one can check the provided urls for more information
#pred_datasets[ , 1:4]

# check the sources of available datasets
pred_layers <- sdmpredictors::list_layers(datasets = pred_datasets)
unique(pred_layers$dataset_code)

# example of terrestrial variables dataset 
#(predictors as given by WorldClim)
unique(pred_layers[pred_layers$dataset_code == "WorldClim", ]$name)

# Select one dataset (e.g., WorldClim) and one particular
# set of variables (e.g., altitude and the bioclimatic ones,
# which are in rows 1 to 20)
layers_choice <- unique(pred_layers[pred_layers$dataset_code=="WorldClim"
                                   , c("name", "layer_code")]
                       )
layers_choice <- layers_choice[1:20, ]

# create directory to store / fetch predictors data (map layers)
pred_dir <- paste0(dest_folder,"/sdmpredictors/")
if(!file.exists(pred_dir))
    dir.create(pred_dir)

# load / download selected layers
# We set rasterstack = FALSE, since TRUE gives error when there
# are layers with different extent
layers <- sdmpredictors::load_layers(layers_choice$layer_code
                                    ,rasterstack = FALSE
                                    ,datadir = pred_dir
                                    )
# check how many elements in layers. This should match our choice above
length(layers)
# names of predictor variables 
names(layers)

# Before plotting some layers, we convert layers to SpatRaster class
# from terra package, which is much faster to process
layers <- lapply(layers, terra::rast)

terra::plot(layers[[1]], main = names(layers)[1])
terra::plot(layers[[5]], main = names(layers)[5])

# find out if layers have different extents or resolutions
# Resolutions:
unique(pred_layers[pred_layers$dataset_code == "WorldClim", ]$cellsize_lonlat)

# Extents:
# we apply the ext function of terra package to the converted layers
sapply(layers, terra::ext)

# Once all layers have the same extent and resolution, one can combine them
# into a single multilayer raster object
# using the rast function of terra package
layers <- terra::rast(layers)
layers
terra::plot(layers)

# crop(trim) layers to model region,
# then mask the values using the model region as template
layers_cut <- terra::mask(terra::crop(layers, model_region)
                         ,mask = model_region
                         )
terra::plot(layers_cut)

# names of the environmental layers (predictors)
names(layers_cut)
# plot first predictor
terra::plot(layers_cut[[1]])
# add countries borders
terra::plot(countries, border = "darkgrey", add = TRUE)
# highlight selected modelling region
terra::plot(model_region, add = TRUE)
# add occurrence points
terra::plot(records, pch = 20, cex = 0.1, add = TRUE)
# Make sure that everything overlaps correctly


# save the cut layers to a folder on disk for later use:
outdir_layers_cut <- paste0(dest_folder,"/sdmpredictors/layers_cut_"
                           ,myspecies
                           )
if(!file.exists(outdir_layers_cut))
    dir.create(outdir_layers_cut)
terra::writeRaster(layers_cut
                  ,filename = paste0(outdir_layers_cut, "/layers_cut.tif")
                  )

# Create a dataframe of the species occurrence data gridded to
# the resolution of the raster variables
# i.e., one row per pixel with the values of the predictor variables
# and the presence/absence of species records
head(df)
#?gridRecords from fuzzySim package
records_coord <- df[, c("GPS.E", "GPS.N")]
# note that predictor layers are passed as stack rasobject
gridded_data <- fuzzySim::gridRecords(rst = raster::stack(layers_cut) 
                                     ,pres.coords = records_coord
                                     )
# inpect the data
head(gridded_data)
dim(gridded_data)


# save the modelling dataframe to a *.csv file on disk for future use
write.csv(gridded_data
         ,paste0("dest_folder,"/dat_gridded_", myspecies, ".csv")
         ,row.names = FALSE
         )


# END OF STEP 3 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 4:	MODEL FITTING

# select the species
myspecies <- "Argiope bruennichi"

#-----------------------------------------------------------
# Import data

#1) dataframe gridded data of occurrence records and 
# predictors
filepath <- paste0(dest_folder,"/dat_gridded_", myspecies, ".csv")
dat <- read.csv(filepath)
head(dat)

#2)environmental layers cut and merged into a single layer 
#  object
filepath <- paste0(dest_folder
			,"/sdmpredictors/layers_cut_",myspecies
			,"/layers_cut.tif"
			)
layers_cut <- terra::rast(filepath)
# visual check
terra::plot(layers_cut)

# define a colour palette and colour breaks (intensity), 
# so that all maps of model predictions show a comparable 
# gradient
# HCL stands for Hue Chroma Luminance
mypalette <- hcl.colors( n = 10         # the number of colors
                       , palette = "viridis" # desired palette
                       )
colorbreaks <- seq(from = 0, to = 1, by = 0.1)

# pixel values only at the locations of the presence points
names(dat) # check the name of the coordinates information
# create mask
records <- terra::vect(dat[dat$presence == 1, ], geom = c("x","y")
                                 ,crs = "+proj=longlat")
terra::plot(presence_centroids) # visual check

#-----------------------------------------------------------
# Predictor selection

# check column names in dataframe
names(dat)
# we use pattern matching (function grep ) to select the 
# predictor variables
selection <- grep(pattern = "WC_alt|WC_bio", x = names(dat))
variables <- names(dat)[selection]
# check if variables predictors were properly selected
print(variables) 

# In the case, we want to exclude some variables, one could
# toremove <- names(dat)[grep(pattern = "bio3|bio14|bio15", x = names(dat))]
# we use the setdiff function that excludes from x any match with y
# selection <- setdiff(x = vars, y = toremove)

# Let us check if there are highly correlated values 
# (see the arbitrary threshold) and exclude them from 
# the analysis. This is, if there are two highly 
# correlated values, we keep only one of them for the 
# modelling
# We use the AIC as the criterion to decide which 
# variable to exclude when a high correlation is found
# AIC: akaike information criterion, an estimator of 
# prediction error
variables_sel <- fuzzySim::corSelect(data = dat
                          ,sp.cols = "presence"  
                          ,var.cols = variables  
                          ,cor.thresh = 0.8      
                          ,select = "AIC"        
                          )
variables_sel   # list with results from the correlation 
		    # analysis
variables_sel <- variables_sel$selected.vars
variables_sel


# write selected variables to *.csv file
write.csv(variables_sel, paste0(dest_folder
					 ,"/variables_sel_"
					 ,myspecies,".csv"),
          row.names = FALSE
          )

#-----------------------------------------------------------
# Model training: Maxnet

# WARNING! IT CAN TAKE LONG TIME TO RUN!

# Maxnet works on a matrix or dataframe! 
# f: a formula to determine the features to be used 
# (below we use the deafault)
myformula <- maxnet::maxnet.formula(p = dat$presence,data = dat[,variables_sel])

maxnet_mod <- maxnet::maxnet(p = dat$presence # a vector: 1 (presence), 0 (background)
                            ,data = dat[,variables_sel] # the predictors
                            ,f = myformula
                            )

# compute and map maxnet predictions
# we use the predict function of raster package
maxnet_pred <- raster::predict(object = raster::stack(layers_cut)
                              ,model = maxnet_mod   # model object with predict attribute
                              ,type = "cloglog"     # complementary log-log type of output
                              )

terra::plot(maxnet_pred, col = mypalette, breaks = colorbreaks, main = "MaxNet")
terra::plot(records, pch = ".", add = TRUE)

# save prediction raster maps to a folder on disk:
outdir_preds <- paste0(dest_folder,"/predictions_", myspecies)
if (!file.exists(outdir_preds)) 
    dir.create(outdir_preds)

raster::writeRaster(maxnet_pred
			 ,filename = paste0(outdir_preds
					  	 ,"/maxnet_pred.tif"
						 )
			 )

#-----------------------------------------------------------
#	Save model object / df with predictions on disk

outdir_models <- paste0(dest_folder,"/models_", myspecies)
if (!file.exists(outdir_models)) 
    dir.create(outdir_models)

saveRDS(maxnet_mod, paste0(outdir_models, "/maxnet_mod.rds"))


# Append predictions to the data table
# Note that this time we perform predictions based on 
# the dataframe instead of raster maps
dat$maxnet_pred  <- as.vector(raster::predict(object = maxnet_mod
                                             ,newdata = dat
                                             ,type = "cloglog"
                                             )
                             )

# convert to binary predictions based on a given threshold
threshold <- 0.5
dat$binary_pred <- ifelse(test = dat$maxnet_pred >= threshold, yes = 1, no = 0) 
# visual check
dim(dat)
names(dat)
pred_robj <- raster::rasterFromXYZ(dat[,c("x","y","binary_pred")])
raster::plot(pred_robj)

# save data to disk
write.csv(dat, paste0(dest_folder,"/dat+preds_",myspecies,".csv")
	   ,row.names = FALSE
	   )

#-----------------------------------------------------------
#	Save sdm of the modelling region on disk
# This will be used later on to define the initial distribution
# of argiope for the AdaptR model (see Rscript 04)

# raster object (binary predictions) on disk
raster::writeRaster(x = pred_robj	# raster object
			 ,filename = paste0(dest_folder
					 ,"/argiope_sdm_model_region.asc")
			 ,format = "ascii"
			 ,overwrite = TRUE
			 )


# END OF STEP 4 
#-----------------------------------------------------------

