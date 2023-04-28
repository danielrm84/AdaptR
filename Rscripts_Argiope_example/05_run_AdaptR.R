############################################################
#
#		RUN ADAPTR MODEL
#
# Species:	ARGIOPE bruennichi
#
# Author:	Daniel Romero Mujalli
#		University of Greifswald
# Last
# Update:   11.01.2022
#
# Comments: This script runs the AdaptR model.
#		Please, refer to the docummentation 
#		?AdaptR::AdaptR
#		for more details on input parameters
#
############################################################

#-----------------------------------------------------------
# STEP 0: DEFINE FILEPATHS, SIMULATION TIME, AND WRITE
#	    TEXT FILES DESCRIBING THE PATH TO EACH INPUT/OUTPUT
#	    VARIABLE

# working directory
# set to $/AdaptR/Agiope/

# store main input / output directory into variable
in_dir  <- file.path(getwd(),"input")
out_dir <- file.path(getwd(),"output")

# create location file
# get initial disttribution as raster object
sp_t0distfile <- file.path(in_dir,"species_inputs/argiope_initial_distribution.asc")
ini_dist <- raster::raster(sp_t0distfile)
fdir <- file.path(in_dir,"species_inputs/species_locations.txt")

# load customFunctions
source("Rscripts/customFunctions.R")
set.locations.func(ini_dist, fdir)

# define number of rows and columns
# Run this after creating the location.txt file
nRows <- dim(ini_dist)[1]
nCols <- dim(ini_dist)[2]

# time range value
# This will determine the number of time steps for the model
yr_min <- 1902
yr_max <- 1910

# filepath to target predictor (tmin)
tmin_dir <- "tmin"
tmin_dir <- file.path(in_dir,tmin_dir)
list.files(tmin_dir)

# filepath to habitat files
habitat_dir <- "habitat"
habitat_dir <- file.path(in_dir,habitat_dir)
list.files(habitat_dir)

# filepath to compact grids
compact_grids_dir <- "compact_grids"
compact_grids_dir <- file.path(in_dir,compact_grids_dir)
if(!dir.exists(compact_grids_dir))
	dir.create(compact_grids_dir)

list.files(compact_grids_dir)

# define time-steps (in years)
yrs <- seq(from = yr_min, to = yr_max, by = 1)
yrs

# tmin lower / max threshold
tmin_low <- -35
tmin_max <- 20
# write filepath to target predictor raster maps
write.table(x = list.files(tmin_dir, pattern = ".asc", full.names = TRUE)
	     ,file = file.path(tmin_dir,"tmin_filenames.txt")
	     ,row.names = FALSE
	     ,col.names = FALSE
	     ,quote = FALSE
	     )

# write filepath to habitat raster maps
write.table(x = list.files(habitat_dir, pattern = ".asc", full.names = TRUE)
	     ,file = file.path(habitat_dir,"habitat_filenames.txt")
	     ,row.names = FALSE
	     ,col.names = FALSE
	     ,quote = FALSE
	     )

# write filepath to compact grids
write.table(x = file.path(compact_grids_dir,paste0("compact_grids_T",yrs))
	     ,file = file.path(compact_grids_dir
				 ,"compact_grids_output_filenames.txt")
	     ,row.names = FALSE
	     ,col.names = FALSE
	     ,quote = FALSE
	     )



# END OF STEP 0 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 1: RUN THE GRID COMPACTOR
compactor_parameter <- file.path(in_dir,"species_inputs/grid_compactor_parameter_file.txt")
raw_env_files <- c(file.path(tmin_dir,"tmin_filenames.txt")
			,file.path(habitat_dir,"habitat_filenames.txt")
			)
compact_out_filenames <- file.path(compact_grids_dir,"compact_grids_output_filenames.txt")

AdaptR::CompactGrids(compactor.parameter.file.name = compactor_parameter
			  ,ncols = nCols
			  ,nrows = nRows
			  ,n.env.vars = 2
			  ,n.time.points = length(yrs)
			  ,raw.env.grids.name.files = raw_env_files
			  ,output.env.name.file = compact_out_filenames
			  )

# END OF STEP 1 
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 2: GENERATE A DISPERSAL KERNEL

dispersal_dir <- file.path(in_dir,"species_inputs")
dispersal_name <- "dispersal_kernel"

AdaptR::Dispersal_Neighbourhood(radius = 5
					 ,type = "neg.power"
					 ,params = c(1,1)
					 ,output.name = dispersal_name
					 ,output.directory = dispersal_dir
					 ,dispersal.plot = TRUE
					 )


# END OF STEP 2
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 3: WRITE FILEPATHS TO COMPACT GRIDS 

# get list of file names (one per year)
compact_fnames <- list.files(compact_grids_dir, pattern = ".mch")
# get a clean name without file extension
compact_fnames <- gsub("\\..*", "", compact_fnames)
compact_fnames

# write compact series filenames into species_inputs directory
write.table(x = compact_fnames
	     ,file = file.path(in_dir,"species_inputs"
				 ,"compact_series_names.txt"
				 )
	     ,row.names = FALSE
	     ,col.names = FALSE
	     ,quote = FALSE
	     )


# END OF STEP 3
#-----------------------------------------------------------
#-----------------------------------------------------------
# STEP 4: RUN THE ADAPTR MODEL 

# THINK WELL WHERE (WHICH PC) TO RUN THE CODE BELOW!

parameter_fpath <- file.path(out_dir,"argiope_test_parameters.txt")
compact_series_fname <- file.path(in_dir,"species_inputs/compact_series_names.txt")
dispersal_file <- file.path(dispersal_dir,paste0(dispersal_name,".dna"))
sp_locations <- file.path(in_dir,"species_inputs/species_locations.txt")
#sp_locations <- file.path(getwd(),"species_inputs/demo_locations_out.txt")
file.exists(sp_locations)

AdaptR::AdaptR(run.name = "argiope_test"
		  ,parameter.file.name = parameter_fpath
		  ,ncols = nCols
		  ,nrows = nRows
		  ,output.folder.path = out_dir
		  ,verbose.outputs = TRUE
		  ,n.time.points = length(yrs)
		  ,n.env.vars = 2
		  ,env.vars.names = c("X", "Y")
		  ,env.grids.folder.path = compact_grids_dir
		  ,env.grids.name.file = compact_series_fname
		  ,species.initial.grid = sp_t0distfile
		  ,minimum.survival.percentage = 5
		  ,resident.population.weighting = 1000
		  ,dispersal.neighbourhood.file = dispersal_file
		  ,species.location.file = sp_locations
		  ,env.lower.thresholds = c(tmin_low,0.99), # lower Tmin and habitat value
		  ,env.upper.thresholds = c(tmin_max,1.01), # higher Tmax and habitat value
		  ,env.low.adaptation = c(TRUE, FALSE)
		  ,env.high.adaptation = c(FALSE, FALSE)
		  ,adapt.limit = c(-40,0)
		  ,heritability = c(0.53,0)
		  ,fitness.cost = c(0.05, 0)
		  ,adapt.threshold.grids = c(FALSE,FALSE)
		  ,phenotypic.sd.grid = c(FALSE, FALSE)
		  ,phenotypic.sd.value = c(1.106, 0)
		  ,plasticity = c(1.106, 0)
		  ) # END ADAPTR CALL

# visual check
par(mfrow = c(1,2))
x <- raster::raster("output/argiope_test_final_occurrence.asc")
y <-raster::raster("input/species_inputs/argiope_initial_distribution.asc")

raster::plot(x)
raster::plot(y)


# END OF STEP 4
#-----------------------------------------------------------

