############################################################
#
#		CUSTOM FUNCTIONS
#
# Species:	ARGIOPE bruennichi
#
# Author:	Daniel Romero Mujalli
#		University of Greifswald
# Last
# Update:   11.01.2022
#
# Comments: This script contains custom functions to help
#		dealing with repetitive tasks when preparing
#		part of the data required by the AdaptR model.
#
#		Currently, some functions functionalities might
#		be biased towards the Argiope example
#
############################################################
#-----------------------------------------------------------
# FUNCTION: CREATE AND WRITE TARGET
create_and_write_target <- function(yr	# current year
					     ,fdir  # data directory path
					     ,fstart# file start pattern
					     ,fend  # file end pattern
					     ,fdest # destination folder
					     ,ext   # desired extent
					     ,ref   # reference template
					     )
{
	# define constant to check tmin value out of range
	#tmin_unlikely <- -100

	message(paste("creating map for year", yr, "..."))

	previous_yr <- yr - 1

	# read tmin data (Dec-Feb)
	dec <- paste0(fdir, fstart, "12_", previous_yr, fend)
	jan <- paste0(fdir, fstart, "1_", yr, fend)
	feb <- paste0(fdir, fstart, "2_", yr, fend)

	r_dec <- raster::raster(dec)
	r_jan <- raster::raster(jan)
	r_feb <- raster::raster(feb)

	mystack <- raster::stack(x = c(r_dec,r_jan,r_feb))

	# crop maps to extent of interest
	mystack <- raster::crop(x = mystack
				     ,y = ext
				     )
	
	# adjust resolution / extent
	raster::crs(mystack) <- raster::crs(ref)
	mystack <- raster::projectRaster(from = mystack
						  ,to = ref
						  )

	# create a new raster map depicting the minimum annual
	# temperature, based on the maps for the winter months
	# Using calc functions in raster package
	r_tmin <- raster::calc(x = mystack, fun = min)

	# equivalent to:
	# x: raster stack object
	# all.equal(min(x), raster::calc(x, min))

	# temperature values are muliplied by 10
	# Transform to realistic temperature range value
	r_tmin <- r_tmin / 10 

	#r_tmin[r_tmin <= tmin_unlikely] <- NA

	# write to *.asc file and store into destination folder
	raster::writeRaster(x = r_tmin	# raster object
				 ,filename = paste0(fdest,fstart, yr,".asc")
				 ,overwrite = TRUE
				 ,NAflag = -3.4e+38
				 ) # Use by AdaptR example, is this needed?
	return ("..Success")					 
}

#-----------------------------------------------------------
# FUNCTION: CREATE AND WRITE HABITAT
create_and_write_habitat <- function(yr	# current year
					     ,sdm   # sdm model projection
					     ,limit # tolerance value limit
					     ,tdir  # target files directory
					     ,fdest # destination folder
					      )
{
	# tracking progress
	message(paste("creating map for year", yr, "..."))

	# read raster target tmin map from target directory (tdir)
	x <- raster::raster(paste0(tdir,list.files(tdir
							,pattern = as.character(yr)
								)
					  )
				 )

	# reclassify raster based on tolerance limit
	x <- raster::reclassify(x
				     ,cbind(-Inf, limit, NA) # (from, to, by)
				     ,right = FALSE
				     )

	# create habitat map by masking values in sdm projection 
	# conditional to target predictor raster map (x)
	habitat <- raster::mask(x = sdm, mask = x)

	# write habitat to file in destination folder
	raster::writeRaster(x = habitat
				 ,filename = paste0(fdest
							 ,"habitat_"
							 ,yr
							 ,".asc"
							 )
				 ,format = "ascii"
				 ,overwrite = TRUE
				 )
 
	return("Success..")

}
#-----------------------------------------------------------
# FUNCTION:           SELECT LOCATIONS TO TRACK
#
# Taken - and modified - from Alex Bush

set.locations.func=function(ini_dist # raster obj of initial dist
				   ,fdir	 # filepath where to store location.txt
				   )
{
  New_locations = readline("Do you want to select locations to track? Y/N: ")
  if( exists("New_locations") & New_locations=="Y" ){
    raster::plot(ini_dist)
    # Crop View
    print("Draw appropriate extent")
    new.ext = raster::drawExtent()
    ini_dist = raster::crop(ini_dist, new.ext)
    raster::plot(ini_dist)
    # Select points
    repeat{
      Q = readline("How many locations are required? (positive integer)")
      Q = as.numeric(Q)
      if(Q > 0){
        print("Select locations using the click tool")
        targets = raster::click(ini_dist, n=Q, id=FALSE, xy=T
				,cell=FALSE, type="p", show=TRUE
				)
      }
      H = readline("Happy? T/F: ")
      if(as.logical(H)){ break }
    }
    
    # Stack as a dataframe in which the first row just says 
    # how many points there are.
    targets = targets[,c("x","y")]
    names(targets) = c("V1","V2")
    heading = data.frame("V1"=c(1,1),"V2"=c(2,2))
    targets = rbind(heading, targets)
    targets[2,] = c("Longitude","Latitude")
    targets[1,] = c("n_locations",nrow(targets)-2)
    # Write out locations file
    write.table(targets, file = fdir,
                sep="\t", col.names=F, row.names = FALSE, quote = FALSE)
    rm(targets)
    
  } else { break }
}

#####################################################################