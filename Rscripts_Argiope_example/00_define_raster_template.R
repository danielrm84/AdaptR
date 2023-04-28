############################################################
#
#		RASTER TEMPLATE FOR EXTENT AND 
'		SPATIAL RESOLUTION
#
# Species:	ARGIOPE bruennichi
# Target:   minimum annual temperature (Tmin)
# Source:   "../input/argiope_habitat_template.asc"
############################################################

# create raster object template of desired extent and
# spatial resolution

myfile <- "../input/argiope_habitat_template.asc"
template <- raster::raster(myfile)

myextent <- raster::extent(template)
myresolution <- raster::res(template)
mycrs <- "+proj=longlat +datum=WGS84 +no_defs"

template <- raster::raster(ext = myextent
				  ,crs = mycrs
				  ,resolution = myresolution
				  ,vals = 1
				  )

# write file
fdir <- "../data/template/"
if(!dir.exists(fdir))
	dir.create(fdir)

# as tif, so it keeps crs info
raster::writeRaster(x = template
			 ,filename = paste0(fdir, "template.tif")
			 ,overwrite = TRUE
			 )
