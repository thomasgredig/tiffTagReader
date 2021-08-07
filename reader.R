# TIFF image: read tags
# ---------------------
# Author: Thomas Gredig
# Date: 2021-08-04
# ---------------------

# load functions
source('tiffTagReader.R')
fname = dir(pattern='tiff$')[1]
source('_myConfig.R')  # overwrite fname


# test random file that does not exist
tagReader('deleted.TIFF')

# test random file to give error
dir()[1]
tagReader(dir()[1])

# check that file exists
file.exists(fname)

# read tags of TIFF file
tags = tagReader(fname)
tags[,1:6]
