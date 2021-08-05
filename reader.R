# Example Code to run read TIFF tags

source('tiffTagReader.R')

# test random file that does not exist
tagReader('deleted.TIFF')

# test random file to give error
dir()[1]
tagReader(dir()[1])

# example file with TIFF file
fname = '008.tiff'
# check that file exists
file.exists(fname)

# read tags of TIFF file
tags = tagReader(fname)

tags[,1:6]
