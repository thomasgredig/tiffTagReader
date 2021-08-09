# TIFF image: creates image
# ---------------------
# Author: Thomas Gredig
# Date: 2021-08-04
# ---------------------

library(ggplot2)
source('tiffTagReader.R')

fname = dir(pattern='tiff$')[1]
source('_myConfig.R')  # overwrite fname
print(paste("Analyzing file:",fname))



# read tiff tag
tiffTags = tagReader(fname)
tiffTags[,1:6]
tiff.getValue(tiffTags, "PhotometricInterpretation")

tiffTags[16,'valueStr']

params = read.ParkAFM.header(tiffTags)
t(params)




# display image
d1 = read.Park_file(fname)
d1$z.nm=d1$z.nm - min(d1$z.nm)

ggplot(d1, aes(x.nm ,y.nm, fill = z.nm)) +
  geom_raster() +
  scale_fill_gradient2(low='black', mid='orange', high='white') +
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_equal() +
  theme_bw()

# # display a line
LINE = 105
dLine = subset(d1,y==LINE)
ggplot(dLine, aes(x.nm,z.nm)) +
  geom_line(col='red') +
  theme_bw()

# EXAMPLES:
# ========

#
# # display a line
# LINE = 105
# dLine = subset(d1,y==LINE)
# ggplot(dLine, aes(x.nm,z.nm)) +
#   geom_line(col='red') +
#   theme_bw()
#
# # display image
# ggplot(d1, aes(x.nm ,y.nm, fill = z.nm)) +
#   geom_raster() +
#   scale_fill_gradient2(low='black', mid='orange', high='white') +
#   scale_y_continuous(expand=c(0,0))+
#   scale_x_continuous(expand=c(0,0))+
#   coord_equal() +
#   theme_bw()
