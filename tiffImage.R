# TIFF image: creates image
# ---------------------
# Author: Thomas Gredig
# Date: 2021-08-04
# ---------------------

library(ggplot2)
source('tiffTagReader.R')

fname = dir(pattern='tiff$')[1]
source('_myConfig.R')  # overwrite fname


# display image
d1 = read.Park_file(fname)
ggplot(d1, aes(x.nm ,y.nm, fill = z.nm)) +
  geom_raster() +
  scale_fill_gradient2(low='black', mid='orange', high='white') +
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_equal() +
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
