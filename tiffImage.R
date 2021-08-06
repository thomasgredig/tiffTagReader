# TIFF image: creates image
# ---------------------
# Author: Thomas Gredig
# Date: 2021-08-04
# ---------------------


library(ggplot2)
source('tiffTagReader.R')

fname = dir(pattern='tiff$')[1]
source('_myConfig.R')  # overwrite fname

# read TIFF tags
t = tagReader(fname)
t[,1:6]

# check that the file can be displayed
if (!tiff.isPaletteColorImage(t)) stop("Not a palette color image.")
if (!tiff.getValue(t,'BitsPerSample') ==  8) stop("Not an 8-bit image.")

# some data values
tiff.getValue(t,'RowsPerStrip')
tiff.getValue(t, 'ImageLength')
tiff.getValue(t,'RowsPerStrip')
StripsPerImage = floor((tiff.getValue(t, 'ImageLength') + tiff.getValue(t,'RowsPerStrip') - 1) / tiff.getValue(t,'RowsPerStrip') )

# find image positions
stripOffsets = as.numeric(strsplit(tiff.getValue(t, 'StripOffsets'), ',')[[1]])
stripLengths = as.numeric(strsplit(tiff.getValue(t, 'StripByteCounts'), ',')[[1]])
if (length(stripOffsets) != length(stripLengths)) stop("tag inconsistency, image offsets and length are not the same.")


# load entire file (again, could be made more efficient)
nLen = ceiling(file.info(fname)$size/2)
to.read = file(fname, 'rb')
q <- readBin(to.read, integer(), n=nLen, endian = "little")
close(to.read)

# create height values
df = c()
for(i in 1:length(stripOffsets)) {
  df = c(df,getStrip(q, stripOffsets[i], stripLengths[i])  )
}

# not sure why there are NAs? bug? error?
which(is.na(df)==TRUE)
df[is.na(df)]<-0

# some values are signed, should be unsigned, so change
# ng = which(df<0)
# df[ng] = df[ng]+256

# check values a bit
if ((max(df)>255 | min(df)<0)) stop("Height values not within bounds.")

# create image
imWidth = tiff.getValue(t, 'ImageWidth')
imHeight = tiff.getValue(t, 'ImageLength')
x=rep(1:imWidth,imHeight)
y=rep(seq(from=imHeight, to=1),each=imWidth)
d1 = data.frame(
  x,
  y,
  z = df-mean(df)
)

# display a line
LINE = 105
dLine = subset(d1,y==LINE)
ggplot(dLine, aes(x,z)) +
  geom_line(col='red') +
  theme_bw()

# display image
ggplot(d1, aes(x,y, fill = z)) +
  geom_raster() +
  scale_fill_gradient2(low='black', mid='orange', high='white') +
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_equal() +
  theme_bw()
