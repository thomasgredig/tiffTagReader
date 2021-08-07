# tiffTagReader

A bit of a hack to read tags in TIFF image files using R, display non-compressed images, and load the Park atomic force microscopy (AFM) parameters. 



## Introduction

This reader function was written to read very specific TIFF files, which are uncompressed. The tool allows you to extract TIFF tags easily on multiple files.

All functions are in [tiffTagReader.R](tiffTagReader.R), which can be loaded as follows:

```{r}
source('tiffTagReader.R')
```

Examples are also available in `tiffImage.R` and `reader.R`.


### Tags

[TIFF files]([TIFF image files](https://en.wikipedia.org/wiki/TIFF)) have one or more image file directories (IFD), which contain [well-defined tags](https://www.loc.gov/preservation/digital/formats/content/tiff_tags.shtml) and special tags. The well-defined tags include 256 (0x100) and 257 (0x101) for ImageWidth and ImageLength. The following example shows you how to read all tags:

```{r}
fname = dir(pattern='tiff$')[1]
tiffTags = tagReader(fname)
tiffTags[,1:6]
```

The included values are *"tag","type","count","value","tagName","typeName","valueStr"*. The *valueStr* includes the data in a string format that is comma separated.


### Validation

Several functions are available to validate a TIFF file, since the header is "II" or "MM" followed by the (version) number 42. This identifies a TIFF file. The binary code of the file can be read into memory with `loadBinaryDatafromTIFF()`. This code checks whether it is a valid TIFF image file as follows:

```{r}
q = loadBinaryDatafromTIFF(fname)
isTIFF(q)
```

For Park AFM images, check that there is a color palette and the data is 8-bit; the code is written with that in mind, both statements should be true:

```{r}
tiff.isPaletteColorImage(q)
tiff.getValue(q,'BitsPerSample') ==  8) 
```

# TIFF image

The file `tiffImage.R` shows an example of how to create an image from the TIFF file; the function `read.Park_file(fname)` will return a dataframe with all the height information.

```{r}
d1 = read.Park_file(fname)
ggplot(d1, aes(x.nm ,y.nm, fill = z.nm)) +
  geom_raster() +
  scale_fill_gradient2(low='black', mid='orange', high='white') +
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  coord_equal() +
  theme_bw()
```




## Bibliography

- [TIFF image files](https://en.wikipedia.org/wiki/TIFF)
- [R TIFF reader: rtiff](https://github.com/cran/rtiff)
- [LibTIFF - library and utilities](http://www.libtiff.org/)
- [TIFF tags](https://www.loc.gov/preservation/digital/formats/content/tiff_tags.shtml)
- [Adobe TIFF v.6.0 documentation](https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf)
- [TIFF colormap](https://www.awaresystems.be/imaging/tiff/tifftags/colormap.html)
- [Park Data Viewer](https://github.com/mdendzik/Park-AFM-data-viewer/blob/master/AFMimage.m)
- [Hex Mode](https://stat.ethz.ch/R-manual/R-devel/library/base/html/hexmode.html)



