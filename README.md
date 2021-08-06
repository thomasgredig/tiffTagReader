# tiffTagReader

A bit of a hack to read tags in TIFF image files using R



## Introduction

This reader function was written to read very specific TIFF files, which are uncompressed. The tool allows you to extract TIFF tags easily on multiple files.

Here is a working example:

```{r}
source('tiffTagReader.R')
fname = dir(pattern='tiff$')[1]
tags = tagReader(fname)
tiffTags[,1:6]
```

The included values are *"tag","type","count","value","tagName","typeName","valueStr"*. The *valueStr* includes the data in a string format that is comma separated.


# TIFF image

The file `tiffImage.R` shows an example of how to create an image from the TIFF file.



## Information

Header is "II" or "MM" followed by the number 42 to identify a TIFF file. The code checks whether it is a valid TIFF image file as follows:

```{r}
isTIFF()
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



