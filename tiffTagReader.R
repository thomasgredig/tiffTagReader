# TIFF tag reader
# ---------------------
# Author: Thomas Gredig
# Date: 2021-08-04
# ---------------------


library(plyr)

# fname = filename with path for TIFF file
# ________________________________________
# mostly reads very specific TIFF files
tagReader <- function(fname) {
  # check if file exists
  if (!file.exists(fname)) stop(paste("file",fname,"not found!"))

  # load entire file
  nLen = ceiling(file.info(fname)$size/2)
  to.read = file(fname, 'rb')
  q <- readBin(to.read, integer(), n=nLen, endian = "little")
  close(to.read)
  if (length(which(is.na(q)==TRUE))>0) {
    warning(paste("reading error: NA found in",fname,"."))
    q[is.na(q)] <- 0
  }

  # check whether it is a TIFF file
  if (!isTIFF(q)) stop("Unrecognized TIFF format.")
  if (isBigEndian(q)) stop("Big Endian encoding not supported yet.")

  # read the first image directory file (IDF)
  A = q[2]

  # read all IDFs until nothing is left
  tiffTags=data.frame()
  while (1==1) {
    newtiffTags = read.IFD(q,A)
    if (nrow(newtiffTags)==0) break;
    tiffTags = rbind(tiffTags, newtiffTags)
    A = nrow(newtiffTags)*12+A+2
  }

  # convert some of the numbers into strings
  tiffTags$tagName = identifyTIFFtags(tiffTags$tag)
  tiffTags$typeName = identifyTIFFtypes(tiffTags$type)

  # for items with the "count">1, read the associated fields
  tiffTags$valueStr = ""

  # read ASCII strings
  mASCII = which(tiffTags$type==2)
  for(m1 in mASCII) {
    tiffTags$valueStr[m1] = readTIFF.ASCII(q, tiffTags$value[m1], tiffTags$count[m1])
  }

  # read Long arrays
  mLong = which(tiffTags$type==4 & tiffTags$count>1)
  for(m1 in mLong) {
    tiffTags$valueStr[m1] = readTIFF.Long(q, tiffTags$value[m1], tiffTags$count[m1])
  }

  # read Short arrays
  mShort = which(tiffTags$type==3 & tiffTags$count>1)
  for(m1 in mShort) {
    tiffTags$valueStr[m1] = readTIFF.ColorMap(q, tiffTags$value[m1], tiffTags$count[m1])
  }

  # read unknown arrays
  mUnkn = which(tiffTags$type==7 & tiffTags$count>1)
  for(m1 in mUnkn) {
    warning(paste("Unknown Tag:",tiffTags$tag[m1],"of length",tiffTags$count[m1]))
    tiffTags$valueStr[m1] = readTIFF.Unknown(q, tiffTags$value[m1], tiffTags$count[m1])
  }

  # return the data frame with all the following columns:
  # "tag"       "type"      "count"     "value"     "tagName"   "typeName"  "valueStr"
  tiffTags
}



# q = file data in 32-bit word chunks
# num = location to read, num should be even
# ________________________________________
# add a function to get the 16bit values
# can return negative number
get16bit <- function(q, num) {
  n = q[floor(num/4)+1]
  if ((num %% 4)==0) {
    res = n %% 65536
  } else {
    res = floor(n / 65536)
    if (res<0) { res = res + 2^16 }
  }
  res
}

# q = file data in 32-bit word chunks
# num = location to read, num should be even
# ________________________________________
# add a function to get the 16bit values
# can return negative number
get32bit <- function(q, num) {
  if ((num %% 4)==0) { n = q[floor(num/4)+1] } else {
    n = get16bit(q,num+2) * 2^16 + get16bit(q,num)
  }
  n
}

# q = file data in 32-bit word chunks
# ________________________________________
# check first 4 bytes of TIFF file, should be "II" and version 42
isLittleEndian <- function(q) { get16bit(q,0)==73*256+73 }
isBigEndian <- function(q) { get16bit(q,0)==77*256+77 }
isTIFF <- function(q) {
  (isLittleEndian(q) | isBigEndian(q)) & get16bit(q,2)==42
}

# q = file data in 32-bit word chunks
# X = location in file, should be even number
# ________________________________________
# reads a directory entry and finds the tag and info
read.DirEntry <- function(q,X) {
  tagID = get16bit(q,X)
  if (tagID<0) { tagID = tagID + 2^16}
  data.frame(
    tag = tagID,
    type = get16bit(q, X+2),
    count = get32bit(q, X+4),
    value = get32bit(q, X+8)
  )
}

# q = file data in 32-bit word chunks
# X = location in file, should be even number
# ________________________________________
# returns data.frame with all entries in one IDF
read.IFD <- function(q,X) {
  numDirEntries = get16bit(q,X)
  dIDF = data.frame()
  if (numDirEntries>0) {
    for (i in 1:numDirEntries) {
      dIDF = rbind(dIDF,read.DirEntry(q,X+2+(i-1)*12))
    }
  }
  dIDF
}

# tagID = tag number
# ________________________________________
# converts the number to a readable string according to the
# conventions https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf
# and https://www.loc.gov/preservation/digital/formats/content/tiff_tags.shtml
identifyTIFFtags <- function(tagID) {
  mapvalues(tagID,
            from = c(256,257,258,259,
                     262,263,264,265,
                     273,274,277,
                     278,279,
                     305,306,315,
                     320
            ),
            to = c("ImageWidth","ImageLength","BitsPerSample","Compression",
                   "PhotometricInterpretation","Thresholding","CellWidth","CellLength",
                   "StripOffsets","Orientation","SamplesPerPixel",
                   "RowsPerStrip","StripByteCounts",
                   "Software","DateTime","Artist",
                   "ColorMap"
            ))
}


# tagsType = type number
# ________________________________________
# converts the number to a readable string according to the
# conventions https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf
# and https://www.loc.gov/preservation/digital/formats/content/tiff_tags.shtml
identifyTIFFtypes <- function(tagsType) {
  mapvalues(tagsType,
            from = 1:12,
            to = c("Byte","ASCII","Short (16-bit)","Long (32-bit)",
                   "Rational","SByte","Undefined","SShort","SLong","SRational",
                   "Float","Double"))
}


# q = file data in 32-bit word chunks
# X = location in file, should be even number
# len = length of 8-bits to be read
# ________________________________________
# returns ASCII string value
readTIFF.ASCII <- function(q,X,len) {
  strASCII = c()
  if ((X %% 2)==1) { X=X-1 }
  for(i in 1:ceiling(len/2)) {
    w2 = get16bit(q,X+(i-1)*2)
    strASCII = c(strASCII,w2 %% 256,floor(w2/256))
  }
  intToUtf8(strASCII[1:len])
}

# q = file data in 32-bit word chunks
# X = location in file, should be even number
# len = length of 8-bits to be read
# ________________________________________
# returns 8-bit string values such as "54,255,0,3"
readTIFF.Unknown <- function(q, X, len) {
  strByte = getStrip(q, X, len)
  paste(strByte, collapse=",")
}

# q = file data in 32-bit word chunks
# X = location in file, should be even number
# len = length of 8-bits to be read
# ________________________________________
# reads type 4, which are 32-bit chunks
readTIFF.Long <- function(q, X, len) {
  str = c()
  for(i in 1:len) {
    str = c(str, get32bit(q,X+(i-1)*4) )
  }
  paste(str, collapse=",")
}



# q = file data in 32-bit word chunks
# X = location in file, should be even number
# len = length of 8-bits to be read
# ________________________________________
# reads type 3, which are 16-bit chunks
readTIFF.ColorMap <- function(q, X, len) {
  cm = c()
  for(i in 1:len) {
    w2 = get16bit(q,X+(i-1)*2)
    cm = c(cm,w2)
  }
  paste(cm, collapse=",")
}

# tiffTags = tag data.frame from tagReader
# tagName = string with tag name, such as "ImageWidth"
# ________________________________________
# returns value for certain tag
tiff.getValue <- function(tiffTags, tagName) {
  val = NA
  tagNo = which(tiffTags$tagName==tagName)
  if (length(tagNo) == 1) {
    if (tiffTags[tagNo,'count']==1) {
      val = tiffTags[tagNo,'value']
    } else {
      val = tiffTags[tagNo,'valueStr']
    }
  }
  val
}

# returns TRUE if TIFF image is a palette color image
tiff.isPaletteColorImage <- function(tiffTags) { tiff.getValue(t, 'PhotometricInterpretation') == 3 }


# q = data
# X = starting position / offset
# len = length in bytes (8bit)
# ________________________________________
# returns a BYTE vector with one image strip
getStrip <- function(q, X,len) {
  A1 = floor(X/4)+1
  A2 = A1 + ceiling(len/4)
  n = q[A1:A2]
  # convert n -> n8, so from 32-bits into 8-bit pieces
  n1 = n %% 2^8
  n2 = n %% 2^16
  n3 = n %% 2^24
  n4 = (n - n3)/2^24
  n3 = (n3 - n2)/2^16
  n2 = (n2 - n1)/2^8
  n4[n4<0] <- n4[n4<0] + 2^8
  n8 = c(rbind(n1,n2,n3,n4))
  B1 = (X+1) %% 4
  if (B1==0) { B1 = 4 }
  B2 = B1 + len -1
  n8[B1:B2]
}
