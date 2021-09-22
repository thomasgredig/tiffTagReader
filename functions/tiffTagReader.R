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
  q = loadBinaryDatafromTIFF(fname)

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

# fname = filename to load
# ________________________________________
# loads all binary data in 32bit chunks
loadBinaryDatafromTIFF <- function(fname) {
  # find the file size and then read in binary format
  nLen = ceiling(file.info(fname)$size/2)
  to.read = file(fname, 'rb')
  q <- readBin(to.read, integer(), n=nLen, endian = "little")
  close(to.read)

  # check for any reading errors
  if (length(which(is.na(q)==TRUE))>0) {
    warning(paste("reading error: NA found in",fname,"."))
    q[is.na(q)] <- 0
  }
  q
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
                     320, 50434, 50435
            ),
            to = c("ImageWidth","ImageLength","BitsPerSample","Compression",
                   "PhotometricInterpretation","Thresholding","CellWidth","CellLength",
                   "StripOffsets","Orientation","SamplesPerPixel",
                   "RowsPerStrip","StripByteCounts",
                   "Software","DateTime","Artist",
                   "ColorMap", "ParkAFMdata", "ParkAFMheader"
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
tiff.isPaletteColorImage <- function(tiffTags) {
  tiff.getValue(tiffTags, 'PhotometricInterpretation') == 3
}


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


# v = vector with 4 bytes
# ________________________________________
# returns a 32bit number
as32Bit <- function(v) { v[4]*2^24+v[3]*2^16+v[2]*2^8+v[1] }


# v = vector with 8 bytes containg sign+11bit exp + 51bit mantissa
# ________________________________________
# byte2double(c(15,89,253,84,251,33,9,64)) returns roughly Pi
# byte2double(c(0,0,0,0,0,0,8,64)) returns 3.0
# returns double number
byte2double <- function(v) {
  if (sum(v)==0) return(0)
  v = rev(v) # reverse vector: little endian
  v2b = v[2] %% 16
  v2a = floor(v[2] / 16)
  dbl.sgn.digit = floor(v[1] / 128)
  dbl.exp = (v[1]-dbl.sgn.digit*128)*16+v2a - 1023
  dbl.mantissa = ((((1*2^4 + v2b)*2^8 + v[3])*2^8 + v[4])*2^8 + v[5])*2^8 + v[6]
  dbl.mantissa * 2^(dbl.exp-36) * sign((dbl.sgn.digit-0.5)*(-2))
}


# afm.params = 580 byte vector with header information
# ________________________________________
# returns data frame
get.ParkAFM.header <- function(afm.params) {
  data.frame(
    # 0=2d mapped image, 1= line profile image, 2=Spectroscopy image
    imageType = as32Bit(afm.params[1:4]),
    sourceName = intToUtf8(afm.params[5:68]),
    imageMode = intToUtf8(afm.params[69:84]),
    dfLPFStrength = byte2double(afm.params[85:92]),
    bAutoFlatten = as32Bit(afm.params[93:96]),
    bACTrack = as32Bit(afm.params[97:100]),
    nWidth = as32Bit(afm.params[101:104]),
    nHeight = as32Bit(afm.params[105:108]),
    dfAngle = byte2double(afm.params[109:116]),
    bSineScan = as32Bit(afm.params[117:120]),
    dfOverScan = byte2double(afm.params[121:128]),
    bFastScanDir = as32Bit(afm.params[129:132]),
    nSlowScanDir = as32Bit(afm.params[133:136]),
    bXYSwap = as32Bit(afm.params[137:140]),

    dfXScanSizeum = byte2double(afm.params[141:148]),
    dfYScanSizeum = byte2double(afm.params[149:156]),
    dfXOffsetum = byte2double(afm.params[157:164]),
    dfYOffsetum = byte2double(afm.params[165:172]),
    dfScanRateHz = byte2double(afm.params[173:180]),
    dfSetPoint = byte2double(afm.params[181:188]),
    SetPointUnitW = intToUtf8(afm.params[189:204]),

    dfTipBiasV = byte2double(afm.params[205:212]),
    dfSampleBiasV = byte2double(afm.params[213:220]),
    dfDataGain = byte2double(afm.params[221:228]),
    dfZScale = byte2double(afm.params[229:236]),
    dfZOffset = byte2double(afm.params[237:244]),

    UnitW  = intToUtf8(afm.params[245:260]),

    nDataMin = as32Bit(afm.params[261:264]),
    nDataMax = as32Bit(afm.params[265:268]),
    nDataAvg = as32Bit(afm.params[269:272]),
    nCompression = as32Bit(afm.params[273:276]),
    bLogScale = as32Bit(afm.params[277:280]),
    bSquare = as32Bit(afm.params[281:284]),

    dfZServoGain = byte2double(afm.params[285:292]),
    dfZScannerRange = byte2double(afm.params[293:300]),
    XYVoltageMode  = intToUtf8(afm.params[301:316]),
    ZVoltageMode  = intToUtf8(afm.params[317:332]),
    XYServoMode   = intToUtf8(afm.params[333:348]),

    # Data Type 0=16bitshort, 1= 32bit int, 2= 32bit float
    nDataType = as32Bit(afm.params[349:352]),
    bXPDDRegion = as32Bit(afm.params[353:356]),
    bYPDDRegion = as32Bit(afm.params[357:360]),

    dfNCMAmplitude = byte2double(afm.params[361:368]),
    dfNCMFrequency = byte2double(afm.params[369:376]),
    dfHeadRotationAngle = byte2double(afm.params[377:384]),
    Cantilever  = intToUtf8(afm.params[385:400]),

    # Non Contact Mode Drive %, range= 0-100
    dfNCMDrivePercent = byte2double(afm.params[401:408]),
    dfIntensityFactor = byte2double(afm.params[409:416])
  )
}

# fname = TIFF file name with AFM image
# ________________________________________
# returns data frame with AFM image
read.Park_file <- function(fname) {
  warning("Depracated: use read.ParkImage() instead.")
  # read TIFF tags
  tiffTags = tagReader(fname)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
  params = get.ParkAFM.header(afm.params)

  # check that the file can be displayed
  if (!tiff.isPaletteColorImage(tiffTags)) stop("Not a palette color image.")
  if (!tiff.getValue(tiffTags,'BitsPerSample') ==  8) stop("Not an 8-bit image.")

  stripOffsets = as.numeric(strsplit(tiff.getValue(tiffTags, 'StripOffsets'), ',')[[1]])
  stripLengths = as.numeric(strsplit(tiff.getValue(tiffTags, 'StripByteCounts'), ',')[[1]])
  if (length(stripOffsets) != length(stripLengths)) stop("tag inconsistency, image offsets and length are not the same.")

  # load entire file (again, could be made more efficient)
  q = loadBinaryDatafromTIFF(fname)

  # create height values
  q=c(q,0)  # some accounting issues here, check getStrip start and end point
  df = c()
  for(i in 1:length(stripOffsets)) {
    df = c(df,getStrip(q, stripOffsets[i], stripLengths[i])  )
  }

  # check values a bit
  if ((max(df)>255 | min(df)<0)) stop("Height values not within bounds.")

  # create image
  imWidth = tiff.getValue(tiffTags, 'ImageWidth')
  imHeight = tiff.getValue(tiffTags, 'ImageLength')
  x=rep(1:imWidth,imHeight)
  y=rep(seq(from=imHeight, to=1),each=imWidth)
  d1 = data.frame(
    x,
    y,
    z = df-mean(df)
  )
  d1$x.nm = params$dfXScanSizeum * d1$x / max(d1$x)*1000
  d1$y.nm = params$dfYScanSizeum * d1$y / max(d1$y)*1000
  d1$z.nm = d1$z * exp(params$dfDataGain)*1000  # for "um" (micrometer)

  d1
}


# fname = TIFF file name with AFM image
# ________________________________________
# returns data frame with AFM image
read.ParkImage <- function(fname) {
  # read TIFF tags
  tiffTags = tagReader(fname)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
  params = get.ParkAFM.header(afm.params)

  # check that the file can be displayed
  if (!tiff.isPaletteColorImage(tiffTags)) stop("Not a palette color image.")
  if (!tiff.getValue(tiffTags,'BitsPerSample') ==  8) stop("Not an 3 x 8-bit image.")

  # read data
  dataStart = tiffTags[which(tiffTags$tag==50434),]$value
  dataLen = tiffTags[which(tiffTags$tag==50434),]$count
  # warning(paste("length:",dataLen))
  df = loadBinaryAFMDatafromTIFF(fname, dataStart, dataLen)


  # create image
  imWidth = tiff.getValue(tiffTags, 'ImageWidth')
  imHeight = tiff.getValue(tiffTags, 'ImageLength')
  if (imHeight != imWidth) {
    warning("Image is not square.")
    imHeight=imWidth
  }
  if (length(df) != imHeight*imWidth) {
    imHeight = sqrt(length(df))
    imWidth = imHeight
  }
  x=rep(1:imWidth,imHeight)
  y=rep(seq(from=imHeight, to=1),each=imWidth)
  # warning(paste("width:",imWidth," pixels"))
  d1 = data.frame(
    x,
    y,
    z = df
  )
  d1$x.nm = params$dfXScanSizeum * d1$x / max(d1$x)*1000
  d1$y.nm = params$dfYScanSizeum * d1$y / max(d1$y)*1000
  d1$z.nm = d1$z * exp(params$dfDataGain)

  d1
}

# fname = TIFF file name with AFM image
# ________________________________________
# returns data frame with AFM image parameters
read.ParkAFM.header <-function(tagsTIFF) {
  afm.params = as.numeric(strsplit(tagsTIFF[16,'valueStr'],',')[[1]])
  params = get.ParkAFM.header(afm.params)
}


# x1, y1, z1: raster AFM image
# ________________________________________________
# returns z.flat components after removing a plane
flatten.AFMimage <- function(x1,y1,z1) {
  b = c(sum(x1*z1), sum(y1*z1), sum(z1))
  a = matrix(data = c(sum(x1*x1), sum(x1*y1), sum(x1),
                      sum(x1*y1), sum(y1*y1), sum(y1),
                      sum(x1), sum(y1), length(z1)),
             nrow=3)
  x = solve(a,b)
  x1*x[1] + y1*x[2] + x[3] - z1
}



# fname:      filename including path
# dataStart:  byte position of where data starts
# dataLen:    length of data in bytes
# ________________________________________________
# returns 32-bit integers with data
loadBinaryAFMDatafromTIFF <- function(fname, dataStart, dataLen) {
  if ((dataLen %% 4) != 0) { warning("Data Length not 32-bit multiple.") }
  to.read = file(fname, 'rb')
  q1 <- readBin(to.read, raw(), n=dataStart, endian = "little")
  q <- readBin(to.read, integer(), n=(dataLen/4), endian = "little")
  close(to.read)
  q
}
