## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  read.myacc.csv(rmc.file = "C:/mystudy/mydata/datafile.csv",
#                 rmc.nrow = Inf,
#                 rmc.skip = 0,
#                 rmc.dec = ".",
#                 rmc.firstrow.acc = 2,
#                 rmc.col.acc = 2:4,
#                 rmc.col.temp = 5,
#                 rmc.col.time=1,
#                 rmc.unit.acc = "g",
#                 rmc.unit.temp = "C",
#                 rmc.unit.time = "POSIX",
#                 rmc.format.time = "%d/%m/%Y %H:%M:%OS",
#                 rmc.desiredtz = "Europe/London",
#                 rmc.sf = 100)

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  GGIR(
#               mode=c(1,2,3,4,5),
#               datadir="C:/mystudy/mydata/datafile.csv",
#               outputdir="D:/myresults",
#               do.report=c(2,4,5),
#               #=====================
#               # read.myacc.csv arguments
#               #=====================
#               rmc.nrow = Inf,
#               rmc.dec = ".",
#               rmc.firstrow.acc = 2,
#               rmc.col.acc = 2:4,
#               rmc.col.temp = 5,
#               rmc.col.time=1,
#               rmc.unit.acc = "g",
#               rmc.unit.temp = "C",
#               rmc.unit.time = "POSIX",
#               rmc.format.time = "%d/%m/%Y %H:%M:%OS",
#               rmc.desiredtz = "Europe/London",
#               rmc.sf = 100,
#               rmc.noise = 0.013
#  )

## ----echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"', fig.alt="GGIR logo"----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

