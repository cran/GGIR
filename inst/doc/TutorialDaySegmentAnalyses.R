## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library("GGIR")
#  GGIR(datadir = "/your/data/directory",
#               outputdir = "/your/output/directory",
#               mode = 1:5, # <= run GGIR parts 1 to 5
#               do.report = c(2, 5), # <= generate csv-report for GGIR part 2 and part 5
#               qwindow = c(0, 6, 12, 18, 24),
#               timewindow = "MM")

## ----eval=FALSE---------------------------------------------------------------
#  library("GGIR")
#  GGIR(datadir = "/your/data/directory",
#               outputdir = "/your/output/directory",
#               mode = 1:5, # <= run GGIR parts 1 to 5
#               do.report = c(2, 5), # <= generate csv-report for GGIR part 2 and part 5
#               qwindow = "/path/to/your/activity/log.csv",
#               timewindow = "MM")

## ----echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

