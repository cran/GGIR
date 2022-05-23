## ---- echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library("GGIR")
#  GGIR(datadir = "/your/data/directory",
#               outputdir = "/your/output/directory",
#               mode = 1:2, # <= run GGIR parts 1 and 2
#               do.report = 2, # <= generate csv-report for GGIR part 2
#               qwindow = c(0, 6, 12, 18, 24))

## ----eval=FALSE---------------------------------------------------------------
#  library("GGIR")
#  GGIR(datadir = "/your/data/directory",
#               outputdir = "/your/output/directory",
#               mode = 1:2, # <= run GGIR parts 1 and 2
#               do.report = 2, # <= generate csv-report for GGIR part 2
#               qwindow = "/path/to/your/activity/log.csv")

## ---- echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

