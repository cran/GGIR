## ----echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"', fig.alt="GGIR logo"----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("GGIR", dependencies = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("wadpac/GGIR")

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  GGIR(datadir="C:/mystudy/mydata",
#   outputdir="D:/myresults")

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  GGIR(mode=c(1,2,3,4,5),
#        datadir="C:/mystudy/mydata",
#        outputdir="D:/myresults",
#        do.report=c(2,4,5),
#        #=====================
#        # Part 2
#        #=====================
#        data_masking_strategy = 1,
#        hrs.del.start = 0,          hrs.del.end = 0,
#        maxdur = 9,                 includedaycrit = 16,
#        qwindow=c(0,24),
#        mvpathreshold =c(100),
#        excludefirstlast = FALSE,
#        includenightcrit = 16,
#        #=====================
#        # Part 3 + 4
#        #=====================
#        def.noc.sleep = 1,
#        outliers.only = TRUE,
#        criterror = 4,
#        do.visual = TRUE,
#        #=====================
#        # Part 5
#        #=====================
#        threshold.lig = c(30), threshold.mod = c(100),  threshold.vig = c(400),
#        boutcriter = 0.8,      boutcriter.in = 0.9,     boutcriter.lig = 0.8,
#        boutcriter.mvpa = 0.8, boutdur.in = c(1,10,30), boutdur.lig = c(1,10),
#        boutdur.mvpa = c(1),
#        includedaycrit.part5 = 2/3,
#        #=====================
#        # Visual report
#        #=====================
#        timewindow = c("WW"),
#        visualreport=TRUE)

## ----out.width = "700px",echo=FALSE, fig.alt="Example visualreport"-----------
knitr::include_graphics("reportexample.jpg")

## ----echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"', fig.alt="GGIR logo"----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

