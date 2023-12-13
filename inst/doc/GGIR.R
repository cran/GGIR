## ----echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
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
#  print(load_params())

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  print(load_params()$params_sleep)

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  print(load_params()$params_sleep[["HASIB.algo"]])

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

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  GGIR(datadir="C:/mystudy/mydata",
#               outputdir="D:/myresults", configfile = "D:/myconfigfiles/config.csv")

## ----eval=FALSE---------------------------------------------------------------
#  options(echo=TRUE)
#  args = commandArgs(TRUE)
#  if(length(args) > 0) {
#    for (i in 1:length(args)) {
#      eval(parse(text = args[[i]]))
#    }
#  }
#  GGIR(f0=f0,f1=f1,...)

## ----out.width = "700px",echo=FALSE-------------------------------------------
knitr::include_graphics("reportexample.jpg")

## ----out.width = "700px",echo=FALSE-------------------------------------------
knitr::include_graphics("example_dovisual.jpg")

## ----out.width = "400px",echo=FALSE-------------------------------------------
knitr::include_graphics("nonwearimage.jpg")

## ----echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

