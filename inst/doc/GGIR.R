## ---- echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("GGIR", dependencies = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  `mode=c(1,2), f0=c(), f1=c(), selectdaysfile = c(), configfile=c(), overwrite = FALSE, strategy = 1, maxdur = 7, do.cal = TRUE,  hrs.del.start = 0, hrs.del.end = 0, loglocation = c(), , acc.metric = "ENMO", storefolderstructure = FALSE, windowsizes = c(5,900,3600), minloadcrit = 72, desiredtz = "Europe/London", chunksize = 1, do.enmo = TRUE, do.lfenmo = FALSE, do.en = FALSE,  do.bfen = FALSE, do.hfen = FALSE, do.hfenplus = FALSE, do.mad = FALSE, do.anglex = FALSE, do.angley = FALSE, do.anglez = FALSE, do.roll_med_acc_x=FALSE, do.roll_med_acc_y=FALSE, do.roll_med_acc_z=FALSE, do.dev_roll_med_acc_x=FALSE, do.dev_roll_med_acc_y=FALSE, do.dev_roll_med_acc_z=FALSE, do.enmoa = FALSE, dynrange = c(), printsummary = FALSE, includedaycrit = 16, M5L5res = 10, winhr = 5, qwindow = c(0,24), qlevels = c(), ilevels = c(), mvpathreshold = 100, boutcriter = 0.8, ndayswindow = 7, idloc = 1, do.imp = TRUE, anglethreshold = 5, timethreshold = 5, ignorenonwear = TRUE, colid=1, coln1=2, nnights=7, outliers.only=FALSE, excludefirstlast=FALSE, excludefirstlast.part5=FALSE, criterror=3, includenightcrit=16, relyonguider=FALSE, sleeplogidnum=TRUE, def.noc.sleep=c(), do.visual=FALSE, viewingwindow = 1, dofirstpage = TRUE, visualreport = FALSE, print.filename = FALSE, backup.cal.coef = c(), bout.metric = 1, closedbout = FALSE, IVIS_windowsize_minutes=60, IVIS_epochsize_seconds=30, constrain2range = TRUE, do.part3.pdf = TRUE, boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8, threshold.lig = 40, threshold.mod = 100,   threshold.vig = 400, timewindow = c("MM","WW"), boutdur.mvpa = c(1,5,10), boutdur.in = c(10,20,30), boutdur.lig = c(1,5,10), save_ms5rawlevels = FALSE,  mvpadur = c(1,5,10), epochvalues2csv = FALSE, bout.metric = 1, window.summary.size = 10, dayborder = 0, iglevels = c()`

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  g.shell.GGIR(datadir="C:/mystudy/mydata",
#               outputdir="D:/myresults")

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  g.shell.GGIR(
#               mode=c(1,2,3,4,5),
#               datadir="C:/mystudy/mydata",
#               outputdir="D:/myresults",
#               do.report=c(2,4,5),
#               #=====================
#               # Part 2
#               #=====================
#               strategy = 1,
#               hrs.del.start = 0,          hrs.del.end = 0,
#               maxdur = 9,                 includedaycrit = 16,
#               qwindow=c(0,24),
#               mvpathreshold =c(100),
#               bout.metric = 4,
#               excludefirstlast = FALSE,
#               includenightcrit = 16,
#               #=====================
#               # Part 3 + 4
#               #=====================
#               def.noc.sleep = 1,
#               outliers.only = TRUE,
#               criterror = 4,
#               do.visual = TRUE,
#               #=====================
#               # Part 5
#               #=====================
#               threshold.lig = c(30), threshold.mod = c(100),  threshold.vig = c(400),
#               boutcriter = 0.8,      boutcriter.in = 0.9,     boutcriter.lig = 0.8,
#               boutcriter.mvpa = 0.8, boutdur.in = c(1,10,30), boutdur.lig = c(1,10),
#               boutdur.mvpa = c(1),
#               includedaycrit.part5 = 2/3,
#               #=====================
#               # Visual report
#               #=====================
#               timewindow = c("WW"),
#               visualreport=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  g.shell.GGIR(datadir="C:/mystudy/mydata",
#               outputdir="D:/myresults", configfile = "D:/myconfigfiles/config.csv")

## ----eval=FALSE---------------------------------------------------------------
#  options(echo=TRUE)
#  args = commandArgs(TRUE)
#  if(length(args) > 0) {
#    for (i in 1:length(args)) {
#      eval(parse(text = args[[i]]))
#    }
#  }
#  g.shell.GGIR(f0=f0,f1=f1,...)

## ---- out.width = "700px",echo=FALSE------------------------------------------
knitr::include_graphics("reportexample.jpg")

## ---- out.width = "700px",echo=FALSE------------------------------------------
knitr::include_graphics("example_dovisual.jpg")

## ---- out.width = "400px",echo=FALSE------------------------------------------
knitr::include_graphics("nonwearimage.jpg")

## ---- echo=FALSE, out.width = "60%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

