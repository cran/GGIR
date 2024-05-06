## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  ?GGIR

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
#  GGIR(datadir = "C:/mystudy/mydata",
#       outputdir = "D:/myresults",
#       configfile = "D:/myconfigfiles/config.csv")

## ----eval=FALSE---------------------------------------------------------------
#  library(GGIR)
#  GGIR(datadir = "C:/mystudy/mydata",
#       outputdir = "D:/myresults")

## ----eval=FALSE---------------------------------------------------------------
#  options(echo=TRUE)
#  args = commandArgs(TRUE)
#  if(length(args) > 0) {
#    for (i in 1:length(args)) {
#      eval(parse(text = args[[i]]))
#    }
#  }
#  GGIR(f0=f0,f1=f1,...)

