\name{GGIR-package}
\alias{GGIR-package}
\alias{GGIR}
\docType{package}
\title{
a package to process multi-day raw accelerometer data
}
\description{
GGIR is a tool to process multi-day raw accelerometer data. The package currently
works for the accelerometer brands Genea and Geneactive. Function \link{g.inspectfile}
assessess to which monitor brand the files belongs  and it extracts the file
header; \link{g.calibrate} to investigate calibration error and propose correction
factors; \link{g.getmeta} extracts the signal features and other meta-data;
\link{g.impute} takes that information, identifies unreliable signal sections
(e.g. monitor not worn or signal clips near its extreme) and replaces these
sections by imputed values; and finally  \link{g.analyse} takes the output from
all the functions, runs a basic descriptive analysis and then summarises the output
both per measurement and per day of measurement.
}
\details{
\tabular{ll}{
Package: \tab GGIR\cr
Type: \tab Package\cr
Version: \tab 0.6-17\cr
Date: \tab 2013-8-8\cr
License: \tab LGPL (>= 2.0, < 3)\cr
}
}
\author{
Vincent T van Hees <vincent.van-hees@newcastle.ac.uk>
Zhou Fang co-developed function \link{g.calibrate}
Jing Hua Zhao <jinghua.zhao@mrc-epid.cam.ac.uk> co-developed function \link{g.binread}
}
\references{
van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013) Separating
Movement and Gravity Components in an Acceleration Signal and Implications for the
Assessment of Human Daily Physical Activity. PLoS ONE 8(4): e61691.
doi:10.1371/journal.pone.0061691
}