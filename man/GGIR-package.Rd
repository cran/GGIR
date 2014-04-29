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
both per measurement and per day of measurement.\cr
GGIR has primarily been developed for binary data collected with Genea and
Geneactive accelerometers. Additionally, it should work for csv data from
Geneactiv and Actigraph accelerometer. If you want to use this package for a
different data format (e.g. from a different accelerometer brand) then please 
provide me with: the R-code to read the data and example files for testing purposes.
}
\details{
\tabular{ll}{
Package: \tab GGIR\cr
Type: \tab Package\cr
Version: \tab 1.0-4\cr
Date: \tab 2014-4-29\cr
License: \tab LGPL (>= 2.0, < 3)\cr
}
}
\examples{
\dontrun{
  #inspect file:
  I = g.inspectfile(datafile)
  
  #autocalibration:
  C = g.calibrate(datafile)
  
  #get meta-data:
  M = g.getmeta(datafile)
}

data(data.getmeta)
data(data.inspectfile)
data(data.calibrate)


#impute meta-data:
IMP = g.impute(M = data.getmeta, I = data.inspectfile)

#analyse and produce summary:
A = g.analyse(I = data.inspectfile, C = data.calibrate, M = data.getmeta, IMP)

#plot data
g.plot(IMP, M = data.getmeta, I = data.inspectfile, durplot=4)
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