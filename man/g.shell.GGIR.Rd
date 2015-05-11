\name{g.shell.GGIR}
\alias{g.shell.GGIR}
\title{
Shell function for analysing a accelerometer dataset.
}
\description{
This function is designed to help users operate all steps of the analysis. It helps to generate and structure milestone data, produces user-friendly reports. The function acts as a shell with calls to \link{g.part1} and \link{g.part2}. Please see these specific functions for clarification on optional input arguments.
}
\usage{
g.shell.GGIR(mode=c(1,2),datadir=c(),outputdir=c(),studyname=c(),f0=1,f1=0,
overwrite=FALSE,do.report=c(2),...)
}
\arguments{
  \item{mode}{
  Specify which of the five parts needs to be run, e.g. mode = 1 makes that \link{g.part1}
  is ran. Default setting, mode = c(1,2), makes that both part1 and part2 are ran
  }
  \item{datadir}{
  Directory where the accelerometer files are stored or list, e.g. "C:/mydata" of
  accelerometer filenames and directories, e.g. c("C:/mydata/myfile1.bin",
  "C:/mydata/myfile2.bin").
  }
  \item{outputdir}{
  Directory where the output needs to be stored. Note that this function will
  attempt to create folders in this directory and uses those folder to keep
  output
  }
    \item{studyname}{
  If the datadir is a folder then the study will be given the name of the
  data directory. If datadir is a list of filenames then the studyname as specified
  by this input argument will be used as name for the study
  }  
  \item{f0}{
   File index to start with (default = 1). Index refers to the filenames sorted
   in increasing order
  }
  \item{f1}{
   File index to finish with (defaults to number of files available)
  }
  
  \item{overwrite}{
  Do you want to overwrite analysis for which milestone data exists? If overwrite=FALSE
  then milestone data from a previous analysis will be used if available and
  visual reports will not be created again.
  }
   \item{do.report}{
  For which parts to generate a summary spreadsheet: 2, 4 and/or 5. Default is c(2,4,5).
  A report will be generated based on the available milestone data. When creating 
  milestone data with multiple machines it is adviseble to create the report
  separately at the end of the process.  
  }
  \item{...}{
   Any input argument needed for functions g.part1, g.part2, g.part3, g.part4, or
   g.part5. See respective function documentation for further clarification.
  }  
}
\value{
 The function provides no values, it only ensures that other functions are called
 and that their output is stored.
}
\examples{
\dontrun{
datafile = "C:/myfolder/mydata"
outputdir = "C:/myresults"
g.shell.GGIR(mode=c(1,2),datadir=datadir,outputdir=outputdir)
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
\references{
\itemize{
\item van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013) Separating
Movement and Gravity Components in an Acceleration Signal and Implications for the
Assessment of Human Daily Physical Activity. PLoS ONE 8(4): e61691.
doi:10.1371/journal.pone.0061691
\item van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC, Trenell MI, 
White T, Wareham NJ, Brage S. Auto-calibration of accelerometer data for
free-living physical activity assessment using local gravity and temperature: 
an evaluation on four continents. J Appl Physiol (1985). 2014 Aug 7
}
}