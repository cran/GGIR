\name{g.shell.GGIR}
\alias{g.shell.GGIR}
\title{
function shell for \link{g.part1} and \link{g.part2}
}
\description{
Function calls \link{g.part1} and \link{g.part2}
}
\usage{
g.shell.GGIR(mode=c(1,2),datadir=c(),outputdir=c(),studyname=c(),f0=1,f1=0,
windowsizes = c(5,900,3600), desiredtz = "Europe/London",do.enmo = TRUE,
do.angle = FALSE,do.lfenmo = FALSE,do.en = FALSE,do.bfen = FALSE,
do.hfen=FALSE,do.hfenplus = FALSE,
do.teLindert2013=FALSE,do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,
do.enmoa=FALSE,chunksize=1,do.cal=TRUE,strategy = 1,
hrs.del.start = 0.5,hrs.del.end = 0.5, maxdur = 7,includedaycrit = 16,
L5M5window = c(0,24), M5L5res = 10,winhr = 5,qwindow=c(0,24),
qlevels = c(0.1), ilevels = c(0,10),mvpathreshold = c(100),
boutcriter = 0.8,do.imp=TRUE,idloc=1,lb = 0.2, hb = 15,  n = 4,
use.temp=TRUE,spherecrit=0.3,minloadcrit=72,printsummary=FALSE,ndayswindow=7,
print.filename=FALSE)
}
\arguments{
  \item{mode}{
  Specify whether \link{g.part1} mode = 1, \link{g.part2} mode =2, or both
  mode = c(1,2) need to be run. Default: mode = c(1,2)
  }
  \item{datadir}{
  Directory where the accelerometer files are stored or list of accelerometer
  filenames and directories.
  }
  \item{outputdir}{
  Directory where the output needs to be stored. Note that this function will
  attempt to create folders in this directory and uses those folder to organise
  output
  }
    \item{studyname}{
  If the datadir is a folder then the study will be given the name of the
  data directory. If datadir is a list of filenames then the studyname will be used
  as name for the analysis
  }  
  \item{f0}{
   File index to start with (default = 1). Index refers to the filenames sorted
   in increasing order
  }
  \item{f1}{
   File index to finish with (defaults to number of files available)
  }
  \item{windowsizes}{
   see \link{g.getmeta}
  }
  \item{desiredtz}{
  see \link{g.getmeta}
  }
  \item{chunksize}{
  see \link{g.getmeta}
  }
  \item{do.bfen}{
  if TRUE, calculate metric BFEN with band-pass filter configuration set by
  \code{lb} and \code{hb}, see \link{g.getmeta}
  }
  \item{do.enmo}{
  if TRUE (default), calculate metric ENMO, see \link{g.getmeta}
  }
  \item{do.angle}{
  if TRUE, calculate metric angle using only the y-axis, see \link{g.getmeta}
  }
  \item{do.lfenmo}{
  if TRUE, calculate metric LFENMO with low-pass filter configuration set by
  \code{hb},see \link{g.getmeta}
  }
  \item{do.en}{
  if TRUE, calculate metric EN, see \link{g.getmeta}
  }
  \item{do.hfen}{
  if TRUE, calculate metric HFEN with low-pass filter configuration set by
  \code{hb}, see \link{g.getmeta}
  }
  \item{do.hfenplus}{
  if TRUE, calculate metric HFENplus with band-pass filter configuration set
  by \code{lb} and \code{hb}, see \link{g.getmeta}
  }
      \item{do.teLindert2013}{
  if TRUE, calculate the 5 sec epoch Actiwatch count replication as described 
  by te Lindert et al 2013 in the journal SLEEP (volume 36, issue 5, page 781)
  }
  \item{do.anglex}{
  if TRUE, calculate the angle of the x-axis relative to the horizontal plane
  (degrees) utilizing all three axes, see \link{g.getmeta}
  }
  \item{do.angley}{
  if TRUE, calculate the angle of the y-axis relative to the horizontal plane
  (degrees) utilizing all three axes, see \link{g.getmeta}
  }
  \item{do.anglez}{
  if TRUE, calculate the angle of the z-axis relative to the horizontal plane
  (degrees) utilizing all three axes, see \link{g.getmeta}
  }
    \item{do.enmoa}{if TRUE (default), calculate metric ENMOa which is equal to
  metric ENMO but with the absolute taken from the Euclidean norm minus one.
}
  \item{lb}{
  lower boundary of the frequency filter (in Hertz), see \link{g.getmeta}
  }
  \item{hb}{
  upper boundary of the frequency filter (in Hertz), see \link{g.getmeta}
  }
  \item{n}{
  order of the frequency filter, see \link{g.getmeta}
  }
  \item{use.temp}{
  see \link{g.calibrate} use temperature sensor data if available (Geneactive only)
  }
  \item{spherecrit}{
  see \link{g.calibrate} the minimum required acceleration value (in g) on both 
  sides of 0 g for each axis. Used to judge whether the sphere is sufficiently 
  populated
  }
  \item{minloadcrit}{
  see \link{g.calibrate} the minimum number of hours the code needs to read for the
  autocalibration procedure to be effective (only sensitive to multitudes of 12 hrs, 
  other values will be ceiled). After loading these hours only extra data is loaded 
  if calibration error has not be reduced to under 0.01 g.
  }
   \item{printsummary}{
  see \link{g.calibrate} if TRUE will print a summary when done
  }
  \item{strategy}{
    how to deal with knowledge about study protocol. value = 1 means select data
    based on \code{hrs.del.start}, \code{hrs.del.end}, and \code{maxdur}.
    Value = 2 makes that only the data between the first midnight and the last
    midnight is used for imputation, see \link{g.impute}
  }
  \item{hrs.del.start}{
    how many HOURS after start of experiment did wearing of monitor start?, 
    see \link{g.impute}
  }
  \item{hrs.del.end}{
    how many HOURS before the end of the experiment did wearing of monitor
    definitely end?, see \link{g.impute}
  }
  \item{maxdur}{
    how many DAYS after start of experiment did experiment
    definitely stop? (set to zero if unknown = default), see \link{g.impute}
  }
  \item{includedaycrit}{
    minimum required number of valid hours in day specific analysis
    (NOTE: there is no minimum required number of hours per day in the summary
    of an entire measurement, every available hour is used to make the best possible
    inference on average metric value per week)
  }
  \item{L5M5window}{
    start and end time, in 24 hour clock hours, over which L5M5 needs to be
    calculated. The calculation is done based on the average day
  }
  \item{M5L5res}{
    resoltion of L5 and M5 analysis in minutes (default: 10 minutes)
  }
   \item{winhr}{
    window size in hours of L5 and M5 analysis (dedault = 5 hours)
  }
  \item{qwindow}{
  start and end time, in 24 hour clock hours, over which distribution in metric 
  values need to be extracted. Value = c(0,24) will consider all 24 hours.
  }
  \item{qlevels}{
    array of percentiles for which value needs to be extracted. These need to
    be expressed as a fraction of 1, e.g. c(0.1, 0.5, 0.75). There is no limit
    to the number of percentiles. If left empty then percentiles will not be 
    extracted. Distribution will be derived from short epoch metric data, see
    \link{g.getmeta}.
  }
  \item{ilevels}{
    Levels for acceleration value frequency distribution in mg, e.g. c(0,100,200)
    There is no constriction to the number of levels.
  }
  \item{mvpathreshold}{
    Threshold for MVPA estimation. Threshold needs to be based on metric ENMO.
    This can be a single number or an array of numbers, e.g. c(100,120). In the 
    later case the code will estimate MVPA seperately for each threshold. If 
    this variable is left blank c() then MVPA is not estimated
  }
  \item{boutcriter}{
    The variable boutcriter is a number between 0 and 1 and defines what fraction
    of a bout needs to be above the mvpathreshold
   }
  \item{ndayswindow}{
    If \code{strategy} is set to 3 then this is the size of the window as a number
    of days
  }
  \item{idloc}{
    If value = 1 (default) the code assumes that ID number is stored in the
    obvious header field. If value = 2 the code uses the character string preceding
    the character '_' in the filename as the ID number
  }
     \item{do.cal}{
  Whether to apply auto-calibration or not, see \link{g.calibrate}. Default and
  recommended setting is TRUE
  }
      \item{do.imp}{
  Whether to impute missing values (e.g. suspected of monitor non-wear) or not
  by \link{g.impute}. Default and recommended setting is TRUE
  }
  \item{print.filename}{
    Whether to print the filename before analysing it (default is FALSE).
    Printing the filename can be useful to investigate problems (e.g. to verify that
    which file is being read).
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
  Vincent T van Hees <vincent.van-hees@newcastle.ac.uk>
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