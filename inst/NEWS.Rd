\name{NEWS}
\title{News for Package \pkg{GGIR}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 1.1-4 (release date: 06-11-2014)}{
\itemize{
  \item Additional bugs fixs related to recognising data format in Actigraph data
  \item Angle variables are now extracted based on 5 second rolling median as
  opposed to 501 sample rolling median
}
}

\section{Changes in version 1.1-3 (release date: 21-10-2014)}{
\itemize{
  \item Package expanded with functions: g.part1, g.part2, and g.shell.GGIR.
  These shell functions should help movement scientists
   to utilize the package without too much prior knowledge about R
  \item Additional bugs fixs related to recognising data format in Actigraph data
  \item Package expanded with axis angle metrics
  \item Package expanded with metric for replicating teLindert2013 counts, see
  \link{g.getmeta}
  \item Package expanded with metric ENMOa, see function tutorial g.getmeta
  }
}
\section{Changes in version 1.0-6 (release date: 1-9-2014)}{
\itemize{
  \item Bug fixed related to recognising date format in csv-file header from
  Actigraph accelerometer brand
  \item Literature reference added to function g.calibrate
  \item function g.getmeta expanded with argument 'chunksize'
  }
}
\section{Changes in version 1.0-4 (release date: 29-4-2014)}{
\itemize{
  \item Implemented functionality for csv-fromat data from GENEActiv and Actigraph.
  It seems to work for the test files I have, more testing may be necessary
  \item Cleaned up some of the NaN and NA output to aim for consistent annotation
  of missing data
  }
}
\section{Changes in version 1.0-3 (release date: 27-3-2014)}{
\itemize{
  \item Fixed bug in modified \link{g.analyse} in version 1.0-2
  }
}
\section{Changes in version 1.0-2 (release date: 14-3-2014)}{
\itemize{
  \item Expanded \link{g.analyse} with estimates of time spent in moderate and
  vigorous activity (a construct popular among physical activity researchers)
  \item Re-named a number of variables in the output from \link{g.analyse} to be
  more friendly for re-use in stata or sas. The majority of variable names are now 
  shorter and do not include spaces, dots or commas
  }
}
\section{Changes in version 1.0-1 (release date: 29-1-2014)}{
\itemize{
  \item Fixed Linux-Windows sensitivty in \link{g.getmeta}. Certain damaged files 
  can only be read with mmap.load set to FALSE in package GENEAread. Function
  \link{g.getmeta} in GGIR catches this problem and turns mmap.load to FALSE if
  necessary. This catch worked  well under Linux, but not for R in Windows.
  I have now fixed this
  }
}
\section{Changes in version 1.0-00 (release date: 21-12-2013)}{
\itemize{
	\item Added examples
  \item Expanded documentation for function \link{g.analyse}
	\item Fixed bug in extraction of starttime that caused the starttime to be
  truncated to  00:00:00 in a fraction of measurements.
	\item Fixed bug in extract of temperature in function \link{g.calibrate}
  \item Deleted three explorative variables that were only extracted in
  \link{g.analyse} if argument doangle was set to TRUE in function \link{g.getmeta}.
  A number of bugs and the lack of referable journal publications made me decide
  to remove these variables while working on them. I intend on re-releasing these
  variables during the course of 2014. Please contact me if this causes you problems
}
}
\section{Changes in version 0.6-17 (release date: 8-8-2013)}{
\itemize{
  \item This was the first version  
}
}