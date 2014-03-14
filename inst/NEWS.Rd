\name{NEWS}
\title{News for Package \pkg{GGIR}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\section{Changes in version 1.0-2 (release date: 14-3-2014)}{
\itemize{
  \item Expanded g.analyse with estimates of time spent in moderate and vigorous
  activity (a construct popular among physical activity researchers)
  \item Re-named a number of variables in the output from g.analyse to be more
  friendly for re-use in stata or sas. The majority of variable names are now 
  shorter and do not include spaces, dots or commas
  }
}
\section{Changes in version 1.0-1 (release date: 29-1-2014)}{
\itemize{
  \item Fixed Linux-Windows sensitivty in g.getmeta. Certain damaged files can only
  be read with mmap.load set to FALSE in package GENEAread. Function g.getmeta in
  GGIR catches this problem and turns mmap.load to FALSE if necessary. This catch
  worked  well under Linux, but not for R in Windows. I have now fixed this
  }
}
\section{Changes in version 1.0-00 (release date: 21-12-2013)}{
\itemize{
	\item Added examples
  \item Expanded documentation for function g.analyse
	\item Fixed bug in extraction of starttime that caused the starttime to be
  truncated to  00:00:00 in a fraction of measurements.
	\item Fixed bug in extract of temperature in function g.calibrate()
  \item Deleted three explorative variables that were only extracted in g.analyse
  if argument doangle was set to TRUE in function g.getmeta. A number of bugs
and the lack of referable journal publications made me decide to remove these
variables while working on them. I intend on re-releasing these variables during
the course of 2014. Please contact me if this causes you problems
}
}
\section{Changes in version 0.6-17 (release date: 8-8-2013)}{
\itemize{
  \item This was the first version released. It was developed and tested
  in recordings from over 15000 individuals with on average one week of accelerometer
  data.  
}
}