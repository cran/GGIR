\name{NEWS}
\title{News for Package \pkg{GGIR}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

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
  in recording from over 15000 individuals with on average one week of accelerometer
  data.  
}
}