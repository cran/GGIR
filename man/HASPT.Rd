\name{HASPT}
\alias{HASPT}
\title{
  Heuristic Algorithms estimating SPT window.
}
\description{
  As used in function \link{g.sib.det}. Function is not intended for direct use
  by GGIR user.
}
\usage{
HASPT(angle, sptblocksize = 30, spt_max_gap = 60, ws3 = 5, 
      HASPT.algo="HDCZA", HDCZA_threshold = c(), invalid, 
      HASPT.ignore.invalid = FALSE, activity = NULL)
}
\arguments{
  \item{angle}{
    Vector of epoch level estimates of angle
  }
  \item{sptblocksize}{
    Number to indicate minimum SPT block size (minutes)
  }
  \item{spt_max_gap}{
    Number to indicate maximum gap (minutes) in SPT window blocks.
  }
  \item{ws3}{
    Number representing epoch length in seconds
  }
  \item{HASPT.algo}{
    See \link{GGIR}
  }
  \item{HDCZA_threshold}{
    See \link{GGIR}
  }
  \item{invalid}{
    Integer vector with per epoch an indicator of valid(=0) or invalid(=1) epoch.
  }
  \item{HASPT.ignore.invalid}{
    Boolean to indicate whether invalid time segments should be ignored
  }
   \item{activity}{
    Magnitude of acceleration, only used when HASPT.algo is set to NotWorn.
    Acceleration metric used is specified by argument \code{acc.metric} 
    elsewhere in GGIR.
  }
}
\value{
  List with start and end times of the SPT window and the threshold as used.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}