\name{iso8601chartime2POSIX}
\alias{iso8601chartime2POSIX}
\title{
Convert iso8601 timestamps to POSIX timestamp
}
\description{
To avoid ambiguities when sharing and comparing timestamps. All timestamps
are expressed in iso8601 format: https://en.wikipedia.org/wiki/ISO_8601
However, to generate plots in R we need to convert them back to POSIX
}
\usage{
iso8601chartime2POSIX(x,tz)
}
\arguments{
  \item{x}{
Vector of timestamps in iso8601 in character format
}
  \item{tz}{
 Timezone of data collection, e.g. "Europe/London". 
 See List_of_tz_database_time_zones on Wikipedia
 for full list.
}
}
\examples{
x ="2017-05-07T13:00:00+0200"
tz = "Europe/Amsterdam"
x_converted = iso8601chartime2POSIX(x,tz)
}