\name{timetree}
\alias{timetree}
\title{
Lockup divergence time from TimeTree
}
\description{
Lockup divergence time from TimeTree}
\usage{
timetree(taxa)
}
\arguments{
  \item{taxa}{a character set specifying two taxa}
}
\value{
an object of class list
\item{divergence}{a data.frame of mode \code{list} with divergence times for \code{taxa}}
\item{references}{a data.frame of mode \code{list} with references}
}
\author{
Franz-Sebastian Krah
}
\examples{
 # timetree(taxa=c("Boletales", "Agaricales"))
 }