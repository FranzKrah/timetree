\name{timetree-package}
\alias{timetree-package}
\docType{package}
\title{
Interface to the TimeTree of Life Webpage
}
\description{An interface to the TimeTree of Life Webpage (www.timetree.org). TimeTree is a public database for information on the evolutionary timescale of life. This package includes functions for searching divergence time for  taxa or all nodes of a phylogeny.
}
\details{
\tabular{ll}{
Package: \tab timetree\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-04-11\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Franz-Sebastian Krah
<f.krah@mailbox.org>
}
\references{
\url{http://www.timetree.org}

Hedges SB., Dudley J. & Kumar S. (2006) \emph{TimeTree: a public knowledge-base of divergence times among organisms.} Bioinformatics 22:2971-2972}
\keyword{ time tree }
\examples{
# timetree(taxa=c("Boletales", "Agaricales"))
# timetree(taxa=c("fungi", "plants"))
}
