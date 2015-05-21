\name{timetree.phylo}
\alias{timetree.phylo}
\title{
Lockup divergence time from TimeTree for a phylogeny
}
\description{
Lockup divergence time from TimeTree for the nodes of a given phylogeny}
\usage{
timetree.phylo(phy, node.time=c("expert","mean","median"))
}
\arguments{
  \item{phy}{an object of class \code{phylo}}
  \item{node.time}{a character specifying which time should be used as \code{node.labels}}
}
\value{
an object of class list
\item{age}{a data.frame of mode \code{list} with divergence times for \code{$tip.label}}
\item{phy}{an object of class \code{"phylo"} with new node labels according to \code{node.times}}
}
\author{
Franz-Sebastian Krah
}
\examples{
# data(dikarya)
# a <- timetree.phylo(dikarya, node.time="mean")
# plot(a$phy, show.node.label=TRUE)
}