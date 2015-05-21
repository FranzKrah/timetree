\name{author.search}
\alias{author.search}
\title{
Lockup publications by author names from TimeTree
}
\description{
Lockup publications by author names from TimeTree}
\usage{
author.search(author)
}
\arguments{
\item{author}{a character specifying the author}
}
\value{
an object of class list
\item{publ.nr}{number of publications found}
\item{publ}{a list of \code{data.frames} with publication names, taxa and times}
}
\author{
Franz-Sebastian Krah
}
\examples{
# author.search(author="Berbee") }