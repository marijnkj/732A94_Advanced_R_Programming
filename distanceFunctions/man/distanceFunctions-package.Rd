\name{distanceFunctions-package}
\alias{distanceFunctions-package}
\alias{distanceFunctions}
\docType{package}
\title{
\packageTitle{distanceFunctions}
}
\description{This package makes two functions available that allow the user to
implement the Euclidean algorithm to find the greatest common divisor between
two integers, and the Dijkstra algorithm to find the shortest path between a
starting node and all other nodes in a graph.
\packageDescription{distanceFunctions}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{distanceFunctions}
\packageIndices{distanceFunctions}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
\packageAuthor{distanceFunctions}

Maintainer: \packageMaintainer{distanceFunctions}
}
\references{
https://en.wikipedia.org/wiki/Euclidean_algorithm
https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS ~~
~~ in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
euclidean(123612, 13892347912)

wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
}
