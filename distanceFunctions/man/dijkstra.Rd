\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dijkstra
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    if (!is.data.frame(graph) | !all(c("v1", "v2", "w") \%in\% 
        colnames(graph)) | !is.numeric(init_node) | length(init_node) != 
        1) {
        stop("Check your variables! graph must be a data.frame with columns v1, v2, and w, and init_node a scalar value.")
    }
    else {
        v_dist <- replicate(n = (length(unique(graph$v1))), Inf)
        v_dist[init_node] <- 0
        names(v_dist) <- unique(graph$v1)
        v_points <- unique(graph$v1)
        while (length(v_points) > 0) {
            min_point <- names(v_dist[v_points])[which.min(v_dist[v_points])]
            v_points <- v_points[v_points != min_point]
            for (neighbour_point in graph[graph$v1 == min_point, 
                "v2"]) {
                new_dist <- v_dist[min_point] + graph[graph$v1 == 
                  min_point & graph$v2 == neighbour_point, "w"]
                if (new_dist < v_dist[neighbour_point]) {
                  v_dist[neighbour_point] <- new_dist
                }
            }
        }
        return(v_dist)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
