dijkstra <- function(graph, init_node) {
  # https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
  if (!is.data.frame(graph) | !all(c("v1", "v2", "w") %in% colnames(graph)) | !is.numeric(init_node) | length(init_node) != 1) {
    stop("Check your variables! graph must be a data.frame with columns v1, v2, and w, and init_node a scalar value.")
  }
  else {
    # https://www.rdocumentation.org/packages/Rpdb/versions/2.2/topics/replicate
    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/is.finite.html
    
    # Start with all infinite distances except at the source node
    v_dist <- replicate(n=(length(unique(graph$v1))), Inf)
    v_dist[init_node] <- 0
    
    for (edge_row in rownames(filter(graph, v1 == init_node))) { # https://stackoverflow.com/questions/2370515/how-to-get-row-index-number-in-r
      node_to <- graph[edge_row, "v2"]
      weight <- graph[edge_row, "w"]
      v_dist[node_to] <- weight
    }
    
    
      
    }
    
    for node %in% unique(graph$v1) {
      
    }
  }
}

graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
