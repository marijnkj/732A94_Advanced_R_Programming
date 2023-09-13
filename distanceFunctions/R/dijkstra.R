dijkstra <-
function(graph, init_node) {
  # Bug from Simon: build in a check that each edge is represented twice with the same weight!
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
    names(v_dist) <- unique(graph$v1)
    
    v_points <- unique(graph$v1) # To use in while loop
    
    while (length(v_points) > 0) {
      # https://stackoverflow.com/questions/9390749/return-index-of-the-smallest-value-in-a-vector
      # https://stackoverflow.com/questions/32403700/name-of-the-minimum-value-in-a-named-vector
      min_point <- names(v_dist[v_points])[which.min(v_dist[v_points])] 
      v_points <- v_points[v_points != min_point] 
      
      for (neighbour_point in graph[graph$v1 == min_point, "v2"]) {
        new_dist <- v_dist[min_point] + graph[graph$v1 == min_point & graph$v2 == neighbour_point, "w"]
        if (new_dist < v_dist[neighbour_point]) {
          v_dist[neighbour_point] <- new_dist
        }
      }
    }
    return(v_dist)
  }
}
