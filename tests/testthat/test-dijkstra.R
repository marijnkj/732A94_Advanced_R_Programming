context("dijkstra")

library(distanceFunctions)

test_that("Outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra(wiki_graph, 1), setNames(c(0, 7, 9, 20, 20, 11), c("1", "2", "3", "4", "5", "6")))
  expect_equal(dijkstra(wiki_graph, 3), setNames(c(9, 10, 0, 11, 11, 2), c("1", "2", "3", "4", "5", "6")))
})

test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_wrong_graph <- wiki_graph
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(dijkstra(wiki_wrong_graph, 3))
  wiki_wrong_graph <- wiki_graph[1:2]
  expect_error(dijkstra(wiki_wrong_graph, 3))
  expect_error(dijkstra(wiki_graph, 7))
  expect_error(dijkstra(as.matrix(wiki_graph), 3))  
})

