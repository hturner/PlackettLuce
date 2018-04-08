context("Ranking networks")

# item 1 always wins; item 4 always loses
X <- matrix(c(1, 2, 3, 4,
              1, 3, 2, 4), nr = 2, byrow = TRUE)

test_that("disconnected network throws a message", {
    expect_message(res <- connectivity(as.rankings(X)),
                  "Network of items is not strongly connected")
})

adj1 <- adjacency(X)
adj2 <- adjacency(X, w = c(1, 0))
adj3 <- adjacency(X, w = c(0, 1))

test_that("adjacency matrix gives weighted counts", {
    expect_identical(adj1, adj2 + adj3)
})
