R <- matrix(c(1, 2, 0,
              1, 2, 0,
              1, 2, 0,
              1, 2, 0,
              1, 0, 2,
              1, 0, 2,
              2, 0, 1,
              1, 0, 2,
              0, 1, 2,
              0, 2, 1,
              0, 2, 1,
              0, 1, 2), ncol = 3, byrow = TRUE)
colnames(R) <- letters[1:3]

# alternative forms for comparison
dat <- as.data.frame(R)
id <- which(!duplicated(dat))

# as rankings
R <- as.rankings(R)

test_that("aggregate counts duplicate rankings [fake paired comparisons]", {
    A <- aggregate(R)
    mat <- as.matrix(A$ranking)
    expect_equal(mat, as.matrix(R[id]))
    agg <- aggregate(rep(1, nrow(dat)), dat, sum)
    expect_equal(A$freq[order(mat[, "c"])], agg$x)
})

test_that("aggregate counts unique rankings [fake paired comparisons]", {
    A <- aggregate(R[id])
    expect_equal(A$ranking, R[id])
    expect_equal(A$freq, rep(1L, length(id)))
})

test_that("aggregate reaggregates if required [fake paired comparisons]", {
    A <- aggregate(R)
    # drop item A (and reaggregate)
    B <- A[, -1]
    expect_equal(as.matrix(B$ranking), rbind(0, as.matrix(A$ranking)[4:5, -1]))
    expect_equal(B$freq, c(sum(A$freq[1:3]), A$freq[4:5]))
})
