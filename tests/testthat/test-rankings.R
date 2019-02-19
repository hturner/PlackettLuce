context("rankings")

# ungrouped rankings (5 rankings, 4 items)
R <- as.rankings(matrix(c(1, 2, 0, 0,
                          0, 2, 1, 0,
                          0, 0, 1, 2,
                          2, 1, 0, 0,
                          0, 1, 2, 3), ncol = 4, byrow = TRUE))

# grouped rankings (1st 3 from group 1, next 2 from group 2)
G <- grouped_rankings(R, c(1, 1, 1, 2, 2))

test_that("].grouped_rankings allows replicates [fake partial rankings]", {
    out <- paste0(
        '                    1                     2                     3 \n',
        '   "2 > 1, 2 > 3 > 4" "1 > 2, 3 > 2, 3 > 4" "1 > 2, 3 > 2, 3 > 4" ')
    expect_output(print(G[c(2, 1, 1)], max = 3), out)
})

# conversion of paircomp
if (require(psychotools)){
    pc <- paircomp(rbind(
        c(1,  1,  1), # a > b, a > c, b > c
        c(1,  1, -1), # a > b, a > c, b < c
        c(1, -1, -1), # a > b, a < c, b < c
        c(1,  1,  1)))
    res <- as.grouped_rankings(pc)
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
    res2 <- grouped_rankings(R, rep(1:4, 3))
    expect_equal(res, res2)
}

