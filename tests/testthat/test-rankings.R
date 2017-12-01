context("rankings")

# ungrouped rankings (5 rankings, 4 items)
R <- as.rankings(matrix(c(1, 2, 0, 0,
                          0, 2, 1, 0,
                          0, 0, 1, 2,
                          2, 1, 0, 0,
                          0, 1, 2, 3), ncol = 4, byrow = TRUE))

# grouped rankings (1st 3 from subject 1, next 2 from subject 2)
G <- grouped_rankings(R, c(1, 1, 1, 2, 2))

test_that("].grouped_rankings allows replicates [fake partial rankings]", {
    out <- paste0(
        '                    1                     2                     3 \n',
        '   "2 > 1, 2 > 3 > 4" "1 > 2, 3 > 2, 3 > 4" "1 > 2, 3 > 2, 3 > 4" ')
    expect_output(print(G[c(2, 1, 1)], max = 3), out)
})
