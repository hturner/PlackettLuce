test_that("can handle .soc files", {
    netflix <- read.soc(system.file("extdata", "netflix.soc",
                                    package = "PlackettLuce"))
    expect_snapshot_value(netflix, style = "json2")
    netflix_rankings <- as.aggregated_rankings(netflix)
    expect_snapshot_value(netflix_rankings, style = "json2")
})

test_that("can handle .soi files", {
    f1 <- read.soi(system.file("extdata", "f1.soi", package = "PlackettLuce"))
    expect_snapshot_value(f1, style = "json2")
    f1_rankings <- as.aggregated_rankings(f1)
    expect_snapshot_value(f1_rankings, style = "json2")
})

test_that("can handle .toc files", {
    skaters <- read.toc(system.file("extdata", "skaters.toc",
                                    package = "PlackettLuce"))
    expect_snapshot_value(skaters, style = "json2")
    skaters_rankings <- as.aggregated_rankings(skaters)
    expect_snapshot_value(skaters_rankings, style = "json2")
})

test_that("can handle .toi files", {
    qualities <- read.toi(system.file("extdata", "education_qualities.toi",
                                      package = "PlackettLuce"))
    expect_snapshot_value(qualities, style = "json2")
    qualities_rankings <- as.aggregated_rankings(qualities)
    expect_snapshot_value(qualities_rankings, style = "json2")
})

test_that("reading non-existant Preflib file gives error", {
    expect_error(read.soc("my.soc"),
                 "cannot open file 'my.soc': No such file or directory")
    expect_error(read.soc("https://www.w3.org/my.soc"))
})

test_that("corrupt Preflib file detected", {
    expect_warning(courses <- read.toi(system.file("extdata",
                                                   "education_courses.toi",
                                                   package = "PlackettLuce")),
                   "Corrupt file. Items with no name:\n13")
    expect_error(as.aggregated_rankings(courses),
                 "Coded items with no name:\n13")
})
