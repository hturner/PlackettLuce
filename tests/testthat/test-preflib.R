context("preflib data formats")

test_that("can handle .soc files", {
    netflix <- read.soc(system.file("extdata", "netflix.soc",
                                    package = "PlackettLuce"))
    expect_known_value(netflix, file = test_path("outputs/soc.rds"))
    netflix_rankings <- as.aggregated_rankings(netflix)
    expect_known_value(netflix_rankings,
                        file = test_path("outputs/soc_rankings.rds"))
})

test_that("can handle .soi files", {
    f1 <- read.soi(system.file("extdata", "f1.soi", package = "PlackettLuce"))
    expect_known_value(f1, file = test_path("outputs/soi.rds"))
    f1_rankings <- as.aggregated_rankings(f1)
    expect_known_value(f1_rankings,
                        file = test_path("outputs/soi_rankings.rds"))
})

test_that("can handle .toc files", {
    skaters <- read.toc(system.file("extdata", "skaters.toc",
                                    package = "PlackettLuce"))
    expect_known_value(skaters, file = test_path("outputs/toc.rds"))
    skaters_rankings <- as.aggregated_rankings(skaters)
    expect_known_value(skaters_rankings,
                        file = test_path("outputs/toc_rankings.rds"))
})

test_that("can handle .toi files", {
    qualities <- read.toi(system.file("extdata", "education_qualities.toi",
                                      package = "PlackettLuce"))
    expect_known_value(qualities, file = test_path("outputs/toi.rds"))
    qualities_rankings <- as.aggregated_rankings(qualities)
    expect_known_value(qualities_rankings,
                       file = test_path("outputs/toi_rankings.rds"))
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
