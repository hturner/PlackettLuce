test_that("can handle .soc files", {
    netflix <- read.soc(system.file("extdata", "netflix.soc",
                                    package = "PlackettLuce"))
    expect_snapshot_value(netflix, style = "json2")
    netflix_rankings <- as.aggregated_rankings(netflix)
    expect_snapshot_value(netflix_rankings, style = "json2")
})

test_that("can handle .soi files", {
    cities <- read.soi(system.file("extdata", "cities.soi",
                                   package = "PlackettLuce"))
    expect_snapshot_value(cities, style = "json2")
    cities_rankings <- as.aggregated_rankings(cities)
    expect_snapshot_value(cities_rankings, style = "json2")
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
    expect_error(read.soc(""),
                 "file` is an empty string: Please supply a valid file path")
    expect_error(read.soc("my.soc"),
                 "cannot open file 'my.soc': No such file or directory")
    expect_error(read.soc("https://www.w3.org/my.soc"))
})

test_that("corrupt Preflib file detected", {
    # Missing an alternative name
    expect_warning(courses <- read.toi(system.file("extdata",
                                                   "education_courses.toi",
                                                   package = "PlackettLuce")),
                   "Corrupt file. Items with no name:\n12")
    expect_error(as.aggregated_rankings(courses),
                 "Coded items with no name:\n12")
})

test_that("okay to skip unused item names and put in any order", {
    # Item 9 does not appear in rankings
    courses <- read.soi(system.file("extdata",
                                    "education_courses.soi",
                                    package = "PlackettLuce"))
    # Same data, but do not give a name for item 9 and define other items in
    # alphabetical vs numeric order
    courses2 <- read.soi(system.file("extdata",
                                     "education_courses2.soi",
                                     package = "PlackettLuce"))
    expect_equal(as.aggregated_rankings(courses),
                 as.aggregated_rankings(courses2))
})
