context("Deflators")

# Setup ----------------------------------------------------------------------

data_vec <- c(204, 219, 240, 258, 272)
deflator <- c(91.478, 93.975, 95.389, 97.978, 100.0)

# Tests: deflate -----------------------------------------------------------

test_that("deflate rejects data this not a numeric vector", {

    msg <- "The data is not a numeric vector."
    expect_error(deflate(NULL, deflator), msg)
    expect_error(deflate(c(NA, NA, NA), deflator), msg)
    expect_error(deflate(c("a", "b", "c"), deflator), msg)
    expect_error(deflate(c(TRUE, TRUE, FALSE), deflator), msg)
    expect_error(deflate(list(a = c(1,2,3)), deflator), msg)
})

test_that("deflate rejects deflator this not a numeric vector", {

    msg <- "The deflator is not a numeric vector."
    expect_error(deflate(data_vec, NULL), msg)
    expect_error(deflate(data_vec, c(NA, NA, NA)), msg)
    expect_error(deflate(data_vec, c("a", "b", "c")), msg)
    expect_error(deflate(data_vec, c(TRUE, TRUE, FALSE)), msg)
    expect_error(deflate(data_vec, list(a = c(1,2,3))), msg)
})

test_that("deflate rejects different length data and deflator", {

    msg <- "The data and deflator are not the same length."
    expect_error(deflate(data_vec[c(-1)], deflator), msg)
    expect_error(deflate(data_vec, deflator[c(-1)]), msg)
    expect_error(deflate(c(data_vec, 1), deflator), msg)
    expect_error(deflate(data_vec, c(deflator, 1)), msg)
})

test_that("deflate rejects deflator with NAs", {

    msg <- "The deflator contains NAs."
    expect_error(deflate(c(data_vec, 1), c(deflator, NA)), msg)
})

test_that("deflate rejects deflator with zeros", {

    msg <- "The deflator contains zeros."
    expect_error(deflate(c(data_vec, 1), c(deflator, 0)), msg)
})

test_that("deflate rejects invalid basepos", {

    msg <- "The basepos is not numeric."
    expect_error(deflate(data_vec, deflator, basepos = NULL), msg)
    expect_error(deflate(data_vec, deflator, basepos = NA), msg)
    expect_error(deflate(data_vec, deflator, basepos = "a"), msg)
    expect_error(deflate(data_vec, deflator, basepos = FALSE), msg)

    msg <- "The basepos must be a vector of length 1."
    expect_error(deflate(data_vec, deflator, basepos = c(1, 2)), msg)

    msg <- "The basepos is out of range."
    expect_error(deflate(data_vec, deflator, basepos = -1), msg)
    expect_error(deflate(data_vec, deflator, basepos = 0), msg)
    expect_error(deflate(data_vec, deflator, basepos = 8), msg)
})
