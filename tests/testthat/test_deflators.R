context("Deflators")

# Setup ----------------------------------------------------------------------

data_vec <- c(NA, 204, 219, 240, 258, 272)
deflator <- c(90.0, 91.478, 93.975, 95.389, 97.978, 100.0)

data <- tibble::tibble(
    a = LETTERS[1:6],
    b = c(NA, 204, 219, 240, 258, 272),
    c = c(NA, 100, 100, 100, 100, 100),
    d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0))

# Tests: deflate -------------------------------------------------------------

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

test_that("deflate returns correct data with defaults", {

    correct <- c(NA, 223.0044, 233.0407, 251.6013, 263.3244, 272.0000)
    output <- deflate(data_vec, deflator)
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("deflate returns correct data with a given basepos", {

    correct <- c(NA, 209.5684, 219.0000, 236.4424, 247.4591, 255.6120)
    output <- deflate(data_vec, deflator, basepos = 3)
    expect_equal(output, correct, tolerance = 1e-4)
})

# Tests: get_real ------------------------------------------------------------

test_that("get_real rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_real(NULL, deflator), msg)
    expect_error(get_real(c(NA, NA, NA), deflator), msg)
    expect_error(get_real(c("a", "b", "c"), deflator), msg)
    expect_error(get_real(c(1, 2, 3), deflator), msg)
    expect_error(get_real(c(TRUE, TRUE, FALSE), deflator), msg)
    expect_error(get_real(list(a = c(1,2,3)), deflator), msg)
})

test_that("get_real rejects different length data and deflator", {

    msg <- "The data and deflator are not the same length."
    expect_error(get_real(data[c(-1), ], deflator), msg)
    expect_error(get_real(data, deflator[c(-1)]), msg)
    expect_error(get_real(data[1, ], deflator), msg)
    expect_error(get_real(data, c(deflator, 1)), msg)
})

test_that("get_real rejects invalid baserow values", {

    msg <- "The baserow argument must be numeric."
    expect_error(get_real(data, deflator, baserow = NULL), msg)
    expect_error(get_real(data, deflator, baserow = c(NA, NA, NA)), msg)
    expect_error(get_real(data, deflator, baserow = c("a", "b", "c")), msg)
    expect_error(get_real(data, deflator, baserow = c(TRUE, FALSE)), msg)
    expect_error(get_real(data, deflator, baserow = list(a = c(1,2,3))), msg)

    expect_error(get_real(data, deflator, baserow = c(1,2,3)),
                 "The baserow must be a vector of length 1.")

    msg <- "The baserow is out of range."
    expect_error(get_real(data, deflator, baserow = 0), msg)
    expect_error(get_real(data, deflator, baserow = 7), msg)
})

test_that("get_real returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 223.0044, 233.0407, 251.6013, 263.3244, 272.0000),
        c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(get_real(data, deflator))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("get_real returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(get_real(data, deflator, from = 3))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("get_real returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 223.0044, 233.0407, 251.6013, 263.3244, 272.0000),
        c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        stringsAsFactors = FALSE)

    output <- as.data.frame(get_real(data, deflator, to = 3))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("get_real returns correct data with column names", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(get_real(data, deflator, from = "c", to = "d"))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("get_real returns correct data with a different baserow", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 209.5684, 219.0000, 236.4424, 247.4591, 255.6120),
        c = c(NA, 102.72962, 100.00000, 98.51765, 95.91439, 93.97500),
        d = c(NA, 93.975, 93.975, 93.975, 93.975, 93.975),
        stringsAsFactors = FALSE)

    output <- as.data.frame(get_real(data, deflator, baserow = 3))
    expect_equal(output, correct, tolerance = 1e-4)
})

# Tests: add_real ------------------------------------------------------------

test_that("add_real rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_real(NULL, deflator), msg)
    expect_error(add_real(c(NA, NA, NA), deflator), msg)
    expect_error(add_real(c("a", "b", "c"), deflator), msg)
    expect_error(add_real(c(1, 2, 3), deflator), msg)
    expect_error(add_real(c(TRUE, TRUE, FALSE), deflator), msg)
    expect_error(add_real(list(a = c(1,2,3)), deflator), msg)
})

test_that("add_real rejects different length data and deflator", {

    msg <- "The data and deflator are not the same length."
    expect_error(add_real(data[c(-1), ], deflator), msg)
    expect_error(add_real(data, deflator[c(-1)]), msg)
    expect_error(add_real(data[1, ], deflator), msg)
    expect_error(add_real(data, c(deflator, 1)), msg)
})

test_that("add_real rejects invalid baserow values", {

    msg <- "The baserow argument must be numeric."
    expect_error(add_real(data, deflator, baserow = NULL), msg)
    expect_error(add_real(data, deflator, baserow = c(NA, NA, NA)), msg)
    expect_error(add_real(data, deflator, baserow = c("a", "b", "c")), msg)
    expect_error(add_real(data, deflator, baserow = c(TRUE, FALSE)), msg)
    expect_error(add_real(data, deflator, baserow = list(a = c(1,2,3))), msg)

    expect_error(add_real(data, deflator, baserow = c(1,2,3)),
                 "The baserow must be a vector of length 1.")

    msg <- "The baserow is out of range."
    expect_error(add_real(data, deflator, baserow = 0), msg)
    expect_error(add_real(data, deflator, baserow = 7), msg)
})

test_that("add_real returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 100, 100, 100, 100, 100),
        d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0),
        rt_b = c(NA, 223.0044, 233.0407, 251.6013, 263.3244, 272.0000),
        rt_c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        rt_d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_real(data, deflator))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("add_real returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 100, 100, 100, 100, 100),
        d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0),
        rt_c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        rt_d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_real(data, deflator, from = 3))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("add_real returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 100, 100, 100, 100, 100),
        d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0),
        rt_b = c(NA, 223.0044, 233.0407, 251.6013, 263.3244, 272.0000),
        rt_c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_real(data, deflator, to = 3))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("add_real returns correct data with column names", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 100, 100, 100, 100, 100),
        d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0),
        rt_c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        rt_d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_real(data, deflator, from = "c", to = "d"))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("add_real returns correct data with a different baserow", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 100, 100, 100, 100, 100),
        d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0),
        rt_b = c(NA, 209.5684, 219.0000, 236.4424, 247.4591, 255.6120),
        rt_c = c(NA, 102.72962, 100.00000, 98.51765, 95.91439, 93.97500),
        rt_d = c(NA, 93.975, 93.975, 93.975, 93.975, 93.975),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_real(data, deflator, baserow = 3))
    expect_equal(output, correct, tolerance = 1e-4)
})

test_that("add_real returns correct data with a different prefix", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(NA, 204, 219, 240, 258, 272),
        c = c(NA, 100, 100, 100, 100, 100),
        d = c(NA, 91.478, 93.975, 95.389, 97.978, 100.0),
        real_b = c(NA, 223.0044, 233.0407, 251.6013, 263.3244, 272.0000),
        real_c = c(NA, 109.3159, 106.4113, 104.8339, 102.0637, 100.0000),
        real_d = c(NA, 100, 100, 100, 100, 100),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_real(data, deflator, prefix = "real_"))
    expect_equal(output, correct, tolerance = 1e-4)
})
