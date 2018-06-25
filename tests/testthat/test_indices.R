context("Indices")

# Setup ----------------------------------------------------------------------

data_vec <- c(200, 250, 300, 350, 400, 450, 500)

data <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(200, 250, 300, 400, NA),
    c = c(400, 300, 200, 100, NA),
    d = c(50, 100, 150, 0, NA))

# Tests: get_index -----------------------------------------------------------

test_that("get_index rejects non-numeric vectors", {

    msg <- "The data is not a numeric vector."
    expect_error(get_index(NULL), msg)
    expect_error(get_index(c(NA, NA, NA)), msg)
    expect_error(get_index(c("a", "b", "c")), msg)
    expect_error(get_index(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_index(list(a = c(1,2,3))), msg)
})

test_that("get_index rejects invalid base", {

    msg <- "The base must be a vector of length 1."
    expect_error(get_index(data_vec, base = NULL), msg)
    expect_error(get_index(data_vec, base = c(1, 2)), msg)

    msg <- "The base for the index must be a positive number above one."
    expect_error(get_index(data_vec, base = NA), msg)
    expect_error(get_index(data_vec, base = "a"), msg)
    expect_error(get_index(data_vec, base = FALSE), msg)
    expect_error(get_index(data_vec, base = 0), msg)
    expect_error(get_index(data_vec, base = -1), msg)
})

test_that("get_index rejects invalid basepos", {

    msg <- "The basepos is not numeric."
    expect_error(get_index(data_vec, basepos = NULL), msg)
    expect_error(get_index(data_vec, basepos = NA), msg)
    expect_error(get_index(data_vec, basepos = "a"), msg)
    expect_error(get_index(data_vec, basepos = FALSE), msg)

    msg <- "The basepos must be a vector of length 1."
    expect_error(get_index(data_vec, basepos = c(1, 2)), msg)

    msg <- "The basepos is out of range."
    expect_error(get_index(data_vec, basepos = -1), msg)
    expect_error(get_index(data_vec, basepos = 0), msg)
    expect_error(get_index(data_vec, basepos = 8), msg)
})

test_that("get_index rejects a NA value at the basepos.", {

    msg <- "The value at the basepos should not be NA."
    expect_error(get_index(c(NA, data_vec), basepos = 1), msg)
})

test_that("get_index rejects a zero value at the basepos.", {

    msg <- "The value at the basepos should not be zero."
    expect_error(get_index(c(0, data_vec), basepos = 1), msg)
})

test_that("get_index rejects invalid baseval.", {

    msg <- "The baseval is not numeric."
    expect_error(get_index(data_vec, baseval = NA), msg)
    expect_error(get_index(data_vec, baseval = "a"), msg)
    expect_error(get_index(data_vec, baseval = FALSE), msg)

    msg <- "The baseval must be a vector of length 1."
    expect_error(get_index(data_vec, baseval = c(1, 2)), msg)

    msg <- "The baseval should not be NA."
    expect_error(get_index(data_vec, baseval = c(NA, 1)[1]), msg)

    msg <- "The baseval should not be zero."
    expect_error(get_index(data_vec, baseval = 0), msg)
})

test_that("get_index returns correct data with defaults", {

    correct <- c(100, 125, 150, 175, 200, 225, 250)
    output <- get_index(data_vec)
    expect_equal(output, correct)
})

test_that("get_index returns correct data with a different base", {

    correct <- c(50.0,  62.5,  75.0,  87.5, 100.0, 112.5, 125.0)
    output <- get_index(data_vec, base = 50)
    expect_equal(output, correct)

    correct <- c(1000,  1250,  1500, 1750, 2000, 2250, 2500)
    output <- get_index(data_vec, base = 1000)
    expect_equal(output, correct)

    correct <- c(4, 5, 6, 7, 8, 9, 10)
    output <- get_index(data_vec, base = 4)
    expect_equal(output, correct)
})

test_that("get_index returns correct data with a different basepos", {

    correct <- c(80, 100, 120, 140, 160, 180, 200)
    output <- get_index(data_vec, basepos = 2)
    expect_equal(output, correct)

    correct <- c(50.0, 62.5, 75.0, 87.5, 100.0, 112.5, 125.0)
    output <- get_index(data_vec, basepos = 5)
    expect_equal(output, correct)

    correct <- c(16, 20, 24, 28, 32, 36, 40)
    output <- get_index(data_vec, basepos = 2, base = 20)
    expect_equal(output, correct)
})

test_that("get_index returns correct data with a given baseval", {

    correct <- c(20, 25, 30, 35, 40, 45, 50)
    output <- get_index(data_vec, baseval = 1000)
    expect_equal(output, correct)

    correct <- c(400, 500, 600, 700, 800, 900, 1000)
    output <- get_index(data_vec, baseval = 50)
    expect_equal(output, correct)

    correct <- c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
    output <- get_index(data_vec, base = 1, baseval = 1000)
    expect_equal(output, correct)
})

# Tests: get_indices -----------------------------------------------------------

test_that("get_indices rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_indices(NULL), msg)
    expect_error(get_indices(c(NA, NA, NA)), msg)
    expect_error(get_indices(c("a", "b", "c")), msg)
    expect_error(get_indices(c(1, 2, 3)), msg)
    expect_error(get_indices(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_indices(list(a = c(1,2,3))), msg)
})

test_that("get_indices rejects invalid baserow values", {

    msg <- "The baserow argument must be numeric."
    expect_error(get_indices(data, baserow = NULL), msg)
    expect_error(get_indices(data, baserow = c(NA, NA, NA)), msg)
    expect_error(get_indices(data, baserow = c("a", "b", "c")), msg)
    expect_error(get_indices(data, baserow = c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_indices(data, baserow = list(a = c(1,2,3))), msg)

    expect_error(get_indices(data, baserow = c(1,2,3)),
                 "The baserow must be a vector of length 1.")

    msg <- "The baserow is out of range."
    expect_error(get_indices(data, baserow = 0), msg)
    expect_error(get_indices(data, baserow = 6), msg)

    expect_error(get_indices(data, baserow = 5),
                 "The baserow should not contain NAs.")

    expect_error(get_indices(data, baserow = 4),
                 "The baserow should not contain zeros.")
})

test_that("get_indices rejects invalid basevals", {

    msg <- "The basevals argument must be numeric."
    expect_error(get_indices(data, basevals = c(NA, NA, NA)), msg)
    expect_error(get_indices(data, basevals = c("a", "b", "c")), msg)
    expect_error(get_indices(data, basevals = c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_indices(data, basevals = list(a = c(1,2,3))), msg)

    msg <- "The number of basevals and target columns are not equal."
    expect_error(get_indices(data, basevals = c(1, 1)), msg)
    expect_error(get_indices(data, basevals = c(1, 1, 1, 1)), msg)

    expect_error(get_indices(data, basevals = c(1, 1, NA)),
                 "The basevals should not contain NAs.")

    expect_error(get_indices(data, basevals = c(0, 1, 1)),
                 "The basevals should not contain zeros.")
})

test_that("get_indices returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(100, 125, 150, 200, NA),
        c = c(100, 75, 50, 25, NA),
        d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data))
    expect_equal(output, correct)
})

test_that("get_indices returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(100, 75, 50, 25, NA),
        d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data, from = 3))
    expect_equal(output, correct)
})

test_that("get_indices returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(100, 125, 150, 200, NA),
        c = c(100, 75, 50, 25, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data, to = 3))
    expect_equal(output, correct)
})

test_that("get_indices returns correct data with column names", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(100, 75, 50, 25, NA),
        d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data, from = "c", to = "d"))
    expect_equal(output, correct)
})

test_that("get_indices returns correct data with a different base", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(4, 5, 6, 8, NA),
        c = c(4, 3, 2, 1, NA),
        d = c(4, 8, 12, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data, base = 4))
    expect_equal(output, correct)
})

test_that("get_indices returns correct data with a different baserow", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(80, 100, 120, 160, NA),
        c = c(400/3, 300/3, 200/3, 100/3, NA),
        d = c(50, 100, 150, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data, baserow = 2))
    expect_equal(output, correct)
})

test_that("get_indices returns correct data with a given baseval", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(200, 150, 100, 50, NA),
        d = c(200, 400, 600, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_indices(data, basevals = c(100, 200, 25)))
    expect_equal(output, correct)
})

# Tests: add_indices -----------------------------------------------------------

test_that("add_indices rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_indices(NULL), msg)
    expect_error(add_indices(c(NA, NA, NA)), msg)
    expect_error(add_indices(c("a", "b", "c")), msg)
    expect_error(add_indices(c(1, 2, 3)), msg)
    expect_error(add_indices(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_indices(list(a = c(1,2,3))), msg)
})

test_that("add_indices rejects invalid baserow values", {

    msg <- "The baserow argument must be numeric."
    expect_error(add_indices(data, baserow = NULL), msg)
    expect_error(add_indices(data, baserow = c(NA, NA, NA)), msg)
    expect_error(add_indices(data, baserow = c("a", "b", "c")), msg)
    expect_error(add_indices(data, baserow = c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_indices(data, baserow = list(a = c(1,2,3))), msg)

    expect_error(add_indices(data, baserow = c(1,2,3)),
                 "The baserow must be a vector of length 1.")

    msg <- "The baserow is out of range."
    expect_error(add_indices(data, baserow = 0), msg)
    expect_error(add_indices(data, baserow = 6), msg)

    expect_error(add_indices(data, baserow = 5),
                 "The baserow should not contain NAs.")

    expect_error(add_indices(data, baserow = 4),
                 "The baserow should not contain zeros.")
})

test_that("add_indices rejects invalid basevals", {

    msg <- "The basevals argument must be numeric."
    expect_error(add_indices(data, basevals = c(NA, NA, NA)), msg)
    expect_error(add_indices(data, basevals = c("a", "b", "c")), msg)
    expect_error(add_indices(data, basevals = c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_indices(data, basevals = list(a = c(1,2,3))), msg)

    msg <- "The number of basevals and target columns are not equal."
    expect_error(add_indices(data, basevals = c(1, 1)), msg)
    expect_error(add_indices(data, basevals = c(1, 1, 1, 1)), msg)

    expect_error(add_indices(data, basevals = c(1, 1, NA)),
                 "The basevals should not contain NAs.")

    expect_error(add_indices(data, basevals = c(0, 1, 1)),
                 "The basevals should not contain zeros.")
})

test_that("add_indices returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_b = c(100, 125, 150, 200, NA),
        ix_c = c(100, 75, 50, 25, NA),
        ix_d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_c = c(100, 75, 50, 25, NA),
        ix_d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, from = 3))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_b = c(100, 125, 150, 200, NA),
        ix_c = c(100, 75, 50, 25, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, to = 3))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with column names", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_c = c(100, 75, 50, 25, NA),
        ix_d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, from = "c", to = "d"))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with a different base", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_b = c(4, 5, 6, 8, NA),
        ix_c = c(4, 3, 2, 1, NA),
        ix_d = c(4, 8, 12, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, base = 4))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with a different baserow", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_b = c(80, 100, 120, 160, NA),
        ix_c = c(400/3, 300/3, 200/3, 100/3, NA),
        ix_d = c(50, 100, 150, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, baserow = 2))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with a given baseval", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        ix_b = c(200, 250, 300, 400, NA),
        ix_c = c(200, 150, 100, 50, NA),
        ix_d = c(200, 400, 600, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, basevals = c(100, 200, 25)))
    expect_equal(output, correct)
})

test_that("add_indices returns correct data with a different prefix", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(200, 250, 300, 400, NA),
        c = c(400, 300, 200, 100, NA),
        d = c(50, 100, 150, 0, NA),
        index_b = c(100, 125, 150, 200, NA),
        index_c = c(100, 75, 50, 25, NA),
        index_d = c(100, 200, 300, 0, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_indices(data, prefix = "index_"))
    expect_equal(output, correct)
})
