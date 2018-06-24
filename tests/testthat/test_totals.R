context("Totals")

# Setup ----------------------------------------------------------------------

data <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(1, 2, 3, 4, 5),
    c = c(6, 7, 8, 9, 10),
    d = c(11, 12, 13, 14, 15))

# Tests: get_row_totals ------------------------------------------------------

test_that("get_row_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_row_totals(NULL), msg)
    expect_error(get_row_totals(c()), msg)
    expect_error(get_row_totals(c(NA, NA, NA)), msg)
    expect_error(get_row_totals(c("a", "b", "c")), msg)
    expect_error(get_row_totals(c(1, 2, 3)), msg)
    expect_error(get_row_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_row_totals(list(a = c(1,2,3))), msg)
})

test_that("get_row_totals rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(get_row_totals(data, from = -1), msg)
    expect_error(get_row_totals(data, from = 0), msg)
    expect_error(get_row_totals(data, from = 5), msg)
    expect_error(get_row_totals(data, to = -1), msg)
    expect_error(get_row_totals(data, to = 0), msg)
    expect_error(get_row_totals(data, to = 5), msg)
    expect_error(get_row_totals(data, from = 2, to = 1), msg)
    expect_error(get_row_totals(data, from = 4, to = 3), msg)
    expect_error(get_row_totals(data, from = "x"), msg)
    expect_error(get_row_totals(data, to = "y"), msg)
    expect_error(get_row_totals(data, from = "d", to = "c"), msg)
})

test_that("get_row_totals rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(get_row_totals(data, from = 1), msg)
})

test_that("get_row_totals returns correct data with defaults", {

    correct <- c(18, 21, 24, 27, 30)
    output <- get_row_totals(data)
    expect_equal(output, correct)
})

test_that("get_row_totals returns correct data with more label columns", {

    correct <- c(17, 19, 21, 23, 25)
    output <- get_row_totals(data, from = 3)
    expect_equal(output, correct)
})

test_that("get_row_totals returns correct data with fewer data columns", {

    correct <- c(7, 9, 11, 13, 15)
    output <- get_row_totals(data, to = 3)
    expect_equal(output, correct)
})

test_that("get_row_totals returns correct data with column names", {

    correct <- c(17, 19, 21, 23, 25)
    output <- get_row_totals(data, from = "c", to = "d")
    expect_equal(output, correct)
})

# Tests: add_row_totals ------------------------------------------------------

test_that("add_row_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_row_totals(NULL), msg)
    expect_error(add_row_totals(c()), msg)
    expect_error(add_row_totals(c(NA, NA, NA)), msg)
    expect_error(add_row_totals(c("a", "b", "c")), msg)
    expect_error(add_row_totals(c(1, 2, 3)), msg)
    expect_error(add_row_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_row_totals(list(a = c(1,2,3))), msg)
})

test_that("add_row_totals rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(add_row_totals(data, from = -1), msg)
    expect_error(add_row_totals(data, from = 0), msg)
    expect_error(add_row_totals(data, from = 5), msg)
    expect_error(add_row_totals(data, to = -1), msg)
    expect_error(add_row_totals(data, to = 0), msg)
    expect_error(add_row_totals(data, to = 5), msg)
    expect_error(add_row_totals(data, from = 2, to = 1), msg)
    expect_error(add_row_totals(data, from = 4, to = 3), msg)
    expect_error(add_row_totals(data, from = "x"), msg)
    expect_error(add_row_totals(data, to = "y"), msg)
})

test_that("add_row_totals rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(add_row_totals(data, from = 1), msg)
})

test_that("add_row_totals returns correct data with defaults", {

    correct <- tibble::tibble(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 4, 5),
        c = c(6, 7, 8, 9, 10),
        d = c(11, 12, 13, 14, 15),
        total = c(18, 21, 24, 27, 30))

    output <- add_row_totals(data)
    expect_equal(output, correct)
})

test_that("add_row_totals returns correct data with more label columns", {

    correct <- tibble::tibble(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 4, 5),
        c = c(6, 7, 8, 9, 10),
        d = c(11, 12, 13, 14, 15),
        total = c(17, 19, 21, 23, 25))

    output <- add_row_totals(data, from = 3)
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data without a label column", {

    correct <- tibble::tibble(
        b = c(1, 2, 3, 4, 5),
        c = c(6, 7, 8, 9, 10),
        d = c(11, 12, 13, 14, 15),
        total = c(18, 21, 24, 27, 30))

    output <- add_row_totals(data[2:ncol(data)], from = 1)
    expect_equal(output, correct)
})

test_that("add_row_totals returns correct data with fewer data columns", {

    correct <- tibble::tibble(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 4, 5),
        c = c(6, 7, 8, 9, 10),
        d = c(11, 12, 13, 14, 15),
        total = c(7, 9, 11, 13, 15))

    output <- add_row_totals(data, to = 3)
    expect_equal(output, correct)
})

test_that("add_row_totals returns correct data with column names", {

    correct <- tibble::tibble(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 4, 5),
        c = c(6, 7, 8, 9, 10),
        d = c(11, 12, 13, 14, 15),
        total = c(17, 19, 21, 23, 25))

    output <- add_row_totals(data, from = "c", to = "d")
    expect_equal(output, correct)
})

# Tests: get_col_totals ------------------------------------------------------

test_that("get_col_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_col_totals(NULL), msg)
    expect_error(get_col_totals(c()), msg)
    expect_error(get_col_totals(c(NA, NA, NA)), msg)
    expect_error(get_col_totals(c("a", "b", "c")), msg)
    expect_error(get_col_totals(c(1, 2, 3)), msg)
    expect_error(get_col_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_col_totals(list(a = c(1,2,3))), msg)
})

test_that("get_col_totals rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(get_col_totals(data, from = -1), msg)
    expect_error(get_col_totals(data, from = 0), msg)
    expect_error(get_col_totals(data, from = 5), msg)
    expect_error(get_col_totals(data, to = -1), msg)
    expect_error(get_col_totals(data, to = 0), msg)
    expect_error(get_col_totals(data, to = 5), msg)
    expect_error(get_col_totals(data, from = 2, to = 1), msg)
    expect_error(get_col_totals(data, from = 4, to = 3), msg)
    expect_error(get_col_totals(data, from = "x"), msg)
    expect_error(get_col_totals(data, to = "y"), msg)
    expect_error(get_col_totals(data, from = "d", to = "c"), msg)
})

test_that("get_col_totals rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(get_col_totals(data, from = 1), msg)
})

test_that("get_col_totals returns correct data with defaults", {

    correct <- c(b = 15, c = 40, d = 65)
    output <- get_col_totals(data)
    expect_equal(output, correct)
})

test_that("get_col_totals returns correct data with more label columns", {

    correct <- c(c = 40, d = 65)
    output <- get_col_totals(data, from = 3)
    expect_equal(output, correct)
})

test_that("get_col_totals returns correct data with fewer data columns", {

    correct <- c(b = 15, c = 40)
    output <- get_col_totals(data, to = 3)
    expect_equal(output, correct)
})

test_that("get_col_totals returns correct data with column names", {

    correct <- c(c = 40, d = 65)
    output <- get_col_totals(data, from = "c", to = "d")
    expect_equal(output, correct)
})

# Tests: add_col_totals ------------------------------------------------------

test_that("add_col_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_col_totals(NULL), msg)
    expect_error(add_col_totals(c()), msg)
    expect_error(add_col_totals(c(NA, NA, NA)), msg)
    expect_error(add_col_totals(c("a", "b", "c")), msg)
    expect_error(add_col_totals(c(1, 2, 3)), msg)
    expect_error(add_col_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_col_totals(list(a = c(1,2,3))), msg)
})

test_that("add_col_totals rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(add_col_totals(data, from = -1), msg)
    expect_error(add_col_totals(data, from = 0), msg)
    expect_error(add_col_totals(data, from = 5), msg)
    expect_error(add_col_totals(data, to = -1), msg)
    expect_error(add_col_totals(data, to = 0), msg)
    expect_error(add_col_totals(data, to = 5), msg)
    expect_error(add_col_totals(data, from = 2, to = 1), msg)
    expect_error(add_col_totals(data, from = 4, to = 3), msg)
    expect_error(add_col_totals(data, from = "x"), msg)
    expect_error(add_col_totals(data, to = "y"), msg)
    expect_error(add_col_totals(data, from = "d", to = "c"), msg)
})

test_that("add_col_totals rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(add_col_totals(data, from = 1), msg)
})

test_that("add_col_totals returns correct data with defaults", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, 15),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data)
    expect_equal(output, correct)
})

test_that("add_col_totals returns correct data with more label columns", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, NA),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data, from = 3)
    expect_equal(output, correct)
})

test_that("add_col_totals returns correct data without a label column", {

    correct <- tibble::tibble(
        b = c(1, 2, 3, 4, 5, 15),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data[2:ncol(data)], from = 1)
    expect_equal(output, correct)
})

test_that("add_col_totals returns correct data with fewer data columns", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, 15),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, NA))

    output <- add_col_totals(data, to = 3)
    expect_equal(output, correct)
})

test_that("add_col_totals returns correct data with column names", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, NA),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data, from = "c", to = "d")
    expect_equal(output, correct)
})

test_that("add_col_totals ignores invalid label columns", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, 15),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data, lcols = c(-1, 1, 2, 5))
    expect_equal(output, correct)
})

test_that("add_col_totals takes a user defined label", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "Grand Total"),
        b = c(1, 2, 3, 4, 5, 15),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data, label = "Grand Total")
    expect_equal(output, correct)
})
