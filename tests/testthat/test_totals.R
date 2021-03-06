context("Totals")

# Setup ----------------------------------------------------------------------

data <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(1, 2, 3, 4, 5),
    c = c(6, 7, 8, 9, 10),
    d = c(11, 12, 13, 14, 15))

data_lcols <- tibble::tibble(
    a = LETTERS[1:5],
    b = letters[1:5],
    c = c(1, 2, 3, 4, 5),
    d = c(6, 7, 8, 9, 10),
    e = c(11, 12, 13, 14, 15))

data_na <- tibble::tibble(
    a = LETTERS[1:6],
    b = c(1, 2, 3, 4, 5, 100),
    c = c(6, 7, 8, 9, 10, 100),
    d = c(11, 12, 13, 14, 15, NA))

# Tests: get_row_totals ------------------------------------------------------

test_that("get_row_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_row_totals(NULL), msg)
    expect_error(get_row_totals(c(NA, NA, NA)), msg)
    expect_error(get_row_totals(c("a", "b", "c")), msg)
    expect_error(get_row_totals(c(1, 2, 3)), msg)
    expect_error(get_row_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_row_totals(list(a = c(1,2,3))), msg)
})

test_that("get_row_totals rejects invalid from and to arguments", {

    msg <- "The given columns are out of range."
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

    expect_error(get_row_totals(data, from = c(2,3)),
                 "The from argument must be a vector of length 1")

    expect_error(get_row_totals(data, to = c(3, 4)),
                 "The to argument must be a vector of length 1")
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

test_that("get_row_totals ignores NAs when na.rm is TRUE", {

    correct <- c(18, 21, 24, 27, 30, NA)
    output <- get_row_totals(data_na)
    expect_equal(output, correct)

    correct <- c(18, 21, 24, 27, 30, 200)
    output <- get_row_totals(data_na, na.rm = TRUE)
    expect_equal(output, correct)
})

# Tests: add_row_totals ------------------------------------------------------

test_that("add_row_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_row_totals(NULL), msg)
    expect_error(add_row_totals(c(NA, NA, NA)), msg)
    expect_error(add_row_totals(c("a", "b", "c")), msg)
    expect_error(add_row_totals(c(1, 2, 3)), msg)
    expect_error(add_row_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_row_totals(list(a = c(1,2,3))), msg)
})

test_that("add_row_totals rejects invalid from and to arguments", {

    msg <- "The given columns are out of range."
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

    expect_error(add_row_totals(data, from = c(2,3)),
                 "The from argument must be a vector of length 1")

    expect_error(add_row_totals(data, to = c(3, 4)),
                 "The to argument must be a vector of length 1")
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

test_that("add_row_totals ignores NAs when na.rm is TRUE", {

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(1, 2, 3, 4, 5, 100),
        c = c(6, 7, 8, 9, 10, 100),
        d = c(11, 12, 13, 14, 15, NA),
        total = c(18, 21, 24, 27, 30, NA),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_row_totals(data_na))
    expect_equal(output, correct)

    correct <- data.frame(
        a = LETTERS[1:6],
        b = c(1, 2, 3, 4, 5, 100),
        c = c(6, 7, 8, 9, 10, 100),
        d = c(11, 12, 13, 14, 15, NA),
        total = c(18, 21, 24, 27, 30, 200),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_row_totals(data_na, na.rm = TRUE))
    expect_equal(output, correct)
})

# Tests: get_col_totals ------------------------------------------------------

test_that("get_col_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_col_totals(NULL), msg)
    expect_error(get_col_totals(c(NA, NA, NA)), msg)
    expect_error(get_col_totals(c("a", "b", "c")), msg)
    expect_error(get_col_totals(c(1, 2, 3)), msg)
    expect_error(get_col_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_col_totals(list(a = c(1,2,3))), msg)
})

test_that("get_col_totals rejects invalid from and to arguments", {

    msg <- "The given columns are out of range."
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

    expect_error(get_col_totals(data, from = c(2,3)),
                 "The from argument must be a vector of length 1")

    expect_error(get_col_totals(data, to = c(3, 4)),
                 "The to argument must be a vector of length 1")
})

test_that("get_col_totals rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(get_col_totals(data, from = 1), msg)
})

test_that("get_col_totals rejects rows that aren't numeric", {

    msg <- "The rows argument is not numeric."
    expect_error(get_col_totals(data, rows = NULL), msg)
    expect_error(get_col_totals(data, rows = c(NA, NA, NA)), msg)
    expect_error(get_col_totals(data, rows = c("a", "b", "c")), msg)
    expect_error(get_col_totals(data, rows = c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_col_totals(data, rows = list(a = c(1,2,3))), msg)
})

test_that("get_col_totals warns of duplicates in rows", {

    msg <- "Duplicates in the rows argument were ignored."
    expect_warning(get_col_totals(data, rows = c(1, 1, 2, 3)), msg)
})

test_that("get_col_totals warns of out of range row numbers in rows", {

    msg <- "Out of range row numbers in the rows argument were ignored."
    expect_warning(get_col_totals(data, rows = c(-1, 1)), msg)
    expect_warning(get_col_totals(data, rows = c(0, 5)), msg)
    expect_warning(get_col_totals(data, rows = c(1, 6)), msg)
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

test_that("get_col_totals returns correct data for a subset of rows", {

    correct <- c(b = 3, c = 13, d = 23)
    output <- get_col_totals(data, rows = c(1, 2))
    expect_equal(output, correct)

    correct <- c(b = 9, c = 24, d = 39)
    output <- get_col_totals(data, rows = c(1, 3, 5))
    expect_equal(output, correct)

    correct <- c(b = 12, c = 27, d = 42)
    output <- get_col_totals(data, rows = 3:5)
    expect_equal(output, correct)
})

test_that("get_col_totals ignores NAs when na.rm is TRUE", {

    correct <- c(b = 115, c = 140, d = NA)
    output <- get_col_totals(data_na)
    expect_equal(output, correct)

    correct <- c(b = 115, c = 140, d = 65)
    output <- get_col_totals(data_na, na.rm = TRUE)
    expect_equal(output, correct)
})

# Tests: add_col_totals ------------------------------------------------------

test_that("add_col_totals rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_col_totals(NULL), msg)
    expect_error(add_col_totals(c(NA, NA, NA)), msg)
    expect_error(add_col_totals(c("a", "b", "c")), msg)
    expect_error(add_col_totals(c(1, 2, 3)), msg)
    expect_error(add_col_totals(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_col_totals(list(a = c(1,2,3))), msg)
})

test_that("add_col_totals rejects invalid from and to arguments", {

    msg <- "The given columns are out of range."
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

    expect_error(add_col_totals(data, from = c(2,3)),
                 "The from argument must be a vector of length 1")

    expect_error(add_col_totals(data, to = c(3, 4)),
                 "The to argument must be a vector of length 1")
})

test_that("add_col_totals rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(add_col_totals(data, from = 1), msg)
})

test_that("add_col_totals rejects rows that aren't numeric", {

    msg <- "The rows argument is not numeric."
    expect_error(add_col_totals(data, rows = NULL), msg)
    expect_error(add_col_totals(data, rows = c(NA, NA, NA)), msg)
    expect_error(add_col_totals(data, rows = c("a", "b", "c")), msg)
    expect_error(add_col_totals(data, rows = c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_col_totals(data, rows = list(a = c(1,2,3))), msg)
})

test_that("add_col_totals warns of duplicates in rows", {

    msg <- "Duplicates in the rows argument were ignored."
    expect_warning(add_col_totals(data, rows = c(1, 1, 2, 3)), msg)
})

test_that("add_col_totals warns of out of range row numbers in rows", {

    msg <- "Out of range row numbers in the rows argument were ignored."
    expect_warning(add_col_totals(data, rows = c(-1, 1)), msg)
    expect_warning(add_col_totals(data, rows = c(0, 5)), msg)
    expect_warning(add_col_totals(data, rows = c(1, 6)), msg)
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

test_that("add_col_totals returns correct data for a subset of rows", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, 3),
        c = c(6, 7, 8, 9, 10, 13),
        d = c(11, 12, 13, 14, 15, 23))

    output <- add_col_totals(data, rows = c(1, 2))
    expect_equal(output, correct)

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(1, 2, 3, 4, 5, 9),
        c = c(6, 7, 8, 9, 10, 24),
        d = c(11, 12, 13, 14, 15, 39))

    output <- add_col_totals(data, rows = c(1, 3, 5))
    expect_equal(output, correct)

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "subtotal"),
        b = c(1, 2, 3, 4, 5, 12),
        c = c(6, 7, 8, 9, 10, 27),
        d = c(11, 12, 13, 14, 15, 42))

    output <- add_col_totals(data, rows = 3:5, label = "subtotal")
    expect_equal(output, correct)
})

test_that("add_col_totals accepts strings as indices for label columns", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], "total"),
        b = c(letters[1:5], "total"),
        c = c(1, 2, 3, 4, 5, 15),
        d = c(6, 7, 8, 9, 10, 40),
        e = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data_lcols, from = "c", lcols = c("a", "b"))
    expect_equal(output, correct)

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], NA),
        b = c(letters[1:5], "total"),
        c = c(1, 2, 3, 4, 5, 15),
        d = c(6, 7, 8, 9, 10, 40),
        e = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data_lcols, from = 3, lcols = c("b"))
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

    output <- add_col_totals(data, lcols = c("-", "a", "b", "e"))
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

test_that("add_col_totals removes labels when lcols is NULL", {

    correct <- tibble::tibble(
        a = c(LETTERS[1:5], NA),
        b = c(1, 2, 3, 4, 5, 15),
        c = c(6, 7, 8, 9, 10, 40),
        d = c(11, 12, 13, 14, 15, 65))

    output <- add_col_totals(data, lcols = NULL)
    expect_equal(output, correct)
})

test_that("add_col_totals ignores NAs when na.rm is TRUE", {

    correct <- data.frame(
        a = c(LETTERS[1:6], "total"),
        b = c(1, 2, 3, 4, 5, 100, 115),
        c = c(6, 7, 8, 9, 10, 100, 140),
        d = c(11, 12, 13, 14, 15, NA, NA),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_col_totals(data_na))
    expect_equal(output, correct)

    correct <- data.frame(
        a =  c(LETTERS[1:6], "total"),
        b = c(1, 2, 3, 4, 5, 100, 115),
        c = c(6, 7, 8, 9, 10, 100, 140),
        d = c(11, 12, 13, 14, 15, NA, 65),
        stringsAsFactors = FALSE)

    output <- as.data.frame(add_col_totals(data_na, na.rm = TRUE))
    expect_equal(output, correct)
})
