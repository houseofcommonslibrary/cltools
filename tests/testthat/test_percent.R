context("Percent")

# Setup ----------------------------------------------------------------------

data_row <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(1, 2, 3, 0, NA),
    c = c(2, 3, 1, 0, NA),
    d = c(3, 1, 2, 0, NA),
    e = c(0, 0, 0, 0, NA))

data_col <- tibble::tibble(
    a = LETTERS[1:4],
    b = c(1, 2, 3, 0),
    c = c(2, 3, 1, 0),
    d = c(3, 1, 2, 0),
    e = c(0, 0, 0, 0),
    f = c(0, 0, 0, NA))

# Tests: get_row_percent -----------------------------------------------------

test_that("get_row_percent rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_row_percent(NULL), msg)
    expect_error(get_row_percent(c()), msg)
    expect_error(get_row_percent(c(NA, NA, NA)), msg)
    expect_error(get_row_percent(c("a", "b", "c")), msg)
    expect_error(get_row_percent(c(1, 2, 3)), msg)
    expect_error(get_row_percent(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_row_percent(list(a = c(1,2,3))), msg)
})

test_that("get_row_percent rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(get_row_percent(data_row, from = -1), msg)
    expect_error(get_row_percent(data_row, from = 0), msg)
    expect_error(get_row_percent(data_row, from = 6), msg)
    expect_error(get_row_percent(data_row, to = -1), msg)
    expect_error(get_row_percent(data_row, to = 0), msg)
    expect_error(get_row_percent(data_row, to = 6), msg)
    expect_error(get_row_percent(data_row, from = 2, to = 1), msg)
    expect_error(get_row_percent(data_row, from = 5, to = 4), msg)
    expect_error(get_row_percent(data_row, from = "a"), msg)
    expect_error(get_row_percent(data_row, to = "b"), msg)
})

test_that("get_row_percent rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(get_row_percent(data_row, from = 1), msg)
})

test_that("get_row_percent returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1/6, 2/6, 3/6, NA, NA),
        c = c(2/6, 3/6, 1/6, NA, NA),
        d = c(3/6, 1/6, 2/6, NA, NA),
        e = c(0/6, 0/6, 0/6, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_row_percent(data_row))
    expect_equal(output, correct)
})

test_that("get_row_percent returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 0, NA),
        c = c(2/5, 3/4, 1/3, NA, NA),
        d = c(3/5, 1/4, 2/3, NA, NA),
        e = c(0/5, 0/4, 0/3, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_row_percent(data_row, from = 3))
    expect_equal(output, correct)
})

test_that("get_row_percent returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1/3, 2/5, 3/4, NA, NA),
        c = c(2/3, 3/5, 1/4, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(get_row_percent(data_row, to = 3))
    expect_equal(output, correct)
})

# Tests: add_row_percent -----------------------------------------------------

test_that("add_row_percent rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_row_percent(NULL), msg)
    expect_error(add_row_percent(c()), msg)
    expect_error(add_row_percent(c(NA, NA, NA)), msg)
    expect_error(add_row_percent(c("a", "b", "c")), msg)
    expect_error(add_row_percent(c(1, 2, 3)), msg)
    expect_error(add_row_percent(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_row_percent(list(a = c(1,2,3))), msg)
})

test_that("add_row_percent rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(add_row_percent(data_row, from = -1), msg)
    expect_error(add_row_percent(data_row, from = 0), msg)
    expect_error(add_row_percent(data_row, from = 6), msg)
    expect_error(add_row_percent(data_row, to = -1), msg)
    expect_error(add_row_percent(data_row, to = 0), msg)
    expect_error(add_row_percent(data_row, to = 6), msg)
    expect_error(add_row_percent(data_row, from = 2, to = 1), msg)
    expect_error(add_row_percent(data_row, from = 5, to = 4), msg)
    expect_error(add_row_percent(data_row, from = "a"), msg)
    expect_error(add_row_percent(data_row, to = "b"), msg)
})

test_that("add_row_percent rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(add_row_percent(data_row, from = 1), msg)
})

test_that("add_row_percent returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 0, NA),
        c = c(2, 3, 1, 0, NA),
        d = c(3, 1, 2, 0, NA),
        e = c(0, 0, 0, 0, NA),
        pc_b = c(1/6, 2/6, 3/6, NA, NA),
        pc_c = c(2/6, 3/6, 1/6, NA, NA),
        pc_d = c(3/6, 1/6, 2/6, NA, NA),
        pc_e = c(0/6, 0/6, 0/6, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_row_percent(data_row))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 0, NA),
        c = c(2, 3, 1, 0, NA),
        d = c(3, 1, 2, 0, NA),
        e = c(0, 0, 0, 0, NA),
        pc_c = c(2/5, 3/4, 1/3, NA, NA),
        pc_d = c(3/5, 1/4, 2/3, NA, NA),
        pc_e = c(0/5, 0/4, 0/3, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_row_percent(data_row, from = 3))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data without a label column", {

    correct <- data.frame(
        b = c(1, 2, 3, 0, NA),
        c = c(2, 3, 1, 0, NA),
        d = c(3, 1, 2, 0, NA),
        e = c(0, 0, 0, 0, NA),
        pc_b = c(1/6, 2/6, 3/6, NA, NA),
        pc_c = c(2/6, 3/6, 1/6, NA, NA),
        pc_d = c(3/6, 1/6, 2/6, NA, NA),
        pc_e = c(0/6, 0/6, 0/6, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(
        add_row_percent(data_row[2:ncol(data_row)], from = 1))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 0, NA),
        c = c(2, 3, 1, 0, NA),
        d = c(3, 1, 2, 0, NA),
        e = c(0, 0, 0, 0, NA),
        pc_b = c(1/3, 2/5, 3/4, NA, NA),
        pc_c = c(2/3, 3/5, 1/4, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_row_percent(data_row, to = 3))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data with a different prefix", {

    correct <- data.frame(
        a = LETTERS[1:5],
        b = c(1, 2, 3, 0, NA),
        c = c(2, 3, 1, 0, NA),
        d = c(3, 1, 2, 0, NA),
        e = c(0, 0, 0, 0, NA),
        percent_b = c(1/6, 2/6, 3/6, NA, NA),
        percent_c = c(2/6, 3/6, 1/6, NA, NA),
        percent_d = c(3/6, 1/6, 2/6, NA, NA),
        percent_e = c(0/6, 0/6, 0/6, NA, NA), stringsAsFactors = FALSE)

    output <- as.data.frame(add_row_percent(data_row, prefix = "percent_"))
    expect_equal(output, correct)
})

# Tests: get_col_percent -----------------------------------------------------

test_that("get_col_percent rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(get_col_percent(NULL), msg)
    expect_error(get_col_percent(c()), msg)
    expect_error(get_col_percent(c(NA, NA, NA)), msg)
    expect_error(get_col_percent(c("a", "b", "c")), msg)
    expect_error(get_col_percent(c(1, 2, 3)), msg)
    expect_error(get_col_percent(c(TRUE, TRUE, FALSE)), msg)
    expect_error(get_col_percent(list(a = c(1,2,3))), msg)
})

test_that("get_col_percent rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(get_col_percent(data_col, from = -1), msg)
    expect_error(get_col_percent(data_col, from = 0), msg)
    expect_error(get_col_percent(data_col, from = 7), msg)
    expect_error(get_col_percent(data_col, to = -1), msg)
    expect_error(get_col_percent(data_col, to = 0), msg)
    expect_error(get_col_percent(data_col, to = 7), msg)
    expect_error(get_col_percent(data_col, from = 2, to = 1), msg)
    expect_error(get_col_percent(data_col, from = 5, to = 4), msg)
    expect_error(get_col_percent(data_col, from = "a"), msg)
    expect_error(get_col_percent(data_col, to = "b"), msg)
})

test_that("get_col_percent rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(get_col_percent(data_col, from = 1), msg)
})

test_that("get_col_percent returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1/6, 2/6, 3/6, 0/6),
        c = c(2/6, 3/6, 1/6, 0/6),
        d = c(3/6, 1/6, 2/6, 0/6),
        e = as.numeric(c(NA, NA, NA, NA)),
        f = as.numeric(c(NA, NA, NA, NA)), stringsAsFactors = FALSE)

    output <- as.data.frame(get_col_percent(data_col))
    expect_equal(output, correct)
})

test_that("get_col_percent returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1, 2, 3, 0),
        c = c(2/6, 3/6, 1/6, 0/6),
        d = c(3/6, 1/6, 2/6, 0/6),
        e = as.numeric(c(NA, NA, NA, NA)),
        f = as.numeric(c(NA, NA, NA, NA)), stringsAsFactors = FALSE)

    output <- as.data.frame(get_col_percent(data_col, from = 3))
    expect_equal(output, correct)
})

test_that("get_col_percent returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1/6, 2/6, 3/6, 0/6),
        c = c(2/6, 3/6, 1/6, 0/6), stringsAsFactors = FALSE)

    output <- as.data.frame(get_col_percent(data_col, to = 3))
    expect_equal(output, correct)
})

# Tests: add_col_percent -----------------------------------------------------

test_that("add_col_percent rejects data that is not a dataframe", {

    msg <- "The data is not a dataframe."
    expect_error(add_col_percent(NULL), msg)
    expect_error(add_col_percent(c()), msg)
    expect_error(add_col_percent(c(NA, NA, NA)), msg)
    expect_error(add_col_percent(c("a", "b", "c")), msg)
    expect_error(add_col_percent(c(1, 2, 3)), msg)
    expect_error(add_col_percent(c(TRUE, TRUE, FALSE)), msg)
    expect_error(add_col_percent(list(a = c(1,2,3))), msg)
})

test_that("add_row_percent rejects invalid column numbers", {

    msg <- "The given column numbers are not valid."
    expect_error(add_row_percent(data_row, from = -1), msg)
    expect_error(add_row_percent(data_row, from = 0), msg)
    expect_error(add_row_percent(data_row, from = 7), msg)
    expect_error(add_row_percent(data_row, to = -1), msg)
    expect_error(add_row_percent(data_row, to = 0), msg)
    expect_error(add_row_percent(data_row, to = 7), msg)
    expect_error(add_row_percent(data_row, from = 2, to = 1), msg)
    expect_error(add_row_percent(data_row, from = 5, to = 4), msg)
    expect_error(add_row_percent(data_row, from = "a"), msg)
    expect_error(add_row_percent(data_row, to = "b"), msg)
})

test_that("add_row_percent rejects columns that aren't numeric", {

    msg <- "The given columns are not all numeric."
    expect_error(add_row_percent(data_row, from = 1), msg)
})

test_that("add_row_percent returns correct data with defaults", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1, 2, 3, 0),
        c = c(2, 3, 1, 0),
        d = c(3, 1, 2, 0),
        e = c(0, 0, 0, 0),
        f = c(0, 0, 0, NA),
        pc_b = c(1/6, 2/6, 3/6, 0/6),
        pc_c = c(2/6, 3/6, 1/6, 0/6),
        pc_d = c(3/6, 1/6, 2/6, 0/6),
        pc_e = as.numeric(c(NA, NA, NA, NA)),
        pc_f = as.numeric(c(NA, NA, NA, NA)), stringsAsFactors = FALSE)

    output <- as.data.frame(add_col_percent(data_col))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data with more label columns", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1, 2, 3, 0),
        c = c(2, 3, 1, 0),
        d = c(3, 1, 2, 0),
        e = c(0, 0, 0, 0),
        f = c(0, 0, 0, NA),
        pc_c = c(2/6, 3/6, 1/6, 0/6),
        pc_d = c(3/6, 1/6, 2/6, 0/6),
        pc_e = as.numeric(c(NA, NA, NA, NA)),
        pc_f = as.numeric(c(NA, NA, NA, NA)), stringsAsFactors = FALSE)

    output <- as.data.frame(add_col_percent(data_col, from = 3))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data without a label column", {

    correct <- data.frame(
        b = c(1, 2, 3, 0),
        c = c(2, 3, 1, 0),
        d = c(3, 1, 2, 0),
        e = c(0, 0, 0, 0),
        f = c(0, 0, 0, NA),
        pc_b = c(1/6, 2/6, 3/6, 0/6),
        pc_c = c(2/6, 3/6, 1/6, 0/6),
        pc_d = c(3/6, 1/6, 2/6, 0/6),
        pc_e = as.numeric(c(NA, NA, NA, NA)),
        pc_f = as.numeric(c(NA, NA, NA, NA)), stringsAsFactors = FALSE)

    output <- as.data.frame(
        add_col_percent(data_col[2:ncol(data_col)], from = 1))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data with fewer data columns", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1, 2, 3, 0),
        c = c(2, 3, 1, 0),
        d = c(3, 1, 2, 0),
        e = c(0, 0, 0, 0),
        f = c(0, 0, 0, NA),
        pc_b = c(1/6, 2/6, 3/6, 0/6),
        pc_c = c(2/6, 3/6, 1/6, 0/6), stringsAsFactors = FALSE)

    output <- as.data.frame(add_col_percent(data_col, to = 3))
    expect_equal(output, correct)
})

test_that("add_row_percent returns correct data with a different prefix", {

    correct <- data.frame(
        a = LETTERS[1:4],
        b = c(1, 2, 3, 0),
        c = c(2, 3, 1, 0),
        d = c(3, 1, 2, 0),
        e = c(0, 0, 0, 0),
        f = c(0, 0, 0, NA),
        percent_b = c(1/6, 2/6, 3/6, 0/6),
        percent_c = c(2/6, 3/6, 1/6, 0/6),
        percent_d = c(3/6, 1/6, 2/6, 0/6),
        percent_e = as.numeric(c(NA, NA, NA, NA)),
        percent_f = as.numeric(c(NA, NA, NA, NA)), stringsAsFactors = FALSE)

    output <- as.data.frame(add_col_percent(data_col, prefix = "percent_"))
    expect_equal(output, correct)
})
