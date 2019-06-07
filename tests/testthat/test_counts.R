context("Counts")

# Setup ----------------------------------------------------------------------

values <- c("a", "b", "b", "c", "c", "c", "d", "d", "d", "d")

# Tests: value_counts --------------------------------------------------------

test_that("value_counts rejects a values argument that is not a vector", {

    msg <- "The values argument is not a vector."
    expect_error(value_counts(NULL), msg)
    expect_error(value_counts(list()), msg)
    expect_error(value_counts(data.frame()), msg)
})

test_that("value_counts rejects a values argument that is an empty factor", {

    msg <- "The values argument is empty."
    expect_error(value_counts(factor()), msg)
})

test_that("value_counts rejects invalid by arguments", {

    msg <- "Invalid \"by\" argument. Must be either \"count\" or \"value\"."
    expect_error(value_counts(values, by = NULL), msg)
    expect_error(value_counts(values, by = NA), msg)
    expect_error(value_counts(values, by = list()), msg)
    expect_error(value_counts(values, by = data.frame()), msg)
    expect_error(value_counts(values, by = FALSE), msg)
    expect_error(value_counts(values, by = 1), msg)
    expect_error(value_counts(values, by = ""), msg)
})

test_that("value_counts rejects invalid order arguments", {

    msg <- "Invalid \"order\" argument. Must be either \"asc\" or \"desc\"."
    expect_error(value_counts(values, order = NULL), msg)
    expect_error(value_counts(values, order = NA), msg)
    expect_error(value_counts(values, order = list()), msg)
    expect_error(value_counts(values, order = data.frame()), msg)
    expect_error(value_counts(values, order = FALSE), msg)
    expect_error(value_counts(values, order = 1), msg)
    expect_error(value_counts(values, order = ""), msg)
})

test_that("value_counts rejects invalid na.rm arguments", {

    msg <- "Invalid \"na.rm\" argument. Must be either TRUE or FALSE."
    expect_error(value_counts(values, na.rm = NULL), msg)
    expect_error(value_counts(values, na.rm = NA), msg)
    expect_error(value_counts(values, na.rm = list()), msg)
    expect_error(value_counts(values, na.rm = data.frame()), msg)
    expect_error(value_counts(values, na.rm = 1), msg)
    expect_error(value_counts(values, na.rm = ""), msg)
})

test_that("value_counts rejects invalid name arguments", {

    msg <- "Invalid \"name\" argument. Must be a string."
    expect_error(value_counts(values, name = NA), msg)
    expect_error(value_counts(values, name = list()), msg)
    expect_error(value_counts(values, name = data.frame()), msg)
})

test_that("value_counts returns correct data with defaults", {

    correct <- tibble::tibble(
        values = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values)
    expect_equal(output, correct)
})

test_that("value_counts returns correct data with a given name argument", {

    correct <- tibble::tibble(
        checkname = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values, name = "checkname")
    expect_equal(output, correct)
})

test_that("value_counts returns correct data with valid by arguments", {

    correct <- tibble::tibble(
        values = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values, by = "count")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        values = letters[1:4],
        count = 1:4,
        percent = count / sum(count))
    output <- value_counts(values, by = "value")
    expect_equal(output, correct)
})

test_that("value_counts returns correct data with valid order arguments", {

    correct <- tibble::tibble(
        values = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values, order = "desc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        values = letters[1:4],
        count = 1:4,
        percent = count / sum(count))
    output <- value_counts(values, order = "asc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        values = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values, by = "count", order = "desc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        values = letters[1:4],
        count = 1:4,
        percent = count / sum(count))
    output <- value_counts(values, by = "count", order = "asc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        values = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values, by = "value", order = "desc")
    expect_equal(output, correct)

    correct <- tibble::tibble(
        values = letters[1:4],
        count = 1:4,
        percent = count / sum(count))
    output <- value_counts(values, by = "value", order = "asc")
    expect_equal(output, correct)
})

test_that("value_counts returns correct data with a valid na.rm argument", {

    values <- c(values, NA)

    correct <- tibble::tibble(
        values = c(letters[4:1], NA),
        count = as.integer(c(4:1, 1)),
        percent = count / sum(count))
    output <- value_counts(values)
    expect_equal(output[2:3], correct[2:3])
    expect_equal(output[1:4, ], correct[1:4, ])
    expect_equal(is.na(output[[5, 1]]), TRUE)

    correct <- tibble::tibble(
        values = c(letters[4:1], NA),
        count = as.integer(c(4:1, 1)),
        percent = count / sum(count))
    output <- value_counts(values, na.rm = FALSE)
    expect_equal(output[2:3], correct[2:3])
    expect_equal(output[1:4, ], correct[1:4, ])
    expect_equal(is.na(output[[5, 1]]), TRUE)

    correct <- tibble::tibble(
        values = letters[4:1],
        count = 4:1,
        percent = count / sum(count))
    output <- value_counts(values, na.rm = TRUE)
    expect_equal(output, correct)
})
