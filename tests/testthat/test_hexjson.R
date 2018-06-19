context("HexJSON")

# Setup ----------------------------------------------------------------------

data <- tibble::tibble(
    name = letters[1:25],
    group = rep(letters[1:5], rep(5, 5)),
    value = 1:25)

tmp_dir <- file.path("tmp")
dir.create(tmp_dir, showWarnings = FALSE)

# Tests ----------------------------------------------------------------------

test_that("create_hexjson creates the correct hexjson for data", {

    json <- jsonlite::fromJSON(create_hexjson(data))

    expect_equal(length(json), 2)
    expect_equal(json$layout, "odd-r")

    expect_equal(length(json$hexes), 25)
    expect_equal(names(json$hexes), letters[1:25])

    for (hex in json$hexes) {
        expect_equal(length(hex), 5)
        expect_equal(names(hex), c("name", "group", "value", "q", "r"))
    }

    expect_equal(json$hexes$a$name, "a", "a", 1, 1, 1)
    expect_equal(json$hexes$y$name, "y", "e", 25, 5, 5)
})

test_that("create_hexjson creates valid layouts", {

    layouts <- c("odd-r", "even-r", "odd-q", "even-q")

    for (layout in layouts) {
        json <- jsonlite::fromJSON(create_hexjson(data, layout))
        expect_equal(json$layout, layout)
    }
})

test_that("create_hexjson rejects invalid layouts", {

    invalid_layouts <- c("odd_r", "evenr", "odd-c", "even_q")

    for (layout in invalid_layouts) {
        msg <- stringr::str_interp("\"${layout}\" is not a valid layout")
        expect_error(create_hexjson(data, layout), msg)
    }
})

test_that("create_hexjson rejects data with duplicate keys in key column", {

    invalid_data <- data
    invalid_data[[1]][25] <- "x"
    msg <- paste("Duplicate values found in first column of data: values in",
        "the first column are used as a key and so must be unique.")
    expect_error(create_hexjson(invalid_data), msg)
})

test_that("create_and_save_hexjson writes data as hexjson", {

    tmp_file <- file.path(tmp_dir, "tmp.hexjson")
    create_and_save_hexjson(data, tmp_file)

    json_from_disk <- readr::read_file(tmp_file)
    json_from_data <- create_hexjson(data)
    expect_equal(json_from_disk, json_from_data)

    file.remove(tmp_file)
})

test_that("create_hexjson_from_csv converts a csv to hexjson", {

    tmp_csv_file <- file.path(tmp_dir, "tmp.csv")
    tmp_hexjson_file <- file.path(tmp_dir, "tmp.hexjson")

    readr::write_csv(data, tmp_csv_file)
    create_hexjson_from_csv(tmp_csv_file, tmp_hexjson_file)

    json_from_disk <- readr::read_file(tmp_hexjson_file)
    json_from_data <- create_hexjson(data)
    expect_equal(json_from_disk, json_from_data)

    file.remove(tmp_csv_file)
    file.remove(tmp_hexjson_file)
})

# Teardown -------------------------------------------------------------------
unlink(tmp_dir, recursive = TRUE)
