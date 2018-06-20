#' Create a hexjson string for a hex from a dataframe row
#'
#' Creates a hexjson string for an individual hex from a dataframe row. The
#' column names are used as the property names and the vales are used as the
#' property values. The value in the first column is used as the key for the
#' hex in the hexjson.
#'
#' @param ... A row of data from a dataframe.
#' @return The hex part of a hexjson string.
#' @keywords internal
#'
get_hex_string <- function(...) {

    row <- list(...)

    props <- purrr::pmap_chr(list(names(row)), function(name) {
        if (is.numeric(row[[name]])) {
            stringr::str_interp("\"${name}\":${row[[name]]},")
        } else {
            stringr::str_interp("\"${name}\":\"${row[[name]]}\",")
        }
    })

    properties <- stringr::str_sub(paste(props, collapse = ""), 1, -2)
    stringr::str_interp("\"${row[1]}\":{${properties}},")
}

#' Convert tabular data to hexjson
#'
#' Converts a dataframe of codes, names and other data to a hexjson string,
#' adding unique column and row coordinates for each hex. The values in the
#' first column are used as the key for each hex in the hexjson and therefore
#' must be unique. This function can be used to produce initial hexjson output
#' which can then be edited in a hexjson editor.
#'
#' @param data A dataframe of labels and data to store in each hex.
#' @param layout The coordinate layout of the hexsjon. Must be one of:
#'   odd-r, even-r, odd-q, even-q.
#' @return A hexjson string
#' @export
#'
create_hexjson <- function(data, layout = "odd-r") {

    layouts <- c("odd-r", "even-r", "odd-q", "even-q")
    layouts_str <- paste(layouts, collapse = ", ")

    if (! (layout %in% layouts)) stop(
        stringr::str_interp("\"${layout}\" is not a valid layout."))

    if (length(data[[1]]) != length(unique(data[[1]]))) stop(paste(
        "Duplicate values found in first column of data: values in",
        "the first column are used as a key and so must be unique."))

    count_unit <- ceiling(sqrt(nrow(data)))
    col_pos <- rep(seq(1, count_unit), count_unit)[1:nrow(data)]
    row_pos <- rep(seq(1, count_unit), each = count_unit)[1:nrow(data)]

    data$q <- col_pos
    data$r <- row_pos

    hex_strings <- purrr::pmap_chr(data, get_hex_string)

    hex_strings[length(hex_strings)] <- stringr::str_sub(
        hex_strings[length(hex_strings)], 1, -2)

    hex_strings <- paste(hex_strings, collapse = "")

    stringr::str_interp(
        "{\"layout\":\"${layout}\",\"hexes\":{${hex_strings}}}")
}

#' Convert tabular data to hexjson and save it to a file
#'
#' Converts a dataframe of codes, names and other data to a hexjson string,
#' adding unique column and row coordinates for each hex, then saves it to a
#' file. The values in the first column are used as the key for each hex in
#' the hexjson and therefore must be unique. This function can be used to
#' produce initial hexjson output which can then be edited in a hexjson
#' editor.
#'
#' @param data A dataframe of labels and data to store in each hex.
#' @param filename The name of an output file for the hexjson.
#' @param layout The coordinate layout of the hexsjon. Must be one of:
#'   odd-r, even-r, odd-q, even-q.
#' @export
#'
create_and_save_hexjson <- function(data, filename, layout = "odd-r") {

    hexjson_str <- create_hexjson(data, layout)
    readr::write_file(hexjson_str, filename)
}

#' Convert a csv of tabular data to hexjson and save it to a file
#'
#' Converts a csv of codes, names and other data to a hexjson string, adding
#' unique column and row coordinates for each hex, then saves it to a file.
#' The values in the first column are used as the key for each hex in the
#' hexjson and therefore must be unique. This function can be used to produce
#' initial hexjson output which can then be edited in a hexjson editor.
#'
#' @param csv_name The name of a csv file of data to store in each hex.
#' @param hexjson_name The name of an output file for the hexjson.
#' @param layout The coordinate layout of the hexsjon. Must be one of:
#'   odd-r, even-r, odd-q, even-q.
#' @export
#'
create_hexjson_from_csv <- function(
    csv_file, hexjson_file, layout = "odd-r") {

    data <- readr::read_csv(csv_file, col_types = readr::cols())
    create_and_save_hexjson(data, hexjson_file, layout)
}
