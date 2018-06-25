#' Run a dataframe range function with validation
#'
#' A dataframe range function is a generic name for a set of functions in this
#' package with a common signature. Dataframe range funtions take a dataframe
#' along with \code{from} and \code{to} arguments, which specify a range of
#' columns over which operations will be performed.
#'
#' This function provides standard validation for the arguments that are
#' common to these functions. It takes a dataframe range function, checks the
#' standard inputs are valid, runs the function and returns any values.
#'
#' @param f The dataframe range function.
#' @param from The start column: either an index or name.
#' @param to The end column: either and index or name.
#' @return The retrun value of the function.
#' @keywords internal
#'
run_dfr_func <- function(f, data, from, to, ...) {

    # Check data is a dataframe
    if (! valid_args(data, from, to)) stop()

    # Convert column names to numbers if necessary
    if (is.character(from)) from <- get_col_num(data, from)
    if (is.character(to)) to <- get_col_num(data, to)

    # Check columns are valid
    if (! valid_columns(data, from, to)) stop()

    # Call the function with the validated arguments
    f(data, from, to, ...)
}

#' Validate the arguments to a dataframe range function are the right type
#' and size.
#'
#' @param data The input data.
#' @param from The number or name of the column from which data is processed.
#' @param to The number or name of the column to which data is processed.
#' @return TRUE if no errors are thrown.
#' @keywords internal
#'
valid_args <- function(data, from, to) {

    # Check data is a dataframe
    if (! is.data.frame(data)) stop(
        "The data is not a dataframe.")

    # Check from is a vetor of length 1.
    if (! is.vector(from) || length(from) != 1) stop(
        "The from argument must be a vector of length 1.")

    # Check to is a vetor of length 1.
    if (! is.vector(to) || length(to) != 1) stop(
        "The to argument must be a vector of length 1.")

    TRUE
}

#' Validate columns for row percentages, totals and indices
#'
#' @param data A dataframe containing columns of numerical data for analysis.
#' @param from The number of the column from which data is processed.
#' @param to The number of the column to which data is processed.
#' @return TRUE if no errors are thrown.
#' @keywords internal
#'
valid_columns <- function(data, from, to) {

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given columns are out of range.")

    # Check columns are numeric
    if (! all(purrr::map_lgl(data[from:to], is.numeric))) stop (
        "The given columns are not all numeric.")

    TRUE
}

#' Get the column number for a column in a dataframe given its name
#'
#' @param data A dataframe.
#' @param name A column name as a string.
#' @return The column number for the column with the given name, or zero if
#'   the column name is not found.
#' @keywords internal
#'
get_col_num <- function(data, name) {

    num <- which(colnames(data) == name)
    if (identical(num, integer(0))) num <- 0
    num
}
