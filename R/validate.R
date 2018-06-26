#' Run a dataframe range function with validation
#'
#' A dataframe range function is a generic name for a set of functions in this
#' package with a common signature. Dataframe range funtions take a dataframe
#' along with \code{from} and \code{to} arguments, which specify a range of
#' columns over which operations will be performed.
#'
#' This function provides standard validation for the arguments that are
#' common to these functions. It takes a dataframe range function, checks the
#' standard inputs are valid, runs the function, and returns any values.
#'
#' @param f The dataframe range function.
#' @param from The start column: either an index or name.
#' @param to The end column: either and index or name.
#' @return The return value of the function.
#' @keywords internal
#'
run_dfr_func <- function(f, data, from, to, ...) {

    # Check data is a dataframe
    if (! is.data.frame(data)) stop(
        "The data is not a dataframe.")

    # Check from is a vector of length 1.
    if (! is.vector(from) || length(from) != 1) stop(
        "The from argument must be a vector of length 1.")

    # Check to is a vector of length 1.
    if (! is.vector(to) || length(to) != 1) stop(
        "The to argument must be a vector of length 1.")

    # Convert column names to numbers if necessary
    if (is.character(from)) from <- get_col_num(data, from)
    if (is.character(to)) to <- get_col_num(data, to)

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given columns are out of range.")

    # Check columns are numeric
    if (! all(purrr::map_lgl(data[from:to], is.numeric))) stop (
        "The given columns are not all numeric.")

    # Call the function with the validated arguments
    f(data, from = from, to = to, ...)
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
