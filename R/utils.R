#' @importFrom magrittr %>%
NULL

#' Validate input data is a dataframe
#'
#' @param data The input data.
#' @keywords internal
#'
valid_df <- function(data) {

    # Check data is a dataframe
    if (! is.data.frame(data)) stop(
        "The data is not a dataframe.")

    TRUE
}

#' Validate column arguments for row percentages, totals and indices
#'
#' @param data A dataframe containing columns of numerical data for analysis.
#' @param from The number of the column from which data is processed.
#' @param to The number of the column to which data is processed.
#' @keywords internal
#'
valid_columns <- function(data, from, to) {

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given column numbers are not valid.")

    # Check columns are numeric
    if (! all(purrr::map_lgl(data[from:to], is.numeric))) stop (
        "The given columns are not all numeric.")

    TRUE
}
