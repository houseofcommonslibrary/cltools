#' Deflate the values in a series by the given deflator
#'
#' Deflates the values in a series by the given deflator. The data series and
#' the deflator should be numeric vectors of equal length. The deflator cannot
#' contain NA or zero values.
#'
#' @param data The data series to be deflated as a numeric vector
#' @param deflator The deflator series as numeric vector. The deflator series
#'   must be the same length as the data series and must not contain either
#'   NAs or zeros.
#' @param basepos The vector index of the baseline for the deflated series:
#'   this equivalent to the period in which all prices will be expressed. The
#'   default value is the index of the last value in the series i.e. the most
#'   recent period.
#' @return The deflated data series as a numeric vector.
#'
deflate <- function(data, deflator, basepos = length(data)) {

    # Check data and deflator are both numeric
    if (! is.numeric(data)) stop("The data is not a numeric vector.")
    if (! is.numeric(deflator)) stop("The deflator is not a numeric vector.")

    # Check data and deflator are the same length
    if (length(data) != length(deflator)) stop(
        "The data and deflator are not the same length.")

    # Check the defalator does not contain NAs or zeros
    if(any(is.na(deflator))) stop("The deflator contains NAs.")
    if(any(deflator == 0)) stop("The deflator contains zeros.")

    # Check the basepos is numeric
    if(! is.numeric(basepos)) stop (
        "The basepos is not numeric.")

    # Check the basepos has length 1
    if(length(basepos) != 1) stop (
        "The basepos must be a vector of length 1.")

    # Check basepos is in range
    if (basepos < 1 || basepos > length(data)) stop(
        "The basepos is out of range.")
}
