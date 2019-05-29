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
#'   default value is the index of the last value in the series, which is
#'   assumed to be the most recent period.
#' @return The deflated data series as a numeric vector.

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

    deflator_index <- deflator[basepos] / deflator
    data * deflator_index
}

#' Create real terms series from columns in a dataframe given a deflator
#' series and a baseline row without validation
#'
#' This is the raw version of \code{get_real}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal

get_real_dfr <- function(data,
                         deflator,
                         from = 2,
                         to = ncol(data),
                         baserow = nrow(data)) {

    # Check deflator is numeric
    if (! is.numeric(deflator)) stop(
        "The deflator is not a numeric vector.")

    # Check the deflator is the same length as the columns
    if (length(deflator) != nrow(data)) stop(
        "The data and deflator are not the same length.")

    # Check baserow is numeric
    if(! is.numeric(baserow)) stop (
        "The baserow argument must be numeric.")

    # Check baserow has length 1
    if(length(baserow) != 1) stop (
        "The baserow must be a vector of length 1.")

    # Check the index of the baseline row
    if (baserow < 1 || baserow > nrow(data)) stop(
        "The baserow is out of range.")

    # Great an index for each target column and return as a tibble
    deflated <- purrr::map_dfc(from:to, ~ deflate(data[[.x]],
                                                  deflator,
                                                  basepos = baserow))

    # Restore the column names
    colnames(deflated) <- names(data)[from:to]

    # Rebind any preceding columns
    if (from > 1) deflated <- dplyr::bind_cols(data[1:from - 1], deflated)

    deflated
}

#' Create real terms series from columns in a dataframe given a deflator
#' series and a baseline row
#'
#' Creates real terms series from a set of columns in a dataframe using the
#' given row deflator series and the number of the row to use as the baseline.
#' The deflator series must have the same length as the number of rows in the
#' dataframe. If the deflator series is already in the dataframe, then pass it
#' into the function with \code{data$deflator_column}. The deflator cannot
#' contain NA or zero values. Use \code{add_real} to append the real terms
#' series to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed in real terms.
#' @param deflator The deflator series as numeric vector. The deflator series
#'   must be the same length as the data series and must not contain either
#'   NAs or zeros.
#' @param from The number or name of the column from which real terms series
#'   are calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The number or name of the column to which real terms series are
#'   calculated. The default is ncol(data), which means real terms series are
#'   calculated across all remaining columms in the dataframe.
#' @param baserow The number of the row to be used as the baseline for
#'   calculating the real terms series. The default is the last row in the
#'   dataframe, which is assumed to be the most recent period.
#' @return A tibble containing real terms series and any preceding columns.
#' @export

get_real <- function(data,
                     deflator,
                     from = 2,
                     to = ncol(data),
                     baserow = nrow(data)) {

    run_dfr_func(get_real_dfr,
                 data,
                 deflator,
                 from = from,
                 to = to,
                 baserow = baserow)
}

#' Create real terms series from columns in a dataframe given a deflator
#' series and a baseline row and add them to the dataframe without validation
#'
#' This is the raw version of \code{add_real}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal

add_real_dfr <- function(data,
                         deflator,
                         from = 2,
                         to = ncol(data),
                         baserow = nrow(data),
                         prefix = "rt_") {

    # Get just the columns for calculating real terms series
    data_cols <- data[from:to]

    # Get the column indices
    series <- get_real_dfr(data_cols,
                           deflator,
                           from = 1,
                           baserow = baserow)

    # Update the column names with the prefix
    colnames(series) <- purrr::map_chr(colnames(series),
                                        ~ paste(c(prefix, .x), collapse = ""))

    # Bind the series columns and return
    dplyr::bind_cols(data, series)
}

#' Create real terms series from columns in a dataframe given a deflator
#' series and a baseline row and add them to the dataframe
#'
#' Creates real terms series from a set of columns in a dataframe using the
#' given row deflator series and the number of the row to use as the baseline
#' add adds them to the dataframe. The deflator series must have the same
#' length as the number of rows in the dataframe. If the deflator series is
#' already in the dataframe, then pass it into the function with
#' \code{data$deflator_column}. The deflator cannot contain NA or zero values.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed in real terms.
#' @param deflator The deflator series as numeric vector. The deflator series
#'   must be the same length as the data series and must not contain either
#'   NAs or zeros.
#' @param from The number or name of the column from which real terms series
#'   are calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The number or name of the column to which real terms series are
#'   calculated. The default is ncol(data), which means real terms series are
#'   calculated across all remaining columms in the dataframe.
#' @param baserow The number of the row to be used as the baseline for
#'   calculating the real terms series. The default is the last row in the
#'   dataframe, which is assumed to be the most recent period.
#' @param prefix A string prefix to add to the column names to identify their
#'   index equivalents. The default is "ix_".
#' @return A tibble containing real terms series and any preceding columns.
#' @export

add_real <- function(data,
                     deflator,
                     from = 2,
                     to = ncol(data),
                     baserow = nrow(data),
                     prefix = "rt_") {

    run_dfr_func(add_real_dfr,
                 data,
                 deflator,
                 from = from,
                 to = to,
                 baserow = baserow,
                 prefix = prefix)
}
