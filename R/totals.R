#' Get row totals for a set of columns in a dataframe without validation
#'
#' This is the raw version of \code{get_row_totals}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
get_row_totals_dfr <- function(data,
                               from = 2,
                               to = ncol(data),
                               na.rm = FALSE) {

    # Get data as a matrix and calculate row totals
    m <- data.matrix(data[from:to])
    rowSums(m, na.rm)
}

#' Get row totals for a set of columns in a dataframe
#'
#' Calculates row totals for a set of columns in a dataframe and returns them
#' as a vector. Use \code{add_row_totals} to append the row percentages to the
#' input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by row.
#' @param from The number or name of the column from which row totals are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The number or name of the column to which row totals are
#'   calculated. The default is ncol(data), which means row totals are
#'   calculated across all remaining columms in the dataframe.
#' @param na.rm A boolean which if TRUE ignores NAs in calculating totals. The
#'   default value is FALSE.
#' @return A numeric vetor of row totals
#' @export
#'
get_row_totals <- function(data,
                           from = 2,
                           to = ncol(data),
                           na.rm = FALSE) {

    run_dfr_func(get_row_totals_dfr, data, from, to, na.rm)
}

#' Add row totals for a set of columns in a dataframe without validation
#'
#' This is the raw version of \code{add_row_totals}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
add_row_totals_dfr <- function(data,
                               from = 2,
                               to = ncol(data),
                               na.rm = FALSE,
                               label = "total") {

    data[[label]] <- get_row_totals_dfr(data, from, to, na.rm)
    data
}

#' Add row totals for a set of columns in a dataframe
#'
#' Calculates row totals for a set of columns in a dataframe and add them as a
#' new column at the end of the dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by row.
#' @param from The number or name of the column from which row totals are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The number or name of the column to which row totals are
#'   calculated. The default is ncol(data), which means row totals are
#'   calculated across all remaining columms in the dataframe.
#' @param na.rm A boolean which if TRUE ignores NAs in calculating totals. The
#'   default value is FALSE.
#' @param label The label for the totals column. The default is "total".
#' @return A tibble containing row totals and any preceding columns.
#' @export
#'
add_row_totals <- function(data,
                           from = 2,
                           to = ncol(data),
                           na.rm = FALSE,
                           label = "total") {

    run_dfr_func(add_row_totals_dfr, data, from, to, na.rm, label)
}

#' Get column totals for a set of columns in a dataframe without validation
#'
#' This is the raw version of \code{get_col_totals}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
get_col_totals_dfr <- function(data,
                               from = 2,
                               to = ncol(data),
                               rows = 1:nrow(data),
                               na.rm = FALSE) {

    # Check rows is either numeric or logical
    if (! is.numeric(rows)) stop(
        "The rows argument is not numeric.")

    # Ensure row indices are unique
    if (length(rows) != length(unique(rows))) warning(
        "Duplicates in the rows argument were ignored.")

    rows <- unique(rows)

    # Ensure row indices are in range
    if (! all(rows %in% 1:nrow(data))) warning(
        "Out of range row numbers in the rows argument were ignored.")

    rows <- rows[rows %in% 1:nrow(data)]

    # Subset the dataframe to get just the target rows
    data <- data[rows, ]

    # Get data as a matrix and calculate row totals
    m <- data.matrix(data[from:to])
    colSums(m, na.rm)
}

#' Get column totals for a set of columns in a dataframe
#'
#' Calculates column totals for a set of columns in a dataframe and returns
#' them as a vector. Use \code{add_col_totals} to append the column
#' percentages to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by column.
#' @param from The number or name of the column from which column totals are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The number or name of the column to which column totals are
#'   calculated. The default is ncol(data), which means column totals are
#'   calculated across all remaining columms in the dataframe.
#' @param rows A numeric vector of the rows to be summed. The default is
#'   all rows in the dataframe.
#' @param na.rm A boolean which if TRUE ignores NAs in calculating totals. The
#'   default value is FALSE.
#' @return A numeric vetor of column totals
#' @export
#'
get_col_totals <- function(data,
                           from = 2,
                           to = ncol(data),
                           rows = 1:nrow(data),
                           na.rm = FALSE) {

    run_dfr_func(get_col_totals_dfr, data, from, to, rows, na.rm)
}

#' Add column totals for a set of columns in a dataframe without validation
#'
#' This is the raw version of \code{add_col_totals}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
add_col_totals_dfr <- function(data,
                               from = 2,
                               to = ncol(data),
                               rows = 1:nrow(data),
                               na.rm = FALSE,
                               label = "total",
                               lcols = 1) {

    # Convert label column names to numbers if necessary
    if (is.character(lcols)) lcols <- get_col_nums(data, lcols)

    # Get the column totals as a list for a totals row
    ct_row <- as.list(get_col_totals_dfr(data, from, to, rows, na.rm))

    # Remove invalid label column indices
    lcols <- unique(lcols)
    valid_lcols <- lcols %in% 1:ncol(data) & ! (lcols %in% from:to)
    lcols <- lcols[valid_lcols]

    # Get the names of the label columns
    label_colnames <- colnames(data)[lcols]

    # Add a label for each label column to the totals row ...
    for (lc in label_colnames) {
        # ... but only if it's a character vector
        if (is.character(data[[lc]])) ct_row[[lc]] <- label
    }

    # Add the totals row to the data and return
    dplyr::bind_rows(data, ct_row)
}

#' Add column totals for a set of columns in a dataframe
#'
#' Calculates column totals for a set of columns in a dataframe and add them
#' as a new row at the end of the dataframe. A label for the total row is
#' added to each of the columns indicated by the \code{label_col} argument.
#' Label columns must be character vectors. NAs are added to any columns
#' not summed or specified as label columns.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by column.
#' @param from The number or name of the column from which column totals are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The number or name of the column to which column totals are
#'   calculated. The default is ncol(data), which means column totals are
#'   calculated for all remaining columms in the dataframe.
#' @param rows A numeric vector of the rows to be summed. The default is
#'   all rows in the dataframe.
#' @param na.rm A boolean which if TRUE ignores NAs in calculating totals. The
#'   default value is FALSE.
#' @param label The label for the totals row. The default is "total".
#' @param lcols A vector or scalar containing the indices of the columns to
#'   to which to add the total label. These columns must be character vectors.
#'   The default is 1, assuming one column for row labels. Invalid label
#'   columns are silently ignored.
#' @return A tibble containing the input dataframe with an additional row for
#'   column totals.
#' @export
#'
add_col_totals <- function(data,
                           from = 2,
                           to = ncol(data),
                           rows = 1:nrow(data),
                           na.rm = FALSE,
                           label = "total",
                           lcols = 1) {

    run_dfr_func(add_col_totals_dfr, data, from, to, rows, na.rm, label, lcols)
}
