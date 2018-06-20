#' Get row totals for a set of columns in a dataframe
#'
#' Calculates row totals for a set of columns in a dataframe and returns them
#' as a vector. Use \code{add_row_totals} to append the row percentages to the
#' input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by row.
#' @param from The index of the column from which row totals are calculated.
#'   The default is 2, assuming one column for row labels. Use 1 if there are
#'   no preceding data columns.
#' @param to The index of the column to which row totals are calculated. The
#'   default is ncol(data), which means row totals are calculated across all
#'   remaining columms in the dataframe.
#' @return A numeric vetor of row totals
#' @export
#'
get_row_totals <- function(data, from = 2, to = ncol(data)) {

    # Check columns are valid
    if (! valid_columns(data, from, to)) stop()

    # Get data as a matrix and calculate row totals
    m <- data.matrix(data[from:to])
    rowSums(m)
}

#' Add row totals for a set of columns in a dataframe
#'
#' Calculates row totals for a set of columns in a dataframe and add them as a
#' new column at the end of the dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by row.
#' @param from The index of the column from which row totals are calculated.
#'   The default is 2, assuming one column for row labels. Use 1 if there are
#'   no preceding data columns.
#' @param to The index of the column to which row totals are calculated. The
#'   default is ncol(data), which means row totals are calculated across all
#'   remaining columms in the dataframe.
#' @param label The label for the totals column. The default is "total".
#' @return A tibble containing row totals and any preceding columns.
#' @export
#'
add_row_totals <- function(data, from = 2, to = ncol(data), label = "total") {

    data[[label]] <- get_row_totals(data, from, to)
    data
}

#' Get column totals for a set of columns in a dataframe
#'
#' Calculates column totals for a set of columns in a dataframe and returns
#' them as a vector. Use \code{add_col_totals} to append the column
#' percentages to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   summed by column.
#' @param from The index of the column from which column totals are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The index of the column to which column totals are calculated.
#'   The default is ncol(data), which means column totals are calculated
#'   across all remaining columms in the dataframe.
#' @return A numeric vetor of column totals
#' @export
#'
get_col_totals <- function(data, from = 2, to = ncol(data)) {

    # Check columns are valid
    if (! valid_columns(data, from, to)) stop()

    # Get data as a matrix and calculate row totals
    m <- data.matrix(data[from:to])
    colSums(m)
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
#' @param from The index of the column from which column totals are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The index of the column to which column totals are calculated.
#'   The default is ncol(data), which means column totals are calculated for
#'   all remaining columms in the dataframe.
#' @param label The label for the totals row. The default is "total".
#' @param lcols A vector or scalar containing the indices of the columns to
#'   to which to add the total label. These columns must be character vectors.
#'   The default is 1, assuming one column for row labels. Invalid label
#'   columns are silently ignored.
#' @return A tibble containing the input dataframe with an additional row for
#'   column totals.
#' @export
#'
add_col_totals <- function(
    data, from = 2, to = ncol(data), label = "total", lcols = 1) {

    # Get the column totals as a list for a totals row
    ct_row <- as.list(get_col_totals(data, from, to))

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
