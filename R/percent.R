#' Calculate row percentages for a set of columns in a dataframe without
#' validation
#'
#' This is the raw version of \code{get_row_percent}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
get_row_percent_dfr <- function(data, from = 2, to = ncol(data)) {

    # Get data as a matrix and calculate row totals
    m <- data.matrix(data[from:to])
    rt <- rowSums(m)

    # Calculate row percentages (if all values are zero set total to NA)
    rt <- ifelse(rt == 0, NA, rt)
    rp <- m / rt

    # Convert to tibble and rebind any preceding columns
    rp <- tibble::as_tibble(rp)
    if (from > 1) rp <- dplyr::bind_cols(data[1:from - 1], rp)

    rp
}

#' Calculate row percentages for a set of columns in a dataframe
#'
#' Calculates row percentages for a set of columns in a dataframe and returns
#' those percentages along with any preceding columns containing row labels or
#' other data. Use \code{add_row_percent} to append the row percentages to the
#' input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as row percentages.
#' @param from The number or name of the column from which row percentages are
#'   calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The number or name of the column to which row percentages are
#'   calculated. The default is ncol(data), which means row percentages are
#'   calculated across all remaining columms in the dataframe.
#' @return A tibble containing row percentages and any preceding columns.
#' @export
#'
get_row_percent <- function(data, from = 2, to = ncol(data)) {

    run_dfr_func(get_row_percent_dfr, data, from, to)
}

#' Calculate row percentages for a set of columns in a dataframe and add
#' them to the dataframe without validation
#'
#' This is the raw version of \code{add_row_percent}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
add_row_percent_dfr <- function(data,
                                from = 2,
                                to = ncol(data),
                                prefix = "pc_") {

    # Get just the columns for calculating percentages
    data_cols <- data[from:to]

    # Get the row percentages
    rp <- get_row_percent_dfr(data_cols, from = 1)

    # Update the column names with the prefix
    colnames(rp) <- purrr::map_chr(
        colnames(rp),
        ~ paste(c(prefix, .x), collapse = ""))

    # Bind the percentage columns and return
    dplyr::bind_cols(data, rp)
}

#' Calculate row percentages for a set of columns in a dataframe and add
#' them to the dataframe
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as row percentages.
#' @param from The number or name of the column from which row percentages are
#'   calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The number or name of the column to which row percentages are
#'   calculated. The default is ncol(data), which means row percentages are
#'   calculated across all remaining columms in the dataframe.
#' @param prefix A string prefix to add to the column names to identify their
#'   percentage equivalents. The default is "pc_"
#' @return The input data with additional columns containing row percentages
#'   for columns specified by \code{from} and \code{to}.
#' @export
#'
add_row_percent <- function(data,
                            from = 2,
                            to = ncol(data),
                            prefix = "pc_") {

    run_dfr_func(add_row_percent_dfr, data, from, to, prefix)
}

#' Calculate column percentages for a set of columns in a dataframe without
#' validation
#'
#' This is the raw version of \code{get_col_percent}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
get_col_percent_dfr <- function(data, from = 2, to = ncol(data)) {

    # Get data as a matrix and calculate column totals
    m <- data.matrix(data[from:to])
    ct <- colSums(m)

    # Calculate column percentages (if all values are zero set total to NA)
    ct <- ifelse(ct == 0, NA, ct)
    cp <- t(t(m) / ct)

    # Convert to tibble and rebind any preceding columns
    cp <- tibble::as_tibble(cp)
    if (from > 1) cp <- dplyr::bind_cols(data[1:from - 1], cp)

    cp
}

#' Calculate column percentages for a set of columns in a dataframe
#'
#' Calculates column percentages for a set of colummns in a dataframe and
#' returns those percentages along with any preceding columns containing row
#' labels or other data. Use \code{add_col_percent} to append the column
#' percentages to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as column percentages.
#' @param from The number or name of the column from which column percentages
#'   are calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The number or name of the column to which column percentages are
#'   calculated. The default is ncol(data), which means column percentages are
#'   calculated across all remaining columms in the dataframe.
#' @return A tibble containing column percentages and any preceding columns.
#' @export
#'
get_col_percent <- function(data, from = 2, to = ncol(data)) {

    run_dfr_func(get_col_percent_dfr, data, from, to)
}

#' Calculate column percentages for a set of columns in a dataframe and add
#' them to the dataframe without validation
#'
#' This is the raw version of \code{add_col_percent}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
add_col_percent_dfr <- function(data,
                                from = 2,
                                to = ncol(data),
                                prefix = "pc_") {

    # Get just the columns for calculating percentages
    data_cols <- data[from:to]

    # Get the columns percentages
    cp <- get_col_percent(data_cols, from = 1)

    # Update the column names with the prefix
    colnames(cp) <- purrr::map_chr(
        colnames(cp),
        ~ paste(c(prefix, .x), collapse = ""))

    # Bind the percentage columns and return
    dplyr::bind_cols(data, cp)
}

#' Calculate column percentages for a set of columns in a dataframe and add
#' them to the dataframe
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as column percentages.
#' @param from The number or name of the column from which column percentages
#'   are calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The number or name of the column to which column percentages are
#'   calculated. The default is ncol(data), which means column percentages
#'   are calculated across all remaining columms in the dataframe.
#' @param prefix A string prefix to add to the column names to identify their
#'   percentage equivalents. The default is "pc_"
#' @return The input data with additional columns containing column
#'   percentages for columns specified by \code{from} and \code{to}.
#' @export
#'
add_col_percent <- function(data,
                            from = 2,
                            to = ncol(data),
                            prefix = "pc_") {

    run_dfr_func(add_col_percent_dfr, data, from, to, prefix)
}
