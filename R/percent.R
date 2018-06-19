#' Calculate row percentages for a set of columns in a dataframe
#'
#' Calculates row percentages for a set of colummns in a dataframe and returns
#' those percentages along with any columns containing row labels or other
#' preceding data. Use \code{add_row_percent} to append the row
#' percentages to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as row percentages.
#' @param from The index of the column from which row percentages will
#'   be calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The index of the column to which row percentages will be
#'   calculated. The default is NULL, which means row percentages will be
#'   calculated across all remaining columms in the dataframe.
#' @return A tibble containing the row percentages and any preceding columns.
#' @export
#'
get_row_percent <- function(data, from = 2, to = NULL) {

    # Set the "to" column if not specified
    if (is.null(to)) to <- ncol(data)

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given column numbers are not valid.")

    # Check columns are numeric
    if (! all(purrr::map_lgl(data[from:to], is.numeric))) stop (
        "The given columns are not all numeric.")

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

#' Calculate row percentages for a set of columns in a dataframe and add
#' them to the dataframe
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as row percentages.
#' @param from The index of the column from which row percentages will
#'   be calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The index of the column to which row percentages will be
#'   calculated. The default is NULL, which means row percentages will be
#'   calculated across all remaining columms in the dataframe.
#' @param prefix A string prefix to add to the column names to identify their
#'   percentage equivalents. The default is "pc_"
#' @return The input data with additional columns containing row percentages
#'   for columns specified by \code{from} and \code{to}.
#' @export
#'
add_row_percent <- function(data, from = 2, to = NULL, prefix = "pc_") {

    # Set the "to" column if not specified
    if (is.null(to)) to <- ncol(data)

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given column numbers are not valid.")

    # Get just the columns for calculating percentages
    data_cols <- data[from:to]

    # Check columns are numeric
    if (! all(purrr::map_lgl(data_cols, is.numeric))) stop (
        "The given columns are not all numeric.")

    # Get the row percentages
    rp <- get_row_percent(data_cols, from = 1)

    # Update the column names with the prefix
    colnames(rp) <- purrr::map_chr(
        colnames(rp),
        ~ paste(c(prefix, .x), collapse = ""))

    # Bind the percentage columns and return
    dplyr::bind_cols(data, rp)
}

#' Calculate column percentages for a set of columns in a dataframe
#'
#' Calculates column percentages for a set of colummns in a dataframe and returns
#' those percentages along with any columns containing row labels or other
#' preceding data. Use \code{add_col_percent} to append the column
#' percentages to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as column percentages.
#' @param from The index of the column from which column percentages will
#'   be calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The index of the column to which column percentages will be
#'   calculated. The default is NULL, which means column percentages will be
#'   calculated across all remaining columms in the dataframe.
#' @return A tibble containing the column percentages and any preceding columns.
#' @export
#'
get_col_percent <- function(data, from = 2, to = NULL) {

    # Set the "to" column if not specified
    if (is.null(to)) to <- ncol(data)

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given column numbers are not valid.")

    # Check columns are numeric
    if (! all(purrr::map_lgl(data[from:to], is.numeric))) stop (
        "The given columns are not all numeric.")

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

#' Calculate column percentages for a set of columns in a dataframe and add
#' them to the dataframe
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as column percentages.
#' @param from The index of the column from which column percentages will
#'   be calculated. The default is 2, assuming one column for row labels.
#'   Use 1 if there are no preceding data columns.
#' @param to The index of the column to which column percentages will be
#'   calculated. The default is NULL, which means column percentages will be
#'   calculated across all remaining columms in the dataframe.
#' @param prefix A string prefix to add to the column names to identify their
#'   percentage equivalents. The default is "pc_"
#' @return The input data with additional columns containing column
#'   percentages for columns specified by \code{from} and \code{to}.
#' @export
#'
add_col_percent <- function(data, from = 2, to = NULL, prefix = "pc_") {

    # Set the "to" column if not specified
    if (is.null(to)) to <- ncol(data)

    # Check column indices are sane
    if (from < 1 || to > ncol(data) || from > to) stop(
        "The given column numbers are not valid.")

    # Get just the columns for calculating percentages
    data_cols <- data[from:to]

    # Check columns are numeric
    if (! all(purrr::map_lgl(data_cols, is.numeric))) stop (
        "The given columns are not all numeric.")

    # Get the columns percentages
    cp <- get_col_percent(data_cols, from = 1)

    # Update the column names with the prefix
    colnames(cp) <- purrr::map_chr(
        colnames(cp),
        ~ paste(c(prefix, .x), collapse = ""))

    # Bind the percentage columns and return
    dplyr::bind_cols(data, cp)
}
