#' Create a statistical index from a numeric vector given a baseline vector
#' index or a baseline value
#'
#' Creates a statistical index from a numeric vector given a baseline
#' vector index or a baseline value.
#'
#' @param data A numeric vector containing values to be expressed as an index.
#' @param base The baseline value of the created index. The default is 100.
#'   The base must be a positive number greater than zero.
#' @param basepos The vector index of the value to be used as the baseline for
#'   calculating the index. The default is 1. The value at basepos must not be
#'   zero. This argument is ignored if the base value is set explcitly with
#'   \code{baseval}.
#' @param baseval Specifies explicitly the value to use as the baseline when
#'   calculating the index. Use this if the baseline value to be used is not
#'   represented in the vector. The default is NULL, which means the function
#'   will use the value in the \code{basepos} instead. The baseline value
#'   must not be zero.
#' @return A vector containing the created index.
#' @export
#'
get_index <- function(data, base = 100, basepos = 1, baseval = NULL) {

    # Check the vector is numeric
    if (! is.numeric(data)) stop ("The data is not a numeric vector.")

    # Check the base has length 1
    if(length(base) != 1) stop (
        "The base must be a vector of length 1.")

    # Check the base is valid
    if (! is.numeric(base) || base < 1) stop(
        "The base for the index must be a positive number above one.")

    # Check the baseline value given how it is set
    if (is.null(baseval)) {

        # Check the basepos is numeric
        if(! is.numeric(basepos)) stop (
            "The basepos is not numeric.")

        # Check the basepos has length 1
        if(length(basepos) != 1) stop (
            "The basepos must be a vector of length 1.")

        # Check the index and value of the basepos if baseval is NULL
        if (basepos < 1 || basepos > length(data)) stop(
            "The basepos is out of range.")

        if (is.na(data[basepos])) stop(
            "The value at the basepos should not be NA.")

        if (data[basepos] == 0) stop(
            "The value at the basepos should not be zero.")

        baseval <- data[basepos]

    } else {

        # Check the baseval is numeric
        if(! is.numeric(baseval)) stop (
            "The baseval is not numeric.")

        # Check the baseval has length 1
        if(length(baseval) != 1) stop (
            "The baseval must be a vector of length 1.")

        if (is.na(baseval)) stop(
            "The baseval should not be NA.")

        if (baseval == 0) stop(
            "The baseval should not be zero.")
    }

    data / baseval * base
}

#' Create indices from columns in a dataframe given a baseline row or a vector
#' of baseline values without validation
#'
#' This is the raw version of \code{get_indices}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
get_indices_dfr <- function(data,
                        from = 2,
                        to = ncol(data),
                        base = 100,
                        baserow = 1,
                        basevals = NULL) {

    # Check the baseline values given how they are set
    if (is.null(basevals)) {

        # Check baserow is numeric
        if(! is.numeric(baserow)) stop (
            "The baserow argument must be numeric.")

        # Check baserow has length 1
        if(length(baserow) != 1) stop (
            "The baserow must be a vector of length 1.")

        # Check the index of the baseline row
        if (baserow < 1 || baserow > nrow(data)) stop(
            "The baserow is out of range.")

        if (any(is.na(data[baserow, from:to]))) stop(
            "The baserow should not contain NAs.")

        if (0 %in% data[baserow, from:to]) stop(
            "The baserow should not contain zeros.")

        basevals <- data[baserow, from:to]

    } else {

        # Check basevals are numeric
        if(! is.numeric(basevals)) stop (
            "The basevals argument must be numeric.")

        # Check the baseline values themselves if set explicitly
        if (length(basevals) != (to - from + 1)) stop(
            "The number of basevals and target columns are not equal.")

        if (any(is.na(basevals))) stop(
            "The basevals should not contain NAs.")

        if (0 %in% basevals) stop(
            "The basevals should not contain zeros.")
    }


    # Great an index for each target column and return as a tibble
    indices <- purrr::map2_dfc(from:to,
                               basevals,
                               ~ get_index(data[[.x]],
                                           base = base,
                                           baseval = .y))

    # Restore the column names
    colnames(indices) <- names(data)[from:to]

    # Rebind any preceding columns
    if (from > 1) indices <- dplyr::bind_cols(data[1:from - 1], indices)

    indices
}

#' Create indices from columns in a dataframe given a baseline row or a vector
#' of baseline values
#'
#' Creates indices from a set of columns in a dataframe using a given row
#' number, or vector of values, as the baseline. Use \code{add_indices} to
#' append the indices to the input dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as indices.
#' @param from The number or name of the column from which indices are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The number or name of the column to which indices are calculated.
#'   The default is ncol(data), which means indices are calculated across all
#'   remaining columms in the dataframe.
#' @param base The baseline value of the created index. The default is 100.
#'   The base must be a positive number greater than zero.
#' @param baserow The number of the row to be used as the baseline for
#'   calculating the indices. The default is 1. The baseline row must contain
#'   no zero values. This argument is ignored if the base values are set
#'   explicitly with \code{basevals}.
#' @param basevals Specifies explicitly the values to use as the baseline when
#'   calculating the indices. Use this if the baseline values to be used are not
#'   represented in any given row. The default is NULL, which means the
#'   function will use the values in the \code{baserow} instead. The baseline
#'   values must not contain zeros.
#' @return A tibble containing indices and any preceding columns.
#' @export
#'
get_indices <- function(data,
                        from = 2,
                        to = ncol(data),
                        base = 100,
                        baserow = 1,
                        basevals = NULL) {

    run_dfr_func(get_indices_dfr, data, from, to, base, baserow, basevals)
}

#' Create indices from columns in a dataframe given a baseline row or a vector
#' of baseline values and add them to the dataframe without validation
#'
#' This is the raw version of \code{add_indices}. It should only be used
#' internally within the package inside functions which provide validation.
#'
#' @keywords internal
#'
add_indices_dfr <- function(data,
                            from = 2,
                            to = ncol(data),
                            base = 100,
                            baserow = 1,
                            basevals = NULL,
                            prefix = "ix_") {

    # Get just the columns for calculating indices
    data_cols <- data[from:to]

    # Get the column indices
    indices <- get_indices_dfr(data_cols,
                               from = 1,
                               base = base,
                               baserow = baserow,
                               basevals = basevals)

    # Update the column names with the prefix
    colnames(indices) <- purrr::map_chr(colnames(indices),
                                        ~ paste(c(prefix, .x), collapse = ""))

    # Bind the index columns and return
    dplyr::bind_cols(data, indices)
}

#' Create indices from columns in a dataframe given a baseline row or a vector
#' of baseline values and add them to the datafame
#'
#' Creates indices from a set of columns in a dataframe using a given row
#' number, or vector of values, as the baseline and adds them to the
#' dataframe.
#'
#' @param data A dataframe containing columns of numerical data to be
#'   expressed as indices.
#' @param from The number or name of the column from which indices are
#'   calculated. The default is 2, assuming one column for row labels. Use 1
#'   if there are no preceding data columns.
#' @param to The number or name of the column to which indices are calculated.
#'   The default is ncol(data), which means indices are calculated across all
#'   remaining columms in the dataframe.
#' @param base The baseline value of the created index. The default is 100.
#'   The base must be a positive number greater than zero.
#' @param baserow The number of the row to be used as the baseline for
#'   calculating the indices. The default is 1. The baseline row must contain
#'   no zero values. This argument is ignored if the base values are set
#'   explicitly with \code{basevals}.
#' @param basevals Specifies explicitly the values to use as the baseline when
#'   calculating the indices. Use this if the baseline values to be used are not
#'   represented in any given row. The default is NULL, which means the
#'   function will use the values in the \code{baserow} instead. The baseline
#'   values must not contain zeros.
#' @param prefix A string prefix to add to the column names to identify their
#'   index equivalents. The default is "ix_".
#' @return The input data with additional columns containing indices for
#'   columns specified by \code{from} and \code{to}.
#' @export
#'
add_indices <- function(data,
                        from = 2,
                        to = ncol(data),
                        base = 100,
                        baserow = 1,
                        basevals = NULL,
                        prefix = "ix_") {

    run_dfr_func(add_indices_dfr,
                 data,
                 from,
                 to,
                 base,
                 baserow,
                 basevals,
                 prefix)
}

