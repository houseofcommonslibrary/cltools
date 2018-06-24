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
                        start = 100,
                        baserow = 1,
                        basevals = NULL) {

    # Check the start value is valid
    if (! is.numeric(start) || start < 1) stop(
        "The start value for the index must be a positive number above one.")

    # Check the baseline values given how they are set
    if (is.null(basevals)) {

        # Check the index and values of the baseline row if basevals is NULL
        if (baserow < 1 && baserow > nrow(data)) stop(
            "The baserow is out of range.")

        if (0 %in% data[baserow, from:to]) stop(
            "The baserow contains zeros.")

    } else {

        # Check the baseline values themselves if set explicitly
        if (length(basevals) != (to - from + 1)) stop(
            "The number of basevals and target columns are not equal."
        )

        if (0 %in% basevals) stop(
            "The basevals contain zeros.")
    }
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
#' @param start The starting value for the baseline. The default is 100.
#'   The start value must be a positive number greater than zero.
#' @param baserow The number of the row to be used as the baseline. The
#'   default is 1. The baseline row must contain no zero values. This argument
#'   is ignored if base values are set explcitly with \code{basevals}.
#' @param basevals Specifies the values to use for the baseline explicitly.
#'   Use this if the baseline values to be used are not represented in any
#'   given row. The default is NULL, which means the function will use the
#'   values in the \code{baserow} instead. The baseline values must not
#'   contain zeros.
#' @return A tibble containing indices and any preceding columns.
#' @export
#'
get_indices <- function(data,
                        from = 2,
                        to = ncol(data),
                        start = 100,
                        baserow = 1,
                        basevals = NULL) {

    run_dfr_func(get_indices_dfr, data, from, to, start, baserow, basevals)
}
