#' Count the frequency of discrete values in a vector
#'
#' Calculates the frequency of discrete values in a vector and returns the
#' figures as a tibble. The results can be sorted by value or by count.
#'
#' @param values A vector of values from which frequencies will be calculated.
#' @param by The column by which to sort the results. Must be one of either
#'   "count" or "value". The default is "count".
#' @param order The order in which to sort the results. Must be one of either
#'   "asc" or "desc". The default depends on the value of the \code{by}
#'   argument. If \code{by} is "count" the default \code{order} is "desc". If
#'   \code{by} is "value" the default \code{order} is "asc".
#' @param name The column name to use for the values column in the results. By
#'   default, the variable name of the input vector is used.
#' @return A tibble showing the frequency of each value in the input vector.
#' @export

value_counts <- function(
    values,
    by = "count",
    order = ifelse(by == "value", "asc", "desc"),
    name = deparse(substitute(values))) {

    # Check arguments are valid
    if (! by %in% c("count", "value")) {
        stop("Invalid \"by\" argument. Must be either \"count\" or \"value\".")
    }

    if (! order %in% c("asc", "desc")) {
        stop("Invalid \"by\" argument. Must be either \"asc\" or \"desc\".")
    }

    # If the name is a dataframe column name, extract the column name
    if (stringr::str_detect(name, "\\$")) {
        name <- stringr::str_sub(stringr::str_extract(name, "\\$.*$"), 2)
    }

    # Set the sort properties for the call to arrange
    by_col <- ifelse(by == "count", "count", name)
    order_func <- ifelse(order == "desc", dplyr::desc, function(d){d})

    # Create the results dataframe and return
    tibble::as_tibble(table(values), .name_repair = ~ c(name, "count")) %>%
        dplyr::arrange(order_func(.data[[by_col]])) %>%
        dplyr::mutate(percent = .data$count / sum(.data$count))
}
