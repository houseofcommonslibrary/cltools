#' Count the frequency of discrete values in a vector
#'
#' Calculates the frequency of discrete values in a vector and returns the
#' counts as a tibble. The results can be sorted by value or by count.
#'
#' @param values A vector of values from which frequencies will be calculated.
#' @param by The column name by which to sort the results as a string. Must be
#'   one of either "count" or "value". The default is "count".
#' @param name The column name to use for the values column in the results as
#'   a string. By default, the variable name of the input vector is used.
#' @param order The order in which to sort the results as a string. Must be
#'   one of either "asc" or "desc". The default depends on the value of the
#'   \code{by} argument. If \code{by} is "count" the default \code{order} is
#'   "desc". If \code{by} is "value" the default \code{order} is "asc".
#' @param na.rm A boolean indicating whether to exclude NAs from the results.
#'   The default is FALSE.
#' @return A tibble showing the frequency of each value in the input vector.
#' @export

value_counts <- function(
    values,
    name = NULL,
    by = "count",
    order = ifelse(by == "value", "asc", "desc"),
    na.rm = FALSE) {

    # Get the variable name of the values argument
    obj_name <- deparse(substitute(values))

    # Check the values argument is not null and is a vector
    if (is.null(values)) {
        stop(stringr::str_c("object '", obj_name, "' not found"))
    }

    if (! is.atomic(values)) {
        stop(stringr::str_c("object '", obj_name, "' is not an atomic vector"))
    }

    if (length(values) == 0) {
        stop(stringr::str_c("object '", obj_name, "' is empty"))
    }

    # If name is not provided get the variable name
    if (is.null(name)) {
        name <- obj_name
    }

    # Check the sort arguments are valid
    if (! by %in% c("count", "value")) {
        stop("Invalid \"by\" argument. Must be either \"count\" or \"value\".")
    }

    if (! order %in% c("asc", "desc")) {
        stop("Invalid \"by\" argument. Must be either \"asc\" or \"desc\".")
    }

    # Check the na.rm argument is valid
    if (! is.logical(na.rm)) {
        stop("Invalid \"na.rm\" argument. Must be either TRUE or FALSE.")
    }

    # Set the sort properties for the call to arrange
    by_col <- ifelse(by == "count", "count", name)
    order_func <- ifelse(order == "desc", dplyr::desc, function(d){d})

    # Set option for handling NAs
    use_na <- ifelse(na.rm, "no", "ifany")

    # Create the results dataframe and return
    tibble::as_tibble(
            table(values, useNA = use_na),
            .name_repair = ~ c(name, "count")) %>%
        dplyr::arrange(order_func(.data[[by_col]])) %>%
        dplyr::mutate(percent = .data$count / sum(.data$count))
}
