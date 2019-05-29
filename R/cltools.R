#' cltools: A Collection of Data Wrangling Tools for Statistical Researchers
#'
#' @docType package
#' @name cltools
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

# Tell R CMD check about new operators
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ":="))
}

