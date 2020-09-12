#' cltools: A Collection of Data Wrangling Tools for Statistical Researchers
#'
#' The cltools package provides a suit of tools for wrangling tabular data. The
#' package is particulary helpful for transforming tidy data into summary
#' tables for presentation. This package is principally designed for use by
#' researchers in the House of Commons Library but may be useful to anyone
#' using R for routine data analysis and statistical reporting.
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

