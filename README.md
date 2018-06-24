# cltools

A collection of data wrangling tools for statistical researchers. This package is principally designed for use by researchers in the House of Commons Library but may be useful to anyone using R for routine data analysis. The package provides functions for manipulating tabular data stored in dataframes.

This package is in active development and I would welcome any feedback. Documentation and unit tests are being added with each new set of functions. I cannot currently guarantee there will be no breaking changes to the API as the package may evolve to reflect feedback from users.

## Installation

Install from GitHub using devtools.

``` r
install.packages("devtools")
devtools::install_github("olihawkins/cltools")
```

## Contents

* [Row and column totals](https://github.com/olihawkins/cltools#row-and-column-totals)
* [Row and column percentages](https://github.com/olihawkins/cltools#row-and-column-percentages)

### Row and column totals

Functions for row and column totals provide a convenient way to create and add row and column totals for data stored in columns in a dataframe. `get_row_totals` sums the values in each row across a set of columns containing numerical data. The target columns are specified with the `from` and `to` arguments. By default the function assumes that all columns other than the first column are to be summed.

``` r
data <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(1, 2, 3, 4, 5),
    c = c(6, 7, 8, 9, 10),
    d = c(11, 12, 13, 14, 15))

get_row_totals(data)
# [1] 18 21 24 27 30

get_row_totals(data, from = 3)
# [1] 17 19 21 23 25

get_row_totals(data, from = 2, to = 3)
# [1]  7  9 11 13 15
```

The `from` and `to` arguments cam be column names or column numbers.

``` r
get_row_totals(data, from = "c")
# [1] 17 19 21 23 25

get_row_totals(data, from = "b", to = "c")
# [1]  7  9 11 13 15
```

Row totals can be added to the input dataframe with `add_row_totals`. You can provide an alternative column name for the row totals with the `label` argument.

``` r
add_row_totals(data)
# # A tibble: 5 x 5
#   a         b     c     d total
#   <chr> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     6    11    18
# 2 B         2     7    12    21
# 3 C         3     8    13    24
# 4 D         4     9    14    27
# 5 E         5    10    15    30

add_row_totals(data, label = "all")
# # A tibble: 5 x 5
#   a         b     c     d   all
#   <chr> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     6    11    18
# 2 B         2     7    12    21
# 3 C         3     8    13    24
# 4 D         4     9    14    27
# 5 E         5    10    15    30
```

The equivalent functions `get_col_totals` and `add_col_totals` produce similar results for column totals but with some differences, which reflect the different behaviour of rows and columns in dataframes. For instance, the vector returned from `get_col_totals` is named with the column labels.

``` r
get_col_totals(data)
#  b  c  d 
# 15 40 65 
```

When adding column totals to a dataframe with `add_col_totals`, NAs are used for all columns that are not being summed. However, label columns can be specified so that a label can be used for the row of totals. By default, the function will try to add a label to the first column of the dataframe, if possible.

``` r
add_col_totals(data)
# # A tibble: 6 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         1     6    11
# 2 B         2     7    12
# 3 C         3     8    13
# 4 D         4     9    14
# 5 E         5    10    15
# 6 total    15    40    65
```

The `lcols` argument can be used to specify which columns should receive a label for totals. 

``` r
data <- tibble::tibble(
    a = LETTERS[1:5],
    b = letters[6:10],
    c = c(1, 2, 3, 4, 5),
    d = c(6, 7, 8, 9, 10),
    e = c(11, 12, 13, 14, 15))
    
add_col_totals(data, from = 3, lcols = c(1, 2))
# # A tibble: 6 x 5
#   a     b         c     d     e
#   <chr> <chr> <dbl> <dbl> <dbl>
# 1 A     f         1     6    11
# 2 B     g         2     7    12
# 3 C     h         3     8    13
# 4 D     i         4     9    14
# 5 E     j         5    10    15
# 6 total total    15    40    65
```

If the column indices specified with `lcols` are invalid for any reason they are silently ignored. Similarly, setting `lcols` to `NULL` will stop the function attempting to include any labels in the new row.

``` r
add_col_totals(data, from = "c", lcols = NULL)
# # A tibble: 6 x 5
#   a     b         c     d     e
#   <chr> <chr> <dbl> <dbl> <dbl>
# 1 A     f         1     6    11
# 2 B     g         2     7    12
# 3 C     h         3     8    13
# 4 D     i         4     9    14
# 5 E     j         5    10    15
# 6 NA    NA       15    40    65
```

As before, the `label` argument can be used to provide an alternative label for the totals.

``` r
add_col_totals(data, from = "c", label = "all", lcols = c(1, 2))
# # A tibble: 6 x 5
#   a     b         c     d     e
#   <chr> <chr> <dbl> <dbl> <dbl>
# 1 A     f         1     6    11
# 2 B     g         2     7    12
# 3 C     h         3     8    13
# 4 D     i         4     9    14
# 5 E     j         5    10    15
# 6 all   all      15    40    65
```

Adding column totals to a dataframe is not very "tidy", but may be useful as a final step before outputting tabular data for presentation.

### Row and column percentages

Functions for row and column percentages provide a convenient way to create and add row and column percentages for data stored in columns in a dataframe. These functions have a similar interface to the functions for row and column totals. The principal difference is that `get_row_percent` and `get_col_percent` return the target columns as percentages **as well as** any preceding data columns, which may be labels.

``` r
data <- tibble::tibble(
    a = LETTERS[1:3],
    b = c(1, 2, 3),
    c = c(1, 2, 3),
    d = c(2, 4, 6))

get_row_percent(data)
# # A tibble: 3 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A      0.25  0.25   0.5
# 2 B      0.25  0.25   0.5
# 3 C      0.25  0.25   0.5

get_col_percent(data)
# # A tibble: 3 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A     0.167 0.167 0.167
# 2 B     0.333 0.333 0.333
# 3 C     0.5   0.5   0.5 

```

By default percentages are calculated for all columns other than the first, but the target columns can be specified with the `from` and `to` arguments. Columns preceding `from` are included in the results, while columns following `to` are omitted. 

``` r
get_row_percent(data, from = 3)
# # A tibble: 3 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         1 0.333 0.667
# 2 B         2 0.333 0.667
# 3 C         3 0.333 0.667

get_col_percent(data, from = 3)
# # A tibble: 3 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         1 0.167 0.167
# 2 B         2 0.333 0.333
# 3 C         3 0.5   0.5  

get_row_percent(data, to = "c")
# # A tibble: 3 x 3
#   a         b     c
#   <chr> <dbl> <dbl>
# 1 A       0.5   0.5
# 2 B       0.5   0.5
# 3 C       0.5   0.5

get_col_percent(data, to = "c")
# # A tibble: 3 x 3
#   a         b     c
#   <chr> <dbl> <dbl>
# 1 A     0.167 0.167
# 2 B     0.333 0.333
# 3 C     0.5   0.5  

```

If you just want the percentages, remove any preceding columns from the input and target all columns from the first.

``` r
get_row_percent(data[c(-1)], from = 1)
# # A tibble: 3 x 3
#       b     c     d
#   <dbl> <dbl> <dbl>
# 1  0.25  0.25   0.5
# 2  0.25  0.25   0.5
# 3  0.25  0.25   0.5
```

Row and column percentages can be added to the input dataframe using `add_row_percent` and `add_col_percent`. These functions perform identical calculations to `get_row_percent` and `get_col_percent` but they return the full input dataframe with the percentage columns added.

``` r
data <- tibble::tibble(
    a = LETTERS[1:3],
    b = c(1, 2, 3),
    c = c(1, 2, 3),
    d = c(2, 4, 6))

add_row_percent(data)
# # A tibble: 3 x 7
#   a         b     c     d  pc_b  pc_c  pc_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2  0.25  0.25   0.5
# 2 B         2     2     4  0.25  0.25   0.5
# 3 C         3     3     6  0.25  0.25   0.5

add_col_percent(data)
# # A tibble: 3 x 7
#   a         b     c     d  pc_b  pc_c  pc_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.167 0.167 0.167
# 2 B         2     2     4 0.333 0.333 0.333
# 3 C         3     3     6 0.5   0.5   0.5  

```

As before, these functions take `from` and `to` arguments to specify the target rows.

``` r
add_row_percent(data, from = "c")
# # A tibble: 3 x 6
#   a         b     c     d  pc_c  pc_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.333 0.667
# 2 B         2     2     4 0.333 0.667
# 3 C         3     3     6 0.333 0.667

add_col_percent(data, to = "c")
# # A tibble: 3 x 6
#   a         b     c     d  pc_b  pc_c
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.167 0.167
# 2 B         2     2     4 0.333 0.333
# 3 C         3     3     6 0.5   0.5 
```

The percentage columns that are added to the dataframe take the name of their source column with a prefix. The default prefix is `pc_` but this can be set with the `prefix` argument.

``` r
add_row_percent(data, prefix = "percent_")
# # A tibble: 3 x 7
#   a         b     c     d percent_b percent_c percent_d
#   <chr> <dbl> <dbl> <dbl>     <dbl>     <dbl>     <dbl>
# 1 A         1     1     2      0.25      0.25       0.5
# 2 B         2     2     4      0.25      0.25       0.5
# 3 C         3     3     6      0.25      0.25       0.5
```

If a row or column sums to zero, the percentages calculated for that row or column will be NA. Similarly, if any of the values used to calculate a row or column of percentages is NA, all the percentages returned for that row or column will be NA.
