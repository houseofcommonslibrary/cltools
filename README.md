# cltools

A collection of data wrangling tools for statistical researchers. This package is principally designed for use by researchers in the House of Commons Library but may be useful to anyone using R for routine data analysis.

The package provides functions for manipulating tabular data stored in dataframes. It is desgined to make it easier to perform routine transformations of data that researchers often use, but which can be error prone when done in Excel, especially with larger datasets. An experienced R user could accomplish most of these operations directly without much difficulty, but a design goal of this package is to make it easier for researchers to be productive with R at lower levels of expertise.

This package is in active development and I would welcome any feedback. The currently available API is set out in the [contents](https://github.com/houseofcommonslibrary/cltools#contents) below. Any functions documented here are ready to use: they have good test coverage and documentation. Further functions may be added over time. I cannot currently guarantee there will be no breaking changes to the API in future, as the package is new and may evolve in response to feedback from users. However, functions documented in this README are less likely to change than those in development.

## Installation

Install from GitHub using devtools.

``` r
install.packages("remotes")
remotes::install_github("houseofcommonslibrary/cltools")
```

## Contents

* [Core principles](https://github.com/houseofcommonslibrary/cltools#core-principles)
* [Row and column totals](https://github.com/houseofcommonslibrary/cltools#row-and-column-totals)
* [Row and column percentages](https://github.com/houseofcommonslibrary/cltools#row-and-column-percentages)
* [Missing values](https://github.com/houseofcommonslibrary/cltools#missing-values)
* [Statistical indices](https://github.com/houseofcommonslibrary/cltools#statistical-indices)
* [Deflators and real terms series](https://github.com/houseofcommonslibrary/cltools#deflators-and-real-terms-series)

### Core principles

The functions in this package are designed to operate on tabular data stored in dataframes and tibbles. A common pattern involves producing new data from a set of columns in the dataframe. Target columns are specified with `from` and `to` arguments. These arguments can be numerical column indices or column names as strings. If the columns you want to process are not adjacent to one another, simply reorder them with [dplyr::select](https://dplyr.tidyverse.org/reference/select.html) -- some good examples of how to use `select` can be found [here](http://r4ds.had.co.nz/transform.html#select).

Functions prefixed `get_` will return the newly generated data. In some cases, the newly generated data will be returned along with any columns preceding the target columns in the input dataframe, which are presumed to be labels or other data you want to preserve. Functions prefixed with `add_` return the whole of the input dataframe with the newly generated columns of data appended to the end.

### Row and column totals

Functions for row and column totals provide a simple way to create and add row and column totals for data stored in columns in a dataframe. `get_row_totals` sums the values in each row across a set of columns containing numerical data. By default the function assumes that all columns other than the first column are to be summed.

``` r
data <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(1, 2, 3, 4, 5),
    c = c(6, 7, 8, 9, 10),
    d = c(11, 12, 13, 14, 15))

data
# # A tibble: 5 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         1     6    11
# 2 B         2     7    12
# 3 C         3     8    13
# 4 D         4     9    14
# 5 E         5    10    15

get_row_totals(data)
# [1] 18 21 24 27 30

get_row_totals(data, from = 3)
# [1] 17 19 21 23 25

get_row_totals(data, from = 2, to = 3)
# [1]  7  9 11 13 15
```

Alternatively, you can use column names for the `from` and `to` arguments.

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

The equivalent functions `get_col_totals` and `add_col_totals` produce similar results for column totals but with some differences, which reflect the different behaviour of rows and columns in dataframes. For example, the vector returned from `get_col_totals` is named with the column labels.

``` r
get_col_totals(data)
#  b  c  d 
# 15 40 65 
```

You can produce a column total for just a subset of rows by providing a vector of row numbers to the `rows` argument.

``` r
get_col_totals(data, rows = 1:3)
#  b  c  d 
#  6 21 36 
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

As before, you can produce a column total for just a subset of rows by providing a vector of row numbers to the `rows` argument.

``` r
add_col_totals(data, rows = 1:3, label = "A-C")
# # A tibble: 6 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         1     6    11
# 2 B         2     7    12
# 3 C         3     8    13
# 4 D         4     9    14
# 5 E         5    10    15
# 6 A-C       6    21    36
```

The `lcols` argument can be used to specify which columns should receive the label for totals. You can use column numbers or column names.

``` r
data <- tibble::tibble(
    a = LETTERS[1:5],
    b = letters[6:10],
    c = c(1, 2, 3, 4, 5),
    d = c(6, 7, 8, 9, 10),
    e = c(11, 12, 13, 14, 15))

data
# # A tibble: 5 x 5
#   a     b         c     d     e
#   <chr> <chr> <dbl> <dbl> <dbl>
# 1 A     f         1     6    11
# 2 B     g         2     7    12
# 3 C     h         3     8    13
# 4 D     i         4     9    14
# 5 E     j         5    10    15
    
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

add_col_totals(data, from = 3, lcols = "b")
# # A tibble: 6 x 5
#   a     b         c     d     e
#   <chr> <chr> <dbl> <dbl> <dbl>
# 1 A     f         1     6    11
# 2 B     g         2     7    12
# 3 C     h         3     8    13
# 4 D     i         4     9    14
# 5 E     j         5    10    15
# 6 NA    total    15    40    65
```

If the column indices specified with `lcols` are invalid for any reason they will be silently ignored - in particular, watch out for columns that are factors rather than strings: `add_col_totals` won't add labels to those. Setting `lcols` to `NULL` will stop the function attempting to include any labels in the new row.

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

Adding column totals to a dataframe is not [tidy](https://en.wikipedia.org/wiki/Tidy_data), but may be useful as a final step before outputting tabular data for presentation.

### Row and column percentages

Functions for row and column percentages provide a simple way to create and add row and column percentages for data stored in columns in a dataframe. These functions have a similar interface to the functions for row and column totals. The principal difference is that `get_row_percent` and `get_col_percent` return the target columns as percentages **as well as** any preceding data columns, which may be labels.

``` r
data <- tibble::tibble(
    a = LETTERS[1:3],
    b = c(1, 2, 3),
    c = c(1, 2, 3),
    d = c(2, 4, 6))

data
# # A tibble: 3 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         1     1     2
# 2 B         2     2     4
# 3 C         3     3     6

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

By default percentages are calculated for all columns other than the first, but the target columns can be specified with the `from` and `to` arguments, which can either be column numbers or column names. Columns preceding `from` are included in the results, while columns following `to` are omitted. 

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
add_row_percent(data)
# # A tibble: 3 x 7
#   a         b     c     d  rp_b  rp_c  rp_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2  0.25  0.25   0.5
# 2 B         2     2     4  0.25  0.25   0.5
# 3 C         3     3     6  0.25  0.25   0.5

add_col_percent(data)
# # A tibble: 3 x 7
#   a         b     c     d  cp_b  cp_c  cp_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.167 0.167 0.167
# 2 B         2     2     4 0.333 0.333 0.333
# 3 C         3     3     6 0.5   0.5   0.5  

```

As before, these functions take `from` and `to` arguments to specify the target columns.

``` r
add_row_percent(data, from = "c")
# # A tibble: 3 x 6
#   a         b     c     d  rp_c  rp_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.333 0.667
# 2 B         2     2     4 0.333 0.667
# 3 C         3     3     6 0.333 0.667

add_col_percent(data, to = "c")
# # A tibble: 3 x 6
#   a         b     c     d  cp_b  cp_c
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.167 0.167
# 2 B         2     2     4 0.333 0.333
# 3 C         3     3     6 0.5   0.5 
```

The percentage columns that are added to the dataframe take the name of their source column with a prefix. The default prefix for row percentages is `rp_`, and the default prefix for column percentages is `cp_`, but these can be set with the `prefix` argument.

``` r
add_row_percent(data, prefix = "percent_")
# # A tibble: 3 x 7
#   a         b     c     d percent_b percent_c percent_d
#   <chr> <dbl> <dbl> <dbl>     <dbl>     <dbl>     <dbl>
# 1 A         1     1     2      0.25      0.25       0.5
# 2 B         2     2     4      0.25      0.25       0.5
# 3 C         3     3     6      0.25      0.25       0.5
```

### Missing values

By default, if any of the values used to calculate totals or percentages is NA, the values returned for that row or column will be NA.

``` r
data <- tibble::tibble(
    a = LETTERS[1:6],
    b = c(2, 2, 2, 2, 2, 20),
    c = c(4, 4, 4, 4, 4, 40),
    d = c(6, 6, 6, 6, 6, NA))

data
# # A tibble: 6 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A         2     4     6
# 2 B         2     4     6
# 3 C         2     4     6
# 4 D         2     4     6
# 5 E         2     4     6
# 6 F        20    40    NA

get_row_totals(data)
# [1] 12 12 12 12 12 NA

get_row_percent(data)
# # A tibble: 6 x 4
#   a          b      c     d
#   <chr>  <dbl>  <dbl> <dbl>
# 1 A      0.167  0.333   0.5
# 2 B      0.167  0.333   0.5
# 3 C      0.167  0.333   0.5
# 4 D      0.167  0.333   0.5
# 5 E      0.167  0.333   0.5
# 6 F     NA     NA      NA 

get_col_totals(data)
#  b  c  d 
# 30 60 NA

get_col_percent(data)
# # A tibble: 6 x 4
#   a          b      c     d
#   <chr>  <dbl>  <dbl> <dbl>
# 1 A     0.0667 0.0667    NA
# 2 B     0.0667 0.0667    NA
# 3 C     0.0667 0.0667    NA
# 4 D     0.0667 0.0667    NA
# 5 E     0.0667 0.0667    NA
# 6 F     0.667  0.667     NA
```

However, these functions will ignore NAs if the `na.rm` argument is set to `TRUE`.

``` r
get_row_totals(data, na.rm = TRUE)
# [1] 12 12 12 12 12 60

get_row_percent(data, na.rm = TRUE)
# # A tibble: 6 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A     0.167 0.333   0.5
# 2 B     0.167 0.333   0.5
# 3 C     0.167 0.333   0.5
# 4 D     0.167 0.333   0.5
# 5 E     0.167 0.333   0.5
# 6 F     0.333 0.667  NA

get_col_totals(data, na.rm = TRUE)
# b  c  d 
# 30 60 30 

get_col_percent(data, na.rm = TRUE)
# # A tibble: 6 x 4
#   a          b      c     d
#   <chr>  <dbl>  <dbl> <dbl>
# 1 A     0.0667 0.0667   0.2
# 2 B     0.0667 0.0667   0.2
# 3 C     0.0667 0.0667   0.2
# 4 D     0.0667 0.0667   0.2
# 5 E     0.0667 0.0667   0.2
# 6 F     0.667  0.667   NA 
```

If a row or column sums to zero, percentages calculated for that row or column will be NAs. This happens irrespective of the value of `na.rm`: you can't use zero as a denominator.

### Statistical indices

Functions for statistical indices provide a simple way to create indices for data stored in columns in a dataframe. These functions use the `from` and `to` arguments to identify the target columns. By default they target all columns other than the first. 

`get_indices` returns a dataframe containing the indices created for each of the target columns along with any columns preceding the targets. This function uses several arguments other than the input dataframe to control the properties of the indices created.

The `base` is the value of the baseline as represented in the created index: the default `base` is 100. The `baserow` is the row number of the values to set to the `base`. In each index all other values are expressed relative to the value that is in the `baserow` for that column. The default value for the `baserow` is 1.

``` r
data <- tibble::tibble(
    a = LETTERS[1:4],
    b = c(200, 250, 300, 400),
    c = c(400, 300, 200, 100),
    d = c(50, 100, 150, 0))

data    
# # A tibble: 4 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A       200   400    50
# 2 B       250   300   100
# 3 C       300   200   150
# 4 D       400   100     0

get_indices(data)
# # A tibble: 4 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A       100   100   100
# 2 B       125    75   200
# 3 C       150    50   300
# 4 D       200    25     0

get_indices(data, base = 1000)
# A tibble: 4 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A      1000  1000  1000
# 2 B      1250   750  2000
# 3 C      1500   500  3000
# 4 D      2000   250     0

get_indices(data, base = 1000, baserow = 2)
# # A tibble: 4 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A       800 1333.   500
# 2 B      1000 1000   1000
# 3 C      1200  667.  1500
# 4 D      1600  333.     0
```

If you want to use baseline values that are not found in the table itself, rather than identify the `baserow` you can instead specify the values directly using the `basevals` argument. This takes a numeric vector which must have the same number of elements as the number of target columns. Setting the `basevals` argument means the `baserow` argument is ignored.

`add_indices` creates indices using the same rules, but appends the created indices to the end of the input dataframe. The index column names are prefixed with value of the `prefix` argument. The default value is `ix_`.

``` r
add_indices(data)
# # A tibble: 4 x 7
#   a         b     c     d  ix_b  ix_c  ix_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A       200   400    50   100   100   100
# 2 B       250   300   100   125    75   200
# 3 C       300   200   150   150    50   300
# 4 D       400   100     0   200    25     0

add_indices(data, prefix = "index_")
# # A tibble: 4 x 7
#   a         b     c     d index_b index_c index_d
#   <chr> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
# 1 A       200   400    50     100     100     100
# 2 B       250   300   100     125      75     200
# 3 C       300   200   150     150      50     300
# 4 D       400   100     0     200      25       0
```

To create an index from a single vector use the singular `get_index`. This takes the `basepos` argument to identify the position of the value to use as the baseline, or aternatively the baseline value can be set directly with `baseval`.

``` r
data <- c(200, 250, 300, 350, 400)

get_index(data, base = 1000, basepos = 5)
# [1]  500  625  750  875 1000

get_index(data, base = 1000, baseval = 500)
# [1] 400 500 600 700 800
```

### Deflators and real terms series

Functions for real terms series provide a simple way to deflate prices stored in columns in a dataframe. The principal arguments are the input dataframe and a vector containing the deflator to use to deflate the prices. These functions use the `from` and `to` arguments to identify the target columns. By default they target all columns other than the first. 

`get_real` returns a dataframe containing real terms series created for each of the target columns using the given deflator, along with any columns preceding the targets. This function uses two arguments other than the input dataframe to control the properties of the real terms series created.

The `deflator` is a vector containing the deflator series used to adjust the prices in the target columns. The `deflator` must have the same number of elements as there are rows in the dataframe. If the deflator series is a column in the dataframe, just pass it as the `deflator` argument with `data$deflator`.

The `baserow` is the row number to use as the baseline for the adjusted prices i.e. it identifies the row in the table that represents the period to which prices will be adjusted. The default value of `baserow` is the last row of the table, which is assumed to be the most recent period.

``` r
deflator <- c(91.478, 93.975, 95.389, 97.978, 100.0)

data <- tibble::tibble(
    a = LETTERS[1:5],
    b = c(204, 219, 240, 258, 272),
    c = c(100, 100, 100, 100, 100),
    d = c(91.478, 93.975, 95.389, 97.978, 100.0))

data    
# # A tibble: 5 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A       204   100  91.5
# 2 B       219   100  94.0
# 3 C       240   100  95.4
# 4 D       258   100  98.0
# 5 E       272   100 100 

get_real(data, deflator)
# # A tibble: 5 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A      223.  109.  100.
# 2 B      233.  106.  100 
# 3 C      252.  105.  100 
# 4 D      263.  102.  100 
# 5 E      272   100   100 

get_real(data, deflator, baserow = 3)
# # A tibble: 5 x 4
#   a         b     c     d
#   <chr> <dbl> <dbl> <dbl>
# 1 A      213. 104.   95.4
# 2 B      222. 102.   95.4
# 3 C      240  100    95.4
# 4 D      251.  97.4  95.4
# 5 E      259.  95.4  95.4
```

`add_real` creates real terms series using the same rules, but appends the created series to the end of the input dataframe. The real terms series column names are prefixed with value of the `prefix` argument. The default value is `rt_`.

``` r
add_real(data, deflator)
# # A tibble: 5 x 7
#   a         b     c     d  rt_b  rt_c  rt_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A       204   100  91.5  223.  109.  100.
# 2 B       219   100  94.0  233.  106.  100 
# 3 C       240   100  95.4  252.  105.  100 
# 4 D       258   100  98.0  263.  102.  100 
# 5 E       272   100 100    272   100   100 

add_real(data, deflator, baserow = 3, prefix = "real_")
# # A tibble: 5 x 7
#   a         b     c     d real_b real_c real_d
#   <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
# 1 A       204   100  91.5   213.  104.    95.4
# 2 B       219   100  94.0   222.  102.    95.4
# 3 C       240   100  95.4   240   100     95.4
# 4 D       258   100  98.0   251.   97.4   95.4
# 5 E       272   100 100     259.   95.4   95.4
```

To deflate prices in a single vector use `deflate`. This takes the `basepos` argument to identify the position of the period to use as the baseline.

``` r
data <- c(204, 219, 240, 258, 272)
deflator <- c(91.478, 93.975, 95.389, 97.978, 100.0)

deflate(data, deflator)
# [1] 223.0044 233.0407 251.6013 263.3244 272.0000

deflate(data, deflator, basepos = 3)
# [1] 212.7217 222.2952 240.0000 251.1825 259.4581
```
