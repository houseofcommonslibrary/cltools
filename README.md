# cltools

A collection of data wrangling tools for statistical researchers. This package is principally designed for use by researchers in the House of Commons Library but may be useful to anyone using R for routine data analysis. The package provides functions for manipulating tabular data stored in dataframes.

This package is in active development and I would welcome any feedback. Documentation and unit tests are being added with each new set of functions. I cannot currently guarantee there will be no breaking changes as the package may evolve to reflect feedback from users.

## Installation

Install from GitHub using devtools.

``` r
install.packages("devtools")
devtools::install_github("olihawkins/cltools")
```

## Row and column totals

Functions for row and column totals provide a convenient way to create and add row and column totals for data stored in columns in a dataframe. `get_row_totals` sums the data in a set of columns containing numerical data, specified with the `from` and `to` arguments. By default the function assumes that all the columns are to be summed except for the first column.

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

The row totals can be added to the input dataframe with `add_row_totals`. You can provide an alternative column name for the row totals with the `label` argument.

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

The equivalent functions `get_col_totals` and `add_col_totals` produce similar results for column totals but with some differences, which reflect the different behaviour of rows and columns in dataframes. The vector returned from `get_col_totals` is named with the column labels.

``` r
get_col_totals(data)
#  b  c  d 
# 15 40 65 
```

When adding column totals with `add_col_totals`, NAs are appended to all columns that are not being summed. However, label columns can be specified so that a label can be included in the totals row. By default, the function will try to add a label to the first column, if possible.

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

The `lcols` argument can be used to specify which label columns should receive the label for totals. 

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

If the column indices specified with `lcols` are invalid for any reason they are silently ignored. Similarly, setting `lcols` to `NULL` will stop the function attempting to include a label in the new row.

``` r
add_col_totals(data, from = 3, lcols = NULL)
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
add_col_totals(data, from = 3, label = "all", lcols = c(1, 2))
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

## Row and column percentages

Functions for row and column percentages provide a convenient way to create and add row and column percentages for data stored in columns in a dataframe. These functions have a similar interface to the functions for row and column totals. The principal difference is that `get_row_percent` and `get_col_percent` return the target columns as percentages **along with** any preceding data columns.

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

get_row_percent(data, to = 3)
# # A tibble: 3 x 3
#   a         b     c
#   <chr> <dbl> <dbl>
# 1 A       0.5   0.5
# 2 B       0.5   0.5
# 3 C       0.5   0.5

get_col_percent(data, to = 3)
# # A tibble: 3 x 3
#   a         b     c
#   <chr> <dbl> <dbl>
# 1 A     0.167 0.167
# 2 B     0.333 0.333
# 3 C     0.5   0.5  

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
add_row_percent(data, from = 3)
# # A tibble: 3 x 6
#   a         b     c     d  pc_c  pc_d
#   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 A         1     1     2 0.333 0.667
# 2 B         2     2     4 0.333 0.667
# 3 C         3     3     6 0.333 0.667

add_col_percent(data, to = 3)
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

## HexJSON

These functions are used to produce initial hexjson data for a hexmap of geographic areas. The hexjson data generated from these functions can then be edited in a [hexjson editor].

These functions take tabular data for a collection of geographic areas and convert it to a set of hexes represented as hexjson, assigning unique grid coordinates to each hex. Data may be provided in the form of a dataframe, tibble or csv. 

The tabular data may contain codes, names, and categorical or numerical values for each area to be mapped. However, it is assumed that the first column of the table contains data that uniquely idenitfies each area, and which functions as the key for each area within the hexjson object. The remaining columns may contain any values that can be validly represented as json.

A [hexjson editor] can be used to position the hexes and design the hexmap, which can then be exported as either hexjson or geojson.

[hexjson editor]: <https://olihawkins.com/project/hexjson-editor/>

`create_hexjson` converts a dataframe of codes, names and other data to a hexjson string, adding unique column and row coordinates for each hex. The values in the first column are used as the key for each hex in the hexjson and therefore must be unique.

``` r
data <- tibble::tibble(
    name = letters[1:25],
    group = rep(letters[1:5], rep(5, 5)),
    value = 1:25)

# Create hexjson with the default layout
hexjson <- create_hexjson(data)

# Create hexjson with the specified layout
hexjson <- create_hexjson(data, "odd-q")
```

`create_and_save_hexjson` is identical to `create_hexjson` but also saves the hexjson string to a file.

``` r
# Create and save hexjson with the default layout
create_and_save_hexjson(data, "output.hexjson")

# Create and save hexjson with the specifed layout
create_and_save_hexjson(data, "output.hexjson", "odd-q")
```

`create_hexjson_from_csv` is identical to `create_and_save_hexjson` but reads the data in from the given csv.

``` r
# Create hexjson from a csv with the default layout
create_and_save_hexjson("input.csv", "output.hexjson")

# Create hexjson from a csv with the specified layout
create_and_save_hexjson("input.csv", "output.hexjson", "odd-q")
```
