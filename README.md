# cltools

A collection of data wrangling tools for statistical researchers. This package is principally designed for use by researchers in the House of Commons Library but may be useful to anyone using R for routine data analysis.

## Installation

Install from GitHub using devtools.

``` r
install.packages("devtools")
devtools::install_github("olihawkins/cltools")
```

## HexJSON functions

These functions are used to produce initial hexjson data for a hexmap of geographic areas. The hexjson data generated from these functions can then be editied in a [hexjson editor].

These functions take tabular data for a collection of geographic areas and convert it to a set of hexes represented as hexjson, assigning unique grid coordinates to each hex. Data may be provided in the form of a dataframe, tibble or csv. 

The tabular data may contain codes, names, and categorical or numerical values for each area to be mapped. However, it is assumed that the first column of the table contains data that uniquely idenitfies each area, and which functions as the key for each area within the hexjson object. The remaining columns may contain any values that can be validly represented as json.

A [hexjson editor] can be used to position the hexes and design the hexmap, which can then be exported as either hexjson or geojson.

[hexjson editor]: <https://olihawkins.com/project/hexjson-editor/>

#### create_hexjson(data, layout = "odd-r")

Converts a dataframe of codes, names and other data to a hexjson string, adding unique column and row coordinates for each hex. The values in the first column are used as the key for each hex in the hexjson and therefore must be unique.

``` r
# Some example data
data <- tibble::tibble(
    name = letters[1:25],
    group = rep(letters[1:5], rep(5, 5)),
    value = 1:25)

# Create hexjson with the default layout
hexjson <- create_hexjson(data)

# Create hexjson with the specified layout
hexjson <- create_hexjson(data, "odd-q")
```

#### create_and_save_hexjson(data, filename, layout = "odd-r") 

This function is identical to create_hexjson but also saves the hexjson string to a file.

``` r
# Create and save hexjson with the default layout
create_and_save_hexjson(data, "output.hexjson")

# Create and save hexjson with the specifed layout
create_and_save_hexjson(data, "output.hexjson", "odd-q")
```

#### create_hexjson_from_csv(csv_file, hexjson_file, layout = "odd-r")

This function is identical to create_and_save_hexjson but reads the data in from the given csv.

``` r
# Create hexjson from a csv with the default layout
create_and_save_hexjson("input.csv", "output.hexjson")

# Create hexjson from a csv with the default layout
create_and_save_hexjson("input.csv", "output.hexjson", "odd-q")
```
