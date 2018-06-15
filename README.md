# cltools

A collection of data wrangling tools for statistical researchers. This package is principally designed for use by researchers in the House of Commons Library but may be useful to anyone using R for routine data analysis.

### HexJSON functions

These functions are used to produce initial hexjson data for a hexmap of geographic areas. The hexjson data generated from these functions can then be editied in a [hexjson editor].

These functions take tabular data for a collection of geographic areas and convert it to a set of hexes represented as hexjson, assigning unique grid coordinates to each area. Data may be provided in the form of a dataframe, tibble, or csv. 

The tabular data may contain codes, names, and categorical or numerical values for each area to be mapped. However, it is assumed that the first column of the table contains data that uniquely idenitfies each area, which can function as the key for each area within the hexjson object. The remaining columns may contain any values that can be validly represented as json.

A [hexjson editor] can be used to position the hexes and design the hexmap, which can then be exported as either hexjson or geojson.

[hexjson editor]: <https://olihawkins.com/project/hexjson-editor/>
