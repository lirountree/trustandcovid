# Average years of schooling - Data package

This data package contains the data that powers the chart ["Average years of schooling"](https://ourworldindata.org/grapher/average-years-of-schooling?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on November 14, 2024.

## CSV Structure

The high level structure of the CSV file is that each row is an observation for an entity (usually a country or region) and a timepoint (usually a year).

The first two columns in the CSV file are "Entity" and "Code". "Entity" is the name of the entity (e.g. "United States"). "Code" is the OWID internal entity code that we use if the entity is a country or region. For normal countries, this is the same as the [iso alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code of the entity (e.g. "USA") - for non-standard countries like historical countries these are custom codes.

The third column is either "Year" or "Day". If the data is annual, this is "Year" and contains only the year as an integer. If the column is "Day", the column contains a date string in the form "YYYY-MM-DD".

The final column is the data column, which is the time series that powers the chart. If the CSV data is downloaded using the "full data" option, then the column corresponds to the time series below. If the CSV data is downloaded using the "only selected data visible in the chart" option then the data column is transformed depending on the chart type and thus the association with the time series might not be as straightforward.

## Metadata.json structure

The .metadata.json file contains metadata about the data package. The "charts" key contains information to recreate the chart, like the title, subtitle etc.. The "columns" key contains information about each of the columns in the csv, like the unit, timespan covered, citation for the data etc..

## About the data

Our World in Data is almost never the original producer of the data - almost all of the data we use has been compiled by others. If you want to re-use data, it is your responsibility to ensure that you adhere to the sources' license and to credit them correctly. Please note that a single time series may have more than one source - e.g. when we stich together data from different time periods by different producers or when we calculate per capita metrics using population data from a second source.

## Detailed information about the data


## Average years of schooling
Average number of years adults over 25 years participated in formal education.
Last updated: April 9, 2024  
Next update: April 2025  
Date range: 1990–2022  
Unit: years  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
UNDP, Human Development Report (2024) – with minor processing by Our World in Data

#### Full citation
UNDP, Human Development Report (2024) – with minor processing by Our World in Data. “Average years of schooling” [dataset]. UNDP, Human Development Report, “Human Development Report 2023-2024” [original data].
Source: UNDP, Human Development Report (2024) – with minor processing by Our World In Data

### What you should know about this data
* Formal education is primary/ISCED 1 or higher.
* This indicator does not include years spent repeating grades.

### How is this data described by its producer - UNDP, Human Development Report (2024)?
UNDP relies on: Barro and Lee (2018), ICF Macro Demographic and Health Surveys (various years), OECD (2023), UNESCO Institute for Statistics (2023) and UNICEF Multiple Indicator Cluster Surveys (various years).

### Source

#### UNDP, Human Development Report – Human Development Report
Retrieved on: 2024-04-09  
Retrieved from: https://hdr.undp.org/  

#### Notes on our processing step for this indicator
We calculated averages over continents and income groups by taking the population-weighted average of the countries in each group. If less than 80% of countries in an area report data for a given year, we do not calculate the average for that area.


    