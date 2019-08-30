## WEATHER

There are 37 Google Sheets for storing weather data. However not all sites actually have the data. Few sites have data that needs special processing including cleaning, unit conversion, and other.

Weather data is grouped into **DAILY** and **HOURLY** sets. Each one contains different amount of variabels. Some sites have data at both temporal reoslutions, while others only have one. 

### Hourly Data
There are 22 sites with **hourly weather data**.  Measurement frequency ranges from 10 mins to 1 hour. Several sites, all located in OH, have reported weather data (mainly precipitation) at irregular intervals. 
```
SITE        FREQUENCY
ACRE          1 hr
AUGLA         - Irregular
BEAR          1 hr
BEAR2         1 hr
BENTON        1 hr
CLAY_U       10 min
CRAWF        30 min
DEFI_M        - Irregular
DEFI_R       15 min, 30 min, 1 hr & Irregular
DIKE          1 hr
DPAC          1 hr
FULTON       30 min
HARDIN_NW     - Irregular
HENRY         - Irregular
HICKORY       1 hr
HICKS_B       1 hr
MAASS         1 hr
SERF_SD       1 hr
SHEARER       1 hr
STJOHNS       1 hr 
TIDE          1 hr
VANWERT      30 min
```

Only 7 variables constitute the hourly weather data.
```
VARIABLES           NUMBER_OF_SITES
Air Temperature            13
Precipitation              22
Relative Humidity           5
Solar Radiation             6
Wind Direction              4
Wind Gust                   3
Wind Speed                  6
```

### Daily Data
Currently there are 31 sites with **daily weather data**. Sites that have only hourly data (BENTON, DIKE, HICKORY, SHEARER) can be aggregated into daily and added to the daily weather data.

The daily weather data is comprised of 36 origianl variables. Some of them are redundant and will be merged or removed as data gets finilized.
```
VARIABLE                             NUMBER_OF_SITES
Precipitation                              31
Snowfall                                    2
Pan Evaporation                             2
Reference ET                                3
Reference ET (Penman-Monteith)              1
Reference ET (Short Crop)                   3
Reference ET (Short Grass)                  1
Reference ET (Tall Grass)                   1
Relative Humidity                          10
Ave Relative Humidity                       1
Min Relative Humidity                       1
Max Relative Humidity                       1
Ave Air Temperature                        14
Min Air Temperature                        18
Max Air Temperature                        18
Dew-Point Temperature                       1
Ave Bare Soil Temperature (10 cm depth)     1
Min Bare Soil Temperature (10 cm depth)     1
Max Bare Soil Temperature (10 cm depth)     1
Min Bare Soil Temperature (2" depth)        2
Max Bare Soil Temperature (2" depth)        2
Min Bare Soil Temperature (4" depth)        2
Max Bare Soil Temperature (4" depth)        2
Min Bare Soil Temperature (8" depth)        2
Max Bare Soil Temperature (8" depth)        2
Solar Radiation                            13
Ave Solar Radiation                         1
Min Solar Radiation                         1
Max Solar Radiation                         1
Photosynthetically Active Radiation         1
Total Energy Density                        1
Wind Speed                                 12
Max Wind Speed                              5
Wind Gust                                   1
Wind Run                                    2
Wind Direction                              7
```


#### Missing Data (all or some years)
* BATH_A (missing all years)
* WILKIN1 (missing all years)

#### Proccessed Data
* ACRE
* AUGLA
* BEAR
* BEAR2
* BENTON
* CLAY_C
* CLAY_U
* CLAY_R
* CRAWF
* DEFI_M
* DEFI_R
* DIKE
* DPAC
* FAIRM
* FULTON
* HARDIN
* HARDIN_NW
* HENRY
* HICKS_B
* MAASS
* MUDS1
* MUDS2
* MUDS3_NEW
* MUDS3_OLD
* MUDS4
* SERF_IA
* SERF_SD
* SHEARER
* STJOHNS
* STORY
* SWROC
* TIDE
* UBWC
* VANWERT
* WILKIN1


> Issues

#### HOURLY DATA

* __Erroneous Air Temp and RH data @ ACRE__

#### DAILY DATA

* [__Merge variables__](https://github.com/isudatateam/datateam/issues/181)
    - `Relative Humidity` and `Ave Relative Humidity`
    - `Solar Radiation` and `Ave Solar Radiation`
    - `Bare Soil Temperature` at 10 cm and 4"
* [__Drop uncommon variables__](https://github.com/isudatateam/datateam/issues/181)
    - `Photosynthetically Active Radiation`
    - others
* __Add missing hourly data__
    - **BENTON**, **DIKE**, **HICKORY**, **SHEARER**, and **DEFI_R (OnSite station)** had only hourly data which need to be aggregated and added to the daily data

> Resolved Issues

* [__Standardize time intervals for HOURLY data__](https://github.com/isudatateam/datateam/issues/181)
    - Make intervals hourly
* [__Discard unclear or uncommon variables for DAILY data__](https://github.com/isudatateam/datateam/issues/181)
    + drop: `Photosynthetically Active Radiation`


For more detials about problem handling and data preparation visit [isudatateam/datateam issues](https://github.com/isudatateam/datateam/issues) at GitHub (Milstone "Finilizng TD Data" within "Transforming Drainage" project).
  
  
### [BACK TO MAIN PAGE](../README.md)
