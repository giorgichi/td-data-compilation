## WEATHER

There are 37 Google Sheets for storing weather data. However not all sites actually have the data. Few sites have data that needs special processing including cleaning, unit conversion, and other.

Weather data is grouped into **DAILY** and **HOURLY** sets. Each one contains different amount of variabels. Some sites have data at both temporal reoslutions, while others only have one. 

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


> Need to Solve

* __Questionable data__
    + Hourly `Wind Speed` and `Wind Gust` at DEFI_R, FULTON and VANWERT
* __Unclear data and/or variables__
    + Daily `Photosynthetically Active Radiation` at TIDE


> Issues

* [__Merge variables__](https://github.com/isudatateam/datateam/issues/181)
    - `Relative Humidity` and `Ave Relative Humidity`
    - `Solar Radiation` and `Ave Solar Radiation`
    - `Bare Soil Temperature` at 10 cm and 4"
* [__Drop variables__](https://github.com/isudatateam/datateam/issues/181)
    - `Photosynthetically Active Radiation`



For more detials about problem handling and data preparation visit [isudatateam/datateam issues](https://github.com/isudatateam/datateam/issues) at GitHub (Milstone "Finilizng TD Data" within "Transforming Drainage" project).
  
  
### [BACK TO MAIN PAGE](../README.md)