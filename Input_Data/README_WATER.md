## WATER

Water data are organized for simplicity of storing into several groups. There are several categories of Google Sheets dedicated for each group:
* Tile Flow
* Water Table
* Loads
* Water Quality



### Water Table Data
There are 20 Google Sheets for storing water table data, and two of them belongs to the same site (DEFI_R). Therefore there are 19 sites with Water Table sheets, but currently 4 of them have no data.

All of them are hourly, except the data from Iowa Saturated Buffer sites and manual readings at DEFI_R, which are reported as daily. Hence, water table data is grouped into **DAILY** and **HOURLY** sets. 

```
SITE        FREQUENCY
ACRE         Hourly
BATH_A       No Data 
BEAR         Daily
BEAR2        Daily
CLAY_C       Hourly
CLAY_R       Hourly
CLAY_U       Hourly
DEFI_R       Daily (manual reading)
DEFI_R       Hourly
DPAC         Hourly
FAIRM        Hourly
HICKS_B      Hourly
HICKS_P      No Data
MAASS        Daily
SERF_IA      Hourly
SERF_SD      Hourly
STJOHNS      Hourly
TIDE         Hourly
WILKIN1      No Data
WILKIN3      No Data
```


### Water Quality Data
There are 38 Google Sheets for storing water quality (**WQ**) data, and two of them belongs to the same site (DEFI_R). Therefore there are 37 sites with WQ sheets, but currently 6 of them have no data.

All of them are daily, except 7 sites (HICKS_B, UBWC, WIRSIS, WILKIN1, WILKIN2), which have data with higher frequency or timestamp. Dispite of this difference, water table data is combined in a single file by having separate columns for date and time. 




## THIS IS FROM WEATHER DATA
There are 37 Google Sheets for storing weather data. However not all sites actually have the data. Few sites have data that needs special processing including cleaning, unit conversion, and other.

Weather data is grouped into **DAILY** and **HOURLY** sets. Each one contains different amount of variabels. Some sites have data at both temporal reoslutions, while others only have one. 

### Hourly Data
There are 22 sites with **hourly weather data**.  Measurement frequency ranges from 10 mins to 1 hour. Several sites, all located in OH, have reported weather data (mainly precipitation) at irregular intervals. 


### Daily Data
Currently there are 31 sites with **daily weather data**. Sites that have only hourly data (BENTON, DIKE, HICKORY, SHEARER) can be aggregated into daily and added to the daily weather data.

The daily weather data is comprised of 36 origianl variables. Some of them are redundant and will be merged or removed as data gets finilized.



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
