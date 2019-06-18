## AGR

There are 32 Google Sheets with agronomic data for TD sites. However not all sites actually have agronomic data. Few sites have data that needs special processing.

#### Missing Data (all or some years)
* BATH_A (missing all years)
* UBWC (not available)
* WILKIN2 (missing 2018)
* WILKIN3 (missing all years)

#### Problematic Data (all or some years)
* ACRE
* MUDS1
* SWROC
* VANWERT

#### Proccessed Data
* AUGLA
* CLAY_C
* CLAY_U
* CLAY_R
* CRAWF
* DEFI_M
* DEFI_R
* DPAC
* FAIRM
* FULTON
* HARDIN
* HARDIN_NW
* HENRY
* HICKS_B
* MUDS2
* MUDS3_NEW
* MUDS3_OLD
* MUDS4
* SERF_IA
* SERF_SD
* STJOHNS
* STORY
* TIDE
* WILKIN1
* WILKIN2
* WILKIN3


> Need to Solve

* __Questionable data__
    + ACRE
* __Unclear data and/or variables__
    + SWROC
* __Plots with multiple treatments (at subplot level)__
    + MUDS1
    + SWROC (N rates)
    + VANWERT (hybrid/cultivar, tillage, foliar fert)


> Issues

* [Non-relevant plots @ STORY](https://github.com/isudatateam/datateam/issues/174)
    - removed plots 2, 3, 5, 8, 9, 11
* __Sub-plot level yield data @ ACRE, MUDS1, SWROC and VANWERT__
* __Site-average yields @ HICKS_B, ....__
* [Yield within DWM zone of influence @ DEFI_M, HARDIN and HARDIN_NW](https://github.com/isudatateam/datateam/issues/172)
    - added corresponding variables
* [Corn development stage @ STORY](https://github.com/isudatateam/datateam/issues/171)
    - Lori (2019-06-14): "we will not publish corn staging data"
* __Unstandardized variables at SWROC__

For more detials about problem handling and data preparation visit [isudatateam/datateam issues](https://github.com/isudatateam/datateam/issues) at GitHub (Milstone "Finilizng TD Data" within "Transforming Drainage" project).
  
  
### [BACK TO MAIN PAGE](../README.md)