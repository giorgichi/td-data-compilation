# TRANSFORMING DRAINAGE

[Transforming Drainage](https://transformingdrainage.org/) (TD) Project is a multi-state research, extension, and education effort aimed at addressing important drainage water management questions. It brings together data from 42 research sites across 8 states that span field- and plot-level scale experiments in an intensively standardized and quality controlled format. The project was funded from 2015-2020 by the United States Department of Agriculture, National Institute of Food and Agriculture (USDA-NIFA, Award No. 2015-68007-23193). 


## Data

TD data has been mainly stored in standardized Google Sheets. Some spreadsheets have been modified to accommodate special needs of the data provided (such as [WRSIS](https://www.ars.usda.gov/midwest-area/columbus-oh/soil-drainage-research/people/barry-allred/wetland-reservoir-subirrigation-system-wrsis/) data). Sheet have been downloaded to the local disk before reading and organized by data categories:

* [AGR](Input_Data/AGR/README.md)
* SOIL
* WATER
* WEATHER
* OTHER
* Metadata

The date when the Google Sheet is downloaded from the Google Drive is logged into a file 'log.csv' along with the date when the Google Sheet was updated. 


### AGR

There are 32 Google Sheets with agronomic data for TD sites. However not all sites have agronomic data. Few sites have data that needs special processing.

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


> List of Issues

* __Questionable data__
    + ACRE
* __Unclear data and/or variables__
    + SWROC
* __Yield based on zone of influence [AGRXX]__ 
    + DEFI_M
    + HARDIN
    + HARDIN_NW
* Corn development data
    + ~~STORY~~
* __Plots to exclude__
    + STORY has tile flow from plots: 2, 3, 5, 8, 9, 11
* __Plots with multiple treatments (at subplot level)__
    + MUDS1
    + SWROC (N rates)
    + VANWERT (hybrid/cultivar, tillage, foliar fert)


> Handling of Problematic Data

* Sub-plot level yield data @ ACRE, MUDS1, SWROC and VANWERT
* Site-average yields @ HICKS_B, 
* Corn development stage @ STORY
    - we will not publish corn staging data (Lori 2019-06-14)
  
  