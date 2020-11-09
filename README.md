
## Code for "Estimating internationally imported cases during the early COVID-19 pandemic" by Menkir et al. 2020
------------

## 1. Libraries
------------
The project requries the following R packages:
```r
c("Bolstad2", "broom", "caret", "cartogram", 
              "data.table", "dplyr", "dvmisc", "formattable", 
              "gam", "ggmap", "ggplot2", "ggpubr", "ggthemes", "grid", "gridExtra", 
              "janitor", "kableExtra", "knitr", "leaflet", "lubridate", "maptools", 
              "pals", "patchwork", "purrr", "RColorBrewer", 
              "readr", "reshape2", "rgdal", "rio", "scales", "spData", "stats", 
              "stringi", "stringr", "tidyr", "tidyverse", "tmap", "zoo")
```

## 2. Pre-requisite data
----------------------
External data are required from a number of sources:

1. MIDAS cumulative case data: province and city-level cumulative case data from China. These should be downloaded from the [MIDAS git repo](https://github.com/midas-network/COVID-19/tree/master/data/cases/china/cumulative_cases_DXY).

2. MIDAS province-level daily case counts in China. Also downloaded from the [MIDAS git repo](https://github.com/midas-network/COVID-19/tree/master/data/cases/china/daily_cases_chinacdc_EN ).

3. ECDC global confirmed cases from [here](https://opendata.ecdc.europa.eu/covid19/casedistribution/csv ).

4. Code and MCMC outputs from [Tsang et al. 2020](https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(20)30089-X/fulltext), [DOI:https://doi.org/10.1016/S2468-2667(20)30089-X](https://doi.org/10.1016/S2468-2667(20)30089-X). Note that you will need to clone the git repo available [here](https://github.com/timktsang/covid19_casedef).

## 3. Git repo structure
----------------------
This repo is split into folders for `data`, `code`, `scripts`, `figures` and `out`.
* `data`: provides pre-computed and generated data for the analyses.
* `code`: stores auxiliary R functions.
* `scripts`: the main folder containing all scripts used to generate figures and analyses. Note that this is split into `pre_computation` scripts, which can be run should the user wish to reproduce data files from scratch. Otherwise, the scripts labeled 1-6 are sufficient to reproduce the analyses.
* `figures`: folder to store generated figures and panels.
* `out`: folder to store larger, intermediate data files.

## 4. Scripts in the `scripts` folder. Run them in the following order: 
--------------------------------------------------------------------
### Header file
1. Source the file `scripts/headers.R` before running any other scripts. **NOTE** you should change the `main_wd` object to the full file path to where this repo is stored. This script contains all libraries and global settings for subsequent scripts.

### Precomputation - optionally run this first.
EXAMPLE: the script `EXAMPLE_flight_volume_estimation.R` provides the code we used to generate flight volume data. However, the original source files are proprietary and cannot be shared, though all of the necessary data are available in `data/flights_all_cities2.csv`. This code is included for transparency only and cannot be run.

1.`ECDC_case_data.R`: produces the files `data/hasdetected.Rdata` and `data/df_country_cont_ecdc.Rdata`.

2.`extract_midas_data.R`: extract MIDAS case count data and produce clean data file.

3.`apportion_tsang_incidence.R`: apportion incidence as per Tsang et al. analysis for Scenario 2.

4.`apportion_province_cases_to_cities.R`: apportion incidence to Chinese cities by different methods (population, % of cases).

5.`calculate_ascertainment_rate_ratio.R`: calculate ascertainment rate ratio between Wuhan and non-Wuhan cities.

### Main analyses
1.`generate_prevalence_estimates.R`: generate time-varying daily prevalence for each province and city for each of the scenarios, incorporating the assumptions specified in `data/scenario_key.xlsx`.

2.`create_master_table.R`: creates large tibble to compute all numbers and create all figures in the text.

3.`all_numbers.R`: computes all numbers shown in the manuscript. Note that the main scenario object needs to be changed to investigate particular scenarios in more detail.

4.`import_figures.R`: creates all panels for Figures 2, 3, S4 and S5.

5.`prevalence_figures.R`: produces panels for Figures 1, S2 and S3.

6.`flight_volume_figure.R`: produces panels for Figure S1.

## 5. Required or *temporarily-stored* data is given in folder `/data`:
--------------------------------------------------------------------
* **who_imports.csv** - contains reported COVID-19 case imports from Wuhan pre-lockdown /n
* **midas** - folder containing data files needed to extract case counts from MIDAS git repo
* **midas/midas_data_final.csv** - contains confirmed COVID-19 case counts for Chinese provinces
* **digitize_verity.csv** - contains the ascertainment rates per age-category digitized from Verity et al. (Lancet Infect Dis 2020)
* **cn_iata_code_shortlist.csv** - contains the names, IATA codes, and city names of Chinese origin airports
* **provinces_popn_size_statista.csv** - contains the population sizes of Chinese provinces
* **df_city_pop.Rdata** - contains the populations sizes of Chinese cities 
* **df_country_cont_ecdc.Rdata** - contains a key for destination countries and continents 
* **frac_popn_city.Rdata** - contains the population fractions of chinese cities relative to their respective provinces 
* **tsang_predictions_apportioned.csv** - contains the COVID-19 incidence estimates from Tsang et al. (Lancet Public Health 2020)
* **flights_all_cities2.csv** - contains flight volume between Chinese origin cities and international destinations by calendar day
* **2019_flight_line.csv** - contains total flight volume between Chinese origin cities and international destinations by calendar day in 2019
* **hasdetected.Rdata** - contains an indicator for when an African location has detected a COVID-19 case
* **master_table_2910.csv** - << MUST BE GENERATED>> contains the prevalence indicators under all scenarios and flight volume estimates in a large dataframe that will be used for all number calculations and figures of the manuscript
* **tsang2020** - folder containing files to extract incidence predictions for Scenario 2 as per Tsang et al.
* **scenario_key.xlsx** - key giving descriptions and names to each of the 9 scenarios.



