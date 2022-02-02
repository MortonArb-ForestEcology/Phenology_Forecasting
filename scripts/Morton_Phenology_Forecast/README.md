# Mortn_Phenology_Forecast README

# Preloaded data

## model_ouput folder

A folder containing the model output from our Thermal Time model for each species. They are created in the Phenology Forecasting repository script 3b_Thermal_Time_model.R

## Oak_collection_budburst.csv

The budburst phenology observations from our volunteer obeservation network.

## Quercus_Collection

Information on the oak species we gather observations for. Mostly used to identify their common names

# Scripts

## Full_Workflow.R

Purpose: This script serves to run throught our daily workflow

Inputs: 1_M1_Meterology_download.R

2_SQL_creation.R

3_run_shiny.R
        
Outputs: Downloads the new weather, updates the forecasts, and launches the app online

CLEAN_Phenology_NPN_combined_leaf.csv         

Notes: 


## 1_M1_Meterology_download.R

Purpose: This script downloads historical weather and forecast weather data. It also performs a bias correction on the forecast ensemble.

Inputs: Arboretum COOP station data (observed Temp & Precip)
 
NOAA CFS forecast -- single ensemble member, but long-range forecast (9 months)

NOAA GEFS Forecast -- 31-member forecast, but short-term (16 day) with potential slow option for longer-range (35 days), but relies on grib format

The met_download_CFS.R script which defines the met_download_CFS function.

The met_download_GHCN.R script which defines the met_download_GHCN function.

The met_gapfill.R script which defines the met_gapfill function.
         
Outputs: Weather_ArbCOOP_historical_latest.csv

MortonArb_GEFS_daily_latest.csvak_collection_leaf_raw.csv

MortonArb_CFS_daily_latest.csv

MortonArb_CFS_daily_FORECAST-READY.csv

MortonArb_GEFS_daily_FORECAST-READY.csv

MortonArb_GEFS_daily_FORECAST-READY-LONGRANGE_latest.csv

Previous-Forecast_Sys.Date().csv
          
Notes: Alongside downloding the forecast it also uploads the forecast to the shiny app. This is the only data uploaded to the shiny app not in 2_data_org.R


## 2_data_org.R

Purpose: This script is for the creation of the sql database that is pulled for the app

Inputs: Preloaded Thermal Time model output for Oak species
        Preloaded Model Convergence data frame
        Preloaded Quercus collection names googlesheet
        Prealoded Oak budburst observations Oak_collection_budburst.csv
        Historical weather data created by 1_M1_Meterology_download.R
        Forecasted weatehr data created by 1_M1_Meterology_download.R

Outputs: Same as the inputs but they are updated and uploaded to the shiny app database.

Notes: Forecasts aren't overwritten they are stored as historical forecasts. Other components are overwritten with updated information


## 3_run_shiny.R

Purpose: This script serves to launch the app locally or online

Inputs:
         
Outputs: The app launched either online or locally
         
Notes:   You can run the app locally or online. Which working directory you need to be in depends on which you are doing
You must be in this scripts wd to run the app locally. You must be in the "shiny_app" folder to run online.

# Functions

## met_download_CFS.R

## met_download_GHCN.R

## met_gapfill.R

