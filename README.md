# Phenology_Forecasting
Scripts for Ecological forecasting model for phenology at the arboretum

# Script Folders

Each of these folders has their own README inside

## Morton_Phenology_Forecast

Purpose: A folder for running our shiny app

## Test Models

Purpose: A folder to contain old or experimental models whose code might be used down the line.

## NPN scripts

Purpose: Some initial attempts at integrating NPN data with arb data for our model. There are better versions of this done in Collections_phenology_vulnerability

## Historic scripts

Purpose: To house old scripts that used to be involved in the workflow and aren't anymore. We keep them in case we want to pull from them later

# Scripts

## 1_Organize_Data_Pheno.R 

Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the model input
         
Inputs: Old metstation data from 1895-2007 found in the "Arboretum Met Data/GHCN-Daily" google drive folder.
        New metstation data from 2007-present found in the "Arboretum Met Data/GHCN-Daily" google drive folder.
        Quercus 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm". 
        The clean_google_form.r script which defines the clean.google function.
        
Outputs: dat.comb dataframe that can be used in the Frequentist GDD5-burst.R and the Bayesian_GDD5-burst.r script in this repository

## 2_Data_weather_match.R

Purpose: This script serves as the download of weather data and the calculation of relevant weather statistics
         
Inputs: The weather_calc.r script which defines the weather_calc function.
        
Outputs: Oak_collection_budburst.csv which contain's GDD5 values for every budburst oobservation

Oak_collection_leaf.csv which contain's GDD5 values for every leaf oobservation

## 3b_Thermal_Time_model.R

Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the Bayesian model which will become the final product
         
Inputs: dat.comb dataframe that is created by the Organize_Data_Pheno.R script

Outputs: Currently, a hindcast of a species modeled day of budburst vs observed date of budburst

Notes: This script runs each species as it's own model because as we add more complex models down the line that require higher data density we will have species that no longer converge. If all species are run in one model, the species that no longer converge will impact species that do. This will make teasing out which species work for which model much more difficult.

# Functions

## Clean_google.R

Purpose: A function that will reformat our googlesheets into a useful data format

Inputs: 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" 
            
Outputs: A reformatted version of our googlesheet data that is easier to work with

## Group_google.R

Purpose: A function that will download all of the google forms of interest

Inputs: 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" 

The clean_google_form.r script which defines the clean.google function
        
Outputs: A dataframe containing the information from all desired google forms.

## weather_calc.R

Purpose: This function serves to calculate weather statistics of interest. 

Currently Growing degree days at 5C, Growing degree days at 0C, Number of chill days, and Growing season mean temperature
         
Inputs: Lubridate package

Outputs:This function will take a data frame of daily weather data and produce the following summary statistics

GDD5 = Growing degree days at 5 degrees C 

GDD0 = Growing degree days at 0 degrees C

NCD = Number of chilling days 

GTmean = Growing season mean temperature

PTTGDD = Growing degree day at 5 degrees C * the amount of light per day
  
Notes: The defaults for this funcion are

Starting year of interest                       y_start = 1975

Ending year of interest                         y_end = 2019

Julian yday for start of growing season         g_start = 1

Julian yday for end of growing season           g_end = 90

## met_download_CFS.R

## met_download_GHCN.R

## met_gapfill.R
