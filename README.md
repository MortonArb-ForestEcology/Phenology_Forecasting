# Phenology_Forecasting
Scripts for Short term iterative ecological forecasting model for phenology at the arboretum

# Scripts

## Arboretum

## 1_Organize_Data_Pheno.R 

Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the model input
         
Inputs: Old metstation data from 1895-2007 found in the "Arboretum Met Data/GHCN-Daily" google drive folder.
        New metstation data from 2007-present found in the "Arboretum Met Data/GHCN-Daily" google drive folder.
        Quercus 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm". 
        The clean_google_form.r script which defines the clean.google function.
        
Outputs: dat.comb dataframe that can be used in the Frequentist GDD5-burst.R and the Bayesian_GDD5-burst.r script in this repository


## 2a_Frequentist GDD5-burst.R

Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the Frequentist model which will largely be for testing/comparison
         
Inputs: dat.comb dataframe that is created by the Organize_Data_Pheno.R script

Outputs: Currently, a hindcast of a species modeled day of budburst vs observed date of budburst


## 2b_Thermal_Time_model.R

Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the Bayesian model which will become the final product
         
Inputs: dat.comb dataframe that is created by the Organize_Data_Pheno.R script

Outputs: Currently, a hindcast of a species modeled day of budburst vs observed date of budburst


# NPN Scripts

## 1_Organize_Data_NPN.R 

Purpose: To use NPN weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the model input
         
Inputs: 
        
Outputs: dat.comb dataframe that can be used in the Frequentist GDD5-burst.R and the Bayesian_GDD5-burst.r script in this repository

## 2_NPN_Thermal_Time_model.R

Purpose: To use NPN weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the Bayesian model which will become the final product
         
Inputs: dat.comb dataframe that is created by the Organize_Data_NPN.R script

Outputs: MCMC chain of model, Density visualizations, and other visualizations

## NPN_Frequentists.R

Purpose: To use NPN weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the Frequentist model which will largely be for testing/comparison
         
Inputs: dat.comb dataframe that is created by the Organize_Data_NPN.R script

Outputs: Currently, a mlm model prediction of GDD5.cum threshold

## NPN_Mapping.R

Purpose: Short script to visualize where our NPN points are coming from

Inputs: dat.comb dataframe that is created by the Organize_Data_NPN.R script

Outputs: A US map showing the sites of our NPN data

## 2_Linear_NPN.R

Purpose: Short script to visualize where our NPN points are coming from

Inputs: dat.comb dataframe that is created by the Organize_Data_NPN.R script

Outputs: Currently a rough version of the linear model using GTmean. Still rough (likely because linear model isn't accurate)



# Functions

Group_google.R

Purpose: A function that will download all of the google forms of interest

Inputs: 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" 
         The clean_google_form.r script which defines the clean.google function
        
Outputs: A dataframe containing the information from all desired google forms.


# Test Models Folder

Purpose: A folder to contain old or experimental models whose code might be used down the line.
