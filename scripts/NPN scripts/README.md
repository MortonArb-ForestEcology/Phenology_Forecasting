# NPN scripts README

# NPN Scripts

## 1_Organize_Data_NPN.R 

Purpose: To use NPN weather data and phenology monitoring data to create a predicitve model of bud burst timing
         This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the model input
         
Inputs: 
        
Outputs: dat.comb dataframe that can be used in the Frequentist GDD5-burst.R and the Bayesian_GDD5-burst.r script in this repository

## 2_Linear_NPN.R

Purpose: Short script to visualize where our NPN points are coming from

Inputs: dat.comb dataframe that is created by the Organize_Data_NPN.R script

Outputs: Currently a rough version of the linear model using GTmean. Still rough (likely because linear model isn't accurate)

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


# A series of script for Christy's career proposal

Lucien doesn't know the purpose of these in detail so he isn't writing them up

## CAREER_01_QURU_download.R

## CAREER_02_QURU_ACRU_model.R

## CAREER_03_QUCU_ACRU_Predict_Yday.R

## CAREER_04_Figure.R
