# Decadal_Variability

## Overview

This repository supports analysis of the NOAA NMFS Northeast Fisheries Science Center spring and fall bottom trawl survey data to calculate a suite of metrics summarizing decadal changes in species distribution and environmental conidtions. This work supports that of Kathy Mills et al in ***Ocean warming and the emergence of climate change effects in Northeast U.S. fish populations and communities***. 

## Navigating the Repo

#### Data

This folder contains the list of fishes included in this analysis as well as the Rds and RData workspace associated with this project.

#### R

This folder contains the prep code `00_HelperFunctions.R`, code for the decadal analysis `01_DecadalSpeciesDist.R`, and code for plotting `02_DecadalPlots.R`. `OO_HelperFunctions.R` contains code to prep the survey data as well as functions that will be mapped over nested data in `01_DecadalSpeciesDist.R`. Please run all code in chronological order. 

#### Temp_Results

This folder contains CSV outputs from the `01_DecadalSpeciesDist.R` and multipanel plots for each response variable. 
