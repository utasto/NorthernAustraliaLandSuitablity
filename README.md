# NorthernAustraliaLandSuitablity
Workflows and scripts for the Northern Australia land suitability assessment projects.


The directories atthe top level of the repo contains the R scripts used for a generic workflow for doing the DSM modelling and Land Suitability assessment processing for the CSIRO Northen Australia Water Resource Assessment Projects. Thus far, these projects include the NAWRA (Mitchell, Darwin and Fitzroy catchments) https://www.csiro.au/en/research/natural-environment/water/water-resource-assessment/nawra and the Roper River Catchment https://www.csiro.au/en/research/natural-environment/water/water-resource-assessment/roper-river-water-resource-assessment.

The generic workflow is based on the scripts used for the Roper Catchment work.

The actual scripts used in the individual catchmnets work are also archived in this repo.

Descriptions of the functionality of the scripts in the various directories

- **Aquaculture** - Calculations used in generating the aquaculture pond suitability maps
- **DataPreparation** - Various routines for whipping all the input data sets into shape
- **DSM** - Digital Soil Modelling routines for generating the soil attribute rasters
- **HPC** - Scripts that provide the processing environment for running jobs on the CSIRO Pearcey High Performance Computer - mostly suitability processing related
- **Modelling** - Random Forest modelling of soil attributes
- **ProjectArchives** - An archive of scripts used in the previous Northern Australia land suitability modelling
- **Suitability** - Calculation of the land suitabilities
- **Utils** - Generic utilites

