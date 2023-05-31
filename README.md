# GKI-Spawning-activity

Author: Hugo Harrison (orcid: 0000-0001-8831-0086)
Date: 2023-05-31
Static repository: https:/doi.org/10.5061/dryad.2fqz612v0
Live repository: https://github.com/HugoBH/GKI-Spawning-activity.git

Dataset Attribution and Usage
-----------------------------

* License: Use of these data is covered by the following license:
  * Title: CC0 1.0 Universal (CC0 1.0)
  * Specification: the authors respectfully request to be contacted by researchers interested in the re-use of these data so that the possibility of collaboration can be discussed. 

* Please cite the manuscript when refering to this work. Suggested Citations:

  * Dataset citation:
    > Harrison HB, Drane L, Berumen ML, Cresswell BJ, Evans RD, Galbraith GF, Srinivasan M, Taylor BM, Williamson DH, Jones GP. 2023  Ageing of juvenile coral grouper (Plectropomus maculatus) reveals year-round spawning and recruitment: implications for seasonal closures. Dryad, Dataset, https:/doi.org/10.5061/dryad.2fqz612v0

  * Corresponding publication:
    > Harrison HB, Drane L, Berumen ML, Cresswell BJ, Evans RD, Galbraith GF, Srinivasan M, Taylor BM, Williamson DH, Jones GP. 2023 Data from Ageing of juvenile coral grouper (Plectropomus maculatus) reveals year-round spawning and recruitment: implications for seasonal closures. Proc Roy Soc B, DOI: https://doi.org/10.1098/rspb.2023.0584


Contact Information
-------------------

  * Name: Hugo B Harrison
  * Affiliations: School of Biological Sciences, University of Bristol
  * ORCID ID: https://orcid.org/0000-0002-2575-0751
  * Email: hugo.harrison@bristol.ac.uk
  * Alternate Email: hb.harrison@gmail.com

Data and File Overview
======================

Setup
-----

* Please refer to the manuscript, associated markdown files (.RMD), and scripts for detailed annotations of the datasets, analyses, and methods.  
* Recommended software tools: RSTUDIO Version 2023.03.1+446; R version 4.2.3 (2023-03-15)


Dataset descriptors
---------------

* Closure_dates.csv: a comma-delimited file containing the time of fishery closures on the Great Barrier Reef.
* csv.w00198.20220815123041.128.csv: Flood gauge data from the Fitzroy river
* IDCJAC0009_033260_1800_Data.csv: Rainfall data at the Keppel Islands
* g4.areaAvgTimeSeries.MODISA_L3m_NSST_8d_4km_R2019_0_sst.20020704-20220601.150E_23S_150E_23S.csv: Sea surface temperature data for the Great Keppel Islands
* GBR Plectropomus age samples_BMT.xlsx: Daily ageing of otoliths of Plectropomus maculatus collected at the Keppel Islands
* Plec_Sample data_Keppels_ KI3.2.xlsz: Juvenile Plectropomus maculatus collected at the Keppel Islands

Script descriptors
---------------

* The SpawningTimes_MS.RMD contains all text and scripts to reproduce the figures in the main text.
* The SpawningTimes_SI.RMD contains all text and scripts to reproduce the electronic supplementary material.

The following scripts follow a numerical order to process and analyse ageing and environmental data.
* SpT_00_packages.R: Loads required packages
* SpT_01_data_wrangle.R: Import and process ageing and collection data.
* SpT_10_est_growth_rate.R: Run GLMM to estimate growth rates.
* SpT_20_spawn_time.R: Estimate the time of hatch/spawn of juvenile P. maculatus collected at the Keppel Islands.
* SpT_21_wrangle enviro data.R: Import and process environment data (Rain, SST, Lunar, Flood).
* SpT_22_combine data for gam.R: Combine spawn time with environmental data for GAM analyses.
* SpT_30_GAM_spawning peaks.R: Run GAM for spawn time only to identify peaks in spawning.
* SpT_31_GAM_enviro.pred.R: Run GAM to test effect of environmental predictors on spawning patterns.
* SpT_32_GBM_enviro.pred.R: Runs a boosted regression to test effect of environmental predictors on spawning patterns.
* SpT_40_SeasonalClosures.R: Import and process fishery closure information. 
* SpT_41_SeasonalClosures glm.R: Run GLMM to test the effect of closures on spawning patterns.


- - -
END OF README