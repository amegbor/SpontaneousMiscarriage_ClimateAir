# SpontaneousMiscarriage_ClimateAir

This repository contains the code and analyses for the paper:

**Amegbor, Prince M.a,*; Quansah, Reginaldb; Gu, Lijuanc.**  
*The effect of climate anomalies and ambient air pollution on spontaneous miscarriage in Ghana: Evidence from a national cross-sectional survey.*  
Environmental Epidemiology 9(5): e420, October 2025.  
DOI: [10.1097/EE9.0000000000000420](https://doi.org/10.1097/EE9.0000000000000420)

---

## Repository Structure

- **MainResults.R**  
  Contains the code for the primary analyses presented in the main manuscript. This includes:
  - Bayesian models of climate anomalies and air pollution exposures
  - Calculation of hazard ratios and 95% credible intervals
  - Generation of summary tables for the main manuscript

- **SupplementaryResults.R**  
  Contains the code for all supplementary analyses, including:

  ### Appendix B – Region-Specific Effects
  - Results of region-specific effects on spontaneous miscarriage

  ### Appendix C – Preconception Exposure by Rural-Urban Residency
  - Figure C1 – Non-linear association between preconception mean temperature anomaly and spontaneous miscarriage
  - Figure C2 – Non-linear association between preconception maximum temperature anomaly and spontaneous miscarriage
  - Figure C3 – Non-linear association between preconception precipitation anomaly and spontaneous miscarriage
  - Figure C4 – Non-linear association between preconception PM2.5 exposure and spontaneous miscarriage
  - Figure C5 – Non-linear association between preconception Carbon Monoxide (CO) exposure and spontaneous miscarriage
  - Figure C6 – Non-linear association between preconception Nitrogen Dioxide (NO2) exposure and spontaneous miscarriage
  - Figure C7 – Non-linear association between preconception Sulphur Dioxide (SO2) exposure and spontaneous miscarriage
  - Figure C8 – Non-linear association between preconception Ozone (O3) exposure and spontaneous miscarriage
  - Figure C9 – Non-linear association between preconception Enhanced Vegetation Index (EVI) exposure and spontaneous miscarriage

  ### Appendix D – Preconception Exposure by Vegetation (EVI)
  - Figure D1 – Non-linear association between preconception mean temperature anomaly and spontaneous miscarriage by vegetation
  - Figure D2 – Non-linear association between preconception maximum temperature anomaly and spontaneous miscarriage by vegetation
  - Figure D3 – Non-linear association between preconception precipitation anomaly and spontaneous miscarriage by vegetation
  - Figure D4 – Non-linear association between preconception PM2.5 exposure and spontaneous miscarriage by vegetation
  - Figure D5 – Non-linear association between preconception Carbon Monoxide (CO) exposure and spontaneous miscarriage by vegetation
  - Figure D6 – Non-linear association between preconception Nitrogen Dioxide (NO2) exposure and spontaneous miscarriage by vegetation
  - Figure D7 – Non-linear association between preconception Sulphur Dioxide (SO2) exposure and spontaneous miscarriage by vegetation
  - Figure D8 – Non-linear association between preconception Ozone (O3) exposure and spontaneous miscarriage by vegetation

---

## Dependencies

The analyses use the following R packages:

- **INLA** – Bayesian modeling  
- **tidyverse** – Data manipulation and plotting  
- **labelled** – Handling labelled survey data  
- **ggplot2** – Plotting hazard ratios and credible intervals  
- **ggh4x** – Enhanced ggplot2 faceting and plotting features  

Make sure these packages are installed before running the scripts:

```r
install.packages(c("tidyverse", "labelled", "ggplot2", "ggh4x"))
# INLA is installed via:
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

```bash
git clone https://github.com/your-username/SpontaneousMiscarriage_ClimateAir.git
