# Phase 1

## Folder structure

* `data/` - processed data in common format, aquired by running scripts from `process_raw.R`
  * `Northwell_US.csv` removed from repository, due to non-disclosure agreement.
* `raw_data/` - data we found, and is present in the repository as-is (except for renaming).
  * `Yan_reply_First_last_wtime.csv` removed from repository, due to non-disclosure agreement. Data from the supplement to the article (Barish et al. 2021), DOI: 10.1038/s42256-020-00254-2
  * `Outcomerea_FR.xlsx` data from the supplement to the article (Dupuis et al. 2021), DOI: 10.1038/s42256-020-00252-4
  * `St_Antonius_NL.xlsx` data from the supplement to the article (Quanjel et al. 2021), DOI: 10.1038/s42256-020-00253-3
  * `Tongji_375_CN.xlsx` training data from the supplement to the article (Yan et al. 2020), DOI: 10.1038/s42256-020-0180-7
  * `Tongji_110_CN.xlsx` validation data from the supplement to the article (Yan et al. 2020), DOI: 10.1038/s42256-020-0180-7
* `3D_plots/` - ready made 3D plots using plotly in the `3dtests.ipynb` notebook.
* `screenshots/` - various images used for reports and presentations
* `reports/` - reports

## Scripts
* `libraries.R` - utility for loading necessary libraries
* `process_raw.R` - loading script for merging the data into one format (`raw_data/` -> `data/`)
* `ppv_graphs.R` - recreate LDH/PPV histogram figure from the (Barish et al. 2020) article
* `histograms.R` - create our version of the histogram
* `freqpoly.R` - create density plots of the data
* `models_*.R`, `xgboost_*.R` - various classification models
* `clustering_*.R` - unsupervised approach to classifying the data
* `3dtests.ipynb` - notebook containing plotly scripts for creating 3d plots
* `SVM_test.ipynb` - notebook containing SVM models

## Northwell data (Barish et al. 2020)
Insert file `Yan_reply_First_last_wtime.csv` into raw_data, run the appropriate function from `process_raw.R`. Output should be present in
`data/Northwell_US.csv`.
