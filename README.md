# Data and analysis scripts for the paper "Safe Options and Gender Differences in Risk Attitudes"

This repository contains all the data and analysis scripts to reproduce all tables and figures in the paper *Safe Options and Gender Differences in Risk Attitudes* by Paolo Crosetto and Antonio Filippin. 

## Organisation of the repository

- Data for the whole project can be found in the `Data` folder. Data is split into 9 different (`.csv` and `.dta`) files for convenience; all the data stems from the `.csv` files.
- You generate figures by running the `Plots.R` script in `R`. The generated plots are saved in the `Figures` folder. This script works based on the three `.csv` data files. 
- You generate all tables and tests -- with the exclusion of Maximum Likelihood estimations presented in Table 10 and 11 in the paper by running the `Tables.R` script in `R`. Tables were mostly written down in latex by hand from the results that are to be found in the `.csv` output from each table. This script works based on the three `.csv` data files. 
- the `Power.R` script performs ex-ante and ex-post power analyses for the paper.
- You generate the Maximum Likelihood estimations by running the `Table_10.do` and `Table_11.do` dofiles in `Stata` (13+). The raw latex tables outputted by `Stata` are to be found in the `Tables` folder. These estimations require data transformations that follow the procedure of Apesteguia, J., Ballester, M.A., 2018. *Monotone stochastic choice models: The case of risk and time preferences*. Journal of Political Economy 126, 74–106. The likelihood function is provided in `define_likelihood.do`. The scripts work based on the data contained in the six `.dta` data files. Those datasets are just transformations of the original `.csv` data following the procedure outlined by Apesteguia and Ballester. These transformations are not provided here. 

## Dependencies

The analysis was carried out partly in `R`(plots), partly in `Stata` (tests, maximum likelihood estimations).

### For R

The R scripts depends only on the `tidyverse` library. This is easily installed from CRAN, using `install.packages("tidyverse")` (go grab a coffee, it takes a while)

### For Stata

The scripts run with `Stata` 13 or higher. They also depend on dedicated `.do` files that apply the data transformation and computation required to run the Apesteguia et al. TODO ADD REF Random Parameter model; but those files are provided within this repository.

