 
********* Maximum Likelihood Estimation
********* using the Random parametr Model 
********* code by Apesteguia et al.


********* Joint analysis over treatments for each task

********* preliminaries

clear all
set more off
capture log close
log using estimation.log, replace


***importing the likelihood function
do define_loglikelihood.do


****getting the data

**HL
clear
use Data/cleanHL

gen safe = 0
replace safe = 1 if treatment == 2
gen femsafe = female*safe

gen HL = 1
gen BRET = 0
gen EG = 0

* binarisation of the soep variable
gen soepb = 0
replace soepb = 1 if soep >=5


ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female safe femsafe ) (LN_lambda: female safe femsafe ) (kapp: female safe femsafe) , ///
	robust cluster(id) technique(nr) maximize
ml display

eststo HL

saveold cleanHLs, replace


clear
use Data/cleanBRET
gen safe = 0
replace safe = 1 if treatment == 2

gen HL = 0
gen BRET = 1
gen EG = 0
gen femsafe = female*safe

* binarisation of the soep variable
gen soepb = 0
replace soepb = 1 if soep >=5


ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female safe femsafe ) (LN_lambda: female safe femsafe) (kapp: female safe femsafe) , ///
	robust cluster(id) technique(nr) maximize
ml display

eststo BRET

saveold cleanBRETs, replace


clear
use Data/cleanEG
gen safe = 0
replace safe = 1 if treatment ==1
gen femsafe = female*safe

gen HL = 0
gen BRET = 0
gen EG = 1

* binarisation of the soep variable
gen soepb = 0
replace soepb = 1 if soep >=5


ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female safe femsafe soep) (LN_lambda: female safe femsafe) (kapp: female safe femsafe) , ///
	robust cluster(id) technique(nr) maximize
ml display

eststo EG

cd Tables

esttab HL BRET EG using Table_11.tex, se replace label booktabs mtitles( "HL" ) title(ML structural diff in diff Apesteguia by task \label{tab:Success}) b(a2) se(a1) star(* 0.10 ** 0.05 *** 0.01) pr2 addnote("notes")
