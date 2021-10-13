 
********* Maximum Likelihood Estimation
********* using the Random parametr Model 
********* code by Apesteguia et al.


********* Separate analysis of safe & no-safe treatments for each task

********* preliminaries

clear all
set more off
capture log close
log using estimation.log, replace


*** importing the likelihood function
do define_loglikelihood.do


clear

**** running the estimation *****

**HL

use Data/cleanHL

**both risky
preserve 
keep if treatment == 1

ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female ) (LN_lambda: female ) (kapp: female ) , ///
	robust cluster(id) technique(nr) maximize

ml display

eststo HL

restore

**onesafe
preserve 
keep if treatment == 2

ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female ) (LN_lambda:  female) (kapp: female ) , ///
	robust cluster(id) technique(nr) maximize

ml display

eststo HLSAFE

restore


**BRET
clear
use Data/cleanBRET

**both risky
preserve 
keep if treatment == 1

ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female ) (LN_lambda: female ) (kapp: female ) , ///
	robust cluster(id) technique(nr) maximize

ml display

eststo BRET

restore

**onesafe
preserve 
keep if treatment == 2

ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female ) (LN_lambda: female ) (kapp: female) , ///
	robust cluster(id) technique(nr) maximize

ml display

eststo BRETSAFE

restore


**EG
clear
use Data/cleanEG

**one safe
preserve 
keep if treatment == 1

ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female soep) (LN_lambda:  female) (kapp: female) , ///
	robust cluster(id) technique(nr) maximize

ml display

eststo EG

restore

**allrisky
preserve 
keep if treatment == 3

ml model lf loglike_RPM_logit (r: CHOICE OMEGA = female soep) (LN_lambda:  female) (kapp: female) , ///
	robust cluster(id) technique(nr) maximize

ml display

eststo EGNOSAFE

restore

** export
cd Tables

esttab HL HLSAFE BRET BRETSAFE EGNOSAFE EG Table_10.tex, se replace label booktabs mtitles( "HL"  "HL safe"  "BRET" "BRETsafe" "EGnosafe" "EG" ) title(ML structural estimations Apesteguia by subtask \label{tab:Success}) b(a2) se(a1) star(* 0.10 ** 0.05 *** 0.01) pr2 addnote("notes")
