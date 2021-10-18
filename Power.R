########################################
## Crosetto and Filippin, JRU 2021    ##
########################################

### This script generates power computations for the paper
### it depends on the "pwr" R package only


## libraries
library(tidyverse)  # R dialect used in this script
library(pwr)        # to nicely compute power
library(broom)      # to format nicely the output of the pwr functions
library(purrr)      # to apply iteratively the pwr function to all of our data in one go
library(kableExtra) # to nicely format .tex tables


#### 0. Assumptions ####

## In this file we compute the "ex-ante" power analysis

## We can get effect sizes from the literature for the *baseline* tasks

## The most reliable effect size estimates for each task are
## EG: Nelson (2015) meta-analysis, d = .46
## BRET: Crosetto & Filippin (JRU 2013) BRET paper: d = ~0
## HL: Filippin & Crosetto (ManSci 2015): d = 0.17

## We can **not** get effect sizes from the liteature for the treatment tasks, that are brand new

## Under our hypothesis, the presence of a safe option is the element that triggers gender differences
## So, under our hypothesis, we can expect each task to "turn into" a sort-of-EG.

## This means that ex-ante we expected
##  - the HLsafe and BRETsafe to move towards d = .46;
##  - the EGnosafe to move towards d = .17 

#### 1. "ex-ante" power analyses ####

# in the `assumptions` table we collect in compact form the assumptions discussed above
assumptions <- tibble(task = rep(c("HL", "BRET", "EG"),2),
                      treatment = c(rep("Baseline", 3), "safe", "safe", "nosafe"),
                      d_ex_ante = c(0.17, 0.01, 0.46, 0.46, 0.46, 0.17))

# let's compute the N needed to obtain 80% power given the assumptions for each condition
# we use the `pwr.t.test` function from the `pwr` package because it has nicer output than the default `power` function
# and because it follows cohen's terminology and practices that make for easier interpretation.

Table_Power <- assumptions %>% 
  group_by(task, treatment) %>% 
  group_modify(~map_df(.$d_ex_ante, ~tidy(pwr.t.test(d = ., sig.level = 0.05, power = 0.8)))) %>% 
  left_join(assumptions, by = c("task", "treatment")) %>% 
  select(task, treatment, d_ex_ante, needed_N = n)

## under our hypotheses, 
## 1. the effect is too low to detect when there is no safe option (and we expect not to detect it)
## 2. we need about 75 subjects per cell to detect an effect with a two-tailed t-test if the triggered effect is as high as in standard EG.

#### 2. exporting the table ####

## exporting to csv
Table_Power %>% 
  write_csv("Tables/Table_power.csv")

## exporting to a nicely-formatted latex table
Table_Power %>% 
  ungroup() %>% 
  filter(treatment != "Baseline") %>% 
  mutate(needed_N = round(needed_N)) %>% 
  kable(format = "latex", booktabs = T, caption = "Sample size needed to reach power of 0.8") %>% 
  add_header_above(c("","","Ex-ante" = 2)) %>% 
  write_file("Tables/Table_power.tex")
