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

## In this file we compute ex-ante and ex-post power analyses

## We can get effect sizes from the literature for the *baseline* tasks

## The most reliable effect size estimates for each task are
## EG: Nelson (2015) meta-analysis, d = .46
## BRET: Crosetto & Filippin (JRU 2013) BRET paper: d = ~0
## HL: Filippin & Crosetto (ManSci 2015): d = 0.17

## We can **not** get effect sizes for the treatment tasks, that are brand new

## Under our hypothesis, the presence of a safe option is the element that triggers gender differences
## So, under our hypothesis, we can expect each task to "turn into" a sort-of-EG.

## This means that ex-ante we expected
##  - the HLsafe and BRETsafe to move towards d = .45;
##  - the EGnosafe to move towards d = .17 

#### 1. Ex-ante power analyses ####

# in the `assumptions` table we collect in compact form the assumptions discussed above
assumptions <- tibble(task = rep(c("HL", "BRET", "EG"),2),
                      treatment = c(rep("Baseline", 3), "safe", "safe", "nosafe"),
                      d_ex_ante = c(0.17, 0.01, 0.46, 0.46, 0.46, 0.17))

# let's compute the N needed to obtain 80% power given the assumptions for each condition
# we use the `pwr.t.test` function from the `pwr` package because it has nicer output than the default `power` function
# and because it follows cohen's terminology and practices that make for easier interpretation.

ex_ante_power <- assumptions %>% 
  group_by(task, treatment) %>% 
  group_modify(~map_df(.$d_ex_ante, ~tidy(pwr.t.test(d = ., sig.level = 0.05, power = 0.8)))) %>% 
  left_join(assumptions, by = c("task", "treatment")) %>% 
  select(task, treatment, d_ex_ante, needed_N = n)

e## under our hypotheses, 
## 1. the effect is too low to detect when there is no safe option (and we expect not to detect it)
## 2. we need about 75 subjects per cell tod etect an effect with a two-tailed t-test if the triggered effect is as high as in standard EG.


#### 2. Ex-post power analyses ####

## It turns out that the effect triggered by introducing / eliminating a safe option was not as strong as per our hypotheses. 
## So, *given our results*, how much power did we have? 
## This is a tricky and not really sound exercise, because it is circular in nature
## But let's run it nonetheless. 

## in the `results` table we list the actual `d` found in the experiments,
## as well as the number of men (nmen) and women (nwomen) in our sample

results <- tibble(task = rep(c("HL", "BRET", "EG"),2),
                  treatment = c(rep("Baseline", 3), "safe", "safe", "nosafe"),
                  cohens_d = c(.185, .018, 0.605, .427, .254, 0.576),
                  nmen = c(84, 106, 57, 79, 73, 67),
                  nwomen = c(95, 165, 77, 86, 118, 78))


## let's compute the power we had ex-post given the effects we found and the sample sizes
## we use the `pwr.t2n.test` function that can deal with unequal subsample sizes, as it is our case. 

ex_post_power <- results %>% 
  group_by(task, treatment) %>% 
  group_modify(~tidy(pwr.t2n.test(n2 = .$nmen, n1 = .$nwomen, d = .$cohens_d, sig.level = 0.05))) %>% 
  select(task, treatment, ex_post_power = power) %>% 
  left_join(results, by = c("task", "treatment")) %>% 
  select(task, treatment, d_ex_post = cohens_d, ex_post_power)


## merging ex-ante and ex-post tables in a single object

Table_Power <- 
  ex_ante_power %>% 
  left_join(ex_post_power, by = c("task", "treatment")) %>% 
  arrange(treatment)

## exporting to csv
Table_Power %>% 
  write_csv("Tables/Table_power.csv")

## exporting to a nicely-formatted latex table
Table_Power %>% 
  ungroup() %>% 
  mutate(needed_N = round(needed_N),
         ex_post_power = round(ex_post_power,2)) %>% 
  kable(format = "latex", booktabs = T, caption = "Power Analysis") %>% 
  add_header_above(c("","","Ex-ante" = 2, "Ex-post" = 2)) %>% 
  write_file("Tables/Table_power.tex")
