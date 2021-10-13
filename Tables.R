## Crosetto and Filippin, JRU 2021    ##

### This script generates most figures for the paper
### it depends on data stored in the /Data folder


#### 0. libraries ####
library(tidyverse)       ## R's dialect used in this and all other scripts
library(scales)          ## extra helper functions to tweak ggplots
library(broom)           ## to tidy different test's outputs and extract the p-values
library(tseries)         ## for the skewness/kurtosis normality test
library(effectsize)      ## to compute cohen's d


#### Table 1: sample ####

HL <- read_csv("Data/HL.csv") %>% 
  mutate(task = "HL") %>% 
  select(task, safe, female,)

BRET <- read_csv("Data/BRET.csv") %>% 
  mutate(task = "BRET") %>% 
  select(task, safe, female)

EG <- read_csv("Data/EG.csv") %>% 
  mutate(task = "EG") %>% 
  select(task, safe, female) 

df <- bind_rows(HL, BRET, EG)

## table

Table_1 <- df %>% 
  group_by(task) %>% 
  mutate(N_task = n()) %>% 
  group_by(task, safe) %>% 
  mutate(N_condition = n()) %>% 
  group_by(task, safe, female) %>% 
  mutate(N_female = n()) %>% 
  distinct() %>% 
  pivot_wider(names_from = female, values_from = N_female)

Table_1 %>% 
  write_csv("Tables/Table_1.csv")



#### Table 4: HL ####
df <- read_csv("Data/HL.csv")


# selecting only consistent subjects
df <- df %>% 
  filter(inconsistent == 0)


# recoding variables
df <- df %>% 
  mutate(safe = as.factor(safe),
         numbersafe = as.factor(numbersafe),
         female = as.factor(female)) %>% 
  mutate(safe = fct_recode(safe, "Holt & Laury" = "0", "Holt & Laury safe" = "1"),
         female = fct_recode(female, "Women" = "1", "Men" = "0"))

## descriptives
table4_descriptives <- df %>% 
  group_by(safe, female) %>% 
  summarise(N = n(), 
            ave_safe_choice = mean(as.numeric(numbersafe)), 
            std_safe_choice = sd(as.numeric(numbersafe)))

## tests

# t test
table4_t_test <- df %>% 
  group_by(safe) %>% 
  group_modify(~ tidy(t.test(as.numeric(numbersafe) ~ female, data = ., alternative = "two.sided"))) %>% 
  select(safe, t_test = p.value)
  
# cohens d
table4_cohens_d <- df %>% 
  group_by(safe) %>% 
  group_modify(~cohens_d(as.numeric(numbersafe) ~ female, data = .)) %>% 
  select(safe, Cohens_d)

# mann whitney
table4_mann_whitney_test <- df %>% 
  group_by(safe) %>% 
  group_modify(~tidy(wilcox.test(as.numeric(.$numbersafe) ~ female, data = .))) %>% 
  select(safe, mann_whitney_test = p.value)

# normality test
table4_normality_test <- df %>% 
  group_by(safe) %>% 
  group_modify(~tidy(jarque.bera.test(as.numeric(.$numbersafe)))) %>% 
  select(safe, normality_test = p.value)

## assembling table 4
Table_4 <- table4_descriptives %>% 
  left_join(table4_cohens_d, by = "safe") %>% 
  left_join(table4_mann_whitney_test, by = "safe") %>% 
  left_join(table4_normality_test, by = "safe") %>% 
  left_join(table4_t_test, by = "safe")

## exporting
Table_4 %>% 
  write_csv("Tables/Table_4.csv")



#### Table 5: BRET ####
df <- read.csv("Data/BRET.csv")

# recoding variables
df <- df %>% 
  as_tibble() %>% 
  mutate(safe = as.factor(safe),
         female = as.factor(female)) %>% 
  mutate(safe = fct_recode(safe, "Bomb Risk Elicitation Task" = "0", "Bomb Risk Elicitation Task safe" = "1"),
         female = fct_recode(female, "Women" = "1", "Men" = "0"))

# selecting only consistent subjects
df <- df %>% 
  filter(outlier == 0)

## descriptives
table5_descriptives <- df %>% 
  group_by(safe, female) %>% 
  summarise(N = n(), 
            ave_safe_choice = mean(perc), 
            std_safe_choice = sd(perc))


## tests

# t test
table5_t_test <- df %>% 
  group_by(safe) %>% 
  group_modify(~tidy(t.test(perc ~ female, data = ., alternative = "two.sided"))) %>% 
  select(safe, t_test = p.value)

# cohens d
table5_cohens_d <- df %>% 
  group_by(safe) %>% 
  group_modify(~cohens_d(perc ~ female, data = .)) %>% 
  select(safe, Cohens_d)

# mann whitney
table5_mann_whitney_test <- df %>% 
  group_by(safe) %>% 
  group_modify(~tidy(wilcox.test(perc ~ female, data = .))) %>% 
  select(safe, mann_whitney_test = p.value)

# normality test
table5_normality_test <- df %>% 
  group_by(safe) %>% 
  group_modify(~tidy(jarque.bera.test(.$perc))) %>% 
  select(safe, normality_test = p.value)

## assembling table 5
Table_5 <- table5_descriptives %>% 
  left_join(table5_cohens_d, by = "safe") %>% 
  left_join(table5_mann_whitney_test, by = "safe") %>% 
  left_join(table5_normality_test, by = "safe") %>% 
  left_join(table5_t_test, by = "safe")

## exporting
Table_5 %>% 
  write_csv("Tables/Table_5.csv")


#### Table 7: Complexity  ####

HL <- read_csv("Data/HL.csv") %>% 
  mutate(task = "HL") %>% 
  select(task, safe, female, easy)

BRET <- read_csv("Data/BRET.csv") %>% 
  mutate(task = "BRET") %>% 
  select(task, safe, female, easy)  %>% 
  filter(!is.na(easy))

EG <- read_csv("Data/EG.csv") %>% 
  mutate(task = "EG") %>% 
  select(task, safe, female, easy) 

df <- bind_rows(HL, BRET, EG)

## descriptives
Table_7_descriptives <- df %>% 
  group_by(task, female, safe) %>% 
  summarise(mean = mean(easy), 
            sd = sd(easy)) %>% 
  pivot_wider(names_from = safe, values_from = c("mean", "sd")) %>% 
  mutate(diff = mean_1 - mean_0) %>% 
  mutate(nosafe = paste0(round(mean_0,2), " (", round(sd_0,2), ")"),
           safe = paste0(round(mean_1,2), " (", round(sd_1,2), ")")) %>% 
  select(task, female, nosafe, safe, diff)


## Wilcoxon rank sum test
Table_7_tests <- df %>% 
  group_by(task, female) %>% 
  group_modify(~tidy(wilcox.test(easy ~ safe, data = .))) %>% 
  select(task, female, WRST = p.value)

## assembling
Table_7 <- Table_7_descriptives %>% 
  left_join(Table_7_tests, by = c("task", "female"))

## exporting
Table_7 %>% 
  write_csv("Tables/Table_7.csv")


#### Table 8: Volatility ####

HL <- read_csv("Data/HL.csv") %>% 
  mutate(task = "HL") %>% 
  filter(inconsistent == 0) %>% 
  select(task, safe, female, choice = numbersafe)

BRET <- read_csv("Data/BRET.csv") %>% 
  mutate(task = "BRET") %>% 
  filter(outlier == 0) %>% 
  select(task, safe, female, choice = perc)

EG <- read_csv("Data/EG.csv") %>% 
  mutate(task = "EG") %>% 
  select(task, safe, female, choice = eg) 

df <- bind_rows(HL, BRET, EG)

## table
Table_8 <- df %>% 
  group_by(task, female, safe) %>% 
  summarise(sd = sd(choice)) %>% 
  pivot_wider(names_from = safe, names_prefix = "sd", values_from = sd) %>% 
  mutate(diff = sd1 - sd0)

## exporting
Table_8 %>% 
  write_csv("Tables/Table_8.csv")

#### Table 9: SOEP ####

# getting the data
HL <- read_csv("Data/HL.csv") %>% 
  mutate(task = "HL") %>% 
  select(task, safe, female, soep)

BRET <- read_csv("Data/BRET.csv") %>% 
  mutate(task = "BRET") %>% 
  select(task, safe, female, soep)

EG <- read_csv("Data/EG.csv") %>% 
  mutate(task = "EG") %>% 
  select(task, safe, female, soep) 

df <- bind_rows(HL, BRET, EG)

# descriptives
table_9_descriptives <- df %>% 
  group_by(task, safe, female) %>% 
  summarise(mean_soep = mean(soep),
            sd_soep = sd(soep))

# mann-whitney
table_9_test <- df %>% 
  group_by(task, safe) %>% 
  group_modify(~tidy(wilcox.test(soep ~ female, data = .))) %>% 
  select(task, safe, mann_whitney = p.value)

# assembling
Table_9 <- table_9_descriptives %>% 
  left_join(table_9_test, by = c("task", "safe"))

# exporting
Table_9 %>% 
  write_csv("Tables/Table_9.csv")
