########################################
## Crosetto and Filippin, JRU 2021    ##
########################################

### This script generates all figures for the paper
### it depends on data stored in the /Data folder

#### 0. libraries ####
library(tidyverse)       ## R's dialect used in this and all other scripts
library(scales)          ## extra helper functions to tweak ggplots

#### 1. HL ####
df <- read_csv("Data/HL.csv")

# recoding variables
df <- df %>% 
  mutate(safe = as.factor(safe),
         numbersafe = as.factor(numbersafe),
         female = as.factor(female)) %>% 
  mutate(safe = fct_recode(safe, "Holt & Laury" = "0", "Holt & Laury safe" = "1"),
         female = fct_recode(female, "Women" = "1", "Men" = "0"))

# selecting only consistent subjects
df <- df %>% 
  filter(inconsistent == 0)

# add the level 1 to the numbersafe factor (not in the data because no one chose it) then reorder
df <- df %>% 
  mutate(numbersafe = fct_expand(numbersafe, "1")) %>% 
  mutate(numbersafe = fct_relevel(numbersafe, "0", "1", "2"))

# computing shares of each option
df <- df %>% 
  group_by(safe, female, numbersafe, .drop = F) %>% 
  summarise(nchoices = n()) %>% 
  group_by(safe, female) %>% 
  mutate(share = nchoices/sum(nchoices))

# plotting
ggplot(df, aes(numbersafe, share, fill=female, color=female))+geom_bar(stat="identity", position = position_dodge(), alpha = 0.5)+
  facet_grid(.~safe)+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  xlab("Number of safe choices")+ylab("")+
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(labels = c("0%","10%","20%","30%","40%"))+
  scale_fill_grey(start = 0.1, end = 0.6)+
  scale_color_grey(start = 0.1, end = 0.6)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# saving the figure
ggsave("Figures/Figure_1.png", width = 6, height = 4.5, units = "in", dpi = 300)


#### 2. BRET ####
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

# plotting
ggplot(df, aes(perc, group=female, color=female, fill=female))+
  geom_density(alpha=0.5, adjust = 0.6)+
  facet_grid(.~safe)+
  scale_y_continuous(labels = percent)+
  scale_fill_grey(start = 0.1, end = 0.6)+
  scale_color_grey(start = 0.1, end = 0.6)+
  xlab("Number of boxes")+ylab("")+  
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  theme(legend.position = "bottom", legend.title = element_blank())

# saving the figure
ggsave("Figures/Figure_4.png", width = 6, height = 4.5, units = "in", dpi = 300)



#### 3. EG ####
df <- read_csv("Data/EG.csv")

# recoding variables
df <- df %>% 
  mutate(safe = as.factor(safe),
         female = as.factor(female)) %>% 
  mutate(safe = fct_recode(safe, "Eckel & Grossman nosafe" = "0",
                           "Eckel & Grossman" = "1"),
         female = fct_recode(female, "Women" = "1", "Men" = "0"))

# computing the shares of each option by gender and safe
df <- df %>% 
  group_by(safe, female, eg) %>% 
  summarise(nchoices = n()) %>% 
  mutate(share = nchoices/sum(nchoices))

# plotting
ggplot(df, aes(eg, share, fill=female, color=female))+
  geom_bar(stat="identity",position = position_dodge(), alpha = 0.5)+
  facet_grid(.~safe)+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  xlab("Chosen lottery")+ylab("")+
  scale_y_continuous(labels = c("0%","10%","20%","30%","40%"))+
  scale_fill_grey(start = 0.1, end = 0.6)+
  scale_color_grey(start = 0.1, end = 0.6)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# saving the figure
ggsave("Figures/Figure_5.png", width = 6, height = 4.5, units = "in", dpi = 300)