## Packages used ####
library(Momocs)
library(ggplot2)
library(ggpubr)
library(readxl)
library(forcats)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(MetBrewer)
library(rstatix)
library(vegan)
library(pairwiseAdonis)
library(cowplot)

Dataset <- read.csv("data/CTC_Dataset_complete.csv")
Dataset_cores <- read.csv("data/CTC_Dataset_cores.csv")


## Total of carinated cores ####
Dataset_cores %>%
  filter(Core_type != "Tested", Laminar_y_n == "TRUE") %>%
  tabyl(Layer, Carinated_y_n) %>%
  adorn_totals("row") %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  adorn_title(placement = "top") %>%
  kbl(align = "lccccr") %>%
  kable_classic_2(lightable_options = "striped", full_width = F)


#### Blades with bladelet negatives ####

Dataset %>%
  filter(Blank == "Blade") %>%
  tabyl(Layer, Bladelet_negatives) %>%
  adorn_totals("row") %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  adorn_title(placement = "top") %>%
  kbl(align = "lccccr") %>%
  kable_classic_2(lightable_options = "striped", full_width = F)


#### Blanks linked to the maintenance of carinated cores ####

Dataset %>%
  filter(Blank_technology == "Maintenance carinated") %>%
  tabyl(Layer, Blank_technology) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns(position = "front") %>%
  adorn_title(placement = "top") %>%
  kbl(align = "lccccr") %>%
  kable_classic_2(lightable_options = "striped", full_width = F)
