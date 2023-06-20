library(tidyverse)
library(readxl)
library(skimr)
library(ggcorrplot)
library(broom)

# raw data ----
data <- read_xlsx("2023_sigevann_Drammen_KMK.xlsx") %>% 
  mutate(prosent_rens = (verdi_inn_korr-verdi_ut_korr)/verdi_inn_korr) %>% 
  filter(kategori_2 != "PFAS",
         !(parameter == "THC >C12-C16" & prosent_rens<0),
         !(parameter == "Sum BTEX" & dato == as.Date("2023-02-27")),
         !(parameter == "Sum PCB 7"),
         !(parameter == "P-total"  & prosent_rens <0),
         !(parameter == "TOC" & prosent_rens <0),
         !(forkortelse == "NH4 + NH3" & dato == as.Date("2022-11-14")),
         !(parameter == "Tot N" & dato == as.Date("2022-11-14"))
         ) %>% 
  select(c(parameter, dato, prosent_rens)) %>% 
  #filter(prosent_rens >0) %>% 
  pivot_wider(names_from = parameter,
              values_from = prosent_rens) %>% 
  select(-c(Ledningsevne, pH))

data_scale <- data |>  
  mutate(across(where(is.numeric) & !matches("Y"), ~ as.vector(scale(.x)))) # the Y variable name needs to be replaced with the correct name of the log kF column

skim(data_scale)

# (Pearson) correlation analysis
data_cor <- data |>
  select(-c(dato)) |> # remove any non-numeric variables. NB: Kf should also be removed here!
  cor(use = "pairwise.complete.obs", # to include the maximum of data even when values are missing
      method = "pearson") # this is the default

data_cor

# visualization of the correlation analysis
ggcorrplot(data_cor, hc.order = TRUE, type = "lower", lab = TRUE)
ggsave("cor_plot.jpeg")

# 31.10.22 ----
data <- read_xlsx("2023_sigevann_Drammen_KMK.xlsx") %>% 
  mutate(prosent_rens = (verdi_inn_korr-verdi_ut_korr)/verdi_inn_korr) %>% 
  filter(dato == as.Date("2022-10-31") | dato == as.Date("2022-11-21") | 
           dato == as.Date("2023-02-13") | dato == as.Date("2022-11-07")) %>%
  select(c(parameter, dato, prosent_rens)) %>% 
  na.omit() %>% 
  pivot_wider(names_from = parameter,
              values_from = prosent_rens) %>% 
  select(-c(Ledningsevne, pH))

data_scale <- data |>  
  mutate(across(where(is.numeric) & !matches("Y"), ~ as.vector(scale(.x)))) # the Y variable name needs to be replaced with the correct name of the log kF column

skim(data_scale)

# (Pearson) correlation analysis
data_cor <- data |>
  select(-c(dato)) |> # remove any non-numeric variables. NB: Kf should also be removed here!
  cor(use = "pairwise.complete.obs", # to include the maximum of data even when values are missing
      method = "pearson") # this is the default

data_cor

# visualization of the correlation analysis
ggcorrplot(data_cor, hc.order = TRUE, type = "lower", lab = TRUE)
ggsave("cor_plot.jpeg")

