# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)
library(ggnewscale)
library(plyr)

# raw data ----
raw_data <- read_xlsx("2023_sigevann_Drammen_KMK.xlsx") %>% 
  mutate(GV_over_inn = ifelse(as.numeric(verdi_inn_korr) >= as.numeric(grenseverdi_min), 
                          TRUE, FALSE),
         prosent_rens = (verdi_inn_korr-verdi_ut_korr)/verdi_inn_korr) 

# metaller ----
# Fe og Mn
raw_data %>% 
  filter(forkortelse %in% c("Fe", "Mn")) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_inn_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4) +
  labs(x = "dato",
       y =  "konsentrasjon (mg/L)",
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Sigevann inn - jern (Fe) og mangan (Mn)")
ggsave("inn/Fe_Mn_inn.jpeg")

#Zn og Ni
raw_data %>% 
  filter(forkortelse %in% c("Ni", "Zn")) %>% 
  drop_na(verdi_inn_korr) %>%
  ggplot(aes(x = as.character(dato), y = verdi_inn_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse, color = ),
            linewidth = 1) +
  geom_point(size = 4) +
  geom_hline(aes(yintercept = 0.5,
                 linetype = "Zn"),
             linewidth = 1,
             alpha = 0.5,
             color = "#d95f02") +
  geom_hline(aes(yintercept = 0.05,
                 linetype = "Ni"),
             linewidth = 1,
             alpha = 0.5,
             color = "#1b9e77") +
  scale_linetype_manual(name = "grenseverdier", values = c(2,2),
                        guide = guide_legend(override.aes = list(color = c("#1b9e77", "#d95f02")))) +
  labs(x = "dato",
       y =  "konsentrasjon (mg/L)",
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Sigevann inn - nikkel (Ni) og sink (Zn)")
ggsave("inn/Zn_Ni_inn.jpeg")

LOQ_metaller <- raw_data %>% 
  filter(LOQ_inn == TRUE) %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn"))) %>% 
  drop_na(LOQ_inn)

GV_metaller <- raw_data %>% 
  filter(GV_over_inn == TRUE,
         kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn", "Cd", "Hg")),
         as.numeric(verdi_inn_korr) >= as.numeric(grenseverdi_min))

# Metaller
raw_data %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn", "Cd", "Hg"))) %>% 
  drop_na(verdi_inn_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_inn_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  "konsentrasjon (mg/L)",
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggnewscale::new_scale_color() +
  geom_point(data = GV_metaller, aes(color = GV_over_inn),
             size = 2) +
  scale_color_manual(name = "",
                     labels = c("over grenseverdi", "under"),
                     breaks = c(T,F),
                     values = c('red', "green")) +
  ggnewscale::new_scale_color() +
  geom_point(data = LOQ_metaller, aes(color = LOQ_inn)) +
  scale_color_manual(name = "",
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'black') +
  ggtitle("Sigevann inn - spormetaller")
ggsave("inn/metaller_inn.jpeg")

# organiske miljøgifter ----
LOQ_organic <- raw_data %>% 
  filter(LOQ_inn == TRUE) %>% 
  filter(kategori_1 == "org milj",
         kategori_2 != "PFAS")

organic_pollutants_inn <- raw_data %>% 
  filter(kategori_1 == "org milj",
         kategori_2 != "PFAS") %>% 
  drop_na(verdi_inn_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_inn_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  TeX(r'($konsentrasjon~(\mu g/l)$)'),
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggnewscale::new_scale_color() +
  geom_point(data = LOQ_organic, aes(color = LOQ_inn)) +
  scale_color_manual(name = "", 
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'black') +
  ggtitle("Sigevann inn - organiske miljogifter")
organic_pollutants_inn
ggsave("inn/organic_pollutants_inn.jpeg")

LOQ_alifater <- raw_data %>% 
  filter(LOQ_inn == TRUE,
         kategori_2 == "alifater")

GV_alifater <- raw_data %>% 
  filter(GV_over_inn == TRUE) %>% 
  filter(kategori_2 == "alifater")

alifater_inn <- raw_data %>% 
  filter(kategori_2 == "alifater") %>% 
  drop_na(verdi_inn_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_inn_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  TeX(r'($konsentrasjon~(\mu g/l)$)'),
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggnewscale::new_scale_color() +
  geom_point(data = GV_alifater, aes(color = GV_over_inn),
             size = 2) +
  scale_color_manual(name = "Grenseverdi", 
                     labels = c("over", "under"),
                     breaks = c(T, F),
                     values = c('red','green')) +
  ggnewscale::new_scale_color() +
  geom_point(data = LOQ_alifater, aes(color = LOQ_inn)) +
  scale_color_manual(name = "", 
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'black') +
  ggtitle("Sigevann inn - alifater")
alifater_inn
ggsave("inn/alifater_inn.jpeg")

# vannparametre ----
GV_vann <- raw_data %>% 
  filter(GV_over_inn == TRUE,
         kategori_1 == "vannparameter",
         !(forkortelse%in% c("pH", "EC")),
         as.numeric(verdi_inn_korr) >= as.numeric(grenseverdi_min)) %>% 
  mutate(GV_over_inn = ifelse(as.numeric(verdi_inn_korr) >= as.numeric(grenseverdi_min), 
                          TRUE, FALSE))  

vannparametre_inn <- raw_data %>% 
  filter(kategori_1 == "vannparameter",
         !(forkortelse%in% c("pH", "EC"))) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_inn_korr, color = parameter)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  TeX(r'($konsentrasjon~(mg/l)$)'),
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Sigevann inn - generelle vannparametre") +
  ggnewscale::new_scale_color() +
  geom_point(data = GV_vann, aes(color = GV_over_inn),
             size = 2) +
  scale_color_manual(name = "", 
                     labels = c("over grenseverdi", "under"),
                     breaks = c(T,F),
                     values = c('red', "green"))
vannparametre_inn
ggsave("inn/vannparametre_inn.jpeg")
