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

# raw data ----
raw_data <- read_xlsx("2023_sigevann_Drammen_KMK.xlsx") %>% 
  mutate(GV_over = ifelse(as.numeric(verdi_ut_korr) >= as.numeric(grenseverdi_min), 
                          TRUE, FALSE),
         prosent_rens = (verdi_inn_korr-verdi_ut_korr)/verdi_inn_korr) 

# metaller ----
Fe_Mn_rens <- raw_data %>% 
  filter(forkortelse %in% c("Fe", "Mn")) %>% 
  ggplot(aes(x = as.character(dato), y = prosent_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Jern og mangan")
Fe_Mn_rens
ggsave("rens/Fe_Mn_rens.jpeg")

Zn_Ni_rens <- raw_data %>% 
  filter(forkortelse %in% c("Ni", "Zn")) %>% 
  drop_na(prosent_rens) %>%
  ggplot(aes(x = as.character(dato), y = prosent_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse, color = ),
            linewidth = 1) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Nikkel og sink")
Zn_Ni_rens
ggsave("rens/Zn_Ni_rens.jpeg")

LOQ_metaller <- raw_data %>% 
  filter(LOQ_ut == TRUE) %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn"))) %>% 
  drop_na(LOQ_ut)

metaller_rens <- raw_data %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn")),
         LOQ_inn == FALSE & LOQ_ut == FALSE
         ) %>% 
  drop_na(prosent_rens) %>% 
  ggplot(aes(x = as.character(dato), y = prosent_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid = element_blank()) +
  # ggnewscale::new_scale_color() +
  # geom_point(data = LOQ_metaller, aes(color = LOQ_ut)) +
  # scale_color_manual(name = "", 
  #                    labels = c("under LOQ"),
  #                    breaks = T,
  #                    values = 'black') +
  ggtitle("Metaller")
metaller_rens
ggsave("rens/metaller_rens.jpeg")

# organiske miljøgifter ----
LOQ_organic <- raw_data %>% 
  filter(LOQ_ut == TRUE) %>% 
  filter(kategori_1 == "org milj",
         kategori_2 != "PFAS")

organic_pollutants_rens <- raw_data %>% 
  filter(kategori_1 == "org milj",
         kategori_2 != "PFAS",
         !(parameter == "Sum BTEX" & dato == as.Date("2023-02-27")),
         !(parameter == "Sum PCB 7" & LOQ_inn == TRUE | LOQ_ut == TRUE),
         LOQ_inn == FALSE & LOQ_ut == FALSE
         ) %>% 
  drop_na(prosent_rens) %>% 
  ggplot(aes(x = as.character(dato), y = prosent_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  # ggnewscale::new_scale_color() +
  # geom_point(data = LOQ_organic, aes(color = LOQ_ut)) +
  # scale_color_manual(name = "", 
  #                    labels = c("under LOQ"),
  #                    breaks = T,
  #                    values = 'black') +
  ggtitle("Organiske")
organic_pollutants_rens
ggsave("rens/organic_pollutants_rens.jpeg")

LOQ_alifater <- raw_data %>% 
  filter(LOQ_ut == TRUE,
         kategori_2 == "alifater")

alifater_rens <- raw_data %>% 
  filter(kategori_2 == "alifater",
         !(parameter == "THC >C12-C16" & prosent_rens<0),
         LOQ_inn == FALSE & LOQ_ut == FALSE
         ) %>% 
  drop_na(prosent_rens) %>% 
  ggplot(aes(x = as.character(dato), y = prosent_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  # ggnewscale::new_scale_color() +
  # geom_point(data = LOQ_alifater, aes(color = LOQ_ut)) +
  # scale_color_manual(name = "", 
  #                    labels = c("under LOQ"),
  #                    breaks = T,
  #                    values = 'black') +
  ggtitle("Alifater")
alifater_rens
ggsave("rens/alifater_rens.jpeg")

# vannparametre ----
vannparametre_rens <- raw_data %>% 
  filter(kategori_1 == "vannparameter",
         !(forkortelse%in% c("pH", "EC")),
         !(parameter == "P-total"  & prosent_rens <0),
         !(parameter == "TOC" & prosent_rens <0),
         !(forkortelse == "NH4 + NH3" & dato == as.Date("2022-11-14")),
         !(parameter == "Tot N" & dato == as.Date("2022-11-14"))
         ) %>% 
  ggplot(aes(x = as.character(dato), y = prosent_rens, color = parameter)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Generelle vannparametre")
vannparametre_rens
ggsave("rens/vannparametre_rens.jpeg")

# PFAS ----
PFAS_rens <- raw_data %>% 
  filter(kategori_2 == "PFAS",
         dato == as.Date("2022-11-14")
  ) %>% 
  ggplot(aes(x = forkortelse, y = prosent_rens, color = parameter)) + 
  geom_point(size = 4,
             alpha = 0.5) +
  labs(x = "dato",
       y =  "% renseeffekt",
       color = "Parameter") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Generelle vannparametre")
PFAS_rens
ggsave("rens/PFAS_rens.jpeg")

# renseeffekt ----
negativ_renseeffekt <- raw_data %>% 
  filter(prosent_rens < 0,
         !(parameter %in% c("pH", "Ledningsevne"))) %>% 
  select(parameter, dato, LOQ_ut, prosent_rens) %>% 
  pivot_wider(names_from = dato,
              values_from = prosent_rens)

write_xlsx(negativ_renseeffekt, "negativ_renseeffekt.xlsx")

positiv_renseeffekt <- raw_data %>% 
  filter(prosent_rens > 0,
         !(parameter %in% c("pH", "Ledningsevne"))) %>% 
  select(parameter, dato, LOQ_ut, LOQ_inn, prosent_rens, kategori_1, kategori_2) %>% 
  mutate(LOQ_begge = ifelse(LOQ_ut == TRUE & LOQ_inn == TRUE, TRUE, FALSE)) %>% 
  pivot_wider(names_from = dato,
              values_from = prosent_rens)