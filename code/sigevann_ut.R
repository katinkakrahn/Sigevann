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
Fe_Mn_ut <- raw_data %>% 
  filter(forkortelse %in% c("Fe", "Mn")) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_ut_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 4) +
  labs(x = "dato",
       y =  TeX(r'($konsentrasjon~(\mu g/l)$)'),
       color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Sigevann ut - jern (Fe) og mangan (Mn)")
Fe_Mn_ut
ggsave("ut/Fe_Mn_ut.jpeg")

Zn_Ni_ut <- raw_data %>% 
  filter(forkortelse %in% c("Ni", "Zn")) %>% 
  drop_na(verdi_ut_korr) %>%
  ggplot(aes(x = as.character(dato), y = verdi_ut_korr, color = forkortelse)) + 
  geom_line(aes(group = forkortelse, color = ),
            linewidth = 1) +
  geom_point(size = 4) +
  geom_hline(aes(yintercept = 0.5,
                 linetype = "Zn"),
             linewidth = 1,
             color = "#d95f02") +
  geom_hline(aes(yintercept = 0.05,
                 linetype = "Ni"),
             linewidth = 1,
             color = "#1b9e77") +
  scale_linetype_manual(name = "grenseverdier", values = c(2,2),
                        guide = guide_legend(override.aes = list(color = c("#1b9e77", "#d95f02")))) +
labs(x = "dato",
     y =  TeX(r'($konsentrasjon~(\mu g/l)$)'),
     color = "Parameter") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Sigevann ut - nikkel (Ni) og sink (Zn)")
Zn_Ni_ut
ggsave("ut/Zn_Ni_ut.jpeg")

LOQ_metaller <- raw_data %>% 
  filter(LOQ_ut == TRUE) %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn"))) %>% 
  drop_na(LOQ_ut)

GV_metaller <- raw_data %>% 
  filter(GV_over == TRUE) %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn")),
         as.numeric(verdi_ut_korr) >= as.numeric(grenseverdi_min))

metaller_ut <- raw_data %>% 
  filter(kategori_1 == "metall",
         !(forkortelse%in% c("Zn", "Ni", "Fe", "Mn"))) %>% 
  drop_na(verdi_ut_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_ut_korr, color = forkortelse)) + 
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
  geom_point(data = GV_metaller, aes(color = GV_over),
             size = 2) +
  scale_color_manual(name = "", 
                     labels = c("over grenseverdi", "under"),
                     breaks = c(T,F),
                     values = c('red', "green")) +
  ggnewscale::new_scale_color() +
  geom_point(data = LOQ_metaller, aes(color = LOQ_ut)) +
  scale_color_manual(name = "", 
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'black') +
  ggtitle("Sigevann ut - spormetaller")
metaller_ut
ggsave("ut/metaller_ut.jpeg")

# organiske miljøgifter ----
LOQ_organic <- raw_data %>% 
  filter(LOQ_ut == TRUE) %>% 
  filter(kategori_1 == "org milj",
         kategori_2 != "PFAS")

organic_pollutants_ut <- raw_data %>% 
  filter(kategori_1 == "org milj",
         kategori_2 != "PFAS") %>% 
  drop_na(verdi_ut_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_ut_korr, color = forkortelse)) + 
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
  geom_point(data = LOQ_organic, aes(color = LOQ_ut)) +
  scale_color_manual(name = "", 
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'black') +
  ggtitle("Sigevann ut - organiske miljogifter")
organic_pollutants_ut
ggsave("ut/organic_pollutants_ut.jpeg")

LOQ_alifater <- raw_data %>% 
  filter(LOQ_ut == TRUE,
         kategori_2 == "alifater")

GV_alifater <- raw_data %>% 
  filter(GV_over == TRUE) %>% 
  filter(kategori_2 == "alifater")

alifater_ut <- raw_data %>% 
  filter(kategori_2 == "alifater") %>% 
  drop_na(verdi_ut_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_ut_korr, color = forkortelse)) + 
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
  geom_point(data = GV_alifater, aes(color = verdi_ut_korr > 20),
             size = 2) +
  scale_color_manual(name = "Grenseverdi", 
                     labels = c("over", "under"),
                     breaks = c(T, F),
                     values = c('red','green')) +
  ggnewscale::new_scale_color() +
  geom_point(data = LOQ_alifater, aes(color = LOQ_ut)) +
  scale_color_manual(name = "", 
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'black') +
  ggtitle("Sigevann ut - alifater")
alifater_ut
ggsave("ut/alifater_ut.jpeg")

# vannparametre ----
GV_vann <- raw_data %>% 
  filter(GV_over == TRUE,
         kategori_1 == "vannparameter",
         !(forkortelse%in% c("pH", "EC")),
         as.numeric(verdi_ut_korr) >= as.numeric(grenseverdi_min)) %>% 
  mutate(GV_over = ifelse(as.numeric(verdi_ut_korr) >= as.numeric(grenseverdi_min), 
                          TRUE, FALSE))  

vannparametre_ut <- raw_data %>% 
  filter(kategori_1 == "vannparameter",
         !(forkortelse%in% c("pH", "EC"))) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_ut_korr, color = parameter)) + 
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
  ggtitle("Sigevann ut - generelle vannparametre") +
  ggnewscale::new_scale_color() +
  geom_point(data = GV_vann, aes(color = GV_over),
             size = 2) +
  scale_color_manual(name = "", 
                     labels = c("over grenseverdi", "under"),
                     breaks = c(T,F),
                     values = c('red', "green"))
vannparametre_ut
ggsave("ut/vannparametre_ut.jpeg")






