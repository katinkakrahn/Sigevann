# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggnewscale)
library(scales)
library(latex2exp)
library(ggnewscale)
library(viridis)

# raw data ----
raw_data0 <- read_xlsx("2023_sigevann_Drammen_KMK4.xlsx") 

raw_data <- raw_data0 %>% 
  filter(krav == TRUE,
         provepunkt != 153,
         forkortelse != "pH",
         !(forkortelse %in% c("PFOS", "PFOA")),
         !(enhet %in% c("mg/kg TS", "%")),
         kategori_2 != "hydrokarboner")

renseeffekt <- raw_data %>% 
  select(-c(verdi, provepunkt, GV_over, LOQ, enhet)) %>% 
  pivot_wider(names_from = sigevann,
              values_from = verdi_korr) %>% 
  mutate(perc_rens = (inn-ut)/inn*100)

inn <- raw_data %>% 
  filter(sigevann == "inn") %>% 
  rename(GV_over_inn = GV_over,
         LOQ_inn = LOQ) %>% 
  select(c(forkortelse, GV_over_inn, LOQ_inn, dato))

ut <- raw_data %>% 
  filter(sigevann == "ut") %>% 
  rename(GV_over_ut = GV_over,
         LOQ_ut = LOQ) %>% 
  select(c(forkortelse, GV_over_ut, LOQ_ut, dato))

inn_ut <- merge(inn, ut, all = TRUE) %>% 
  mutate(GV_over_enav = ifelse(GV_over_inn == TRUE | GV_over_ut == TRUE, TRUE, FALSE),
         LOQ_enav = ifelse(LOQ_ut == TRUE | LOQ_inn == TRUE, TRUE, FALSE))

renseeffekt_2 <- merge(renseeffekt, inn_ut, all = TRUE) %>% 
  #filter(GV_over_enav == TRUE | LOQ_enav == TRUE) %>% 
  filter(GV_over_ut == TRUE | LOQ_ut)

pH <- raw_data0 %>% 
  filter(forkortelse == "pH",
         provepunkt != 153)

# grupperingsparametere for loops ----
forkortelse_vals <- unique(raw_data$forkortelse)
kategori_vals <- unique(raw_data$kategori_2)
kategori1_vals <- unique(raw_data$kategori_1)

#enkeltparametere ----
for (forkortelse_val in forkortelse_vals) {
  tryCatch({
    y_label <- paste0("konsentrasjon (", unique(raw_data$enhet[raw_data$forkortelse == forkortelse_val]), ")")
    
    plot_object <- raw_data %>% 
      filter(forkortelse == forkortelse_val) %>% 
      drop_na(verdi_korr) %>% 
      ggplot(aes(x = as.character(dato), y = verdi_korr, group = sigevann)) + 
      geom_line(aes(linetype = sigevann), linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      new_scale_color() +
      geom_hline(data = raw_data %>% filter(forkortelse == forkortelse_val & !is.na(gv_max)),
                 aes(yintercept = as.numeric(gv_max),
                     color = "grenseverdi"),
                 linewidth = 1,
                 linetype = "dashed") +
      scale_color_manual(name = "",
                         labels = c("Parameter", "grenseverdi"),
                         breaks = c("sigevann", "grenseverdi"),
                         values = c("black", "red")) +
      new_scale_color() +
      geom_point(data = raw_data %>% filter(forkortelse == forkortelse_val & 
                                              LOQ == TRUE),
                 aes(color = LOQ),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ"),
                         breaks = T,
                         values = 'blue') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      ggtitle(paste(forkortelse_val)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs_270423_270423/sigevann_inn_ut/enkeltparametere/enkelt_kons_", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", forkortelse_val)
  })
}

# pH ----
pH %>% 
  ggplot(aes(x = as.character(dato), y = verdi_korr, group = sigevann)) + 
  geom_line(aes(linetype = sigevann), linewidth = 1) +
  geom_point(size = 3, alpha = 0.5) +
  ggnewscale::new_scale_color() +
  geom_hline(aes(yintercept = as.numeric(gv_max),
                 color = "grenseverdi maks"),
             linewidth = 1,
             linetype = "dashed") +
  geom_hline(aes(yintercept = as.numeric(gv_min),
                 color = "grenseverdi min"),
             linewidth = 1,
             linetype = "dashed") +
  scale_color_manual(name = "",
                     labels = c("Parameter", "grenseverdi min", "grenseverdi maks"),
                     breaks = c("sigevann", "grenseverdi min", "grenseverdi maks"),
                     values = c("black", "red", "red")) +
  labs(x = "dato",
       y = "pH",
       color = "") +
  ggtitle("pH") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("loop_figs_270423/sigevann_inn_ut/enkeltparametere/enkelt_pH.png")

# alifater ---- 
raw_data %>% 
  filter(kategori_1 =="hydrokarboner") %>% 
  drop_na(verdi_korr) %>% 
  group_by(sigevann, dato, kategori_1) %>% 
  summarise(sum_verdi_korr = sum(verdi_korr)) %>% 
  ggplot(aes(x = as.character(dato), y = sum_verdi_korr, group = sigevann)) + 
  geom_line(aes(linetype = sigevann), linewidth = 1) +
  geom_point(size = 3, alpha = 0.5) +
  ggnewscale::new_scale_color() +
  geom_hline(aes(yintercept = 20000,
                 color = "grenseverdi maks"),
             linewidth = 1,
             linetype = "dashed") +
  scale_color_manual(name = "",
                     labels = c("Parameter", "grenseverdi maks"),
                     breaks = c("sigevann", "grenseverdi maks"),
                     values = c("black", "red")) +
  labs(x = "dato",
       y = expression(paste("konsentrasjon (",mu,"g/l)")),
       color = "") +
  ggtitle("Totale hydrokarboner (C10-C40)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("loop_figs_270423/sigevann_inn_ut/enkeltparametere/enkelt_tot_hydrokarboner.png")

# sigevann ut ----
for (kategori_val in kategori_vals) {
  tryCatch({
    y_label <- paste0("konsentrasjon (", unique(raw_data$enhet[raw_data$kategori_2 == kategori_val]), ")")
    
    plot_object <- raw_data %>% 
      filter(kategori_2 == kategori_val,
             sigevann == "ut") %>% 
      drop_na(verdi_korr) %>% 
      ggplot(aes(x = as.character(dato), y = verdi_korr, color = forkortelse)) + 
      geom_line(aes(group = forkortelse),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      labs(x = "dato",
           y = y_label,
           color = "Parameter") +
      ggtitle(paste(kategori_val)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      new_scale_color() +
      geom_point(data = raw_data %>% filter(kategori_2 == kategori_val &
                                              GV_over == TRUE &
                                              sigevann == "ut" &
                                              !is.na(gv_max)),
                 aes(color = "over grenseverdi"),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = "over grenseverdi",
                         values = 'red') +
      new_scale_color() +
      geom_point(data = raw_data %>% filter(kategori_2 == kategori_val & 
                                              LOQ == TRUE &
                                              sigevann == "ut"),
                 aes(color = "under LOQ"),
                 size = 1) +
      scale_color_manual(name = "",
                         labels = "under LOQ",
                         values = 'blue')
    
    print(plot_object)
    ggsave(paste0("loop_figs_270423/sigevann_ut/kategorier/kategori_kons_", kategori_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for kategori: ", group_val)
  })
}


#renseeffekt enkeltparametere og suspendert stoff med markering av GV og LOQ----
for (forkortelse_val in forkortelse_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(forkortelse == forkortelse_val) %>% 
      drop_na(perc_rens) %>% 
      ggplot(aes(x = as.character(dato), y = perc_rens)) + 
      geom_line(aes(group = forkortelse_val),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      new_scale_color() +
      geom_line(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                aes(x = as.character(dato), y = perc_rens, group = forkortelse,
                    color = "Suspendert stoff"),
                linewidth = 1) +
      geom_point(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                 aes(x = as.character(dato), y = perc_rens, color = "Suspendert stoff"),
                 size = 3, alpha = 0.5) +
      scale_color_manual(name = "",
                         labels = "Suspendert stoff",
                         values = 'brown') +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(forkortelse == forkortelse_val & 
                                        GV_over_ut == TRUE),
                 aes(color = GV_over_ut),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("over grenseverdi ut"),
                         breaks = T,
                         values = 'red') +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(forkortelse == forkortelse_val & 
                                        LOQ_ut == TRUE),
                 aes(color = LOQ_ut),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ ut"),
                         breaks = T,
                         values = 'blue') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      #scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", forkortelse_val, collapse = " ")) +
      geom_vline(xintercept = "2023-02-13",
                 linetype = 2,
                 color = "grey") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs_270423/renseeffekt/enkeltparametere/med_SS/rens_SS_", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", forkortelse_val)
  })
}

#renseeffekt enkeltparametere og suspendert stoff 0-100% uten negative  verdier----
for (forkortelse_val in forkortelse_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(forkortelse == forkortelse_val,
             perc_rens >= 0) %>% 
      drop_na(perc_rens) %>% 
      ggplot(aes(x = as.character(dato), y = perc_rens)) + 
      geom_line(aes(group = forkortelse_val),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      new_scale_color() +
      geom_line(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                aes(x = as.character(dato), y = perc_rens, group = forkortelse,
                    color = "Suspendert stoff"),
                linewidth = 1) +
      geom_point(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                 aes(x = as.character(dato), y = perc_rens, color = "Suspendert stoff"),
                 size = 3, alpha = 0.5) +
      scale_color_manual(name = "",
                         labels = "Suspendert stoff",
                         values = 'brown') +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(forkortelse == forkortelse_val & 
                                                   GV_over_enav == TRUE),
                 aes(color = GV_over_enav),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("over grenseverdi"),
                         breaks = T,
                         values = 'red') +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(forkortelse == forkortelse_val & 
                                                   LOQ_enav == TRUE),
                 aes(color = LOQ_enav),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ"),
                         breaks = T,
                         values = 'blue') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", forkortelse_val, collapse = " ")) +
      geom_vline(xintercept = "2023-02-13",
                 linetype = 2,
                 color = "grey") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs_270423/renseeffekt/enkeltparametere/med_SS/fiksert_y_akse/rens_SS_", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", forkortelse_val)
  })
}


# renseeffekt kategorier og suspendert stoff 
SS <- renseeffekt %>% 
  filter(forkortelse == "Suspendert stoff")

for (kategori_val in kategori_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(kategori_2 == kategori_val) %>% 
      drop_na(perc_rens) %>% 
      ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
      geom_line(aes(group = forkortelse),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      scale_color_brewer(palette = "Dark2") +
      geom_line(data = filter(renseeffekt, forkortelse == "Suspendert stoff"), 
                aes(x = as.character(dato), y = perc_rens, group = "Suspendert stoff"),
                linewidth = 2) +
      geom_vline(xintercept = "2023-02-13",
                 linetype = 2,
                 color = "grey") +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(kategori_2 == kategori_val & 
                                                   GV_over_enav == TRUE),
                 aes(color = GV_over_enav),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("over grenseverdi"),
                         breaks = T,
                         values = 'red') +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(kategori_2 == kategori_val & 
                                                   LOQ_enav == TRUE),
                 aes(color = LOQ_enav),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ"),
                         breaks = T,
                         values = 'blue') +
      labs(x = "dato",
           y = y_label,
           color = "parameter") +
      ggtitle(paste("Renseeffekt", kategori_val, collapse = " ")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs_270423/renseeffekt/kategorier/med_SS/rens_SS_", kategori_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", kategori_val)
  })
}


# renseeffekt kategorier og suspendert stoff med fiksert y akse 0-100----
for (kategori_val in kategori_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(kategori_2 == kategori_val) %>% 
      drop_na(perc_rens) %>% 
      ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
      geom_line(aes(group = forkortelse),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      scale_color_brewer(palette = "Dark2") +
      geom_line(data = filter(renseeffekt, forkortelse == "Suspendert stoff"), 
                aes(group = "Suspendert stoff"),
                linewidth = 2) +
      geom_vline(xintercept = "2023-02-13",
                 linetype = 2,
                 color = "grey") +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(kategori_2 == kategori_val & 
                                                   GV_over_enav == TRUE),
                 aes(color = GV_over_enav),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("over grenseverdi"),
                         breaks = T,
                         values = 'red') +
      new_scale_color() +
      geom_point(data = renseeffekt_2 %>% filter(kategori_2 == kategori_val & 
                                                   LOQ_enav == TRUE),
                 aes(color = LOQ_enav),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ"),
                         breaks = T,
                         values = 'blue') +
      labs(x = "dato",
           y = y_label,
           color = "parameter") +
      scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", kategori_val, collapse = " ")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs_270423/renseeffekt/kategorier/med_SS/rens_SS_", kategori_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", kategori_val)
  })
}

# renseeffekt kategorier og suspendert stoff utvalg metaller ----
renseeffekt %>% 
  filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn")) %>%
  drop_na(perc_rens) %>% 
  ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  geom_line(data = filter(renseeffekt, forkortelse == "Suspendert stoff"), 
            aes(group = "Suspendert stoff"),
            linewidth = 2) +
  geom_vline(xintercept = "2023-02-13",
             linetype = 2,
             color = "grey") +
  new_scale_color() +
  geom_point(data = renseeffekt_2 %>% filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn") & 
                                               GV_over_enav == TRUE),
             aes(color = GV_over_enav),
             size = 3) +
  scale_color_manual(name = "",
                     labels = c("over grenseverdi"),
                     breaks = T,
                     values = 'red') +
  new_scale_color() +
  geom_point(data = renseeffekt_2 %>% filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn") & 
                                               LOQ_enav == TRUE),
             aes(color = LOQ_enav),
             size = 3) +
  scale_color_manual(name = "",
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'blue') +
  labs(x = "dato",
       y = y_label,
       color = "parameter") +
  ggtitle("% renseeffekt") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("loop_figs_270423/kategorier/renseeffekt/med_SS/rens_SS_alle_tungmetaller.png"), plot = plot_object, dpi = 300)


# renseeffekt kategorier og suspendert stoff utvalg metaller fiksert y akse 0-100 uten negative verdier----
renseeffekt %>% 
  filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn")) %>%
  drop_na(perc_rens) %>% 
  ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  geom_line(data = filter(renseeffekt, forkortelse == "Suspendert stoff"), 
            aes(group = "Suspendert stoff"),
            linewidth = 2) +
  geom_vline(xintercept = "2023-02-13",
             linetype = 2,
             color = "grey") +
  new_scale_color() +
  geom_point(data = renseeffekt_2 %>% filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn") & 
                                               GV_over_enav == TRUE),
             aes(color = GV_over_enav),
             size = 3) +
  scale_color_manual(name = "",
                     labels = c("over grenseverdi"),
                     breaks = T,
                     values = 'red') +
  new_scale_color() +
  geom_point(data = renseeffekt_2 %>% filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn") & 
                                               LOQ_enav == TRUE),
             aes(color = LOQ_enav),
             size = 3) +
  scale_color_manual(name = "",
                     labels = c("under LOQ"),
                     breaks = T,
                     values = 'blue') +
  labs(x = "dato",
       y = y_label,
       color = "parameter") +
  ggtitle("% renseeffekt") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("loop_figs_270423/kategorier/renseeffekt/med_SS/fiksert_y_akse/rens_SS_alle_tungmetaller.png"), plot = plot_object, dpi = 300)
