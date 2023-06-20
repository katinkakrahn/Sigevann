# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggnewscale)
library(scales)
library(latex2exp)
library(ggnewscale)

# raw data ----
raw_data <- read_xlsx("2023_sigevann_Drammen_KMK4.xlsx") %>% 
  filter(krav == TRUE,
         provepunkt != 153,
         !(forkortelse %in% c("PFOS", "PFOA")),
         !(enhet %in% c("mg/kg TS", "%")))

renseeffekt <- raw_data %>% 
  select(-c(verdi, provepunkt, GV_over, LOQ)) %>% 
  pivot_wider(names_from = sigevann,
              values_from = verdi_korr) %>% 
  mutate(perc_rens = (inn-ut)/inn*100)

# grupperingsparametere for loops ----
forkortelse_vals <- unique(raw_data$forkortelse)
kategori_vals <- unique(raw_data$kategori_2)
kategori1_vals <- unique(raw_data$kategori_1)

#enkeltparametere ----
for (forkortelse_val in forkortelse_vals) {
  tryCatch({
    y_label <- paste0("konsentrasjon (", unique(raw_data$enhet[raw_data$forkortelse == forkortelse_val]), ")")
    
    plot_object <- raw_data %>% 
      filter(forkortelse == forkortelse_val,
             forkortelse != "pH") %>% 
      drop_na(verdi_korr) %>% 
      ggplot(aes(x = as.character(dato), y = verdi_korr, group = sigevann)) + 
      geom_line(aes(linetype = sigevann), linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      new_scale_color() +
      geom_hline(data = raw_data %>% filter(forkortelse == forkortelse_val & !is.na(gv_min)),
                 aes(yintercept = as.numeric(gv_min),
                     color = "grenseverdi"),
                 linewidth = 1,
                 linetype = "dashed") +
      scale_color_manual(name = "",
                         labels = c("Parameter", "grenseverdi"),
                         breaks = c("sigevann", "grenseverdi"),
                         values = c("black", "red")) +
      labs(x = "dato",
           y = y_label,
           color = "") +
      ggtitle(paste(forkortelse_val)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      new_scale_color() +
      geom_point(data = raw_data %>% filter(forkortelse == forkortelse_val & 
                                              LOQ == TRUE),
                 aes(color = LOQ),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ"),
                         breaks = T,
                         values = 'blue')
    
    print(plot_object)
    ggsave(paste0("loop_figs/enkeltparametere/sigevann_inn_ut/enkelt_kons_", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", forkortelse_val)
  })
}

# pH ----
raw_data %>% 
  filter(forkortelse == "pH") %>% 
  drop_na(verdi_korr) %>% 
  ggplot(aes(x = as.character(dato), y = verdi_korr, group = sigevann)) + 
  geom_line(aes(linetype = sigevann), linewidth = 1) +
  geom_point(size = 3, alpha = 0.5) +
  ggnewscale::new_scale_color() +
  geom_hline(aes(yintercept = as.numeric(gv_min),
                 color = "grenseverdi min"),
             linewidth = 1,
             linetype = "dashed") +
  geom_hline(aes(yintercept = as.numeric(gv_max),
                 color = "grenseverdi maks"),
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
ggsave("loop_figs/enkeltparametere/sigevann_inn_ut/enkelt_pH.png")

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
                 color = "grenseverdi min"),
             linewidth = 1,
             linetype = "dashed") +
  scale_color_manual(name = "",
                     labels = c("Parameter", "grenseverdi min"),
                     breaks = c("sigevann", "grenseverdi min"),
                     values = c("black", "red")) +
  labs(x = "dato",
       y = expression(paste("konsentrasjon (",mu,"g/l)")),
       color = "") +
  ggtitle("Totale hydrokarboner (C10-C40)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("loop_figs/enkeltparametere/sigevann_inn_ut/enkelt_tot_hydrokarboner.png")
  
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
                                              !is.na(gv_min)),
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
    ggsave(paste0("loop_figs/kategorier/sigevann_ut/kategori_kons_", kategori_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for kategori: ", group_val)
  })
}


#renseeffekt enkeltparametere og suspendert stoff ----
for (forkortelse_val in forkortelse_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(forkortelse == forkortelse_val,
             forkortelse != "pH") %>% 
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
                         values = 'red') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      #scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", forkortelse_val, collapse = " ")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs/enkeltparametere/renseeffekt/med_SS/rens_SS_", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
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
             forkortelse != "pH",
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
                         values = 'red') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", forkortelse_val, collapse = " ")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs/enkeltparametere/renseeffekt/med_SS/fiksert_y_akse/rens_SS_", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", forkortelse_val)
  })
}


# renseeffekt kategorier og suspendert stoff 
for (kategori_val in kategori_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(kategori_2 == kategori_val,
             forkortelse != "pH") %>% 
      drop_na(perc_rens) %>% 
      ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
      geom_line(aes(group = forkortelse),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      new_scale_color() +
      geom_line(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                aes(x = as.character(dato), y = perc_rens, group = forkortelse,
                    color = "red"),
                linewidth = 2) +
      geom_point(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                 aes(x = as.character(dato), y = perc_rens, color = "red"),
                 size = 3, alpha = 0.5) +
      scale_color_manual(name = "",
                         labels = "Suspendert stoff",
                         values = 'red') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      #scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", kategori_val, collapse = " ")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs/kategorier/renseeffekt/med_SS/rens_SS_", kategori_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", kategori_val)
  })
}

# renseeffekt kategorier og suspendert stoff med fiksert y akse 0-100----
for (kategori_val in kategori_vals) {
  tryCatch({
    y_label <- paste0("% renseeffekt")
    
    plot_object <- renseeffekt %>% 
      filter(kategori_2 == kategori_val,
             forkortelse != "pH",
             perc_rens >= 0) %>% 
      drop_na(perc_rens) %>% 
      ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
      geom_line(aes(group = forkortelse),
                linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      new_scale_color() +
      geom_line(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                aes(x = as.character(dato), y = perc_rens, group = forkortelse,
                    color = "red"),
                linewidth = 2) +
      geom_point(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
                 aes(x = as.character(dato), y = perc_rens, color = "red"),
                 size = 3, alpha = 0.5) +
      scale_color_manual(name = "",
                         labels = "Suspendert stoff",
                         values = 'red') +
      labs(x = "dato",
           y = y_label,
           color = "") +
      scale_y_continuous(limits = c(0, 100)) +
      ggtitle(paste("Renseeffekt", kategori_val, collapse = " ")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_object)
    ggsave(paste0("loop_figs/kategorier/renseeffekt/med_SS/fiksert_y_akse/rens_SS_", kategori_val, ".png"), plot = plot_object, dpi = 300)
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
  new_scale_color() +
  geom_line(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
            aes(x = as.character(dato), y = perc_rens, group = forkortelse,
                color = "red"),
            linewidth = 2) +
  geom_point(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
             aes(x = as.character(dato), y = perc_rens, color = "red"),
             size = 3, alpha = 0.5) +
  scale_color_manual(name = "",
                     labels = "Suspendert stoff",
                     values = 'red') +
  labs(x = "dato",
       y = "% renseeffekt",
       color = "") +
  #scale_y_continuous(limits = c(0, 100)) +
  ggtitle("Renseeffekt tungmetaller") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("loop_figs/kategorier/renseeffekt/med_SS/rens_SS_alle_tungmetaller.png"), plot = plot_object, dpi = 300)


# renseeffekt kategorier og suspendert stoff utvalg metaller fiksert y akse 0-100 uten negative verdier----
renseeffekt %>% 
  filter(forkortelse %in% c("Pb", "Cr", "Cd", "Ni", "Cu", "Zn"),
         perc_rens >= 0) %>% 
  drop_na(perc_rens) %>% 
  ggplot(aes(x = as.character(dato), y = perc_rens, color = forkortelse)) + 
  geom_line(aes(group = forkortelse),
            linewidth = 1) +
  geom_point(size = 3, alpha = 0.5) +
  new_scale_color() +
  geom_line(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
            aes(x = as.character(dato), y = perc_rens, group = forkortelse,
                color = "red"),
            linewidth = 2) +
  geom_point(data = renseeffekt %>% filter(forkortelse == "Suspendert stoff"),
             aes(x = as.character(dato), y = perc_rens, color = "red"),
             size = 3, alpha = 0.5) +
  scale_color_manual(name = "",
                     labels = "Suspendert stoff",
                     values = 'red') +
  labs(x = "dato",
       y = "% renseeffekt",
       color = "") +
  #scale_y_continuous(limits = c(0, 100)) +
  ggtitle("Renseeffekt tungmetaller") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0("loop_figs/kategorier/renseeffekt/med_SS/fiksert_y_akse/rens_SS_alle_tungmetaller.png"), plot = plot_object, dpi = 300)
