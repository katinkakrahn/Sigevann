# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggnewscale)
library(scales)
library(latex2exp)

# raw data ----
raw_data <- read_xlsx("2023_sigevann_Drammen_KMK3.xlsx")
renseeffekt <- read_xlsx("2023_sigevann_Drammen_KMK.xlsx")

raw_data_filtered <- raw_data %>% 
  filter(kategori_2 != "PFAS")

# loop sigevann----
forkortelse_vals <- unique(raw_data_filtered$forkortelse)

for (forkortelse_val in forkortelse_vals) {
  tryCatch({
    y_label <- paste0("konsentrasjon (", unique(raw_data$enhet[raw_data$forkortelse == forkortelse_val]), ")")
    
    plot_object <- raw_data_filtered %>% 
      filter(forkortelse == forkortelse_val) %>% 
      drop_na(verdi_korr) %>% 
      ggplot(aes(x = as.character(dato), y = verdi_korr, group = sigevann)) + 
      geom_line(aes(linetype = sigevann), linewidth = 1) +
      geom_point(size = 3, alpha = 0.5) +
      labs(x = "dato",
           y = y_label,
           color = "Parameter") +
      ggtitle(paste(forkortelse_val)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      ggnewscale::new_scale_color() +
      geom_point(data = raw_data %>% filter(forkortelse == forkortelse_val & 
                                              GV_over == TRUE & 
                                              kategori_2 != "PFAS"),
                 aes(color = GV_over),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = "over grenseverdi",
                         breaks = T,
                         values = 'red') +
      ggnewscale::new_scale_color() +
      geom_point(data = raw_data %>% filter(forkortelse == forkortelse_val & 
                                              LOQ == TRUE & 
                                              kategori_2 != "PFAS"),
                 aes(color = LOQ),
                 size = 3) +
      scale_color_manual(name = "",
                         labels = c("under LOQ"),
                         breaks = T,
                         values = 'blue')
    
    print(plot_object)
    ggsave(paste0("loop_figs/sigevann_inn_ut/", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
  }, error = function(e) {
    message("Error occurred for forkortelse: ", forkortelse_val)
  })
}

# PFAS ----
raw_data %>% 
  filter(kategori_2 == "PFAS" & !is.na(verdi_korr)) %>% 
  ggplot(aes(x = forkortelse, y = as.numeric(verdi_korr), fill = sigevann)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Parameter", 
       y = TeX(r'($konsentrasjon~(\mu g/l)$)')) +
  ggtitle("PFAS") +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave("loop_figs/sigevann_inn_ut/PFAS.png")

# loop renseeffekt ----
for (forkortelse_val in forkortelse_vals) {
  
  y_label <- paste0("renseeffekt (%)")
  
  plot_object <- renseeffekt %>% 
    filter(forkortelse == forkortelse_val,
           kategori_2 != "PFAS") %>% 
    filter(
      !(parameter == "P-total"  & prosent_rens <0),
      !(parameter == "TOC" & prosent_rens <0),
      !(forkortelse == "NH4 + NH3" & dato == as.Date("2022-11-14")),
      !(parameter == "Tot N" & dato == as.Date("2022-11-14")),
      !(parameter == "THC >C12-C16" & prosent_rens<0),
      LOQ_inn == FALSE & LOQ_ut == FALSE,
      !(parameter == "Sum BTEX" & dato == as.Date("2023-02-27")),
      !(parameter == "Sum PCB 7" & LOQ_inn == TRUE | LOQ_ut == TRUE)) %>% 
    drop_na(prosent_rens) %>% 
    ggplot(aes(x = as.character(dato), y = prosent_rens, group = sigevann)) + 
    geom_line(linewidth = 1) +
    geom_point(size = 3, alpha = 0.5) +
    labs(x = "dato",
         y = y_label,
         color = "Parameter") +
    ggtitle(paste("Renseeffekt ",forkortelse_val)) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  print(plot_object)
  ggsave(paste0("loop_figs/renseeffekt/", forkortelse_val, ".png"), plot = plot_object, dpi = 300)
}

