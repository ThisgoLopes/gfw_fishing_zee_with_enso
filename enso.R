library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


enso_txt <- "detrend.nino34.ascii.txt"

enso_clean <- read_table(enso_txt, 
                         skip = 1, 
                         col_names = c("year", "month", "total", "climadjust", "anom"),
                         col_types = cols(
                           year = col_integer(),
                           month = col_integer(),
                           total = col_double(),
                           climadjust = col_double(),
                           anom = col_double()
                          )) %>% 
  mutate(
  date = as.Date(sprintf("%04d-%02d-01", year, month))
  )

enso_trimestres <- enso_clean %>%
  mutate(
    trimestre = case_when(
      month %in% c(12, 1, 2) ~ "DJF",
      month %in% c(3, 4, 5) ~ "MAM",
      month %in% c(6, 7, 8) ~ "JJA",
      month %in% c(9, 10, 11) ~ "SON",
      TRUE ~ NA_character_
    ),
    ano_trimestre = if_else(trimestre == "DJF" & month == 12, year + 1, year)
  ) %>%
  group_by(ano_trimestre, trimestre) %>%
  summarise(
    oni_media = mean(anom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    enso_class = case_when(
      oni_media >= 0.5 ~ "El_Nino",
      oni_media <= -0.5 ~ "La_Nina",
      TRUE ~ "Neutral",
    ),
      bar_color = case_when(
        enso_class == "El_Nino" ~ "firebrick",
        enso_class == "La_Nina" ~ "steelblue",
        TRUE ~ "gray80",
    ),
    mutate(
      data_inicio = case_when(
        trimestre == "DJF" ~ as.Date(paste0(ano_trimestre, "-01-01")),
        trimestre == "MAM" ~ as.Date(paste0(ano_trimestre, "-03-01")),
        trimestre == "JJA" ~ as.Date(paste0(ano_trimestre, "-06-01")),
        trimestre == "SON" ~ as.Date(paste0(ano_trimestre, "-09-01")),
        TRUE ~ NA_Date_
      )
    )
  )

enso_trimestres <- enso_trimestres %>%
  mutate(
    enso_fase = factor(enso_class, levels = c("La_Nina", "Neutral", "El_Nino")),
    data_inicio = as.Date(sprintf("%d-%02d-01", ano_trimestre, match(trimestre, c(
      "DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS", "ASO", "SON", "OND", "NDJ"
    ))))
  )

plot_trimestres <- ggplot(enso_trimestres, aes(x = data_inicio, y = oni_media, fill = enso_fase)) +
  geom_col(width = 75) +
  scale_fill_manual(
    values = c("La_Nina" = "#1f78b4", "Neutral" = "gray80", "El_Nino" = "#e31a1c"),
    name = "Fase ENSO",
    labels = c("La Niña", "Neutro", "El Niño")
  ) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "black") +
  labs(
    title = "Anomalia Trimestral da Temperatura Superficial do Pacífico (ONI)",
    subtitle = "Classificação da fase ENSO baseada em anomalias trimestrais do índice ONI (1950–2024)",
    x = "Ano",
    y = "Anomalia trimestral da TSM (°C)",
    caption = "Fonte: NOAA | Processamento: GFW + Análise Climatológica"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "top"
  )
ggsave("oni_trimestres_1950_2024v3.png", plot = plot_trimestres, width = 12, height = 6, dpi = 300)

enso_trimestres_recent <- enso_trimestres %>%
  mutate(
    enso_fase = factor(enso_class, levels = c("La_Nina", "Neutral", "El_Nino")),
    data_inicio = as.Date(sprintf("%d-%02d-01", ano_trimestre, match(trimestre, c(
      "DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS", "ASO", "SON", "OND", "NDJ"
    ))))
  ) %>%
  filter(ano_trimestre >= 2015)

plot_trimestres_recent <- ggplot(enso_trimestres_recent, aes(x = data_inicio, y = oni_media, fill = enso_fase)) +
  geom_col(width = 75) +
  scale_fill_manual(
    values = c("La_Nina" = "#1f78b4", "Neutral" = "gray80", "El_Nino" = "#e31a1c"),
    name = "Fase ENSO",
    labels = c("La Niña", "Neutro", "El Niño")
  ) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "black") +
  labs(
    title = "Fases Trimestrais do ENSO (ONI) | Recorte 2015–2024",
    subtitle = "Anomalia média trimestral da TSM no Pacífico Equatorial (ONI) com classificação ENSO",
    x = "Ano",
    y = "Anomalia trimestral da TSM (°C)",
    caption = "Fonte: NOAA | Processamento: GFW + Análise Climatológica"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "top"
  )
ggsave("oni_trimestres_2015_2024.png", plot = plot_trimestres_recent, width = 12, height = 6, dpi = 300)

enso_trimestre_2022_2024 <- enso_trimestres_recent %>%
  filter(ano_trimestre >= 2022 & anotrimestre <= 2024)

rm(list = setdiff(ls(), c("eventos_pesca_zee_brasil_2020_2024", "enso_trimestres", "enso_trimestres_recent", "plot_trimestres", "plot_trimestres_recent")))

write.csv(enso_trimestres, "enso_trimestres_1950_2025.csv")
write.csv(enso_trimestres_recent, "enso_trimestres_2014_2024.csv")


