library(tidyverse)
library(readr)
library(gfwr)
library(sf)
library(geobr)
library(janitor)

token <- gfw_auth()

regions <- get_regions()

buscar_eventos_ano <- function(ano, region_id = 8464, token) {
  start_date <- as.Date(paste0(ano, "-01-01"))
  end_date <- as.Date(paste0(ano, "-12-31"))
  
  message("Buscando eventos de pesca para o ano: ", ano)
  
  get_event(
    event_type = "FISHING",
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    region = region_id,
    region_source = "EEZ",
    key = token
  )
}
anos <- 2014:2024
eventos_lista <- lapply(anos, buscar_eventos_ano, token = token)

eventos_total <- bind_rows(eventos_lista)
cat("Total de eventos somando todos os anos:", nrow(eventos_total), "\n")

if (!inherits(eventos_total, "sf")) {
  eventos_sf <- st_as_sf(eventos_total, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
} else {
  eventos_sf <- eventos_total
}

eventos_export <- eventos_total[, !sapply(eventos_total, is.list)]
write.csv(eventos_export, "eventos_pesca_zee_brasil_2014_2024_anual.csv", row.names = FALSE)
eventos_export <- eventos_export %>%
  select((-vessel_nextPort))
eventos_pesca_zee_2014_2024 <- eventos_export
rm(list = c("eventos_lista", "eventos_sf","eventos_total", "eventos_export"))

eventos_pesca_2022_2024 <- eventos_pesca_zee_brasil_2020_2024 %>%
  mutate(ano = year(as.Date(start)),
         mes = month(as.Date(start)),
         trimestre = case_when(
           mes %in% c(12, 1, 2)  ~ "DJF",
           mes %in% c(1, 2, 3)   ~ "JFM",
           mes %in% c(2, 3, 4)   ~ "FMA",
           mes %in% c(3, 4, 5)   ~ "MAM",
           mes %in% c(4, 5, 6)   ~ "AMJ",
           mes %in% c(5, 6, 7)   ~ "MJJ",
           mes %in% c(6, 7, 8)   ~ "JJA",
           mes %in% c(7, 8, 9)   ~ "JAS",
           mes %in% c(8, 9, 10)  ~ "ASO",
           mes %in% c(9, 10, 11) ~ "SON",
           mes %in% c(10, 11, 12)~ "OND",
           mes %in% c(11, 12, 1) ~ "NDJ",
           TRUE ~ NA_character_),
         ano_trimestre = if_else(trimestre =="DJF" & mes == 12, ano +1, ano),
         ) %>%
  filter(ano >= 2022 & ano <= 2024)

eventos_enso_2022_2024 <- eventos_pesca_2022_2024 %>%
  left_join(
    enso_trimestre_2022_2024 %>%
      select(ano_trimestre, trimestre, enso_class, oni_media),
    by = c("ano_trimestre", "trimestre")
  )

write.csv(eventos_enso_2022_2024, "eventos_pesca_enso_2022_2024.csv")

eventos_por_fase <- eventos_enso_2022_2024 %>%
  group_by(enso_class) %>%
  summarise(n_eventos = n(), .groups = "drop")

ggplot(eventos_por_fase, aes(x = enso_class, y = n_eventos, fill = enso_class)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("El_Nino" = "#e31a1c", "La_Nina" = "#1f78b4", "Neutral" = "gray80")) +
  labs(
    title = "Eventos de Pesca por Fase ENSO (2022–2024)",
    x = "Fase ENSO",
    y = "Número de Eventos de Pesca",
    caption = "Fonte: GFW + NOAA"
  ) +
  theme_minimal(base_size = 14)
