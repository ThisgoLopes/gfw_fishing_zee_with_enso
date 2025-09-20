library(gfwr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(readr)
library(lubridate)
library(sf)
library(glue)
library(ggplot2)
library(janitor)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

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
anos <- 2022:2025

eventos_lista <- lapply(anos, buscar_eventos_ano, token = token)

eventos_proc <- eventos_lista %>%
  bind_rows() %>%
  mutate(
    start = ymd_hms(start, tz = "UTC"),
    end   = ymd_hms(end, tz = "UTC"),
    duration_h = as.numeric(difftime(end, start, units = "hours")),
    ano = year(start),
    mes = month(start),
    trimestre = case_when(
      mes %in% c(12,1,2)  ~ "DJF",
      mes %in% c(3,4,5)   ~ "MAM",
      mes %in% c(6,7,8)   ~ "JJA",
      mes %in% c(9,10,11) ~ "SON"
    )
  )

eventos_lista %>% 
  bind_rows() %>%
  filter(!lubridate::is.POSIXt(lubridate::ymd_hms(start, quiet = TRUE)))

oni_url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
oni_raw <- readr::read_lines(oni_url)

oni_tbl <- tibble(line = oni_raw) %>%
  filter(!str_detect(line, "SEAS")) %>%
  mutate(line = str_squish(line)) %>%
  separate(line, into = c("trimestre","ano","total","anom"),
           sep = "\\s+", convert = TRUE) %>%
  transmute(
    ano = as.integer(ano),
    trimestre,
    ONI = as.numeric(anom)
  ) %>%
  filter(ano >= 2022, ano <= 2025) %>%
  filter(trimestre %in% c("DJF","MAM","JJA","SON"))

agg_q <- eventos_proc %>%
  group_by(ano, trimestre) %>%
  summarise(
    n_events = n(),
    hours_sum = sum(duration_h, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(oni_tbl, by = c("ano","trimestre")) %>%
  arrange(ano, factor(trimestre, levels = c("DJF","MAM","JJA","SON")))

##Analise exploratoria dos eventos agregados

cor(agg_q$hours_sum, agg_q$ONI, use = "complete.obs")
cor(agg_q$n_events, agg_q$ONI, use = "complete.obs")

m1 <- lm(log1p(hours_sum) ~ ONI + trimestre, data = agg_q)
summary(m1)

m2 <- lm(log1p(n_events) ~ ONI + trimestre, data = agg_q)
summary(m2)

ev_grid <- eventos_proc %>%
  mutate(
    lat_bin = round(lat, 0.01),
    lon_bin = round(lon, 0.01)
  ) %>%
  group_by(ano, lat_bin, lon_bin) %>%
  summarise(hours = sum(duration_h, na.rm = TRUE), .groups = "drop")

zee_br <- st_read("eez_v12.dpkg")

zee_br <- zee %>%
  filter(Territory1 == "Brazil" | ISO_Ter1 == "BRA")

ev_grid_quarter <- eventos_proc %>%
  mutate(
    lat_bin = round(lat, 0.01),
    lon_bin = round(lon, 0.01)
  ) %>%
  group_by(ano, trimestre, lat_bin, lon_bin) %>%
  summarise(hours = sum(duration_h, na.rm = TRUE), .groups = "drop")

ev_grid_quarter$trimestre <- factor(ev_grid_quarter$trimestre, 
                                    levels = c("DJF", "MAM", "JJA", "SON"))
max_hours <- max(ev_grid_quarter$hours, na.rm = TRUE)

plot_esforco_ano <- function(ano_selecionado) {
  ggplot() +
    geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
    geom_tile(data = ev_grid_quarter %>% filter(ano == ano_selecionado),
              aes(x = lon_bin, y = lat_bin, fill = hours)) +
    geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.6) +
    scale_fill_viridis_c(
      option = "magma", trans = "log1p",
      labels = label_number(scale_cut = cut_short_scale()),
      limits = c(0, max_hours),
      name = "Horas de pesca"
    ) +
    coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
    facet_wrap(~ trimestre, ncol = 2) +
    labs(
      title = paste("Esforço de pesca aparente na ZEE brasileira –", ano_selecionado),
      subtitle = "Eventos de Pesca – horas agregadas por trimestre",
      caption = "Fonte: Global Fishing Watch API | Elaboração própria",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "gray90"),
      axis.text = element_text(size = 8)
    )
}
plot_esforco_ano(2024)


ggplot() +
  geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
  geom_tile(data = ev_grid_quarter,
            aes(x = lon_bin, y = lat_bin, fill = hours)) +
  geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "magma", trans = "log1p",
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Horas de pesca"
  ) +
  coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
  facet_grid(ano ~ trimestre) +
  labs(
    title = "Esforço de pesca aparente na ZEE brasileira",
    subtitle = "Eventos FISHING – horas agregadas por trimestre (2022–2025)",
    caption = "Fonte: Global Fishing Watch API | Elaboração própria",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 8)
  )

ggplot() +
  geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
  geom_tile(data = ev_grid,
            aes(x = lon_bin, y = lat_bin, fill = hours)) +
  geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "magma",
    trans   = "log1p",
    labels  = label_number(scale_cut = cut_si(" ")),
    name    = "Horas de pesca"
  ) +
  coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
  labs(
    title = "Esforço de pesca aparente na ZEE brasileira",
    subtitle = "Eventos FISHING – Global Fishing Watch (2022–2025)",
    caption = "Fonte: GFW API | Elaboração própria"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 10)
  )
ggplot() +
  geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
  geom_tile(data = ev_grid,
            aes(x = lon_bin, y = lat_bin, fill = hours)) +
  geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "magma", trans = "log1p",
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Horas de pesca"
  ) +
  coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
  facet_wrap(~ ano, ncol = 2) +
  labs(
    title = "Esforço de pesca aparente na ZEE brasileira (2022–2025)",
    subtitle = "Eventos FISHING – horas agregadas por ano",
    caption = "Fonte: Global Fishing Watch API | Elaboração própria",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 9)
  )

ggplot() +
  geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
  geom_point(data = eventos_proc,
             aes(x = lon, y = lat),
             color = "red", alpha = 0.2, size = 0.3) +
  geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
  labs(
    title = "Atividade pesqueira registrada na ZEE brasileira",
    subtitle = "Eventos FISHING – Global Fishing Watch (2022–2025)",
    caption = "Fonte: GFW API | Elaboração própria",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()