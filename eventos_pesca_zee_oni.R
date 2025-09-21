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

## Busca na API todos os eventos de pesca, do primeiro ao último dia do ano

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

## Organiza os eventos e classifica por trimestre

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

## captura os dados de ONI no site NOAA

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
  filter(ano >= 2021, ano <= 2025) %>%
  filter(trimestre %in% c("DJF","MAM","JJA","SON"))

## left join entre os trimestres dos eventos de Pesca com os trimestres ONI
agg_q <- eventos_proc %>%
  group_by(ano, trimestre) %>%
  summarise(
    n_events = n(),
    hours_sum = sum(duration_h, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(oni_tbl, by = c("ano","trimestre")) %>%
  arrange(ano, factor(trimestre, levels = c("DJF","MAM","JJA","SON")))

##Condensa as horas de pesca em cada lat e lon com o trimestre correspondente

ev_grid_quarter <- eventos_proc %>%
  mutate(
    lat_bin = round(lat, 0.01),
    lon_bin = round(lon, 0.01)
  ) %>%
  group_by(ano, lat_bin, lon_bin, trimestre) %>%
  summarise(hours = sum(duration_h, na.rm = TRUE), .groups = "drop")

ev_grid_quarter_oni <- ev_grid_quarter %>%
  left_join(agg_q %>% select(ano, trimestre, ONI), by = c("ano","trimestre"))

# captura a ZEE para o plot

zee <- st_read("eez_v12.gpkg")
names(zee)
unique(zee_br$ISO_TER1)
zee_br <- zee %>%
    filter(TERRITORY1 == "Brazil" | ISO_TER1 == "BRA")
plot(st_geometry(zee_br))
rm(zee)

## Função de Plot ano a ano dos dados agregados por trimestres

lab_fun <- function(x) {
  o <- agg_hora_trimestre %>% select(trimestre, ano, ONI) %>% distinct()
  setNames(
    paste0(x$trimestre, " (ONI=", round(dplyr::first(o$ONI[o$trimestre==x$trimestre & o$ano==x$ano]),2), ")"),
    x$trimestre
  )
}

plot_esforco_ano <- function(ano_selecionado) {
  
  ev_grid_quarter_oni <- ev_grid_quarter %>%
    left_join(
      agg_q %>% select(ano, trimestre, ONI),
      by = c("ano", "trimestre")
    ) %>%
    mutate(trimestre_ONI = paste0(trimestre, " (ONI=", round(ONI, 2), ")"))
  
  max_hours <- max(ev_grid_quarter_oni$hours, na.rm = TRUE)
  
  ggplot() +
    geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
    geom_tile(data = ev_grid_quarter_oni %>% filter(ano == ano_selecionado),
              aes(x = lon_bin, y = lat_bin, fill = hours)) +
    geom_sf(data = zee_br, fill = NA, color = "black", linewidth = 0.2) +
    geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.6) +
    scale_fill_viridis_c(
      option = "magma", trans = "log1p",
      breaks = c(1e2, 1e3, 1e4, 1e5, 1e6),
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      limits = c(0, max_hours),
      name = "Horas de pesca"
    ) +
    coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
    scale_x_continuous(
      breaks = seq(-60, -25, by = 10),
      labels = function(x) paste0(abs(x), "°W")
    ) +
    scale_y_continuous(
      breaks = seq(-40, 10, by = 10),
      labels = function(y) paste0(abs(y), ifelse(y > 0, "°N", "°S"))
    ) +
    facet_wrap(~ trimestre_ONI, ncol = 2) +
    labs(
      title = paste("Esforço de pesca aparente na ZEE brasileira –", ano_selecionado),
      subtitle = "Eventos de Pesca agregados por trimestre | Média ONI por trimestre",
      caption = "Fonte: Global Fishing Watch API + NOAA ONI | Elaboração própria",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "gray90"),
      axis.text = element_text(size = 8)
    )
}
plot_esforco_ano(2025)


# PLOT POR TRIMESTRE #
tri <- "SON"
ano_A <- 2023
ano_B <- 2022 

A <- ev_grid_quarter %>% filter(ano == ano_A, trimestre == tri) %>% 
  select(lon_bin, lat_bin, hours) %>% rename(hA = hours)
B <- ev_grid_quarter %>% filter(ano == ano_B, trimestre == tri) %>% 
  select(lon_bin, lat_bin, hours) %>% rename(hB = hours)

diff_df <- full_join(A, B, by = c("lon_bin","lat_bin")) %>%
  mutate(across(c(hA, hB), ~replace_na(.x, 0)),
         diff = hA - hB,
         ldiff = log1p(hA) - log1p(hB))

ggplot() +
  geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
  geom_tile(data = diff_df, aes(lon_bin, lat_bin, fill = ldiff)) +
  geom_sf(data = zee_br, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       name = "Δ log horas") +
  coord_sf(xlim = c(-60,-25), ylim = c(-40,10), expand = FALSE) +
  labs(title = paste0("Diferença de esforço – ", tri, " (", ano_A, " − ", ano_B, ")"),
       subtitle = "Vermelho: maior em El Niño | Azul: maior em La Niña") +
  theme_minimal()

##Beta Oni por célula

grid_q_oni <- ev_grid_quarter %>%
  left_join(agg_q %>% select(ano, trimestre, ONI), by = c("ano","trimestre"))

by_cell <- grid_q_oni %>%
  group_by(lat_bin, lon_bin) %>%
  filter(n() >= 6) %>%
  group_modify(~{
    m <- try(lm(log1p(hours) ~ ONI + trimestre, data = .x), silent = TRUE)
    if (inherits(m, "try-error")) return(tibble(beta_ONI = NA_real_, p = NA_real_))
    s <- summary(m)
    tibble(
      beta_ONI = coef(m)["ONI"],
      p = coef(s)[ "ONI","Pr(>|t|)" ]
    )
  }) %>% ungroup()

betas_sig <- by_cell %>%
  filter(p <= 0.05)

ggplot(betas_sig) +
  geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
  geom_tile(aes(lon_bin, lat_bin, fill = beta_ONI)) +
  geom_sf(data = zee_br, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = brasil, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "β (ONI)") +
  coord_sf(xlim = c(-60,-25), ylim = c(-40,10), expand = FALSE) +
  labs(title = "Sensibilidade espacial do esforço ao ENSO (β de ONI)",
       subtitle = "Coeficiente β do ONI (apenas significativos, p ≤ 0.05)
       ONI>1 El Niño Forte ONI < -1 La Niña forte",
       caption = "Fonte: GFW API + NOAA ONI | Elaboração própria") +
  theme_minimal()
