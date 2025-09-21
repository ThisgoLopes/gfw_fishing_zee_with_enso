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
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)

token <- gfw_auth()

start_date <- as.Date("2022-01-01")   
end_date   <- Sys.Date() - 10         
temporal_resolution <- "MONTHLY"      
spatial_resolution  <- "LOW"   
group_by_var <- NA_character_

br_eez_tbl <- get_region_id(region_name = "BRA", region_source = "EEZ")
stopifnot(nrow(br_eez_tbl) >= 1)
br_eez_id <- br_eez_tbl$id[1]

seq_months <- function(from, to) {
  tibble(date = seq.Date(from = as.Date(paste0(year(from), "-", month(from), "-01")),
                         to   = as.Date(paste0(year(to),   "-", month(to),   "-01")),
                         by   = "month"))
}
enso_quarter <- function(d) {
  m <- month(d)
  case_when(
    m %in% c(12, 1, 2) ~ "DJF",
    m %in% c(3, 4, 5)  ~ "MAM",
    m %in% c(6, 7, 8)  ~ "JJA",
    m %in% c(9,10,11)  ~ "SON",
    TRUE ~ NA_character_
  )
}

get_effort_monthly_chunk <- function(start_date, end_date, region_id,
                                     spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     group_by = NA_character_) {
  tries <- 0
  repeat {
    tries <- tries + 1
    res <- try({
      get_raster(
        spatial_resolution  = spatial_resolution,
        temporal_resolution = temporal_resolution,
        start_date          = as.character(start_date),
        end_date            = as.character(end_date),
        region_source       = "EEZ",
        region              = region_id,
        group_by            = if (is.na(group_by)) NULL else group_by
      )
    }, silent = TRUE)
    if (!inherits(res, "try-error")) return(res)
    if (tries >= 3) stop("Falhou 3x ao consultar o esforço. Verifique conexão/limites e tente novamente.")
    message("Timeout/erro na API. Aguardando 10s e tentando de novo...")
    Sys.sleep(10)
  }
}

years <- seq(year(start_date), year(end_date), by = 1)

eff_monthly <- map_dfr(
  years,
  function(yy) {
    s <- as.Date(glue("{yy}-01-01"))
    e <- as.Date(glue("{yy}-12-31"))
    s <- max(s, start_date)
    e <- min(e, end_date)
    message(glue("Baixando esforço: {s} a {e}"))
    get_effort_monthly_chunk(
      start_date = s,
      end_date   = e,
      region_id  = br_eez_id,
      spatial_resolution  = spatial_resolution,
      temporal_resolution = temporal_resolution,
      group_by            = group_by_var
    )
  }
)
eff_monthly <- eff_monthly %>% clean_names()

eff_monthly <- eff_monthly %>%
  mutate(
    date = as.Date(paste0(time_range, "-01")),  
    year = year(date),
    month = month(date),
    enso_q = enso_quarter(date)
  ) %>%
  select(date, year, month, enso_q, everything())

eff_enso_q <- eff_monthly %>%
  group_by(year, enso_q) %>%
  summarise(
    total_hours   = sum(apparent_fishing_hours, na.rm = TRUE),
    total_vessels = sum(vessel_i_ds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, factor(enso_q, levels = c("DJF","MAM","JJA","SON")))

oni_url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"

oni_raw <- tryCatch(
  read_lines(oni_url),
  error = function(e) {
    warning("Falha ao baixar ONI; verifique conexão/URL. Baixe manualmente e leia de arquivo local.")
    character()
  }
)

oni_tbl <- oni_raw %>%
  as_tibble() %>%
  rename(line = value) %>%
  filter(!str_detect(line, "SEAS")) %>%
  mutate(line = str_squish(line)) %>%
  separate(line, into = c("season","year","total","anom"), sep = "\\s+", convert = TRUE) %>%
  mutate(
    year = as.integer(year),
    ONI  = as.numeric(anom)
  )

oni_quarters <- oni_tbl %>%
  filter(season %in% c("DJF","MAM","JJA","SON")) %>%
  mutate(enso_q = season,
         enso_q_index = paste0(year,"-",enso_q)) %>%
  select(year, enso_q, ONI, enso_q_index)

eff_enso_oni <- eff_enso_q %>%
  left_join(oni_quarters, by = c("year", "enso_q"))


#Mostra o esforço total por trimestre, colorido pela intensidade do ENSO (ONI).
ggplot(eff_enso_oni, aes(x = interaction(year, enso_q), y = total_hours, fill = ONI)) +
  geom_col() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    x = "Trimestre ENSO",
    y = "Horas de pesca aparente (ZEE Brasil)",
    fill = "ONI",
    title = "Esforço trimestral de pesca vs ENSO (ONI)",
    subtitle = "Fonte: GFW (esforço aparente AIS) + NOAA (ONI Niño 3.4)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Aqui você vê a relação linear entre ONI e esforço. O log1p ajuda a reduzir a assimetria (muitos zeros ou valores grandes

ggplot(eff_enso_oni, aes(x = ONI, y = log1p(total_hours), color = enso_q)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "ONI (NOAA/CPC)",
    y = "log(1 + horas de pesca)",
    color = "Trimestre ENSO",
    title = "Correlação entre ONI e esforço de pesca",
    subtitle = "Cada ponto representa um trimestre na ZEE brasileira"
  ) +
  theme_minimal()

m1 <- lm(log1p(total_hours) ~ ONI + enso_q, data = eff_enso_oni)
summary(m1)

eff_map <- eff_monthly %>%
  group_by(year, enso_q, lat, lon) %>%
  summarise(
    hours = sum(apparent_fishing_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enso_q_index = paste0(year, "-", enso_q))
eff_map <- eff_map %>%
  left_join(oni_quarters, by = c("year","enso_q"))


brasil <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")
america_sul <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")

plot_effort_map <- function(df, year_select, enso_q_select) {
  
  df_plot <- df %>%
    filter(year == year_select, enso_q == enso_q_select)
  
  oni_val <- round(unique(df_plot$ONI), 2)
  
  ggplot() +
    geom_sf(data = america_sul, fill = "gray95", color = "gray70") +
    geom_tile(data = df_plot,
              aes(x = lon, y = lat, fill = hours)) +
    geom_sf(data = brasil, fill = NA, color = "black", size = 0.6) +
    scale_fill_viridis_c(option = "C", trans = "log1p") +
    coord_sf(xlim = c(-60, -25), ylim = c(-40, 10), expand = FALSE) +
    labs(
      title = glue::glue("Esforço aparente de pesca - ZEE Brasil ({year_select}-{enso_q_select})"),
      subtitle = glue::glue("ONI = {oni_val}"),
      fill = "Horas"
    ) +
    theme_minimal()
}

plot_effort_map(eff_map, 2022, "DJF")
plot_effort_map(eff_map, 2024, "DJF")