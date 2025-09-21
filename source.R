library(tidyverse)
library(readr)
library(gfwr)
library(sf)
library(geobr)
library(janitor)

gfw_auth()

emb_itaipava <- read.csv("embarcacoes_itaipava_v1.csv")

id_embarcacoes <- emb_itaipava$vesselId

busca_eventos <- possibly(
  .f = function(id_embarcacoes){
    print(paste("Buscando eventos para o id:", id_embarcacoes))
    
    get_event(
      event_type = 'fishing',
      vessels = id_embarcacoes,
      start_date = '2020-01-01',
      end_date = '2025-08-20'
      )
  },
  otherwise = NULL
)
lista_eventos <- map(id_embarcacoes, busca_eventos)

eventos_frota<- list_rbind(lista_eventos)

eventos_limpos <- eventos_frota %>%
  filter(!is.na(lat) & !is.na(lon))

eventos_organizados <- eventos_limpos %>%
  mutate(
    mes = month(start, label = TRUE, abbr = FALSE),
    regioes = map_chr(regions, ~paste(unlist(.x), collapse = ";")),
    distancias_km = map_chr(distances, ~paste(unlist(.x), collapse = "; ")),
    )
  
eventos_mensal <- eventos_organizados %>%
  select(
    start,
    end,
    mes,
    lat,
    lon,
    eventType,
    vesselId,
    vessel_name,
    regioes,
    distancias_km,
  )
eventos_estacao <- eventos_mensal %>%
  

analise_mensal_df <- eventos_mensal %>%
  group_by(mes, vessel_name) %>%
  summarise(total_eventos = n())


write.csv(eventos_mensal, "eventos_mensal_frota_v1_20201-20205.csv")

writexl::write_xlsx(emb_itaipava, "embarcacoes_itaipava_v1.xlsx")

sf_mapa_brasil <- read_country(year=2020)

##plot mensal ##


ggplot() +
  geom_sf(data = sf_mapa_brasil, fill= "gray90", color="white")+
  stat_density2d(
    data = eventos_mensal,
    aes(x=lat, y=, fill = after_stat()),
    geom = "polygon",
    alpha = 0.5,
    )
  facet_wrap(~ mes, ncol = 4) +
  scale_fill_viridis_d() +
    labs(
      title = "Comportamento Mensal da Frota de Itaipava (Nível Brasil)",
      subtitle = "Distribuição da atividade de pesca para cada mês do ano",
      fill = "Densidade da\nAtividade"
    ) +
    theme_minimal()
    

## plot geral
ggplot() +
  geom_sf(data = sf_mapa_brasil, fill="gray90", color="white") +
  
  geom_density_2d_filled(data = eventos_limpos, 
                         aes(x = lon, y = lat), 
                         alpha = 0.8) +
    
    geom_point(data = eventos_frota,
               aes(x=lon, y=lat),
               color="red", size = 0.2, alpha= 0.2)+
  coord_sf(xlim = c(-55, -25), ylim = c(0, -35)) +
    labs(
      title = "Principais Áreas de Pesca da Frota de Itaipava (2024)",
      subtitle = "As áreas mais escuras indicam maior concentração de eventos de pesca",
      x = "Longitude",
      y = "Latitude",
      fill = "Densidade da Atividade"
    ) +
    theme_minimal()


