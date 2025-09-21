library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)

df <- eventos_export

df <- df %>%
  mutate(
    start = as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    end   = as.POSIXct(end,   format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    year  = year(start),
    month = month(start)
  )

df <- df %>%
  mutate(
    enso_period = case_when(
      year %in% 2020:2022 ~ "La_Nina",
      year %in% 2023:2024 ~ "El_Nino",
      TRUE ~ "Neutral"
    )
  )
df <- df %>%
  filter(enso_period != "Neutral")

agg_year <- df %>%
  group_by(enso_period, year) %>%
  summarise(
    n_events = n(),
    mean_duration_h = mean(difftime(end, start, units = "hours"), na.rm = TRUE),
    median_duration_h = median(difftime(end, start, units = "hours"), na.rm = TRUE)
  ) %>%
  ungroup()

print(agg_year)

agg_month <- df %>%
  group_by(enso_period, month) %>%
  summarise(
    n_events = n(),
    mean_duration_h = mean(difftime(end, start, units = "hours"), na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(agg_year, aes(x = factor(year), y = n_events, fill = enso_period)) +
  geom_col(position = "dodge") +
  labs(title = "Número de eventos de pesca por ano segundo regime ENSO",
       x = "Ano",
       y = "Número de eventos",
       fill = "Regime ENSO")

ggplot(agg_year, aes(x = factor(year), y = mean_duration_h, color = enso_period, group = enso_period)) +
  geom_line() + geom_point() +
  labs(title = "Duração média dos eventos por ano segundo ENSO",
       x = "Ano", y = "Duração média (h)")

la <- agg_month %>% filter(enso_period == "La_Nina") %>% pull(n_events)
el <- agg_month %>% filter(enso_period == "El_Nino") %>% pull(n_events)

t.test(la, el, alternative = "two.sided")

diff_abs <- mean(el) - mean(la)
diff_pct <- (diff_abs / mean(la)) * 100

resultado_teste <- data.frame(
  Estatística = c(
    "Teste",
    "Hipótese alternativa",
    "Valor t",
    "Graus de liberdade",
    "p-valor",
    "Intervalo de confiança 95%",
    "Média mensal (La Niña)",
    "Média mensal (El Niño / Transição)",
    "Diferença absoluta",
    "Diferença percentual"
  ),
  Valor = c(
    "Welch Two Sample t-test",
    "Diferença ≠ 0",
    round(t.test(la, el)$statistic, 4),
    round(t.test(la, el)$parameter, 3),
    signif(t.test(la, el)$p.value, 5),
    paste0("[", round(t.test(la, el)$conf.int[1], 2), ", ",
           round(t.test(la, el)$conf.int[2], 2), "]"),
    round(mean(la), 2),
    round(mean(el), 2),
    round(diff_abs, 2),
    paste0(round(diff_pct, 1), "%")
  )
)

writexl::write_xlsx(resultado_teste, "Resultado_teste_t_ENSO.xlsx")

agg_enso <- df %>%
  group_by(enso_period, year, month) %>%
  summarise(n_events = n()) %>%
  group_by(enso_period) %>%
  summarise(
    media_mensal = mean(n_events),
    sd_mensal = sd(n_events)
  )
ggplot(agg_enso, aes(x = enso_period, y = media_mensal, fill = enso_period)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = media_mensal - sd_mensal,
                    ymax = media_mensal + sd_mensal), width = 0.2) +
  labs(title = "Média mensal de eventos de pesca por regime ENSO",
       x = "Regime ENSO", y = "Média mensal de eventos") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")




