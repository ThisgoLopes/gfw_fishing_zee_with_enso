install.packages(c("ggplot2", "dplyr", "viridis", "sf"))
library(ggplot2)
library(dplyr)

ggplot(df, aes(x = lon, y = lat)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c() +
  facet_wrap(~ year, ncol = 2) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Heatmap de densidade de pesca por ano",
       x = "Longitude", y = "Latitude", fill = "Densidade")