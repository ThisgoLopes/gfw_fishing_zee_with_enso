pkgs <- c(
  "gfwr","dplyr","tidyr","purrr","tibble","stringr","readr","lubridate",
  "sf","units","httr","jsonlite","glue","ggplot2","stars","raster","ncdf4","rerddap", "rnaturalearth", "rnaturalearthdat")

new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")