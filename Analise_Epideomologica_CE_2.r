library(dplyr)
library(ggplot2)
library(geobr)
library(sf)
library(viridis)

dados_2025 <- dados_influenza %>% 
  filter(ANO_CMPT == 2025)

casos_por_municipio <- dados_2025 %>% 
  group_by(MUNIC_RES) %>%
  summarise(n_casos = n()) %>%
  mutate(MUNIC_RES = as.character(MUNIC_RES))

mapa_CE <- read_municipality(code_muni = "CE", year = 2023)

mapa_CE <- mapa_CE %>%
  mutate(code_muni6 = substr(code_muni, 1, 6))

mapa_casos <- mapa_CE %>%
  left_join(casos_por_municipio, by = c("code_muni6" = "MUNIC_RES"))

ggplot(data = mapa_casos) +
  geom_sf(aes(fill = n_casos), color = "black") +
  scale_fill_viridis(option = "plasma", na.value = "white", name = "Nº de Casos") +
  theme_minimal() +
  labs(
    title = "Mapa de Calor - Casos Internações Influenza - Ceará (2025)",
    subtitle = "Fonte: DATASUS",
    caption = "Elaborado com geobr e ggplot2"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )