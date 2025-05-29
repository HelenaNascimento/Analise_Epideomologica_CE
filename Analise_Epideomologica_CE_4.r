library(dplyr)
library(ggplot2)
library(geobr)
library(sf)
library(viridis)
library(purrr)

# 1. Baixar o shapefile do Ceará
mapa_CE <- read_municipality(code_muni = "CE", year = 2025) %>%
  mutate(code_muni6 = substr(code_muni, 1, 6))

# 2. Filtrar dados de 2025
dados_2025 <- dados_influenza %>%
  filter(ANO_CMPT == 2025) %>%
  mutate(MUNIC_RES = as.character(MUNIC_RES))

# 3. Função para gerar mapa para um mês específico
gerar_mapa_mes <- function(mes) {
  
  # Filtrar para o mês
  dados_mes <- dados_2025 %>%
    filter(MES_CMPT == mes)
  
  # Contar número de casos por município
  casos_por_municipio <- dados_mes %>%
    group_by(MUNIC_RES) %>%
    summarise(n_casos = n())
  
  # Juntar com o shapefile
  mapa_casos <- mapa_CE %>%
    left_join(casos_por_municipio, by = c("code_muni6" = "MUNIC_RES"))
  
  # Criar o mapa
  p <- ggplot(data = mapa_casos) +
    geom_sf(aes(fill = n_casos), color = "black") +
    scale_fill_viridis(option = "plasma", na.value = "white", name = "Nº de Casos") +
    theme_minimal() +
    labs(
      title = paste("Mapa de Calor - Casos - Ceará (2025) - Mês:", mes),
      subtitle = "Fonte: DATASUS",
      caption = "Elaborado com geobr e ggplot2"
    ) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(p)
}

# 4. Gerar os mapas para todos os meses (1 a 12)
mapas_por_mes <- map(1:12, gerar_mapa_mes)

# 5. Exibir um exemplo: Janeiro
mapas_por_mes[[1]]  # Exibe o mapa do mês 1 (Janeiro)

# 6. Se quiser salvar cada mapa como imagem:
# for (mes in 1:12) {
#   ggsave(filename = paste0("mapa_mes_", mes, ".png"), plot = mapas_por_mes[[mes]], width = 10, height = 8)
# }
