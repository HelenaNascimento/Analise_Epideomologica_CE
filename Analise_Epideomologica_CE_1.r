install.packages("remotes")
install.packages("read.dbc")

library(remotes)

remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(dplyr)
library(stringr)

# Baixando dados SIH-RD para o CE em 2023

dados_sih_2023 <- fetch_datasus(
  year_start = 2023, 
  month_start = 1,
  year_end = 2023,
  month_end = 12,
  uf = "CE",
  information_system = "SIH-RD"
)

# Baixando dados SIH-RD para o CE em 2024
dados_sih_2025 <- fetch_datasus(
  year_start = 2025, 
  month_start = 1,
  year_end = 2025,
  month_end = 4,
  uf = "CE",
  information_system = "SIH-RD"
)

# Juntando os dados
dados_sih <- bind_rows(dados_sih_2025)

# Processando os dados
dados_sih_proc <- process_sih(dados_sih)

# Filtrando casos de Influenza (CID-10 J09, J10, J11)
dados_influenza <- dados_sih_2023 %>%
  filter(str_detect(DIAG_PRINC, "^J09|^J10|^J11"))

# Visualizando
head(dados_influenza)

colnames(dados_sih_proc)

colnames(dados_influenza)

library(dplyr)

casos_muni <- dados_influenza %>%
  group_by(MUNIC_RES) %>%  # ou o nome correto que encontrar!
  summarise(n_casos = n())

colnames(mapa_CE)

unique(nchar(dados_influenza$MUNIC_RES))

mapa_CE$cod6 <- substr(mapa_CE$code_muni, 1, 6)

mapa_CE$cod6 <- as.character(mapa_CE$cod6)
casos_muni <- dados_influenza %>%
  group_by(MUNIC_RES) %>%
  summarise(n_casos = n()) %>%
  mutate(MUNIC_RES = as.character(MUNIC_RES))

mapa_CE <- mapa_CE %>%
  left_join(casos_muni, by = c("cod6" = "MUNIC_RES"))

install.packages("viridis")


library(ggplot2)
library(viridis)

ggplot() +
  geom_sf(data = mapa_CE, aes(fill = n_casos), color = "black") +
  scale_fill_viridis(option = "plasma", na.value = "white", name = "Casos") +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

install.packages("ggrepel")

library(ggrepel)

mapa_CE$n_casos[is.na(mapa_CE$n_casos)] <- 0


limite_destaque <- 50

ggplot(mapa_CE) +
  geom_sf(aes(fill = n_casos), color = "black") +
  scale_fill_viridis(option = "plasma", na.value = "white", name = "Casos") +
  geom_text_repel(
    data = subset(mapa_CE, n_casos > limite_destaque),
    aes(label = name_muni, geometry = geom),
    stat = "sf_coordinates",
    size = 3,
    max.overlaps = 10
  ) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

##############
library(plotly)


p <- ggplot(mapa_CE) +
  geom_sf(aes(fill = n_casos, text = paste0(name_muni, "\nCasos: ", n_casos)), color = "black") +
  scale_fill_viridis(option = "plasma", na.value = "white", name = "Casos") +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


ggplotly(p, tooltip = "text")

unique(dados_influenza$ANO_CMPT)

table(dados_influenza$ANO_CMPT)

table(dados_influenza$ANO_CMPT, dados_influenza$MES_CMPT)

dados_2025 <- dados_influenza %>% filter(ANO_CMPT == 2025)

