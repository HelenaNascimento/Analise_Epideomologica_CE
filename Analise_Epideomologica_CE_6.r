# Instalação e carregamento dos pacotes necessários
if(!require(remotes)) install.packages("remotes")
if(!require(dplyr)) install.packages("dplyr")
if(!require(stringr)) install.packages("stringr")
if(!require(geobr)) install.packages("geobr")
if(!require(sf)) install.packages("sf")
if(!require(readr)) install.packages("readr")
if(!require(microdatasus)) remotes::install_github("rfsaldanha/microdatasus")

library(remotes)
library(dplyr)
library(stringr)
library(geobr)
library(sf)
library(readr)
library(microdatasus)

# Baixando dados SIH-RD para o Ceará de 2023 a 2025
dados_sih_2023 <- fetch_datasus(
  year_start = 2023, month_start = 1,
  year_end = 2023, month_end = 12,
  uf = "CE",
  information_system = "SIH-RD"
)

dados_sih_2024 <- fetch_datasus(
  year_start = 2024, month_start = 1,
  year_end = 2024, month_end = 12,
  uf = "CE",
  information_system = "SIH-RD"
)

dados_sih_2025 <- fetch_datasus(
  year_start = 2025, month_start = 1,
  year_end = 2025, month_end = 4,
  uf = "CE",
  information_system = "SIH-RD"
)

# Juntando os dados de todos os anos
dados_sih <- bind_rows(dados_sih_2023, dados_sih_2024, dados_sih_2025)

# Processando os dados
dados_sih_proc <- process_sih(dados_sih)

# Criando coluna "mes_ano"
dados_sih_proc <- dados_sih_proc %>%
  mutate(
    MES_CMPT = as.numeric(MES_CMPT),
    ANO_CMPT = as.numeric(ANO_CMPT),
    mes_ano = paste0(sprintf("%02d", MES_CMPT), "-", ANO_CMPT)
  )

# Filtrando casos de Influenza (CID-10 J09, J10, J11)
dados_influenza <- dados_sih_proc %>%
  filter(str_detect(DIAG_PRINC, "^J09|^J10|^J11"))

# Contando casos por município e mes_ano
casos_muni_mes <- dados_influenza %>%
  group_by(MUNIC_RES, mes_ano) %>%
  summarise(n_casos = n(), .groups = "drop") %>%
  mutate(MUNIC_RES = as.character(MUNIC_RES))

# Baixando o shapefile dos municípios do Ceará
mapa_CE <- read_municipality(code_muni = "CE", year = 2023)

# Criando código de 6 dígitos para compatibilizar
mapa_CE <- mapa_CE %>%
  mutate(cod6 = substr(code_muni, 1, 6))

# Juntando com os nomes dos municípios
casos_muni_mes_nome <- casos_muni_mes %>%
  left_join(
    mapa_CE %>% select(cod6, name_muni) %>%
      mutate(MUNIC_RES = as.character(cod6)),
    by = "MUNIC_RES"
  ) %>%
  select(MUNIC_RES, name_muni, mes_ano, n_casos) %>%
  arrange(desc(n_casos))

# Visualizando a tabela
print(casos_muni_mes_nome)

# Exportando para CSV
write_csv(casos_muni_mes_nome, "casos_influenza_por_municipio_mes.csv")
