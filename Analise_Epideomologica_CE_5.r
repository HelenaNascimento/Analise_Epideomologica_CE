# ========================================================
# Instalação de pacotes necessários
# ========================================================

# Instalação do pacote 'remotes' para instalar pacotes do GitHub
#install.packages("remotes")

# Instalação do pacote 'read.dbc' para leitura de arquivos DBC do DATASUS
#install.packages("read.dbc")

# ========================================================
# Carregando bibliotecas
# ========================================================

library(remotes)       # Instalação de pacotes a partir do GitHub
library(dplyr)         # Manipulação de dados
library(readr)         # Importação e exportação de dados
library(microdatasus)  # Acesso aos microdados do DATASUS
library(stringr)
library(geobr)

# ========================================================
# Instalação da versão mais recente do microdatasus via GitHub
# ========================================================

remotes::install_github("rfsaldanha/microdatasus")

# ========================================================
# Coleta de dados SIH-RD (Sistema de Informações Hospitalares - RD)
# ========================================================

# Baixando dados para o Ceará no ano de 2023
dados_sih_2023 <- fetch_datasus(
  year_start = 2023, 
  month_start = 1,
  year_end = 2023,
  month_end = 12,
  uf = "CE",
  information_system = "SIH-RD"
)

# Baixando dados para o Ceará no ano de 2024
dados_sih_2024 <- fetch_datasus(
  year_start = 2024, 
  month_start = 1,
  year_end = 2024,
  month_end = 12,
  uf = "CE",
  information_system = "SIH-RD"
)

# Baixando dados para o Ceará no ano de 2025 (até abril)
dados_sih_2025 <- fetch_datasus(
  year_start = 2025, 
  month_start = 1,
  year_end = 2025,
  month_end = 4,
  uf = "CE",
  information_system = "SIH-RD"
)

# ========================================================
# Consolidação dos dados de todos os anos
# ========================================================

dados_sih <- bind_rows(dados_sih_2023, dados_sih_2024, dados_sih_2025)

# ========================================================
# Processamento dos dados para padronização
# ========================================================

dados_sih_proc <- process_sih(dados_sih)

# ========================================================
# Filtragem de casos de Influenza (CID-10: J09, J10, J11)
# ========================================================

##### alterações realizadas à partir desse ponto ##### 

dados_influenza <- dados_sih_2025 %>%
  filter(str_detect(DIAG_PRINC, "^J09|^J10|^J11"))

# ========================================================
# Filtragem específica: casos do Ceará em 2023
# ========================================================

dados_CE_2025 <- dados_influenza %>%
  filter(ANO_CMPT == 2025)

# ========================================================
# Contagem de casos por município de residência
# ========================================================

casos_por_municipio <- dados_CE_2025 %>%
  group_by(MUNIC_RES) %>%
  summarise(n_casos = n()) %>%
  filter(n_casos > 0)  # Considera apenas municípios com pelo menos 1 caso


# ========================================================
# Preparação do shapefile dos municípios
# ========================================================
# Baixando o shapefile dos municípios do Ceará
mapa_CE <- read_municipality(code_muni = "CE", year = 2025)

# Criando variável com código de 6 dígitos para compatibilização
mapa_CE$cod6 <- substr(mapa_CE$code_muni, 1, 6)
mapa_CE$cod6 <- as.character(mapa_CE$cod6)

# ========================================================
# Contagem geral de casos de Influenza
# ========================================================

casos_muni <- dados_influenza %>%
  group_by(MUNIC_RES) %>%
  summarise(n_casos = n()) %>%
  mutate(MUNIC_RES = as.character(MUNIC_RES))

# ========================================================
# Juntando contagem de casos com o shapefile
# ========================================================

mapa_CE <- mapa_CE %>%
  left_join(casos_muni, by = c("cod6" = "MUNIC_RES"))

# ========================================================
# Preparação da tabela final com nomes dos municípios
# ========================================================

tabela_casos <- casos_por_municipio %>%
  left_join(
    mapa_CE %>%
      mutate(MUNIC_RES = substr(code_muni, 1, 6)) %>%
      select(MUNIC_RES, name_muni),
    by = "MUNIC_RES"
  ) %>%
  select(MUNIC_RES, name_muni, n_casos) %>%
  arrange(desc(n_casos))  # Ordena do maior para o menor número de casos

# ========================================================
# Visualização da tabela
# ========================================================

print(tabela_casos)

# ========================================================
# Exportação para CSV (opcional)
# ========================================================

# Descomente a linha abaixo caso queira salvar a tabela em CSV:
write_csv(tabela_casos, "casos_por_municipio_CE_2025.csv")

