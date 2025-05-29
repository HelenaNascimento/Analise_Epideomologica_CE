if(!require(remotes)){ install.packages("remotes") }
if(!require(tidyverse)){install.packages("tidyverse") }
if(!require(readxl)){install.packages("readxl") }
if(!require(writexl)){install.packages("writexl") }
if(!require(ggplot2)){install.packages("ggplot2") }
if(!require(sf)){install.packages("sf") }
if(!require(geobr)){install.packages("geobr") }
if(!require(plotly)){install.packages("plotly") }
if(!require(RColorBrewer)){ install.packages("RColorBrewer") }
if(!require(openxlsx)){ install.packages("openxlsx") }
if(!require(ggiraph)){ install.packages("ggiraph") }
if(!require(sp)){ install.packages("sp") }
if(!require(spdep)){ install.packages("spdep") }

# Carregando os pacotes
require(remotes)
require(tidyverse)
require(readxl)
require(writexl)
require(ggplot2)
require(sf)
require(geobr)
require(plotly)
require(RColorBrewer)
require(openxlsx)
require(ggiraph)
require(sp)
require(spdep)

install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')


# Instalar o 'microdatasus' diretamente do GitHub
remotes::install_github("rfsaldanha/microdatasus")


# Baixando o shapefile do Brasil
mapa_Brasil = read_country(year=2020,simplified = T, 
                           showProgress = F)

#Criando o grafico
ggplot()+geom_sf(data=mapa_Brasil, fill = "grey90", 
                 color = "black")+ 
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

# Baixando o shapefile das regioes do Brasil
mapa_regioes = read_region(year = 2020, simplified = TRUE, 
                           showProgress = FALSE)


#criando grafico
ggplot() +
  geom_sf(data = mapa_regioes, aes(fill = name_region), 
          color = "black") + 
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  ) +
  labs(fill = "Regiões do Brasil")  # Nome da legenda



# Tornando mapa das Regiões do Brasil interativo
p = ggplot() +
  geom_sf(data = mapa_regioes, aes(fill = name_region, 
                                   text = name_region), color = "black") + 
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  ) +
  labs(fill = "Regiões do Brasil")  # Nome da legenda
# Transformando em gráfico interativo
ggplotly(p, tooltip = "text")

# Mudando a unidade geográfica para Estados
mapa_Estados = read_state(code_state="all",year=2020,simplified = T, 
                          showProgress = F)

ggplot()+geom_sf(data=mapa_Estados, fill = "grey90", color = "black")+ 
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

# Baixando o shapefile dos municípios do CE
mapa_CE <- read_municipality(code_muni = "CE", year = 2023)

ggplot()+geom_sf(data=mapa_CE,fill = "grey90", color = "black")+ 
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

#Plotando o mapa dos municípios do Estado do CE 
#com o nome dos municípios aparecendo
# Criando coluna com números de 1 a 92
mapa_CE$numero <- 1:nrow(mapa_CE)

p <- ggplot() +
  geom_sf(data = mapa_CE, aes(text = name_muni), 
          fill = "grey90", color = "black") + 
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )

ggplotly(p, tooltip = "text")



# Gráfico com rótulos
p <- ggplot() +
  geom_sf(data = mapa_CE, aes(text = name_muni), 
          fill = "grey90", color = "black") + 
  geom_sf_text(data = mapa_CE, aes(label = numero), size = 3, 
               color = "blue") + 
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )+
  labs(fill = "name_muni")  # Nome da legenda

# Transformando em gráfico interativo
ggplotly(p, tooltip = "text")


#Regiões Imediatas do Brasil
mapa_reg_imed = read_immediate_region(code_immediate = "all",year=2020,
                                      simplified = T, showProgress = F)

# Adiciona uma coluna com o número da linha
#mapa_reg_imed$linha <- 1:nrow(mapa_reg_imed)
ggplot()+geom_sf(data=mapa_reg_imed, fill = "grey90", color = "black")+ 
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

#Regiões Intermediárias
mapa_reg_interm = read_intermediate_region(
  code_intermediate = "all",
  year=2020,simplified = T, showProgress = F)
ggplot()+geom_sf(data=mapa_reg_interm,fill = "grey90", 
                 color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

#microsaude
mapa_reg_Saude = read_health_region(macro=F,year=2013,
                                    simplified = T, showProgress = F)

#macrosaude
#mapa_reg_Saude = read_health_region(macro=T,year=2013,
#                      simplified = T, showProgress = F)
ggplot()+geom_sf(data=mapa_reg_Saude, fill = "grey90", 
                 color = "black")+ 
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

remotes::install_github("rfsaldanha/microdatasus")

require(microdatasus)
require(dplyr)
require(stringr)

dados_sih_2023 <- fetch_datasus(
  year_start = 2023, 
  month_start = 1,
  year_end = 2023,
  month_end = 12,
  uf = "CE",
  information_system = "SIH-RD"
)