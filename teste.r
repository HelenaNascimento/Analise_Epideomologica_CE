install.packages("RODBC")


# Carregar o pacote RODBC
library(RODBC)

# Definir os detalhes da conexão
server <- "NOVACE-TI\DEV"  # Substitua pelo nome ou endereço do servidor MS SQL
database <- "TESTE"  # Substitua pelo nome do banco de dados
user <- "sa"  # Substitua pelo nome de usuário
password <- "senha@1234"  # Substitua pela senha

# Criar a string de conexão
connection_string <- paste0(
  "Driver={SQL Server};Server=", server, ";Database=", database,
  ";Uid=", user, ";Pwd=", password
)

# Estabelecer a conexão
con <- odbcDriverConnect(connection_string)

# Exemplo: executar uma consulta SQL
query <- "SELECT * FROM VW_Venda_Diaria_Telev"
result <- sqlQuery(con, query)

# Fechar a conexão
odbcClose(con)

# Exibir os resultados
print(result)
