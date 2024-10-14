import pandas as pd
import random

# Definindo as listas de valores possíveis
anos = [2019, 2020, 2021, 2022]
trimestres = ['Q1', 'Q2', 'Q3', 'Q4']
cidades = ['Cidade A', 'Cidade B', 'Cidade C', 'Cidade D', 'Cidade E']
tipos_imovel = ['Residencial', 'Comercial', 'Industrial']
materiais = ['Barra Metal', 'Madeira', 'Pedra Britada', 'Fio Algodão', 'Cimento', 
             'Tecido', 'Papel', 'Poliéster', 'Couro', 'Areia']

# Função para gerar valores aleatórios de quantidade e valor
def gerar_quantidade_valor():
    quantidade = random.randint(10, 100)  # Quantidade aleatória entre 10 e 100 m³
    valor = quantidade * random.randint(100, 1000)  # Valor aleatório multiplicado pela quantidade
    return quantidade, valor

# Criando uma lista para armazenar os dados
dados = []

# Gerando os dados fictícios
for ano in anos:
    for trimestre in trimestres:
        for cidade in cidades:
            for tipo_imovel in tipos_imovel:
                for material in materiais:
                    quantidade, valor = gerar_quantidade_valor()
                    area_construida = random.randint(50, 200)  # Área construída aleatória entre 50 e 200 m²
                    dados.append([ano, trimestre, cidade, tipo_imovel, material, quantidade, valor, area_construida])

# Criando um DataFrame com os dados
df = pd.DataFrame(dados, columns=['Ano', 'Trimestre', 'Cidade', 'Tipo de Imóvel', 'Material Utilizado', 
                                  'Quantidade (m³)', 'Valor Material (R$)', 'Área Construída (m²)'])

# Exibindo as primeiras linhas do DataFrame
print(df.head(20))

# Caso queira salvar o DataFrame em um arquivo CSV
df.to_csv('dados_construcao.csv', index=False)
