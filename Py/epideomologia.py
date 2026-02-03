# 1. Instalação das bibliotecas (No Colab funciona sem erros)
import pandas as pd
import pysus as sus
import matplotlib.pyplot as plt
import seaborn as sns

# 2. CONFIGURAÇÃO - Ceará 2023 a 2025
# Nota: Dados de 2025 podem estar parciais no DATASUS
anos = [2023, 2024]
meses = range(1, 13)
uf = 'CE'
dfs = []

print(f"Iniciando coleta de dados do SIH para {uf}...")

for ano in anos:
    for mes in meses:
        try:
            # Baixa e converte automaticamente para DataFrame
            df = sus.data(uf, ano, mes)
            # Selecionamos apenas as colunas essenciais para economizar memória
            cols = ['MUNIC_RES', 'DIAG_PRINC', 'DT_INTER', 'SEXO', 'IDADE']
            dfs.append(df[cols])
            print(f"Sucesso: {mes:02d}/{ano}")
        except:
            continue
        
        # Unificando os dados
if dfs:
    dados_ce = pd.concat(dfs, ignore_index=True)
    
    # 3. FILTRAGEM: Doenças Contagiosas (CID-10: Cap I - códigos Iniciados em A ou B)
    # Exemplo: A90 (Dengue), A15 (Tuberculose), B24 (HIV)
    contagiosas = dados_ce[dados_ce['DIAG_PRINC'].str.startswith(('A', 'B'), na=False)].copy()

    # 4. RANKING DAS 10 MAIS COMUNS
    ranking = contagiosas['DIAG_PRINC'].value_counts().head(10).reset_index()
    ranking.columns = ['CID_10', 'Total']

    # 5. VISUALIZAÇÃO
    plt.figure(figsize=(12, 6))
    sns.barplot(data=ranking, x='Total', y='CID_10', palette='magma')
    plt.title(f'Top 10 Doenças Contagiosas no Ceará ({anos[0]}-{anos[-1]})')
    plt.grid(axis='x', linestyle='--', alpha=0.7)
    plt.show()
    
    print("\nResumo dos dados filtrados:")
    print(ranking)
else:
    print("Erro: Nenhum dado foi baixado. Verifique a conexão com o DATASUS.")