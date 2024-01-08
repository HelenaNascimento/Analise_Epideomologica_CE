import shutil
import schedule
import time
import os

def copiar_e_colar_arquivo():
    origem = r'Z:\d$\BKP_DIARIO_FILL.bak'  # Substitua pelo caminho correto
    destino = r'D:\NOVA\BKP_DIARIO_FILL.bak'  # Substitua pelo caminho correto

    try:
        # Copia o arquivo
        shutil.copy(origem, destino)
        print(f'Arquivo copiado de {origem} para {destino}')
    except Exception as e:
        print(f'Erro ao copiar o arquivo: {str(e)}')

# Agendando a tarefa para ser executada diariamente às 08:30
schedule.every().day.at("04:00").do(copiar_e_colar_arquivo)

while True:
    schedule.run_pending()
    time.sleep(1)
