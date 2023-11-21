from flask import Flask, render_template
import pyodbc
import pandas as pd

app = Flask(__name__)

# Configurações do SQL Server
server = 'NOVACE-TI\DEV'
username = 'sa'
password = 'senha1234'
database = 'TESTE'
driver = 'ODBC Driver 17 for SQL Server'

# Conectar ao SQL Server
conn = pyodbc.connect(f'DRIVER={driver};SERVER={server};DATABASE={database};UID={username};PWD={password}')

@app.route('/')
def index():
    # Consulta SQL para obter dados do SQL Server
    query = "select * from SANDOZ_OL2"
    data = pd.read_sql(query, conn)
    return render_template('index.html', data=data.to_html())

if __name__ == '__main__':
    app.run(debug=True)
