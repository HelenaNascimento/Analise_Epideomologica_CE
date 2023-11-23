import streamlit as st #https://docs.streamlit.io/
import pyodbc as bd
import pandas as pd

server = 'NOVACE-TI\DEV'
username = 'sa'
password = 'senha1234'
database = 'TESTE'

cnxn = bd.connect('DRIVER={SQL Server};SERVER='+server+';DATABASE='+database+';UID='+username+';PWD='+ password)
cursor = cnxn.cursor()

st.set_page_config(layout="wide")



st.sidebar.title('Menu')

opcao = st.sidebar.selectbox('Opção: ',  ['Tabela', 'Deashboard'])

if opcao == 'Tabela' :

    query = "select * from SANDOZ_OL2" 
    df = pd.read_sql(query, cnxn)
    st.title('Dados do SQL Server')
    st.dataframe(df.style.format({'Est.Dispo': '{:.0f}'}))

elif opcao == 'Deashboard' :
    st.title('Deash')
