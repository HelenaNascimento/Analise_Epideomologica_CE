import streamlit as st #https://docs.streamlit.io/
import pyodbc as bd
import pandas as pd
import datetime

server = 'NOVACE-TI\DEV'
username = 'sa'
password = 'senha1234'
database = 'TESTE'

cnxn = bd.connect('DRIVER={SQL Server};SERVER='+server+';DATABASE='+database+';UID='+username+';PWD='+ password)
cursor = cnxn.cursor()

st.set_page_config(layout="wide")

today = datetime.datetime.now()
next_year = today.year 
jan_1 = datetime.date(next_year, 1, 1)
dec_31 = datetime.date(next_year, 12, 31)

st.sidebar.title('Menu')

st.session_state.horizontal = True
genre = st.sidebar.radio(
    "Agrupar por: ",
    ['Fabricante', 'Política'],
    index=None,   
    horizontal=st.session_state.horizontal
)
if genre == 'Fabricante' :

    opcaofabricante = st.sidebar.multiselect(
        'Fabricante: ',
        ['Eurofarma_GEN', 'Eurofarma_OTC', 'Teuto', 'Natulab', 'Nova_Quim_GEN', 'Biolab-GEN', 'Essity', 'Cremer', 'Geolab', 'Sandoz'],
        ['Eurofarma_GEN'])

elif genre == 'Política':
    
    opcaopolcom = st.sidebar.multiselect(
        'Política: ',
        [   'CE - ESSITY X REDES',
            'CE - CREMER ECONOMIA',
            'CE - ND BIOLAB',
            'CE - OL 001',
            'CE - ND SANDOZ',
            'CE - OL 002',
            'CE - ND NOVA QUIMICA',
            'CE- RODADA DE NEGÓCIOS',
            'CE - ESSITY X ECONOMIA',
            'CE - ND NATULAB',
            'CE - OL PROMOÇÃO DA SEMAN',
            'CE - ND TEUTO',
            'CE - EUROFARMA 6%',
            'CE - OL PROMO FECHAMENTO',
            'CE - OL ESSITY',
            'CE - OL INVESTIMENTO',
            'CE- PROMOÇÃO VALID PROXIM',
            'CE - OL NCO01',
            'CE - 0000',
            'CE - EUROFARMA G7'],
        ['CE - EUROFARMA G7'])

opcaoapresentacao = st.sidebar.selectbox('Modo Exibição: ',  ['Tabela', 'Deashboard'])

if opcaoapresentacao == 'Tabela' :

    query = "select * from SANDOZ_OL2" 
    df = pd.read_sql(query, cnxn)
    st.title('Dados do SQL Server')
    st.dataframe(df.style.format({'Est.Dispo': '{:.0f}'}))

elif opcaoapresentacao == ':gray[Deashboard]' :
    st.title('Deash')
    

opcaodata = st.sidebar.date_input ('Período: ',
        (jan_1, datetime.date(next_year, 1, 15)),
        jan_1,
        dec_31,
        format="DD.MM.YYYY",
    )


st.divider()
st.write(':blue[Exibição:] ', opcaoapresentacao, '/', ':blue[Período:] ', opcaodata, '/', ":blue[Agrupamento Por:]", genre )