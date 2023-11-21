import streamlit as st #https://extras.streamlit.app/
import pyodbc as bd
import pandas as pd
#import flask as fk



server = 'NOVACE-TI\DEV'
username = 'sa'
password = 'senha1234'
database = 'TESTE'

cnxn = bd.connect('DRIVER={SQL Server};SERVER='+server+';DATABASE='+database+';UID='+username+';PWD='+ password)
cursor = cnxn.cursor()

query = "select * from SANDOZ_OL2"

st.set_page_config(layout="wide")

df = pd.read_sql(query, cnxn)

st.title('Dados do SQL Server')
st.dataframe(df.style.format({'Est.Dispo': '{:.0f}'}))

