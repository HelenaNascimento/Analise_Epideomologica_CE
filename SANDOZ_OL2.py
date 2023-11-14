import streamlit as st #https://extras.streamlit.app/
import pyodbc as bd
import pandas as pd

server = 'NOVACE-TI\DEV'
username = 'sa'
password = 'senha@1234'
database = 'TESTE'

cnxn = bd.connect('DRIVER={SQL Server};SERVER='+server+';DATABASE='+database+';UID='+username+';PWD='+ password)
cursor = cnxn.cursor()

query = "select * from SANDOZ_OL2"

st.set_page_config(layout="wide")

df = pd.read_sql(query, cnxn)
df["Dat_Ent"] = pd.to_datetime(df["Dat_Ent"])
df=df.sort_values("Dat_Ent")
df["Month"] = df["Dat_Ent"].apply(lambda x: str(x.year) + "-" + str(x.month))
month = st.sidebar.selectbox("Mês", df["Month"].unique())

df_filtered = df[df["Month"] == month]
df_filtered
