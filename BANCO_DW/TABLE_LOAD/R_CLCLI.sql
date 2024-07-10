INSERT INTO [R_CLCLI]([Codigo]
      ,[Descricao]
      ,[Des_Cor]
      ,[Vlr_FaiIni]
      ,[Vlr_FaiFin])

SELECT [Codigo]
      ,[Descricao]
      ,[Des_Cor]
      ,[Vlr_FaiIni]
      ,[Vlr_FaiFin]
  FROM PROD_2023.[dbo].[CLCLI]