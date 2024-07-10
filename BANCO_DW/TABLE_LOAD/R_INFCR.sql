INSERT INTO [R_INFCR]([Codigo]
      ,[Descricao]
      ,[Controle]
      ,[CodAnt]
      ,[NovoCodigo])
SELECT [Codigo]
      ,[Descricao]
      ,[Controle]
      ,[CodAnt]
      ,[NovoCodigo]
FROM PROD_2023.[dbo].INFCR