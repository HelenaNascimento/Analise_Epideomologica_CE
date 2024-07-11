INSERT INTO R_CLXGC ([Cod_CliPag]
      ,[Cod_GrpCli]
      ,[CodAnt]
      ,[NovoCodigo])
SELECT [Cod_CliPag]
      ,[Cod_GrpCli]
      ,[CodAnt]
      ,[NovoCodigo]
  FROM PROD_2023.[dbo].[CLXGC]