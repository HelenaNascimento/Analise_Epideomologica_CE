
INSERT INTO [R_BAIRR] ([Cod_Estado]
      ,[Cod_Cidade]
      ,[Codigo]
      ,[Descricao]
      ,[Cod_MacroReg]
      ,[Cod_MicroReg]
      ,[Flg_Excluido]
      ,[Versao])
SELECT [Cod_Estado]
      ,[Cod_Cidade]
      ,[Codigo]
      ,[Descricao]
      ,[Cod_MacroReg]
      ,[Cod_MicroReg]
      ,[Flg_Excluido]
      ,[Versao]
  FROM PROD_2023.[dbo].BAIRR