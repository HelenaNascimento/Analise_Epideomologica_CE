INSERT INTO [DW_PROD].[dbo].[R_CLASS] (Codigo, Descricao, Nivel, CodAnt, NovoCodigo, Flg_BlqCtrDocClaPrd, Cod_EnqIpi)
  SELECT Codigo, Descricao, Nivel, CodAnt, NovoCodigo, Flg_BlqCtrDocClaPrd, Cod_EnqIpi FROM PROD_2023.DBO.CLASS 
