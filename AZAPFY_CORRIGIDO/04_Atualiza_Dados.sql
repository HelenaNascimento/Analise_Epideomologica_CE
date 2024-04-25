-- ATUALIZA DADOS valor da nota
Update tb 
Set tb.VALOR_NOTA = nf.Vlr_TotalNota
From DMD_AZAPFY.dbo.tb_envio tb
Inner Join PROD_2023.dbo.ESTAB es On es.Num_Cnpj = tb.REMETENTE_CNPJ
Inner Join PROD_2023.dbo.NFSCB nf On nf.Cod_Estabe = es.Cod_Estabe and nf.Ser_Nota = convert(varchar(3),tb.SERIE_NOTA) and nf.Num_Nota = tb.NUMERO_NOTA
Where tb.VALOR_NOTA <> nf.Vlr_TotalNota
GO


USE PROD_2023
--------------------------------------------------
-- ATIVAR PARAMETROS INTEGRACAO AZAPFY
--------------------------------------------------
Insert Into FS_PARAM (Cod_Estabe,Cod_Param,Tip_Param,Val_Param,Dsc_Param)
Select e.Cod_Estabe,'AZAPFY_STATUS' as Cod_Param,'T','ATIVO','Status da integra��o de documentos AZAPFY' 
From ESTAB e
Where Not Exists(Select 1 from FS_PARAM x 
                 Where x.Cod_Estabe = e.Cod_Estabe 
				 and Cod_Param = 'AZAPFY_STATUS')
GO

